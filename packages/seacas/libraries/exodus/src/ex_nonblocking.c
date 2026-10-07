/*
 * Copyright(C) 1999-2020, 2024 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Non-blocking parallel output support, layered on PnetCDF's
 * ncmpi_iput_vara_* / ncmpi_wait_all interface.
 *
 * Rationale
 * ---------
 * Exodus normally writes variable values with nc_put_vara_{float,double}(),
 * which netcdf-c turns into the blocking collective ncmpi_put_vara_*_all()
 * when the file is backed by PnetCDF.  That is one collective MPI-IO
 * operation per (entity block x variable x time step), and a process
 * contributing a zero-length request has historically been able to fall out
 * of step with the others (see PnetCDF PR #239).
 *
 * Posting the same writes with the non-blocking interface and completing
 * them with a single ncmpi_wait_all() removes the per-call collective
 * entirely and lets PnetCDF merge the requests into one file view.
 *
 * Applicability
 * -------------
 * This is a PnetCDF-specific optimization.  It is active only when all of:
 *
 *   - the netCDF library was built with PnetCDF support (NC_HAS_PNETCDF),
 *   - the file was opened for parallel output, and
 *   - the file is *actually* in PnetCDF format, as reported by
 *     nc_inq_format_extended() == NC_FORMATX_PNETCDF.
 *
 * A netCDF-4/HDF5 file doing parallel I/O through HDF5, a serial classic
 * file, or a netCDF library built without PnetCDF all fail that test, and
 * every caller transparently falls back to the ordinary blocking path.
 * Nothing outside this file needs to know which case applies.
 *
 * Buffer ownership
 * ----------------
 * ncmpi_iput_vara_*() does not copy the caller's buffer; it records a
 * pointer that is dereferenced later, at wait time.  The Exodus API
 * promises that the caller may free or reuse its array as soon as the put
 * returns, and callers rely on that (PETSc's DMPlex writer, for instance,
 * hands over a temporary subvector's array and destroys it immediately).
 * So values are copied into a staging arena owned by this file and the
 * request points at the copy.
 *
 * The arena is a list of chunks that is only ever appended to.  It is never
 * realloc'd while requests are outstanding, because that would move the
 * bytes PnetCDF is holding pointers to.
 *
 * Flushing
 * --------
 * Pending requests must be completed before anything disturbs the file
 * header or closes the file:
 *
 *   - entering define mode (exi_redef/exi_persist_redef) -- PnetCDF's
 *     request records hold a raw pointer to the NC_var and a file offset,
 *     and enddef may move both.
 *   - ex_update() -- nc_sync() does not complete outstanding requests.
 *   - ex_close()  -- PnetCDF *cancels* pending put requests at close and
 *                    only prints a warning, so the data would be lost.
 *
 * ncmpi_wait_all() is collective, so every process must reach each flush.
 * The request *counts* may differ between processes (that is the whole
 * point), but the calls themselves may not be skipped.  To guarantee that,
 * the PnetCDF file id is resolved once when the option is enabled -- a
 * collective point -- rather than lazily on first write, so that a process
 * which happens to post nothing still knows it must participate.
 */

#include "exodusII.h"
#include "exodusII_int.h"

#include <stdlib.h>
#include <string.h>

#if NC_HAS_PNETCDF
#include <pnetcdf.h>

/*
 * netcdf-c keeps an external id (what it hands back to us) and an internal
 * id (what the backend library uses).  For a PnetCDF-backed file they are
 * different, and ncmpi_* calls need the internal one.  netcdf-c declares
 * the containing struct in include/nc.h, which is not installed, but
 * NC_check_id() is an exported symbol and the first members of the struct
 * have been stable for many releases.
 *
 * Only ext_ncid and int_ncid are read.  ext_ncid is verified against the id
 * we passed in: if netcdf-c ever reorders these members the check fails, we
 * report no PnetCDF id, and every caller quietly uses the blocking path.
 * A layout change therefore costs performance, never correctness.
 */
struct exi_nc_prefix
{
  int         ext_ncid;
  int         int_ncid;
  const void *dispatch;
  void       *dispatchdata;
};

extern int NC_check_id(int ncid, struct exi_nc_prefix **ncpp);

/* Staging arena chunk.  Allocated once, never moved, never grown. */
struct exi_nb_chunk
{
  struct exi_nb_chunk *next;
  size_t               capacity;
  size_t               used;
  char                *data;
};

#define EXI_NB_MIN_CHUNK (1024 * 1024) /* 1 MiB */
#define EXI_NB_REQ_GROW  64

/*!
 * \internal
 * Resolve the PnetCDF file id behind an exodus file id.
 * Returns EX_NOERR and sets \p pncid when the file really is PnetCDF-backed;
 * returns EX_FATAL (without emitting an error) otherwise.
 */
static int exi_nb_resolve_pncid(int exoid, int *pncid)
{
  int                   format = 0;
  int                   mode   = 0;
  struct exi_nc_prefix *ncp    = NULL;
  int                   root_id;

  if (nc_inq_format_extended(exoid, &format, &mode) != NC_NOERR) {
    return EX_FATAL;
  }
  if (format != NC_FORMATX_PNETCDF) {
    /* netCDF-4/HDF5, classic serial, or anything else: not our business. */
    return EX_FATAL;
  }

  root_id = (int)((unsigned)exoid & EX_FILE_ID_MASK);
  if (NC_check_id(root_id, &ncp) != NC_NOERR || ncp == NULL) {
    return EX_FATAL;
  }

  /* Self-check against struct layout drift in netcdf-c (see comment above). */
  if (ncp->ext_ncid != root_id) {
    return EX_FATAL;
  }

  *pncid = ncp->int_ncid;
  return EX_NOERR;
}

/*!
 * \internal
 * Enable or disable non-blocking output for a file.  Enabling resolves the
 * PnetCDF id up front so that all processes agree on whether they must
 * participate in the collective flushes.  If the file is not PnetCDF-backed
 * the request is silently ignored and the blocking path stays in use.
 */
int exi_nb_set_enabled(int exoid, int enable)
{
  struct exi_file_item *file = exi_find_file_item(exoid);
  if (!file) {
    return EX_FATAL;
  }

  if (!enable) {
    if (file->use_nonblocking) {
      if (exi_nb_flush(exoid) != EX_NOERR) {
        return EX_FATAL;
      }
    }
    file->use_nonblocking = 0;
    return EX_NOERR;
  }

  if (file->use_nonblocking) {
    return EX_NOERR; /* already on */
  }

  /* Note: file->is_write is only set from the EX_WRITE bit, which callers of
     ex_create_par() do not pass, so it says nothing useful about a freshly
     created file and is deliberately not tested here.  Resolving the PnetCDF
     id below is the real applicability test. */
  if (!file->is_parallel || !file->is_pnetcdf) {
    return EX_NOERR; /* not applicable; stay blocking */
  }

  {
    int pncid = -1;
    if (exi_nb_resolve_pncid(exoid, &pncid) != EX_NOERR) {
      return EX_NOERR; /* not PnetCDF-backed after all; stay blocking */
    }
    file->nb_pncid        = pncid;
    file->use_nonblocking = 1;
  }
  return EX_NOERR;
}

/*!
 * \internal
 * Is non-blocking output active for this file?
 */
int exi_nb_enabled(int exoid)
{
  struct exi_file_item *file = exi_find_file_item(exoid);
  return (file && file->use_nonblocking && file->nb_pncid >= 0) ? 1 : 0;
}

/*! \internal Copy \p nbytes of \p data into the staging arena. */
static void *exi_nb_stage(struct exi_file_item *file, const void *data, size_t nbytes)
{
  struct exi_nb_chunk *chunk = (struct exi_nb_chunk *)file->nb_chunks;
  void                *dest;

  /* Reuse the head chunk while it has room; otherwise push a fresh one.
     Existing chunks are never realloc'd -- PnetCDF holds pointers into them. */
  if (chunk == NULL || chunk->capacity - chunk->used < nbytes) {
    size_t               capacity = nbytes > EXI_NB_MIN_CHUNK ? nbytes : EXI_NB_MIN_CHUNK;
    struct exi_nb_chunk *fresh    = (struct exi_nb_chunk *)malloc(sizeof(struct exi_nb_chunk));
    if (fresh == NULL) {
      return NULL;
    }
    fresh->data = (char *)malloc(capacity);
    if (fresh->data == NULL) {
      free(fresh);
      return NULL;
    }
    fresh->capacity = capacity;
    fresh->used     = 0;
    fresh->next     = chunk;
    file->nb_chunks = fresh;
    chunk           = fresh;
  }

  dest = chunk->data + chunk->used;
  memcpy(dest, data, nbytes);
  chunk->used += nbytes;
  return dest;
}

/*! \internal Remember a request id so a later flush can wait on it. */
static int exi_nb_track(struct exi_file_item *file, int req)
{
  if (file->nb_nreqs == file->nb_maxreqs) {
    int  grow = file->nb_maxreqs ? file->nb_maxreqs * 2 : EXI_NB_REQ_GROW;
    int *reqs = (int *)realloc(file->nb_reqs, (size_t)grow * sizeof(int));
    if (reqs == NULL) {
      return EX_FATAL;
    }
    file->nb_reqs    = reqs;
    file->nb_maxreqs = grow;
  }
  file->nb_reqs[file->nb_nreqs++] = req;
  return EX_NOERR;
}

/*! \internal Release every staging chunk. Only safe once no request is pending. */
static void exi_nb_release_chunks(struct exi_file_item *file)
{
  struct exi_nb_chunk *chunk = (struct exi_nb_chunk *)file->nb_chunks;
  while (chunk != NULL) {
    struct exi_nb_chunk *next = chunk->next;
    free(chunk->data);
    free(chunk);
    chunk = next;
  }
  file->nb_chunks = NULL;
}

/*!
 * \internal
 * Post a non-blocking write of a 2D (time, entity) hyperslab.  The values
 * are copied first, so \p data may be freed or reused as soon as this
 * returns, exactly as for the blocking path.
 *
 * \param exoid     exodus file id
 * \param varid     netCDF variable id
 * \param start     [2] hyperslab origin
 * \param count     [2] hyperslab extent; a zero extent is legal and posts nothing
 * \param is_float  non-zero if \p data is float rather than double
 * \param data      caller's values
 */
int exi_nb_put_vara(int exoid, int varid, const size_t *start, const size_t *count, int is_float,
                    const void *data)
{
  struct exi_file_item *file = exi_find_file_item(exoid);
  MPI_Offset            mstart[2];
  MPI_Offset            mcount[2];
  size_t                nvals;
  size_t                nbytes;
  const void           *buffer = data;
  int                   req    = NC_REQ_NULL;
  int                   status;

  if (!file || !file->use_nonblocking || file->nb_pncid < 0) {
    return EX_FATAL;
  }

  nvals  = count[0] * count[1];
  nbytes = nvals * (is_float ? sizeof(float) : sizeof(double));

  if (nbytes > 0) {
    buffer = exi_nb_stage(file, data, nbytes);
    if (buffer == NULL) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to allocate %zu bytes to stage a non-blocking write in file id %d",
               nbytes, exoid);
      ex_err_fn(exoid, __func__, errmsg, EX_MEMFAIL);
      return EX_FATAL;
    }
  }

  mstart[0] = (MPI_Offset)start[0];
  mstart[1] = (MPI_Offset)start[1];
  mcount[0] = (MPI_Offset)count[0];
  mcount[1] = (MPI_Offset)count[1];

  if (is_float) {
    status =
        ncmpi_iput_vara_float(file->nb_pncid, varid, mstart, mcount, (const float *)buffer, &req);
  }
  else {
    status =
        ncmpi_iput_vara_double(file->nb_pncid, varid, mstart, mcount, (const double *)buffer, &req);
  }

  if (status != NC_NOERR) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to post non-blocking write of variable %d in file id %d: %s", varid,
             exoid, ncmpi_strerror(status));
    ex_err_fn(exoid, __func__, errmsg, status);
    return EX_FATAL;
  }

  /* A zero-length request completes immediately and yields NC_REQ_NULL;
     there is nothing to wait on, so do not take up a slot for it. */
  if (req != NC_REQ_NULL) {
    if (exi_nb_track(file, req) != EX_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to grow the non-blocking request list for file id %d", exoid);
      ex_err_fn(exoid, __func__, errmsg, EX_MEMFAIL);
      return EX_FATAL;
    }
  }

  return EX_NOERR;
}

/*!
 * \internal
 * Complete every outstanding non-blocking request.
 *
 * Collective across the file's communicator.  Callers must not make the
 * call conditional on having posted anything: a process with no pending
 * requests still has to enter ncmpi_wait_all() with the others.
 *
 * Does not take the API lock, so it is safe to call from inside another
 * exodus entry point that already holds it.
 */
int exi_nb_flush(int exoid)
{
  struct exi_file_item *file = exi_find_file_item(exoid);
  int                  *statuses;
  int                   result = EX_NOERR;
  int                   status;
  int                   i;

  if (!file || !file->use_nonblocking || file->nb_pncid < 0) {
    return EX_NOERR;
  }

  /* netcdf-c leaves a PnetCDF file in *independent* data mode by default
     (libsrcp/ncpdispatch.c puts it there at open and after every enddef), and
     ncmpi_wait_all() is a collective call that is illegal there -- it returns
     NC_EINDEP.  Ask netcdf-c to switch the file to collective access rather
     than calling ncmpi_end_indep_data() directly, so that its own record of
     the access mode stays consistent with PnetCDF's.  The call is idempotent
     and returns immediately when the file is already collective.

     This is collective, which is fine: so is the flush.  The file is left in
     collective mode afterwards, which is what exodus already asks for on
     transient variables (see ex_open_par.c and exi_set_coll_access). */
  status = nc_var_par_access(exoid, NC_GLOBAL, NC_COLLECTIVE);
  if (status != NC_NOERR) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to select collective access before flushing non-blocking writes in "
             "file id %d",
             exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    return EX_FATAL;
  }

  statuses = file->nb_nreqs > 0 ? (int *)calloc((size_t)file->nb_nreqs, sizeof(int)) : NULL;
  if (file->nb_nreqs > 0 && statuses == NULL) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to allocate status array to flush %d non-blocking requests in file "
             "id %d",
             file->nb_nreqs, exoid);
    ex_err_fn(exoid, __func__, errmsg, EX_MEMFAIL);
    return EX_FATAL;
  }

  status = ncmpi_wait_all(file->nb_pncid, file->nb_nreqs, file->nb_reqs, statuses);
  if (status != NC_NOERR) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to complete %d non-blocking writes in file id %d: %s", file->nb_nreqs,
             exoid, ncmpi_strerror(status));
    ex_err_fn(exoid, __func__, errmsg, status);
    result = EX_FATAL;
  }
  else {
    for (i = 0; i < file->nb_nreqs; i++) {
      if (statuses[i] != NC_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: non-blocking write %d of %d failed in file id %d: %s", i + 1,
                 file->nb_nreqs, exoid, ncmpi_strerror(statuses[i]));
        ex_err_fn(exoid, __func__, errmsg, statuses[i]);
        result = EX_FATAL;
      }
    }
  }

  free(statuses);

  /* The requests are done either way; drop them and reclaim the staging
     space so a failure cannot leave stale pointers behind. */
  file->nb_nreqs = 0;
  exi_nb_release_chunks(file);

  return result;
}

/*! \internal Tear down the per-file non-blocking state. */
void exi_nb_free(struct exi_file_item *file)
{
  if (!file) {
    return;
  }
  exi_nb_release_chunks(file);
  free(file->nb_reqs);
  file->nb_reqs         = NULL;
  file->nb_nreqs        = 0;
  file->nb_maxreqs      = 0;
  file->nb_pncid        = -1;
  file->use_nonblocking = 0;
}

#else /* !NC_HAS_PNETCDF -- no PnetCDF in this netCDF build */

int exi_nb_set_enabled(int exoid, int enable)
{
  EX_UNUSED(exoid);
  EX_UNUSED(enable);
  return EX_NOERR; /* silently stays blocking */
}

int exi_nb_enabled(int exoid)
{
  EX_UNUSED(exoid);
  return 0;
}

int exi_nb_put_vara(int exoid, int varid, const size_t *start, const size_t *count, int is_float,
                    const void *data)
{
  EX_UNUSED(exoid);
  EX_UNUSED(varid);
  EX_UNUSED(start);
  EX_UNUSED(count);
  EX_UNUSED(is_float);
  EX_UNUSED(data);
  return EX_FATAL;
}

int exi_nb_flush(int exoid)
{
  EX_UNUSED(exoid);
  return EX_NOERR;
}

void exi_nb_free(struct exi_file_item *file) { EX_UNUSED(file); }

#endif /* NC_HAS_PNETCDF */
