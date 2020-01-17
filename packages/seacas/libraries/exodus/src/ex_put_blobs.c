/*
 * Copyright (c) 2005-2017 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of NTESS nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "exodusII.h"
#include "exodusII_int.h"
#include <stdbool.h>
/*!
 * writes the blob parameters and optionally blob data for 1 or more blobs
 * \param   exoid                   exodus file id
 * \param  *blob                array of ex_blob structures
 */

int ex_put_blobs(int exoid, size_t count, const struct ex_blob *blobs)
{
  int  dimid, status, n1dim, dims[1];
  char errmsg[MAX_ERR_LENGTH];

  int int_type;

  EX_FUNC_ENTER();

  ex__check_valid_file_id(exoid, __func__);

  /* Note that this routine can be called:
     1) just define the blobs
     2) just output the blob data (after a previous call to define)
     3) define and output the blob data in one call.
  */

  int *entlst_id = (int *)calloc(count, sizeof(int));

  if ((status = nc_redef(exoid)) != NC_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to put file id %d into define mode", exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  status = nc_inq_dimid(exoid, DIM_N1, &n1dim);
  if (status != NC_NOERR) {
    if ((status = nc_def_dim(exoid, DIM_N1, 1L, &n1dim)) != NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to define number \"1\" dimension in file id %d", exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      goto error_ret; /* exit define mode and return */
    }
  }

  for (size_t i = 0; i < count; i++) {
    char *numentryptr = DIM_NUM_VALUES_BLOB(blobs[i].id);

    /* define dimensions and variables */
    if ((status = nc_def_dim(exoid, numentryptr, blobs[i].num_entry, &dimid)) != NC_NOERR) {
      if (status == NC_ENAMEINUSE) {
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: blob %" PRId64 " -- size already defined in file id %d", blobs[i].id,
                 exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
      }
      else {
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: failed to define number of entries in blob %" PRId64 " in file id %d",
                 blobs[i].id, exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
      }
      goto error_ret;
    }

    int_type = NC_INT;
    if (ex_int64_status(exoid) & EX_IDS_INT64_DB) {
      int_type = NC_INT64;
    }

    /* create a variable just as a way to have a blob and its attributes; values not used for
     * anything */
    dims[0] = n1dim;
    if ((status = nc_def_var(exoid, VAR_ENTITY_BLOB(blobs[i].id), NC_INT, 1, dims,
                             &entlst_id[i])) != NC_NOERR) {
      if (status == NC_ENAMEINUSE) {
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: entity already exists for blob %" PRId64 " in file id %d", blobs[i].id,
                 exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
      }
      else {
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: failed to create entity for blob %" PRId64 " in file id %d", blobs[i].id,
                 exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
      }
      goto error_ret; /* exit define mode and return */
    }
    ex__compress_variable(exoid, entlst_id[i], 1);

    if (ex_int64_status(exoid) & EX_IDS_INT64_DB) {
      status = nc_put_att_longlong(exoid, entlst_id[i], EX_ATTRIBUTE_ID, NC_INT64, 1, &blobs[i].id);
    }
    else {
      int id = blobs[i].id;
      status = nc_put_att_int(exoid, entlst_id[i], EX_ATTRIBUTE_ID, NC_INT, 1, &id);
    }
    if (status != NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to store blob id %" PRId64 " in file id %d",
               blobs[i].id, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      goto error_ret; /* exit define mode and return */
    }

    if ((status = nc_put_att_text(exoid, entlst_id[i], EX_ATTRIBUTE_NAME, strlen(blobs[i].name) + 1,
                                  blobs[i].name)) != NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to store blob name %s in file id %d",
               blobs[i].name, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      goto error_ret; /* exit define mode and return */
    }

    /* Increment blob count */
    struct ex__file_item *file = ex__find_file_item(exoid);
    if (file) {
      file->blob_count++;
    }
  }
  /* leave define mode  */
  if ((status = ex__leavedef(exoid, __func__)) != NC_NOERR) {
    free(entlst_id);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  free(entlst_id);
  EX_FUNC_LEAVE(EX_NOERR);

/* Fatal error: exit definition mode and return */
error_ret:
  ex__leavedef(exoid, __func__);
  free(entlst_id);
  EX_FUNC_LEAVE(EX_FATAL);
}
