/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Concatenated set-length inquiries must propagate fatal metadata errors instead
 * of returning success with a zero or partial length.
 */

#include "exodusII.h"
#include <inttypes.h>
#include <netcdf.h>
#include <stdint.h>
#include <stdio.h>

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    if ((funcall) != EX_NOERR) {                                                                   \
      fprintf(stderr, "%s failed\n", #funcall);                                                    \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

#define NCCHECK(funcall)                                                                           \
  do {                                                                                             \
    int nc_status_ = (funcall);                                                                    \
    if (nc_status_ != NC_NOERR) {                                                                  \
      fprintf(stderr, "%s failed: %s\n", #funcall, nc_strerror(nc_status_));                       \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

static const char *filename = "inquire-concat-set-len.exo";

static int define_set_metadata(int ncid, const char *set_count_dim, const char *status_var,
                               int status_type, const char *entry_dim, int define_entry_dim,
                               const char *df_dim, int define_df_dim, int *status_varid)
{
  int set_dimid;
  int dimids[1];
  NCCHECK(nc_def_dim(ncid, set_count_dim, 1, &set_dimid));
  dimids[0] = set_dimid;
  NCCHECK(nc_def_var(ncid, status_var, status_type, 1, dimids, status_varid));

  if (define_entry_dim) {
    int entry_dimid;
    NCCHECK(nc_def_dim(ncid, entry_dim, status_type == NC_INT ? 2 : 1, &entry_dimid));
  }
  if (define_df_dim) {
    int df_dimid;
    NCCHECK(nc_def_dim(ncid, df_dim, 1, &df_dimid));
  }

  return 0;
}

static int write_status(int ncid, int status_varid, int status_type)
{
  if (status_type == NC_STRING) {
    const char *active = "active";
    NCCHECK(nc_put_var_string(ncid, status_varid, &active));
  }
  else {
    int active = 1;
    NCCHECK(nc_put_var_int(ncid, status_varid, &active));
  }
  return 0;
}

static int create_metadata_file(int corrupt)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid =
      ex_create(filename, EX_CLOBBER | EX_NETCDF4 | EX_NOCLASSIC, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "inquire concat set length", 2, 0, 0, 0, 0, 0));
  EXCHECK(ex_close(exoid));

  int ncid;
  int ns_status_varid;
  int ss_status_varid;
  int es_status_varid;
  int fs_status_varid;
  int els_status_varid;

  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));

  if (define_set_metadata(ncid, "num_node_sets", "ns_status", NC_INT, "num_nod_ns1", !corrupt, NULL,
                          0, &ns_status_varid) != 0) {
    return 1;
  }
  if (define_set_metadata(ncid, "num_side_sets", "ss_status", NC_INT, "num_side_ss1", !corrupt,
                          NULL, 0, &ss_status_varid) != 0) {
    return 1;
  }

  int edge_status_type = corrupt ? NC_STRING : NC_INT;
  if (define_set_metadata(ncid, "num_edge_sets", "es_status", edge_status_type, "num_edge_es1", 1,
                          "num_df_es1", 1, &es_status_varid) != 0) {
    return 1;
  }

  int face_status_type = corrupt ? NC_STRING : NC_INT;
  if (define_set_metadata(ncid, "num_face_sets", "fs_status", face_status_type, "num_face_fs1", 1,
                          "num_df_fs1", 1, &fs_status_varid) != 0) {
    return 1;
  }

  int elem_status_type = corrupt ? NC_STRING : NC_INT;
  if (define_set_metadata(ncid, "num_elem_sets", "els_status", elem_status_type, "num_ele_els1", 1,
                          "num_df_els1", 1, &els_status_varid) != 0) {
    return 1;
  }

  NCCHECK(nc_enddef(ncid));

  if (write_status(ncid, ns_status_varid, NC_INT) != 0 ||
      write_status(ncid, ss_status_varid, NC_INT) != 0 ||
      write_status(ncid, es_status_varid, edge_status_type) != 0 ||
      write_status(ncid, fs_status_varid, face_status_type) != 0 ||
      write_status(ncid, els_status_varid, elem_status_type) != 0) {
    return 1;
  }

  NCCHECK(nc_close(ncid));
  return 0;
}

struct inquiry_check
{
  ex_inquiry  query;
  const char *label;
  int         expected;
};

static const struct inquiry_check checks[] = {
    {EX_INQ_NS_NODE_LEN, "EX_INQ_NS_NODE_LEN", 2}, {EX_INQ_SS_ELEM_LEN, "EX_INQ_SS_ELEM_LEN", 2},
    {EX_INQ_ES_LEN, "EX_INQ_ES_LEN", 2},           {EX_INQ_ES_DF_LEN, "EX_INQ_ES_DF_LEN", 1},
    {EX_INQ_FS_LEN, "EX_INQ_FS_LEN", 2},           {EX_INQ_FS_DF_LEN, "EX_INQ_FS_DF_LEN", 1},
    {EX_INQ_ELS_LEN, "EX_INQ_ELS_LEN", 2},         {EX_INQ_ELS_DF_LEN, "EX_INQ_ELS_DF_LEN", 1},
};

static int open_file(void)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  return ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
}

static int check_good_inquiry(const struct inquiry_check *check)
{
  int exoid = open_file();
  if (exoid < 0) {
    return 1;
  }

  int   ret_int     = -1;
  float ret_float   = 0.0F;
  char  ret_char[1] = {'\0'};
  int   status      = ex_inquire(exoid, check->query, &ret_int, &ret_float, ret_char);
  EXCHECK(ex_close(exoid));
  if (status != EX_NOERR || ret_int != check->expected) {
    fprintf(stderr, "%s returned status %d and length %d for valid metadata\n", check->label,
            status, ret_int);
    return 1;
  }

  exoid = open_file();
  if (exoid < 0) {
    return 1;
  }
  int64_t ret_int64 = ex_inquire_int(exoid, check->query);
  EXCHECK(ex_close(exoid));
  if (ret_int64 != check->expected) {
    fprintf(stderr, "%s via ex_inquire_int returned length %" PRId64 " for valid metadata\n",
            check->label, ret_int64);
    return 1;
  }

  return 0;
}

static int check_bad_inquiry(const struct inquiry_check *check)
{
  int exoid = open_file();
  if (exoid < 0) {
    return 1;
  }

  int   ret_int     = -99;
  float ret_float   = 0.0F;
  char  ret_char[1] = {'\0'};
  int   status      = ex_inquire(exoid, check->query, &ret_int, &ret_float, ret_char);
  EXCHECK(ex_close(exoid));
  if (status != EX_FATAL) {
    fprintf(stderr, "%s returned status %d and length %d for invalid metadata\n", check->label,
            status, ret_int);
    return 1;
  }

  exoid = open_file();
  if (exoid < 0) {
    return 1;
  }
  int64_t ret_int64 = ex_inquire_int(exoid, check->query);
  EXCHECK(ex_close(exoid));
  if (ret_int64 != EX_FATAL) {
    fprintf(stderr, "%s via ex_inquire_int returned length %" PRId64 " for invalid metadata\n",
            check->label, ret_int64);
    return 1;
  }

  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_metadata_file(0) != 0) {
    return 1;
  }
  for (size_t i = 0; i < sizeof(checks) / sizeof(checks[0]); i++) {
    if (check_good_inquiry(&checks[i]) != 0) {
      return 1;
    }
  }

  if (create_metadata_file(1) != 0) {
    return 1;
  }
  for (size_t i = 0; i < sizeof(checks) / sizeof(checks[0]); i++) {
    if (check_bad_inquiry(&checks[i]) != 0) {
      return 1;
    }
  }

  printf("test-inquire-concat-set-len: all checks passed\n");
  return 0;
}
