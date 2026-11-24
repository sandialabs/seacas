/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * File-supplied name-string lengths must not produce negative name sizes for
 * Exodus name reads.
 */

#include "exodusII.h"
#include "exodusII_int.h"
#include <netcdf.h>
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

static const char *zero_len_name_file = "name-string-len-zero.exo";
static const char *direct_name_file   = "name-string-len-direct.exo";

static int create_zero_len_name_file(void)
{
  int ncid;
  int dimid;
  NCCHECK(nc_create(zero_len_name_file, NC_CLOBBER, &ncid));
  NCCHECK(nc_def_dim(ncid, DIM_STR_NAME, NC_UNLIMITED, &dimid));

  float version       = EXODUS_VERSION_MAJOR + EXODUS_VERSION_MINOR / 100.0F;
  int   file_wordsize = 8;
  NCCHECK(nc_put_att_float(ncid, NC_GLOBAL, ATT_VERSION, NC_FLOAT, 1, &version));
  NCCHECK(nc_put_att_int(ncid, NC_GLOBAL, ATT_FLT_WORDSIZE, NC_INT, 1, &file_wordsize));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int check_zero_len_name_inquiry(void)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(zero_len_name_file, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }

  int length = ex_inquire_int(exoid, EX_INQ_DB_MAX_ALLOWED_NAME_LENGTH);
  EXCHECK(ex_close(exoid));

  if (length != 32) {
    fprintf(stderr, "zero-length name dimension returned %d, expected 32\n", length);
    return 1;
  }
  return 0;
}

static int create_direct_name_file(void)
{
  int ncid;
  int dims[2];
  int varid;
  NCCHECK(nc_create(direct_name_file, NC_CLOBBER, &ncid));
  NCCHECK(nc_def_dim(ncid, "num_names", 1, &dims[0]));
  NCCHECK(nc_def_dim(ncid, DIM_STR_NAME, 6, &dims[1]));
  NCCHECK(nc_def_var(ncid, "name_direct", NC_CHAR, 2, dims, &varid));
  NCCHECK(nc_enddef(ncid));

  size_t start[2] = {0, 0};
  size_t count[2] = {1, 6};
  NCCHECK(nc_put_vara_text(ncid, varid, start, count, "alpha"));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int check_negative_name_size_rejected(void)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(direct_name_file, NC_NOWRITE, &ncid));
  NCCHECK(nc_inq_varid(ncid, "name_direct", &varid));

  char name[8] = "sentinel";
  int  status  = exi_get_name(ncid, varid, 0, name, -1, EX_ELEM_BLOCK, __func__);
  NCCHECK(nc_close(ncid));

  if (status != EX_FATAL) {
    fprintf(stderr, "negative name size returned status %d, expected EX_FATAL\n", status);
    return 1;
  }
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  int failures = 0;
  if (create_zero_len_name_file() != 0) {
    failures++;
  }
  else if (check_zero_len_name_inquiry() != 0) {
    failures++;
  }

  if (create_direct_name_file() != 0) {
    failures++;
  }
  else if (check_negative_name_size_rejected() != 0) {
    failures++;
  }

  if (failures != 0) {
    return 1;
  }

  printf("test-name-string-len: all checks passed\n");
  return 0;
}
