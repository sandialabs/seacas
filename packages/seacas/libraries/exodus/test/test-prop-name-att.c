/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Property-name attributes must be bounded NC_CHAR values before property
 * lookup APIs read them into fixed-size name buffers.
 */

#include "exodusII.h"
#include "exodusII_int.h"
#include <netcdf.h>
#include <stdio.h>
#include <string.h>

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

static const char *filename = "prop-name-att.exo";

static int create_file(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "property name metadata", 3, 4, 1, 1, 0, 0));
  EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, 7, "QUAD4", 1, 4, 0, 0, 0));
  int connect[4] = {1, 2, 3, 4};
  EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, 7, connect, NULL, NULL));
  EXCHECK(ex_put_prop(exoid, EX_ELEM_BLOCK, 7, "quality", 11));
  EXCHECK(ex_close(exoid));
  return 0;
}

static int open_file(int mode)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, mode, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return -1;
  }
  return exoid;
}

static int expect_valid_properties(void)
{
  int exoid = open_file(EX_READ);
  if (exoid < 0) {
    return 1;
  }

  int value = 0;
  EXCHECK(ex_get_prop(exoid, EX_ELEM_BLOCK, 7, "quality", &value));
  if (value != 11) {
    fprintf(stderr, "property value %d, expected 11\n", value);
    ex_close(exoid);
    return 1;
  }

  int values[1] = {0};
  EXCHECK(ex_get_prop_array(exoid, EX_ELEM_BLOCK, "quality", values));
  if (values[0] != 11) {
    fprintf(stderr, "property array value %d, expected 11\n", values[0]);
    ex_close(exoid);
    return 1;
  }

  char  prop0[MAX_STR_LENGTH + 1] = "";
  char  prop1[MAX_STR_LENGTH + 1] = "";
  char *names[2]                  = {prop0, prop1};
  EXCHECK(ex_get_prop_names(exoid, EX_ELEM_BLOCK, names));
  if (strcmp(names[0], "ID") != 0 || strcmp(names[1], "quality") != 0) {
    fprintf(stderr, "property names '%s', '%s' were not as expected\n", names[0], names[1]);
    ex_close(exoid);
    return 1;
  }

  EXCHECK(ex_close(exoid));
  return 0;
}

static int prop_varid(int ncid, int *varid)
{
  NCCHECK(nc_inq_varid(ncid, VAR_EB_PROP(2), varid));
  return 0;
}

static int overwrite_prop_name_text(size_t len)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  if (prop_varid(ncid, &varid) != 0) {
    nc_close(ncid);
    return 1;
  }
  NCCHECK(nc_redef(ncid));
  char value[MAX_STR_LENGTH + 4];
  memset(value, 'p', sizeof(value));
  value[sizeof(value) - 1] = '\0';
  NCCHECK(nc_put_att_text(ncid, varid, ATT_PROP_NAME, len, value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int overwrite_prop_name_int(void)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  if (prop_varid(ncid, &varid) != 0) {
    nc_close(ncid);
    return 1;
  }
  NCCHECK(nc_redef(ncid));
  int value = 1;
  NCCHECK(nc_put_att_int(ncid, varid, ATT_PROP_NAME, NC_INT, 1, &value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int expect_get_prop_status(int expected)
{
  int exoid = open_file(EX_READ);
  if (exoid < 0) {
    return 1;
  }
  int value  = 0;
  int status = ex_get_prop(exoid, EX_ELEM_BLOCK, 7, "quality", &value);
  if (status != expected) {
    fprintf(stderr, "ex_get_prop status %d, expected %d\n", status, expected);
    ex_close(exoid);
    return 1;
  }
  ex_close(exoid);
  return 0;
}

static int expect_get_prop_array_status(int expected)
{
  int exoid = open_file(EX_READ);
  if (exoid < 0) {
    return 1;
  }
  int values[1] = {0};
  int status    = ex_get_prop_array(exoid, EX_ELEM_BLOCK, "quality", values);
  if (status != expected) {
    fprintf(stderr, "ex_get_prop_array status %d, expected %d\n", status, expected);
    ex_close(exoid);
    return 1;
  }
  ex_close(exoid);
  return 0;
}

static int expect_get_prop_names_status(int expected)
{
  int exoid = open_file(EX_READ);
  if (exoid < 0) {
    return 1;
  }
  char  prop0[MAX_STR_LENGTH + 1] = "";
  char  prop1[MAX_STR_LENGTH + 1] = "";
  char *names[2]                  = {prop0, prop1};
  int   status                    = ex_get_prop_names(exoid, EX_ELEM_BLOCK, names);
  if (status != expected) {
    fprintf(stderr, "ex_get_prop_names status %d, expected %d\n", status, expected);
    ex_close(exoid);
    return 1;
  }
  ex_close(exoid);
  return 0;
}

static int expect_put_prop_status(int expected)
{
  int exoid = open_file(EX_WRITE);
  if (exoid < 0) {
    return 1;
  }
  int status = ex_put_prop(exoid, EX_ELEM_BLOCK, 7, "other", 22);
  if (status != expected) {
    fprintf(stderr, "ex_put_prop status %d, expected %d\n", status, expected);
    ex_close(exoid);
    return 1;
  }
  ex_close(exoid);
  return 0;
}

static int expect_put_prop_array_status(int expected)
{
  int exoid = open_file(EX_WRITE);
  if (exoid < 0) {
    return 1;
  }
  int values[1] = {22};
  int status    = ex_put_prop_array(exoid, EX_ELEM_BLOCK, "other", values);
  if (status != expected) {
    fprintf(stderr, "ex_put_prop_array status %d, expected %d\n", status, expected);
    ex_close(exoid);
    return 1;
  }
  ex_close(exoid);
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file() != 0 || expect_valid_properties() != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_prop_name_text(MAX_STR_LENGTH + 2) != 0 ||
      expect_get_prop_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_prop_name_text(MAX_STR_LENGTH + 2) != 0 ||
      expect_get_prop_array_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_prop_name_text(MAX_STR_LENGTH + 2) != 0 ||
      expect_get_prop_names_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_prop_name_text(MAX_STR_LENGTH + 2) != 0 ||
      expect_put_prop_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_prop_name_text(MAX_STR_LENGTH + 2) != 0 ||
      expect_put_prop_array_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_prop_name_int() != 0 ||
      expect_get_prop_status(EX_FATAL) != 0) {
    return 1;
  }

  printf("test-prop-name-att: all checks passed\n");
  return 0;
}
