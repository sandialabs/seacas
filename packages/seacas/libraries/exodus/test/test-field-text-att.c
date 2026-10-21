/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Field metadata text attributes must be NC_CHAR values that fit in their
 * destination ex_field buffers before ex_get_field_metadata reads them.
 */

#include "exodusII.h"
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

static const char *filename = "field-text-att.exo";

static int create_file(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "field text metadata", 2, 0, 0, 0, 0, 0));

  ex_field field = {0};
  field.entity_type = EX_GLOBAL;
  field.entity_id   = 0;
  strcpy(field.name, "text_field");
  field.nesting                = 1;
  field.type[0]                = EX_FIELD_TYPE_USER_DEFINED;
  field.cardinality[0]         = 2;
  field.component_separator[0] = ':';
  field.component_separator[1] = '\0';
  strcpy(field.type_name, "custom_type");

  EXCHECK(ex_put_field_metadata(exoid, field));
  EXCHECK(ex_put_field_suffices(exoid, field, "x,y"));
  EXCHECK(ex_close(exoid));
  return 0;
}

static int expect_field_status(int expected)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }

  int count = ex_get_field_metadata_count(exoid, EX_GLOBAL, 0);
  if (count != 1) {
    fprintf(stderr, "field metadata count %d, expected 1\n", count);
    ex_close(exoid);
    return 1;
  }

  ex_field field[1] = {{0}};
  field[0].entity_type = EX_GLOBAL;
  field[0].entity_id   = 0;
  int status           = ex_get_field_metadata(exoid, field);
  if (status != expected) {
    fprintf(stderr, "field metadata status %d, expected %d\n", status, expected);
    ex_close(exoid);
    return 1;
  }

  if (expected == EX_NOERR) {
    if (strcmp(field[0].name, "text_field") != 0 || strcmp(field[0].type_name, "custom_type") != 0 ||
        strcmp(field[0].component_separator, ":") != 0 || strcmp(field[0].suffices, "x,y") != 0 ||
        field[0].type[0] != EX_FIELD_TYPE_USER_DEFINED || field[0].cardinality[0] != 2) {
      fprintf(stderr, "valid field metadata was not read back as written\n");
      ex_close(exoid);
      return 1;
    }
  }

  ex_close(exoid);
  return 0;
}

static int overwrite_text_attribute(const char *attname, size_t len)
{
  int ncid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  char value[EX_MAX_NAME + 4];
  memset(value, 'a', sizeof(value));
  value[sizeof(value) - 1] = '\0';
  NCCHECK(nc_put_att_text(ncid, NC_GLOBAL, attname, len, value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int overwrite_int_attribute(const char *attname)
{
  int ncid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  int value = 1;
  NCCHECK(nc_put_att_int(ncid, NC_GLOBAL, attname, NC_INT, 1, &value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int overwrite_int_array_attribute(const char *attname, size_t len)
{
  int ncid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  int values[EX_MAX_FIELD_NESTING + 1] = {EX_FIELD_TYPE_USER_DEFINED, EX_FIELD_TYPE_USER_DEFINED,
                                          EX_FIELD_TYPE_USER_DEFINED};
  NCCHECK(nc_put_att_int(ncid, NC_GLOBAL, attname, NC_INT, len, values));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int overwrite_double_attribute(const char *attname)
{
  int ncid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  double value = 1.0;
  NCCHECK(nc_put_att_double(ncid, NC_GLOBAL, attname, NC_DOUBLE, 1, &value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file() != 0 || expect_field_status(EX_NOERR) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_text_attribute("Field@extra_field@separator", 2) != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 ||
      overwrite_text_attribute("Field@text_field@separator", EX_MAX_FIELD_NESTING + 1) != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 ||
      overwrite_text_attribute("Field@text_field@type_name", EX_MAX_NAME + 2) != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 ||
      overwrite_text_attribute("Field@text_field@suffices", EX_MAX_NAME + 2) != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_int_attribute("Field@text_field@type_name") != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 ||
      overwrite_int_array_attribute("Field@text_field@type", EX_MAX_FIELD_NESTING + 1) != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_double_attribute("Field@text_field@type") != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 ||
      overwrite_int_array_attribute("Field@text_field@cardinality", EX_MAX_FIELD_NESTING + 1) != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_double_attribute("Field@text_field@cardinality") != 0 ||
      expect_field_status(EX_FATAL) != 0) {
    return 1;
  }

  printf("test-field-text-att: all checks passed\n");
  return 0;
}
