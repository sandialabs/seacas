/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Assembly metadata attributes must have the expected type and length before
 * ex_get_assembly reads them into scalar or fixed-size destinations.
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

static const char *filename = "assembly-att.exo";

static int create_file(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "assembly metadata", 3, 0, 0, 0, 0, 0));

  ex_entity_id list[1] = {7};
  ex_assembly  assembly;
  assembly.id           = 9;
  assembly.name         = "good_assembly";
  assembly.type         = EX_ELEM_BLOCK;
  assembly.entity_count = 1;
  assembly.entity_list  = list;
  EXCHECK(ex_put_assembly(exoid, assembly));
  EXCHECK(ex_close(exoid));
  return 0;
}

static int expect_assembly_status(int expected)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }

  char         name[EX_MAX_NAME + 1] = "";
  ex_entity_id list[1]               = {0};
  ex_assembly  assembly;
  assembly.id           = 9;
  assembly.name         = name;
  assembly.type         = EX_INVALID;
  assembly.entity_count = 0;
  assembly.entity_list  = list;

  int status = ex_get_assembly(exoid, &assembly);
  if (status != expected) {
    fprintf(stderr, "assembly status %d, expected %d\n", status, expected);
    ex_close(exoid);
    return 1;
  }

  if (expected == EX_NOERR &&
      (strcmp(name, "good_assembly") != 0 || assembly.type != EX_ELEM_BLOCK ||
       assembly.entity_count != 1 || list[0] != 7)) {
    fprintf(stderr, "valid assembly metadata was not read back as written\n");
    ex_close(exoid);
    return 1;
  }

  ex_close(exoid);
  return 0;
}

static int assembly_varid(int ncid, int *varid)
{
  NCCHECK(nc_inq_varid(ncid, VAR_ENTITY_ASSEMBLY(9), varid));
  return 0;
}

static int overwrite_name_text(size_t len)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  if (assembly_varid(ncid, &varid) != 0) {
    nc_close(ncid);
    return 1;
  }
  NCCHECK(nc_redef(ncid));
  char value[EX_MAX_NAME + 4];
  memset(value, 'a', sizeof(value));
  value[sizeof(value) - 1] = '\0';
  NCCHECK(nc_put_att_text(ncid, varid, EX_ATTRIBUTE_NAME, len, value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int overwrite_name_int(void)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  if (assembly_varid(ncid, &varid) != 0) {
    nc_close(ncid);
    return 1;
  }
  NCCHECK(nc_redef(ncid));
  int value = 1;
  NCCHECK(nc_put_att_int(ncid, varid, EX_ATTRIBUTE_NAME, NC_INT, 1, &value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int overwrite_type_int(size_t len)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  if (assembly_varid(ncid, &varid) != 0) {
    nc_close(ncid);
    return 1;
  }
  NCCHECK(nc_redef(ncid));
  int values[2] = {EX_ELEM_BLOCK, EX_ASSEMBLY};
  NCCHECK(nc_put_att_int(ncid, varid, EX_ATTRIBUTE_TYPE, NC_INT, len, values));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int overwrite_type_double(void)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  if (assembly_varid(ncid, &varid) != 0) {
    nc_close(ncid);
    return 1;
  }
  NCCHECK(nc_redef(ncid));
  double value = 1.0;
  NCCHECK(nc_put_att_double(ncid, varid, EX_ATTRIBUTE_TYPE, NC_DOUBLE, 1, &value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file() != 0 || expect_assembly_status(EX_NOERR) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_name_text(EX_MAX_NAME + 2) != 0 ||
      expect_assembly_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_name_int() != 0 || expect_assembly_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_type_int(2) != 0 || expect_assembly_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_type_double() != 0 || expect_assembly_status(EX_FATAL) != 0) {
    return 1;
  }

  printf("test-assembly-att: all checks passed\n");
  return 0;
}
