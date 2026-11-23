/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Block topology attributes must be bounded NC_CHAR values before
 * ex_get_block_param reads them into ex_block::topology.
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

static const char *filename = "block-topology-att.exo";

static int create_file(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "block topology metadata", 3, 4, 1, 1, 0, 0));
  EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, 7, "QUAD4", 1, 4, 0, 0, 0));
  int connect[4] = {1, 2, 3, 4};
  EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, 7, connect, NULL, NULL));
  EXCHECK(ex_close(exoid));
  return 0;
}

static int expect_block_status(int expected)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }

  ex_block block = {0};
  block.type     = EX_ELEM_BLOCK;
  block.id       = 7;
  int status     = ex_get_block_param(exoid, &block);
  if (status != expected) {
    fprintf(stderr, "block status %d, expected %d\n", status, expected);
    ex_close(exoid);
    return 1;
  }

  if (expected == EX_NOERR && strcmp(block.topology, "QUAD4") != 0) {
    fprintf(stderr, "topology '%s', expected QUAD4\n", block.topology);
    ex_close(exoid);
    return 1;
  }

  ex_close(exoid);
  return 0;
}

static int overwrite_topology_text(size_t len)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_inq_varid(ncid, VAR_CONN(1), &varid));
  NCCHECK(nc_redef(ncid));
  char value[MAX_STR_LENGTH + 4];
  memset(value, 'q', sizeof(value));
  value[sizeof(value) - 1] = '\0';
  NCCHECK(nc_put_att_text(ncid, varid, ATT_NAME_ELB, len, value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int overwrite_topology_int(void)
{
  int ncid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_inq_varid(ncid, VAR_CONN(1), &varid));
  NCCHECK(nc_redef(ncid));
  int value = 1;
  NCCHECK(nc_put_att_int(ncid, varid, ATT_NAME_ELB, NC_INT, 1, &value));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file() != 0 || expect_block_status(EX_NOERR) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_topology_text(MAX_STR_LENGTH + 2) != 0 ||
      expect_block_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_topology_int() != 0 || expect_block_status(EX_FATAL) != 0) {
    return 1;
  }

  printf("test-block-topology-att: all checks passed\n");
  return 0;
}
