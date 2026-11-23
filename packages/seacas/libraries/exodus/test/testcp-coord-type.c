/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * ex_copy must reject legacy, non-large coordinate arrays whose netCDF type is
 * not one of the floating-point types that the coordinate copy path reads.
 */

#include "exodusII.h"
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

#define NUM_NODES 4

static const char *input_filename  = "testcp-coord-type-input.exo";
static const char *output_filename = "testcp-coord-type-output.exo";

static int write_legacy_coord_file(nc_type coord_type)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(input_filename, EX_CLOBBER | EX_NORMAL_MODEL, &cpu_word_size,
                                &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "coordinate type", 2, NUM_NODES, 1, 1, 0, 0));
  double x[NUM_NODES] = {0.0, 1.0, 1.0, 0.0};
  double y[NUM_NODES] = {0.0, 0.0, 1.0, 1.0};
  EXCHECK(ex_put_coord(exoid, x, y, NULL));
  EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0));
  int connect[4] = {1, 2, 3, 4};
  EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, 1, connect, NULL, NULL));
  EXCHECK(ex_close(exoid));

  int ncid;
  int coordx_varid;
  int coordy_varid;
  int dimids[2];
  int coord_varid;
  int file_size = 0;
  NCCHECK(nc_open(input_filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  NCCHECK(nc_put_att_int(ncid, NC_GLOBAL, "file_size", NC_INT, 1, &file_size));
  NCCHECK(nc_inq_varid(ncid, "coordx", &coordx_varid));
  NCCHECK(nc_inq_varid(ncid, "coordy", &coordy_varid));
  NCCHECK(nc_rename_var(ncid, coordx_varid, "original_coordx"));
  NCCHECK(nc_rename_var(ncid, coordy_varid, "original_coordy"));
  NCCHECK(nc_inq_dimid(ncid, "num_dim", &dimids[0]));
  NCCHECK(nc_inq_dimid(ncid, "num_nodes", &dimids[1]));
  NCCHECK(nc_def_var(ncid, "coord", coord_type, 2, dimids, &coord_varid));
  NCCHECK(nc_enddef(ncid));

  if (coord_type == NC_INT) {
    int coord_values[2][NUM_NODES] = {{0, 1, 1, 0}, {0, 0, 1, 1}};
    NCCHECK(nc_put_var_int(ncid, coord_varid, &coord_values[0][0]));
  }
  else if (coord_type == NC_FLOAT) {
    float coord_values[2][NUM_NODES] = {{0.0F, 1.0F, 1.0F, 0.0F}, {0.0F, 0.0F, 1.0F, 1.0F}};
    NCCHECK(nc_put_var_float(ncid, coord_varid, &coord_values[0][0]));
  }
  else if (coord_type == NC_DOUBLE) {
    double coord_values[2][NUM_NODES] = {{0.0, 1.0, 1.0, 0.0}, {0.0, 0.0, 1.0, 1.0}};
    NCCHECK(nc_put_var_double(ncid, coord_varid, &coord_values[0][0]));
  }
  else {
    fprintf(stderr, "unsupported test coordinate type %d\n", coord_type);
    nc_close(ncid);
    return 1;
  }

  NCCHECK(nc_close(ncid));
  return 0;
}

static int copy_file(void)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   in_exoid = ex_open(input_filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (in_exoid < 0) {
    return 98;
  }

  cpu_word_size   = 8;
  io_word_size    = 8;
  int out_exoid   = ex_create(output_filename, EX_CLOBBER | EX_LARGE_MODEL, &cpu_word_size,
                              &io_word_size);
  if (out_exoid < 0) {
    ex_close(in_exoid);
    return 97;
  }

  int status = ex_copy(in_exoid, out_exoid);
  ex_close(in_exoid);
  ex_close(out_exoid);
  return status;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (write_legacy_coord_file(NC_DOUBLE) != 0 || copy_file() != EX_NOERR) {
    fprintf(stderr, "legacy double coordinate copy failed\n");
    return 1;
  }

  if (write_legacy_coord_file(NC_FLOAT) != 0 || copy_file() != EX_NOERR) {
    fprintf(stderr, "legacy float coordinate copy failed\n");
    return 1;
  }

  if (write_legacy_coord_file(NC_INT) != 0 || copy_file() != EX_FATAL) {
    fprintf(stderr, "legacy integer coordinate copy was not rejected\n");
    return 1;
  }

  printf("testcp-coord-type: all checks passed\n");
  return 0;
}
