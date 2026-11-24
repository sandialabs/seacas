/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Set distribution-factor reads must only read the factor count advertised by
 * Exodus metadata, even if a forged netCDF variable is dimensioned larger.
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

static const char *filename = "set-dist-factor-len.exo";

static int create_node_set_file(int stored_factor_count)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "set dist factor length", 2, 4, 1, 1, 1, 0));
  double x[4] = {0.0, 1.0, 1.0, 0.0};
  double y[4] = {0.0, 0.0, 1.0, 1.0};
  EXCHECK(ex_put_coord(exoid, x, y, NULL));
  EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0));
  int connect[4] = {1, 2, 3, 4};
  EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, 1, connect, NULL, NULL));

  EXCHECK(ex_put_set_param(exoid, EX_NODE_SET, 10, 2, 2));
  int nodes[2] = {1, 2};
  EXCHECK(ex_put_set(exoid, EX_NODE_SET, 10, nodes, NULL));
  double factors[2] = {2.0, 3.0};
  EXCHECK(ex_put_set_dist_fact(exoid, EX_NODE_SET, 10, factors));
  EXCHECK(ex_close(exoid));

  if (stored_factor_count == 2) {
    return 0;
  }

  int ncid;
  int old_varid;
  int dimid;
  int varid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  NCCHECK(nc_inq_varid(ncid, "dist_fact_ns1", &old_varid));
  NCCHECK(nc_rename_var(ncid, old_varid, "original_dist_fact_ns1"));
  NCCHECK(nc_def_dim(ncid, "test_num_df_ns1", stored_factor_count, &dimid));
  NCCHECK(nc_def_var(ncid, "dist_fact_ns1", NC_DOUBLE, 1, &dimid, &varid));
  NCCHECK(nc_enddef(ncid));

  double mutated_factors[4] = {2.0, 3.0, 4.0, 5.0};
  NCCHECK(nc_put_var_double(ncid, varid, mutated_factors));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int read_single_set(double *factors)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 98;
  }

  int status = ex_get_set_dist_fact(exoid, EX_NODE_SET, 10, factors);
  ex_close(exoid);
  return status;
}

static int read_concat_sets(double *factors)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 98;
  }

  int                 ids[1]         = {0};
  int                 num_entries[1] = {0};
  int                 num_dist[1]    = {0};
  int                 entry_index[1] = {0};
  int                 dist_index[1]  = {0};
  int                 entry_list[2]  = {0, 0};
  struct ex_set_specs sp             = {0};
  sp.sets_ids                        = ids;
  sp.num_entries_per_set             = num_entries;
  sp.num_dist_per_set                = num_dist;
  sp.sets_entry_index                = entry_index;
  sp.sets_dist_index                 = dist_index;
  sp.sets_entry_list                 = entry_list;
  sp.sets_dist_fact                  = factors;

  int status = ex_get_concat_sets(exoid, EX_NODE_SET, &sp);
  ex_close(exoid);
  return status;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  double factors[4] = {0.0, 0.0, 0.0, 0.0};
  if (create_node_set_file(2) != 0 || read_single_set(factors) != EX_NOERR || factors[0] != 2.0 ||
      factors[1] != 3.0) {
    fprintf(stderr, "well-formed node-set distribution factors did not round-trip\n");
    return 1;
  }

  factors[0] = 0.0;
  factors[1] = 0.0;
  if (read_concat_sets(factors) != EX_NOERR || factors[0] != 2.0 || factors[1] != 3.0) {
    fprintf(stderr, "well-formed concatenated distribution factors did not round-trip\n");
    return 1;
  }

  if (create_node_set_file(4) != 0) {
    return 1;
  }
  factors[0] = 0.0;
  factors[1] = 0.0;
  factors[2] = 0.0;
  factors[3] = 0.0;
  if (read_single_set(factors) != EX_FATAL) {
    fprintf(stderr, "oversized node-set distribution factors were not rejected\n");
    return 1;
  }

  factors[0] = 0.0;
  factors[1] = 0.0;
  factors[2] = 0.0;
  factors[3] = 0.0;
  if (read_concat_sets(factors) != EX_FATAL) {
    fprintf(stderr, "oversized concatenated distribution factors were not rejected\n");
    return 1;
  }

  printf("test-set-dist-factor-len: all checks passed\n");
  return 0;
}
