/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * ex_get_id_map must fill exactly as many entries as the entity-count
 * dimension (the size callers allocate from ex_inquire), even when the
 * file's map variable is dimensioned differently.
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
#define GUARD     -12345

static const char *filename = "id-map-dim.exo";

/* Write a 4-node mesh. If map_len > 0, replace node_num_map with a
 * variable over a new dimension of length map_len (ndims 1 or 2). */
static int write_mesh(size_t map_len, int ndims)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }
  EXCHECK(ex_put_init(exoid, "id map dimension", 2, NUM_NODES, 1, 1, 0, 0));
  double x[NUM_NODES] = {0, 1, 1, 0};
  double y[NUM_NODES] = {0, 0, 1, 1};
  EXCHECK(ex_put_coord(exoid, x, y, NULL));
  EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0));
  int connect[4] = {1, 2, 3, 4};
  EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, 1, connect, NULL, NULL));
  int node_map[NUM_NODES] = {10, 20, 30, 40};
  EXCHECK(ex_put_id_map(exoid, EX_NODE_MAP, node_map));
  EXCHECK(ex_close(exoid));

  if (map_len == 0) {
    return 0;
  }

  int ncid;
  int varid;
  int dimids[2];
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  NCCHECK(nc_inq_varid(ncid, "node_num_map", &varid));
  NCCHECK(nc_rename_var(ncid, varid, "original_node_num_map"));
  NCCHECK(nc_def_dim(ncid, "test_map_len", map_len, &dimids[0]));
  if (ndims == 2) {
    NCCHECK(nc_def_dim(ncid, "test_map_width", 2, &dimids[1]));
  }
  NCCHECK(nc_def_var(ncid, "node_num_map", NC_INT, ndims, dimids, &varid));
  NCCHECK(nc_enddef(ncid));
  int values[64 * 2];
  for (size_t i = 0; i < map_len * ndims; i++) {
    values[i] = (int)(100 + i);
  }
  NCCHECK(nc_put_var_int(ncid, varid, values));
  NCCHECK(nc_close(ncid));
  return 0;
}

/* Read the node map into a NUM_NODES buffer followed by guard values.
 * Returns the ex_get_id_map status, or 99 if the guard was overwritten. */
static int read_map(int *map)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 98;
  }
  int buffer[NUM_NODES + 64 * 2];
  for (size_t i = 0; i < sizeof(buffer) / sizeof(buffer[0]); i++) {
    buffer[i] = GUARD;
  }
  int status = ex_get_id_map(exoid, EX_NODE_MAP, buffer);
  ex_close(exoid);
  for (size_t i = NUM_NODES; i < sizeof(buffer) / sizeof(buffer[0]); i++) {
    if (buffer[i] != GUARD) {
      fprintf(stderr, "ex_get_id_map wrote past the %d-entry buffer (index %zu)\n", NUM_NODES, i);
      return 99;
    }
  }
  for (int i = 0; i < NUM_NODES; i++) {
    map[i] = buffer[i];
  }
  return status;
}

int main(void)
{
  int map[NUM_NODES];
  ex_opts(EX_VERBOSE);

  /* Well-formed file: the map round-trips. */
  if (write_mesh(0, 1) != 0 || read_map(map) != EX_NOERR || map[0] != 10 || map[3] != 40) {
    fprintf(stderr, "well-formed node map did not round-trip\n");
    return 1;
  }

  /* Map variable longer than num_nodes: only num_nodes entries are read. */
  if (write_mesh(64, 1) != 0) {
    return 1;
  }
  int status = read_map(map);
  if (status == 99 || (status == EX_NOERR && (map[0] != 100 || map[3] != 103))) {
    fprintf(stderr, "oversized node map was not bounded (status %d)\n", status);
    return 1;
  }

  /* Map variable shorter than num_nodes: must fail, not succeed short. */
  if (write_mesh(2, 1) != 0 || read_map(map) != EX_FATAL) {
    fprintf(stderr, "short node map was not rejected\n");
    return 1;
  }

  /* Two-dimensional map variable: must fail. */
  if (write_mesh(4, 2) != 0) {
    return 1;
  }
  status = read_map(map);
  if (status != EX_FATAL) {
    fprintf(stderr, "two-dimensional node map was not rejected (status %d)\n", status);
    return 1;
  }

  printf("test-id-map-dim: all checks passed\n");
  return 0;
}
