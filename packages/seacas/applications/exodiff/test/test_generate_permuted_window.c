/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"
#include <stdio.h>

static int create_file(const char *filename, const double *x, const double *y, const int *conn,
                       const int *node_map)
{
  int cpu_ws = 8;
  int io_ws  = 8;
  int exoid  = ex_create(filename, EX_CLOBBER, &cpu_ws, &io_ws);
  if (exoid < 0) {
    fprintf(stderr, "ERROR: could not create '%s'\n", filename);
    return 1;
  }

  int elem_map[] = {1, 2};
  if (ex_put_init(exoid, "permuted connectivity window", 2, 8, 2, 1, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 || ex_put_id_map(exoid, EX_NODE_MAP, node_map) < 0 ||
      ex_put_id_map(exoid, EX_ELEM_MAP, elem_map) < 0 ||
      ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 2, 4, 0, 0, 0) < 0 ||
      ex_put_conn(exoid, EX_ELEM_BLOCK, 1, conn, NULL, NULL) < 0) {
    fprintf(stderr, "ERROR: could not write '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  return ex_close(exoid) < 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  const double x1[]        = {0.0, 1.0, 1.0, 0.0, 10.0, 11.0, 11.0, 10.0};
  const double y1[]        = {0.0, 0.0, 1.0, 1.0, 10.0, 10.0, 11.0, 11.0};
  const int    node_map1[] = {1, 2, 3, 4, 5, 6, 7, 8};
  const int    conn1[]     = {5, 1, 2, 3, 1, 2, 3, 4};

  /* File 2 has a non-identity node id map, so exodiff maps file1 local
   * connectivities through node_map before comparing them.  For element 2,
   * the mapped file1 connectivity is {2,3,4,5}.  That matches the old buggy
   * sliding window conn2[1..4], but not the actual element-2 window {5,6,7,8}.
   */
  const double x2[]        = {10.0, 0.0, 1.0, 1.0, 0.0, 11.0, 11.0, 10.0};
  const double y2[]        = {10.0, 0.0, 0.0, 1.0, 1.0, 10.0, 11.0, 11.0};
  const int    node_map2[] = {5, 1, 2, 3, 4, 6, 7, 8};
  const int    conn2[]     = {1, 2, 3, 4, 5, 6, 7, 8};

  int status = create_file("permuted-window-a.exo", x1, y1, conn1, node_map1);
  status += create_file("permuted-window-b.exo", x2, y2, conn2, node_map2);
  return status;
}
