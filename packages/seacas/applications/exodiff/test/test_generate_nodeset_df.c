/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"
#include <stdio.h>

static int create_file(const char *filename, const double *dist_factors)
{
  int cpu_ws = 8;
  int io_ws  = 8;
  int exoid  = ex_create(filename, EX_CLOBBER, &cpu_ws, &io_ws);
  if (exoid < 0) {
    fprintf(stderr, "ERROR: could not create '%s'\n", filename);
    return 1;
  }

  double x[]      = {0.0, 1.0, 2.0};
  double y[]      = {0.0, 0.0, 0.0};
  int    node_ids[] = {1, 2, 3};

  if (ex_put_init(exoid, "nodeset distribution factors", 2, 3, 0, 0, 1, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 ||
      ex_put_set_param(exoid, EX_NODE_SET, 100, 3, 3) < 0 ||
      ex_put_set(exoid, EX_NODE_SET, 100, node_ids, NULL) < 0 ||
      ex_put_set_dist_fact(exoid, EX_NODE_SET, 100, dist_factors) < 0) {
    fprintf(stderr, "ERROR: could not write '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  return ex_close(exoid) < 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  const double factors_a[] = {1.0, 2.0, 3.0};
  const double factors_b[] = {1.0, 9.0, 3.0};

  int status = create_file("nodeset-df-a.exo", factors_a);
  status += create_file("nodeset-df-b.exo", factors_b);
  return status;
}
