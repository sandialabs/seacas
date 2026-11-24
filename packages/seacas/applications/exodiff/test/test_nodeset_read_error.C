/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "ED_SystemInterface.h"
#include "exodusII.h"
#include "node_set.h"
#include <cstdio>

SystemInterface interFace;

int main()
{
  int cpu_ws = 8;
  int io_ws  = 8;
  int exoid  = ex_create("nodeset-read-error.exo", EX_CLOBBER, &cpu_ws, &io_ws);
  if (exoid < 0) {
    std::fprintf(stderr, "ERROR: could not create test file\n");
    return 2;
  }

  double x[] = {0.0};
  double y[] = {0.0};
  if (ex_put_init(exoid, "nodeset read error", 2, 1, 0, 0, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, nullptr) < 0) {
    std::fprintf(stderr, "ERROR: could not initialize test file\n");
    ex_close(exoid);
    return 2;
  }

  if (ex_close(exoid) < 0) {
    std::fprintf(stderr, "ERROR: could not close test file\n");
    return 2;
  }

  Node_Set<int> missing_nodes(exoid, 999, 1, 0);
  (void)missing_nodes.Nodes();

  return 0;
}
