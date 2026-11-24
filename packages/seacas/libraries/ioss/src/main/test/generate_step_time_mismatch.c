/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"
#include <stdio.h>

static int create_file(const char *filename, double first_time)
{
  int cpu_ws = 8;
  int io_ws  = 8;
  int exoid  = ex_create(filename, EX_CLOBBER, &cpu_ws, &io_ws);
  if (exoid < 0) {
    fprintf(stderr, "ERROR: could not create '%s'\n", filename);
    return 1;
  }

  double x[]    = {0.0, 1.0, 1.0, 0.0};
  double y[]    = {0.0, 0.0, 1.0, 1.0};
  int    conn[] = {1, 2, 3, 4};

  if (ex_put_init(exoid, "step time compare", 2, 4, 1, 1, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 ||
      ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0) < 0 ||
      ex_put_conn(exoid, EX_ELEM_BLOCK, 1, conn, NULL, NULL) < 0) {
    fprintf(stderr, "ERROR: could not write mesh metadata to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  double time_1 = first_time;
  double time_2 = first_time + 1.0;
  if (ex_put_time(exoid, 1, &time_1) < 0 || ex_put_time(exoid, 2, &time_2) < 0) {
    fprintf(stderr, "ERROR: could not write times to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  return ex_close(exoid) < 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);
  int status = create_file("step-time-a.g", 0.0);
  status += create_file("step-time-b.g", 10.0);
  return status;
}
