/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"
#include <math.h>
#include <stdio.h>

static int create_coord_file(const char *filename, double x0)
{
  int cpu_ws = 8;
  int io_ws  = 8;
  int exoid  = ex_create(filename, EX_CLOBBER, &cpu_ws, &io_ws);
  if (exoid < 0) {
    fprintf(stderr, "ERROR: could not create '%s'\n", filename);
    return 1;
  }

  double x[] = {x0, 1.0};
  double y[] = {0.0, 0.0};
  if (ex_put_init(exoid, "invalid coordinate compare", 2, 2, 0, 0, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0) {
    fprintf(stderr, "ERROR: could not write '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  return ex_close(exoid) < 0;
}

static int create_time_file(const char *filename, double time)
{
  int cpu_ws = 8;
  int io_ws  = 8;
  int exoid  = ex_create(filename, EX_CLOBBER, &cpu_ws, &io_ws);
  if (exoid < 0) {
    fprintf(stderr, "ERROR: could not create '%s'\n", filename);
    return 1;
  }

  double x[] = {0.0, 1.0};
  double y[] = {0.0, 0.0};
  if (ex_put_init(exoid, "invalid time compare", 2, 2, 0, 0, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 || ex_put_time(exoid, 1, &time) < 0) {
    fprintf(stderr, "ERROR: could not write '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  return ex_close(exoid) < 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  int status = create_coord_file("coord-invalid-a.exo", 0.0);
  status += create_coord_file("coord-invalid-b.exo", INFINITY);
  status += create_time_file("time-invalid-a.exo", 0.0);
  status += create_time_file("time-invalid-b.exo", INFINITY);

  return status;
}
