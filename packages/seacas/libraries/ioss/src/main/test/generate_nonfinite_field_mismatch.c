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

static int create_file(const char *filename, double first_value)
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

  if (ex_put_init(exoid, "nonfinite field compare", 2, 4, 1, 1, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 ||
      ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0) < 0 ||
      ex_put_conn(exoid, EX_ELEM_BLOCK, 1, conn, NULL, NULL) < 0) {
    fprintf(stderr, "ERROR: could not write mesh metadata to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  char  var_name[] = "bad";
  char *var_names[] = {var_name};
  if (ex_put_variable_param(exoid, EX_NODAL, 1) < 0 ||
      ex_put_variable_names(exoid, EX_NODAL, 1, var_names) < 0) {
    fprintf(stderr, "ERROR: could not write variable metadata to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  double time     = 0.0;
  double values[] = {first_value, 2.0, 3.0, 4.0};
  if (ex_put_time(exoid, 1, &time) < 0 ||
      ex_put_var(exoid, 1, EX_NODAL, 1, 1, 4, values) < 0) {
    fprintf(stderr, "ERROR: could not write transient data to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  return ex_close(exoid) < 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);
  int status = create_file("nonfinite-field-a.g", 1.0);
  status += create_file("nonfinite-field-b.g", NAN);
  return status;
}
