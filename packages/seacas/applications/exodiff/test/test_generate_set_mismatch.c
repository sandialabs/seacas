/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"
#include <stdio.h>

static int create_nodeset_file(const char *filename, int include_nodeset)
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
  if (ex_put_init(exoid, "nodeset metadata mismatch", 2, 2, 0, 0, include_nodeset, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0) {
    fprintf(stderr, "ERROR: could not initialize '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  if (include_nodeset) {
    int nodes[] = {1, 2};
    if (ex_put_set_param(exoid, EX_NODE_SET, 100, 2, 0) < 0 ||
        ex_put_set(exoid, EX_NODE_SET, 100, nodes, NULL) < 0) {
      fprintf(stderr, "ERROR: could not write nodeset to '%s'\n", filename);
      ex_close(exoid);
      return 1;
    }
  }

  return ex_close(exoid) < 0;
}

static int create_sideset_file(const char *filename, int include_sideset)
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
  if (ex_put_init(exoid, "sideset metadata mismatch", 2, 4, 1, 1, 0, include_sideset) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 ||
      ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0) < 0 ||
      ex_put_conn(exoid, EX_ELEM_BLOCK, 1, conn, NULL, NULL) < 0) {
    fprintf(stderr, "ERROR: could not initialize '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  if (include_sideset) {
    int elems[] = {1};
    int sides[] = {1};
    if (ex_put_set_param(exoid, EX_SIDE_SET, 200, 1, 0) < 0 ||
        ex_put_set(exoid, EX_SIDE_SET, 200, elems, sides) < 0) {
      fprintf(stderr, "ERROR: could not write sideset to '%s'\n", filename);
      ex_close(exoid);
      return 1;
    }
  }

  return ex_close(exoid) < 0;
}

static int create_entity_var_file(const char *filename, int second_block_has_var)
{
  int cpu_ws = 8;
  int io_ws  = 8;
  int exoid  = ex_create(filename, EX_CLOBBER, &cpu_ws, &io_ws);
  if (exoid < 0) {
    fprintf(stderr, "ERROR: could not create '%s'\n", filename);
    return 1;
  }

  double x[]     = {0.0, 1.0, 1.0, 0.0, 10.0, 11.0, 11.0, 10.0};
  double y[]     = {0.0, 0.0, 1.0, 1.0, 10.0, 10.0, 11.0, 11.0};
  int    conn1[] = {1, 2, 3, 4};
  int    conn2[] = {5, 6, 7, 8};
  if (ex_put_init(exoid, "entity variable miss", 2, 8, 2, 2, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 ||
      ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0) < 0 ||
      ex_put_block(exoid, EX_ELEM_BLOCK, 2, "QUAD4", 1, 4, 0, 0, 0) < 0 ||
      ex_put_conn(exoid, EX_ELEM_BLOCK, 1, conn1, NULL, NULL) < 0 ||
      ex_put_conn(exoid, EX_ELEM_BLOCK, 2, conn2, NULL, NULL) < 0 ||
      ex_put_variable_param(exoid, EX_ELEM_BLOCK, 1) < 0) {
    fprintf(stderr, "ERROR: could not initialize '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  char  var_name[]  = "ev";
  char *var_names[] = {var_name};
  int   truth[]     = {1, second_block_has_var};
  if (ex_put_variable_names(exoid, EX_ELEM_BLOCK, 1, var_names) < 0 ||
      ex_put_truth_table(exoid, EX_ELEM_BLOCK, 2, 1, truth) < 0) {
    fprintf(stderr, "ERROR: could not write variable metadata to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  double time = 0.0;
  double val1 = 1.0;
  double val2 = 2.0;
  if (ex_put_time(exoid, 1, &time) < 0 || ex_put_var(exoid, 1, EX_ELEM_BLOCK, 1, 1, 1, &val1) < 0) {
    fprintf(stderr, "ERROR: could not write variable data to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  if (second_block_has_var && ex_put_var(exoid, 1, EX_ELEM_BLOCK, 1, 2, 1, &val2) < 0) {
    fprintf(stderr, "ERROR: could not write second block variable data to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  return ex_close(exoid) < 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  int status = create_nodeset_file("nodeset-missing-a.exo", 1);
  status += create_nodeset_file("nodeset-missing-b.exo", 0);
  status += create_sideset_file("sideset-missing-a.exo", 1);
  status += create_sideset_file("sideset-missing-b.exo", 0);
  status += create_entity_var_file("entity-var-a.exo", 1);
  status += create_entity_var_file("entity-var-b.exo", 0);

  return status;
}
