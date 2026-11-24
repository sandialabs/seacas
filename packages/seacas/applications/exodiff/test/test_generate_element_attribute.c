/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"
#include <stdio.h>

static int create_file(const char *filename, int include_attribute, double attribute_value)
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
  int    attr_count = include_attribute ? 1 : 0;
  if (ex_put_init(exoid, "element attribute compare", 2, 4, 1, 1, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 ||
      ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, attr_count) < 0 ||
      ex_put_conn(exoid, EX_ELEM_BLOCK, 1, conn, NULL, NULL) < 0) {
    fprintf(stderr, "ERROR: could not initialize '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  if (include_attribute) {
    char  attr_name[]  = "thick";
    char *attr_names[] = {attr_name};
    if (ex_put_attr_names(exoid, EX_ELEM_BLOCK, 1, attr_names) < 0 ||
        ex_put_attr(exoid, EX_ELEM_BLOCK, 1, &attribute_value) < 0) {
      fprintf(stderr, "ERROR: could not write attributes to '%s'\n", filename);
      ex_close(exoid);
      return 1;
    }
  }

  return ex_close(exoid) < 0;
}

static int create_mapped_file(const char *filename, const int *conn, const double *attribute_values)
{
  int cpu_ws = 8;
  int io_ws  = 8;
  int exoid  = ex_create(filename, EX_CLOBBER, &cpu_ws, &io_ws);
  if (exoid < 0) {
    fprintf(stderr, "ERROR: could not create '%s'\n", filename);
    return 1;
  }

  double x[] = {0.0, 1.0, 1.0, 0.0, 10.0, 11.0, 11.0, 10.0};
  double y[] = {0.0, 0.0, 1.0, 1.0, 10.0, 10.0, 11.0, 11.0};
  if (ex_put_init(exoid, "mapped element attribute compare", 2, 8, 2, 1, 0, 0) < 0 ||
      ex_put_coord(exoid, x, y, NULL) < 0 ||
      ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 2, 4, 0, 0, 1) < 0 ||
      ex_put_conn(exoid, EX_ELEM_BLOCK, 1, conn, NULL, NULL) < 0) {
    fprintf(stderr, "ERROR: could not initialize '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  char  attr_name[]  = "thick";
  char *attr_names[] = {attr_name};
  if (ex_put_attr_names(exoid, EX_ELEM_BLOCK, 1, attr_names) < 0 ||
      ex_put_attr(exoid, EX_ELEM_BLOCK, 1, attribute_values) < 0) {
    fprintf(stderr, "ERROR: could not write attributes to '%s'\n", filename);
    ex_close(exoid);
    return 1;
  }

  return ex_close(exoid) < 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  int status = create_file("element-attr-a.exo", 1, 1.0);
  status += create_file("element-attr-b.exo", 1, 9.0);
  status += create_file("element-attr-missing-a.exo", 1, 1.0);
  status += create_file("element-attr-missing-b.exo", 0, 0.0);

  const int    mapped_conn_a[]  = {1, 2, 3, 4, 5, 6, 7, 8};
  const int    mapped_conn_b[]  = {5, 6, 7, 8, 1, 2, 3, 4};
  const double mapped_attrs_a[] = {1.0, 2.0};
  const double mapped_attrs_b[] = {9.0, 1.0};
  status += create_mapped_file("element-attr-map-a.exo", mapped_conn_a, mapped_attrs_a);
  status += create_mapped_file("element-attr-map-b.exo", mapped_conn_b, mapped_attrs_b);

  return status;
}
