/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Side-set node-list length and node-list reads must return EX_FATAL when a
 * side set references a side number outside the element topology's valid side
 * range.  The length walker must reject the side before indexing the fixed
 * num_nodes_per_side array, and the node-list walker must preserve that error.
 */

#include "exodusII.h"
#include <stdio.h>

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    if ((funcall) != EX_NOERR) {                                                                   \
      fprintf(stderr, "%s failed\n", #funcall);                                                    \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

static int create_file(const char *filename, int side)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "invalid side set side", 2, 4, 1, 1, 0, 1));
  double x[4] = {0.0, 1.0, 1.0, 0.0};
  double y[4] = {0.0, 0.0, 1.0, 1.0};
  EXCHECK(ex_put_coord(exoid, x, y, NULL));
  EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0));
  int connect[4] = {1, 2, 3, 4};
  EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, 1, connect, NULL, NULL));

  int elements[1] = {1};
  int sides[1]    = {side};
  EXCHECK(ex_put_set_param(exoid, EX_SIDE_SET, 1, 1, 0));
  EXCHECK(ex_put_set(exoid, EX_SIDE_SET, 1, elements, sides));
  EXCHECK(ex_close(exoid));
  return 0;
}

static int check_status(const char *filename, int expected_status, int expected_len)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }

  int length     = 99;
  int len_status = ex_get_side_set_node_list_len(exoid, 1, &length);
  if (len_status != expected_status) {
    fprintf(stderr, "node-list-len status %d, expected %d\n", len_status, expected_status);
    ex_close(exoid);
    return 1;
  }
  if (expected_status == EX_NOERR && length != expected_len) {
    fprintf(stderr, "node-list length %d, expected %d\n", length, expected_len);
    ex_close(exoid);
    return 1;
  }

  int counts[1] = {99};
  int nodes[2]  = {99, 99};
  int status    = ex_get_side_set_node_list(exoid, 1, counts, nodes);
  EXCHECK(ex_close(exoid));

  if (status != expected_status) {
    fprintf(stderr, "node-list status %d, expected %d\n", status, expected_status);
    return 1;
  }
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file("side-set-valid-side.exo", 4) != 0 ||
      check_status("side-set-valid-side.exo", EX_NOERR, 2) != 0) {
    return 1;
  }

  if (create_file("side-set-invalid-side.exo", 5) != 0 ||
      check_status("side-set-invalid-side.exo", EX_FATAL, 0) != 0) {
    return 1;
  }

  printf("test-side-set-invalid-side: all checks passed\n");
  return 0;
}
