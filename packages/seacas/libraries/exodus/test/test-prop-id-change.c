#include "exodusII.h"
#include <stdio.h>

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    if ((funcall) != EX_NOERR) {                                                                   \
      fprintf(stderr, "%s failed\n", #funcall);                                                    \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

int main(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;

  int exoid = ex_create("prop-id-change.exo", EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "change node set ID", 2, 3, 0, 0, 1, 0));
  double x[3] = {0, 1, 0};
  double y[3] = {0, 0, 1};
  EXCHECK(ex_put_coord(exoid, x, y, NULL));
  EXCHECK(ex_put_set_param(exoid, EX_NODE_SET, 10, 3, 0));
  int nodes[3] = {1, 2, 3};
  EXCHECK(ex_put_set(exoid, EX_NODE_SET, 10, nodes, NULL));

  /* Only the default ID property exists; changing it must update that property. */
  EXCHECK(ex_put_prop(exoid, EX_NODE_SET, 10, "ID", 20));
  EXCHECK(ex_close(exoid));

  float version;
  io_word_size = 0;
  exoid        = ex_open("prop-id-change.exo", EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }

  int num_props = ex_get_num_props(exoid, EX_NODE_SET);
  int id        = -1;
  EXCHECK(ex_get_ids(exoid, EX_NODE_SET, &id));
  EXCHECK(ex_close(exoid));

  printf("node set: id=%d num_props=%d\n", id, num_props);
  if (id != 20) {
    fprintf(stderr, "node set ID: expected 20, got %d\n", id);
    return 1;
  }
  if (num_props != 1) {
    fprintf(stderr, "node set properties: expected 1, got %d\n", num_props);
    return 1;
  }
  return 0;
}
