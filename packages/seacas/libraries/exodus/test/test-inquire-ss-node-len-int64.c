/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * EX_INQ_SS_NODE_LEN must pass an output buffer to
 * ex_get_side_set_node_list_len whose width matches EX_BULK_INT64_API, even
 * when side-set ids are still using the 32-bit API.
 */

#include "exodusII.h"
#include <inttypes.h>
#include <stdio.h>

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    if ((funcall) != EX_NOERR) {                                                                   \
      fprintf(stderr, "%s failed\n", #funcall);                                                    \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

static const char *filename = "inquire-ss-node-len-int64.exo";

static int create_file(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "side set node length int64", 2, 4, 1, 1, 0, 1));
  double x[4] = {0.0, 1.0, 1.0, 0.0};
  double y[4] = {0.0, 0.0, 1.0, 1.0};
  EXCHECK(ex_put_coord(exoid, x, y, NULL));
  EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, 1, "QUAD4", 1, 4, 0, 0, 0));
  int connect[4] = {1, 2, 3, 4};
  EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, 1, connect, NULL, NULL));

  int elements[1] = {1};
  int sides[1]    = {1};
  EXCHECK(ex_put_set_param(exoid, EX_SIDE_SET, 1, 1, 0));
  EXCHECK(ex_put_set(exoid, EX_SIDE_SET, 1, elements, sides));
  EXCHECK(ex_close(exoid));
  return 0;
}

static int open_file(void)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  return ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
}

static int check_ex_inquire_int(void)
{
  int exoid = open_file();
  if (exoid < 0) {
    return 1;
  }

  if ((ex_set_int64_status(exoid, EX_BULK_INT64_API) & EX_BULK_INT64_API) == 0) {
    fprintf(stderr, "failed to enable EX_BULK_INT64_API\n");
    ex_close(exoid);
    return 1;
  }
  int64_t length = ex_inquire_int(exoid, EX_INQ_SS_NODE_LEN);
  EXCHECK(ex_close(exoid));

  if (length != 2) {
    fprintf(stderr, "ex_inquire_int returned %" PRId64 ", expected 2\n", length);
    return 1;
  }
  return 0;
}

static int check_ex_inquire_64(void)
{
  int exoid = open_file();
  if (exoid < 0) {
    return 1;
  }

  if ((ex_set_int64_status(exoid, EX_BULK_INT64_API | EX_INQ_INT64_API) &
       (EX_BULK_INT64_API | EX_INQ_INT64_API)) != (EX_BULK_INT64_API | EX_INQ_INT64_API)) {
    fprintf(stderr, "failed to enable EX_BULK_INT64_API and EX_INQ_INT64_API\n");
    ex_close(exoid);
    return 1;
  }
  int64_t length = -1;
  float   fdum   = 0.0;
  char    cdum[MAX_LINE_LENGTH + 1];
  int     status = ex_inquire(exoid, EX_INQ_SS_NODE_LEN, &length, &fdum, cdum);
  EXCHECK(ex_close(exoid));

  if (status != EX_NOERR || length != 2) {
    fprintf(stderr, "ex_inquire returned status %d length %" PRId64 ", expected status 0 length 2\n",
            status, length);
    return 1;
  }
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file() != 0 || check_ex_inquire_int() != 0 || check_ex_inquire_64() != 0) {
    return 1;
  }

  printf("test-inquire-ss-node-len-int64: all checks passed\n");
  return 0;
}
