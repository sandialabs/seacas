/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * File-derived set counts must size status arrays with the full size_t count,
 * not a truncated int.  The test interposes calloc only for the huge status
 * array allocation so the fixed code follows its clean EX_MEMFAIL path quickly;
 * the old malloc((int)num_sets * sizeof(int)) path bypasses that interposition
 * and overruns the truncated allocation under ASan.
 */

#include "exodusII.h"
#include <inttypes.h>
#include <netcdf.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    if ((funcall) != EX_NOERR) {                                                                   \
      fprintf(stderr, "%s failed\n", #funcall);                                                    \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

#define NCCHECK(funcall)                                                                           \
  do {                                                                                             \
    int nc_status_ = (funcall);                                                                    \
    if (nc_status_ != NC_NOERR) {                                                                  \
      fprintf(stderr, "%s failed: %s\n", #funcall, nc_strerror(nc_status_));                       \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

static int fail_huge_status_calloc = 0;

void *calloc(size_t nmemb, size_t size)
{
  if (fail_huge_status_calloc && size == sizeof(int) && nmemb > UINT32_MAX) {
    return NULL;
  }
  if (size != 0 && nmemb > SIZE_MAX / size) {
    return NULL;
  }

  size_t bytes = nmemb * size;
  void  *ptr   = malloc(bytes);
  if (ptr != NULL) {
    memset(ptr, 0, bytes);
  }
  return ptr;
}

static const char *filename = "inquire-set-count-overflow.exo";

static int create_file_with_huge_node_set_count(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid = ex_create(filename, EX_CLOBBER | EX_NETCDF4 | EX_NOCLASSIC, &cpu_word_size,
                        &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "huge set count", 2, 0, 0, 0, 0, 0));
  EXCHECK(ex_close(exoid));

  int ncid;
  int dimid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  NCCHECK(nc_def_dim(ncid, "num_node_sets", (size_t)UINT32_MAX + 17, &dimid));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int open_file(void)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  return ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
}

static int check_ex_inquire(void)
{
  int exoid = open_file();
  if (exoid < 0) {
    return 1;
  }

  int   ret_int   = -99;
  float ret_float = 0.0F;
  char  ret_char[1] = {'\0'};
  fail_huge_status_calloc = 1;
  int status = ex_inquire(exoid, EX_INQ_NS_NODE_LEN, &ret_int, &ret_float, ret_char);
  fail_huge_status_calloc = 0;
  EXCHECK(ex_close(exoid));

  if (status != EX_FATAL) {
    fprintf(stderr, "ex_inquire returned status %d and length %d, expected EX_FATAL\n", status,
            ret_int);
    return 1;
  }
  return 0;
}

static int check_ex_inquire_int(void)
{
  int exoid = open_file();
  if (exoid < 0) {
    return 1;
  }

  fail_huge_status_calloc = 1;
  int64_t ret_int64 = ex_inquire_int(exoid, EX_INQ_NS_NODE_LEN);
  fail_huge_status_calloc = 0;
  EXCHECK(ex_close(exoid));

  if (ret_int64 != EX_FATAL) {
    fprintf(stderr, "ex_inquire_int returned length %" PRId64 ", expected EX_FATAL\n", ret_int64);
    return 1;
  }
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file_with_huge_node_set_count() != 0 || check_ex_inquire() != 0 ||
      check_ex_inquire_int() != 0) {
    return 1;
  }

  printf("test-inquire-set-count-overflow: all checks passed\n");
  return 0;
}
