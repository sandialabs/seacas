/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * exi_check_att_text validates text attribute metadata before callers read
 * into fixed-size buffers.
 */

#include "exodusII.h"
#include "exodusII_int.h"
#include <netcdf.h>
#include <stdio.h>

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

static const char *filename = "att-text-len.exo";
static const char *attname  = "att_text_len_test";

static int create_file(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "attribute text length", 2, 0, 0, 0, 0, 0));
  EXCHECK(ex_close(exoid));
  return 0;
}

static int set_text_attribute(int ncid, const char *value, size_t len)
{
  NCCHECK(nc_redef(ncid));
  NCCHECK(nc_put_att_text(ncid, NC_GLOBAL, attname, len, value));
  NCCHECK(nc_enddef(ncid));
  return 0;
}

static int set_int_attribute(int ncid)
{
  int value = 7;
  NCCHECK(nc_redef(ncid));
  NCCHECK(nc_put_att_int(ncid, NC_GLOBAL, attname, NC_INT, 1, &value));
  NCCHECK(nc_enddef(ncid));
  return 0;
}

static int expect_status(const char *label, int got, int expected)
{
  if (got != expected) {
    fprintf(stderr, "%s: expected status %d, got %d\n", label, expected, got);
    return 1;
  }
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file() != 0) {
    return 1;
  }

  int ncid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));

  if (expect_status("missing attribute",
                    exi_check_att_text(ncid, NC_GLOBAL, "missing_text_attribute", 4, __func__),
                    EX_FATAL) != 0) {
    nc_close(ncid);
    return 1;
  }

  if (set_text_attribute(ncid, "abc", 3) != 0) {
    nc_close(ncid);
    return 1;
  }
  if (expect_status("short text attribute",
                    exi_check_att_text(ncid, NC_GLOBAL, attname, 4, __func__), EX_NOERR) != 0) {
    nc_close(ncid);
    return 1;
  }

  if (set_text_attribute(ncid, "abcd", 4) != 0) {
    nc_close(ncid);
    return 1;
  }
  if (expect_status("exact-fit text attribute",
                    exi_check_att_text(ncid, NC_GLOBAL, attname, 4, __func__), EX_NOERR) != 0) {
    nc_close(ncid);
    return 1;
  }

  if (set_text_attribute(ncid, "abcde", 5) != 0) {
    nc_close(ncid);
    return 1;
  }
  if (expect_status("overlong text attribute",
                    exi_check_att_text(ncid, NC_GLOBAL, attname, 4, __func__), EX_FATAL) != 0) {
    nc_close(ncid);
    return 1;
  }

  if (set_int_attribute(ncid) != 0) {
    nc_close(ncid);
    return 1;
  }
  if (expect_status("non-text attribute", exi_check_att_text(ncid, NC_GLOBAL, attname, 4, __func__),
                    EX_FATAL) != 0) {
    nc_close(ncid);
    return 1;
  }

  NCCHECK(nc_close(ncid));

  printf("test-att-text-len: all checks passed\n");
  return 0;
}
