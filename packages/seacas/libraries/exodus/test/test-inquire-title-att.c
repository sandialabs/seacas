/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * ex_inquire(EX_INQ_TITLE) must bound title attribute reads before copying the
 * public MAX_LINE_LENGTH result.
 */

#include "exodusII.h"
#include <netcdf.h>
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

static const char *filename = "inquire-title-att.exo";

static int create_file(const char *title)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, title, 2, 0, 0, 0, 0, 0));
  EXCHECK(ex_close(exoid));
  return 0;
}

static int set_title_attribute(size_t title_len)
{
  char *title = malloc(title_len + 1);
  if (title == NULL) {
    return 1;
  }
  for (size_t i = 0; i < title_len; i++) {
    title[i] = (char)('A' + (i % 26));
  }
  title[title_len] = '\0';

  int ncid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  NCCHECK(nc_put_att_text(ncid, NC_GLOBAL, "title", title_len, title));
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  free(title);
  return 0;
}

static int read_title(char *title)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 98;
  }
  int   ret_int;
  float ret_float;
  int   status = ex_inquire(exoid, EX_INQ_TITLE, &ret_int, &ret_float, title);
  ex_close(exoid);
  return status;
}

static int expect_title(size_t stored_len, size_t expected_len)
{
  if (create_file("short title") != 0 || set_title_attribute(stored_len) != 0) {
    return 1;
  }

  char title[MAX_LINE_LENGTH + 1];
  memset(title, 0x5a, sizeof(title));
  int status = read_title(title);
  if (status != EX_NOERR) {
    fprintf(stderr, "ex_inquire title failed for stored length %zu (status %d)\n", stored_len,
            status);
    return 1;
  }
  if (strlen(title) != expected_len) {
    fprintf(stderr, "stored length %zu produced title length %zu, expected %zu\n", stored_len,
            strlen(title), expected_len);
    return 1;
  }
  for (size_t i = 0; i < expected_len; i++) {
    char expected = (char)('A' + (i % 26));
    if (title[i] != expected) {
      fprintf(stderr, "title byte %zu was %c, expected %c\n", i, title[i], expected);
      return 1;
    }
  }
  if (title[expected_len] != '\0') {
    fprintf(stderr, "title was not terminated at expected length %zu\n", expected_len);
    return 1;
  }
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (expect_title(12, 12) != 0) {
    return 1;
  }

  if (expect_title(2048, MAX_LINE_LENGTH) != 0) {
    return 1;
  }

  if (expect_title(4096, MAX_LINE_LENGTH) != 0) {
    return 1;
  }

  printf("test-inquire-title-att: all checks passed\n");
  return 0;
}
