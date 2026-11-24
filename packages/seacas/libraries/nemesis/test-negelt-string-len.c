/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * The NEGELT Fortran jacket must use an Exodus-sized scratch buffer when it
 * calls ex_get_elem_type(), even if the caller's Fortran character argument is
 * shorter than MAX_STR_LENGTH.  The returned Fortran string is then bounded to
 * the caller-provided length and blank-filled.
 */

#include "exodusII.h"
#include <stdio.h>
#include <string.h>

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    if ((funcall) != EX_NOERR) {                                                                   \
      fprintf(stderr, "%s failed\n", #funcall);                                                    \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

static const char *filename = "negelt-string-len.exo";

/* This test compiles forbind/ne_jack.c without Build64, so F2C(negelt) is negelt
 * and the jacket element-block id argument is a 32-bit int. */
void negelt(int *idne, int *elem_blk_id, char *elem_type, int *ierr, size_t elem_typelen);

static int create_file(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "negelt string length", 2, 4, 1, 1, 0, 0));
  double x[4] = {0.0, 1.0, 1.0, 0.0};
  double y[4] = {0.0, 0.0, 1.0, 1.0};
  EXCHECK(ex_put_coord(exoid, x, y, NULL));
  EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, 10, "quad", 1, 4, 0, 0, 0));
  int connect[4] = {1, 2, 3, 4};
  EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, 10, connect, NULL, NULL));
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

static int check_short_fortran_string(void)
{
  int exoid = open_file();
  if (exoid < 0) {
    return 1;
  }

  int  block_id = 10;
  int  ierr     = -1;
  char elem_type[8];
  memset(elem_type, '#', sizeof(elem_type));

  negelt(&exoid, &block_id, elem_type, &ierr, 2);
  EXCHECK(ex_close(exoid));

  if (ierr != EX_NOERR) {
    fprintf(stderr, "NEGELT failed for a short Fortran string (status %d)\n", ierr);
    return 1;
  }
  if (elem_type[0] != 'q' || elem_type[1] != 'u') {
    fprintf(stderr, "NEGELT did not copy the bounded element type prefix\n");
    return 1;
  }
  for (size_t i = 2; i < sizeof(elem_type); i++) {
    if (elem_type[i] != '#') {
      fprintf(stderr, "NEGELT wrote beyond the caller's short Fortran string at index %zu\n", i);
      return 1;
    }
  }

  return 0;
}

static int check_standard_fortran_string(void)
{
  int exoid = open_file();
  if (exoid < 0) {
    return 1;
  }

  int  block_id = 10;
  int  ierr     = -1;
  char elem_type[MAX_STR_LENGTH];
  memset(elem_type, '#', sizeof(elem_type));

  negelt(&exoid, &block_id, elem_type, &ierr, MAX_STR_LENGTH);
  EXCHECK(ex_close(exoid));

  if (ierr != EX_NOERR) {
    fprintf(stderr, "NEGELT failed for a standard Fortran string (status %d)\n", ierr);
    return 1;
  }
  if (memcmp(elem_type, "quad", 4) != 0) {
    fprintf(stderr, "NEGELT did not copy the element type\n");
    return 1;
  }
  for (size_t i = 4; i < MAX_STR_LENGTH; i++) {
    if (elem_type[i] != ' ') {
      fprintf(stderr, "NEGELT did not blank-fill the standard Fortran string at index %zu\n", i);
      return 1;
    }
  }

  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file() != 0 || check_short_fortran_string() != 0 ||
      check_standard_fortran_string() != 0) {
    return 1;
  }

  printf("test-negelt-string-len: all checks passed\n");
  return 0;
}
