/*
 * Copyright(C) 1999-2022 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */
#include "exodusII.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
{
  ex_opts(EX_VERBOSE | EX_ABORT);

  /* open EXODUS II files */
  float version;
  int   CPU_word_size = 0; /* sizeof(float) */
  int   IO_word_size  = 0; /* use what is stored in file */

  int exoid = ex_open("test-field-metadata.exo", /* filename path */
                      EX_READ,                   /* access mode = READ */
                      &CPU_word_size,            /* CPU word size */
                      &IO_word_size,             /* IO word size */
                      &version);                 /* ExodusII library version */

  printf("\nafter ex_open\n");
  if (exoid < 0) {
    exit(1);
  }

  int fld_cnt = ex_get_field_metadata_count(exoid, EX_ELEM_BLOCK, 10);
  assert(fld_cnt == 2);

  fld_cnt = ex_get_field_metadata_count(exoid, EX_ELEM_BLOCK, 11);
  assert(fld_cnt == 2);

  ex_field fields[2];
  ex_get_field_metadata(exoid, EX_ELEM_BLOCK, 10, fields);
  ex_get_field_metadata(exoid, EX_ELEM_BLOCK, 11, fields);
  int error = ex_close(exoid);
  printf("\nafter ex_close, error = %3d\n", error);
  return 0;
}
