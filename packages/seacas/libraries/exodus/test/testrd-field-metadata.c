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

#define STRINGIFY(x) #x
#define TOSTRING(x)  STRINGIFY(x)

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    int error = (funcall);                                                                         \
    printf("after %s, error = %d\n", TOSTRING(funcall), error);                                    \
    if (error != EX_NOERR) {                                                                       \
      fprintf(stderr, "Error calling %s\n", TOSTRING(funcall));                                    \
      ex_close(exoid);                                                                             \
      exit(-1);                                                                                    \
    }                                                                                              \
  } while (0)

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

  fld_cnt = ex_get_field_metadata_count(exoid, EX_ELEM_BLOCK, 12);
  assert(fld_cnt == 0);

  ex_field fields[2];
  fields[0].entity_id   = 10;
  fields[1].entity_id   = 10;
  fields[0].entity_type = EX_ELEM_BLOCK;
  fields[1].entity_type = EX_ELEM_BLOCK;
  EXCHECK(ex_get_field_metadata(exoid, fields));

  fields[0].entity_id = 11;
  fields[1].entity_id = 11;
  EXCHECK(ex_get_field_metadata(exoid, fields));

  struct ex_basis basis = (ex_basis){.name             = "",
                                     .cardinality      = 0,
                                     .subc_dim         = NULL,
                                     .subc_ordinal     = NULL,
                                     .subc_dof_ordinal = NULL,
                                     .subc_num_dof     = NULL,
                                     .xi               = NULL,
                                     .eta              = NULL,
                                     .zeta             = NULL};
  EXCHECK(ex_get_basis_metadata(exoid, EX_ELEM_BLOCK, 10, &basis));
  EXCHECK(ex_get_basis_metadata(exoid, EX_ELEM_BLOCK, 11, &basis));

  int error = ex_close(exoid);
  printf("\nafter ex_close, error = %3d\n", error);
  return 0;
}
