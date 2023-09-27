/*
 * Copyright(C) 1999-2023 National Technology & Engineering Solutions
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

static void get_field_cardinality(ex_field *field, ex_basis *basis)
{
  for (int j = 0; j < field->nesting; j++) {
    if (field->cardinality[j] == 0) {
      if (field->type[j] == EX_BASIS) {
        field->cardinality[j] = basis->cardinality;
      }
      else {
        field->cardinality[j] = ex_field_cardinality(field->type[j]);
      }
    }
  }
}

static void print_field_metadata(ex_field *field)
{
  fprintf(stderr, "\n");
  for (int j = 0; j < field->nesting; j++) {
    char sep = field->component_separator[j] == 0 ? ' ' : field->component_separator[j];
    fprintf(stderr,
            "Field Metadata: Name: %s, Nesting: %d, Type: %s, Cardinality: %d, Separator: \"%c\"\n",
            field->name, field->nesting, ex_field_type_enum_to_string(field->type[j]),
            field->cardinality[j], sep);
    if (field->type[0] == EX_FIELD_TYPE_USER_DEFINED) {
      fprintf(stderr, "\tUser-defined suffices: %s\n", field->suffices);
    }
  }
}

static void print_full_field_names(ex_field *field)
{
  if (field->nesting == 1) {
    for (int jj = 1; jj <= field->cardinality[0]; jj++) {
      const char *name = ex_component_field_name(field, (int[]){jj});
      fprintf(stderr, "\tComponent %d, Full name = %s\n", jj, name);
    }
  }
  else if (field->nesting == 2) {
    for (int kk = 1; kk <= field->cardinality[1]; kk++) {
      for (int jj = 1; jj <= field->cardinality[0]; jj++) {
        const char *name = ex_component_field_name(field, (int[]){jj, kk});
        fprintf(stderr, "\tComponent %d %d, Full name = %s\n", jj, kk, name);
      }
    }
  }
  else if (field->nesting == 3) {
    for (int ii = 1; ii <= field->cardinality[2]; ii++) {
      for (int kk = 1; kk <= field->cardinality[1]; kk++) {
        for (int jj = 1; jj <= field->cardinality[0]; jj++) {
          const char *name = ex_component_field_name(field, (int[]){jj, kk, ii});
          fprintf(stderr, "\tComponent %d %d %d, Full name = %s\n", jj, kk, ii, name);
        }
      }
    }
  }
  else if (field->nesting == 4) {
    for (int mm = 1; mm <= field->cardinality[3]; mm++) {
      for (int ii = 1; ii <= field->cardinality[2]; ii++) {
        for (int kk = 1; kk <= field->cardinality[1]; kk++) {
          for (int jj = 1; jj <= field->cardinality[0]; jj++) {
            const char *name = ex_component_field_name(field, (int[]){jj, kk, ii, mm});
            fprintf(stderr, "\tComponent %d %d %d %d, Full name = %s\n", jj, kk, ii, mm, name);
          }
        }
      }
    }
  }
}

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

  ex_field fields[3];
  fields[0].entity_id   = 10;
  fields[1].entity_id   = 10;
  fields[0].entity_type = EX_ELEM_BLOCK;
  fields[1].entity_type = EX_ELEM_BLOCK;
  EXCHECK(ex_get_field_metadata(exoid, fields));

  for (int i = 0; i < fld_cnt; i++) {
    get_field_cardinality(&fields[i], NULL);
    print_field_metadata(&fields[i]);
    print_full_field_names(&fields[i]);
  }

  // ------------------------------------------------------------------------
  struct ex_basis basis = (ex_basis){.name             = "",
                                     .cardinality      = 0,
                                     .subc_dim         = NULL,
                                     .subc_ordinal     = NULL,
                                     .subc_dof_ordinal = NULL,
                                     .subc_num_dof     = NULL,
                                     .xi               = NULL,
                                     .eta              = NULL,
                                     .zeta             = NULL};
  /* Query basis on a block where it doesn't exist */
  fprintf(stderr, "\nEXPECT Warning/Error about no basis on block 10.");
  int status = ex_get_basis_metadata(exoid, EX_ELEM_BLOCK, 10, &basis);
  if (status != EX_NOTFOUND) {
    fprintf(stderr,
            "Error calling ex_get_basis for non-existent basis.  Should return EX_NOTFOUND");
  }

  EXCHECK(ex_get_basis_metadata(exoid, EX_ELEM_BLOCK, 11, &basis));
  /*
   * Now, allocate memory for all pointer members of basis and call to populate...
   */
  basis.subc_dim         = calloc(basis.cardinality, sizeof(int));
  basis.subc_ordinal     = calloc(basis.cardinality, sizeof(int));
  basis.subc_dof_ordinal = calloc(basis.cardinality, sizeof(int));
  basis.subc_num_dof     = calloc(basis.cardinality, sizeof(int));
  basis.xi               = calloc(basis.cardinality, sizeof(double));
  basis.eta              = calloc(basis.cardinality, sizeof(double));
  basis.zeta             = calloc(basis.cardinality, sizeof(double));
  EXCHECK(ex_get_basis_metadata(exoid, EX_ELEM_BLOCK, 11, &basis));

  fld_cnt = ex_get_field_metadata_count(exoid, EX_ELEM_BLOCK, 11);
  assert(fld_cnt == 3);

  fields[0].entity_id = 11;
  fields[1].entity_id = 11;
  EXCHECK(ex_get_field_metadata(exoid, fields));

  for (int i = 0; i < fld_cnt; i++) {
    get_field_cardinality(&fields[i], &basis);
    print_field_metadata(&fields[i]);
    print_full_field_names(&fields[i]);
  }

  fld_cnt = ex_get_field_metadata_count(exoid, EX_ELEM_BLOCK, 12);
  assert(fld_cnt == 0);

  int error = ex_close(exoid);
  printf("\nafter ex_close, error = %3d\n", error);
  return 0;
}