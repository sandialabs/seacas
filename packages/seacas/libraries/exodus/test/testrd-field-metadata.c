/*
 * Copyright(C) 1999-2024 National Technology & Engineering Solutions
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

static void get_field_cardinality(ex_field *field, ex_basis *basis, int bas_cnt)
{
  for (int j = 0; j < field->nesting; j++) {
    if (field->cardinality[j] == 0) {
      if (field->type[j] == EX_BASIS) {
        field->cardinality[j] = basis[0].cardinality;
      }
      else {
        field->cardinality[j] = ex_field_cardinality(field->type[j]);
      }
    }
  }
}

static void print_basis_metadata(ex_basis *basis, size_t num_basis)
{
  for (size_t j = 0; j < num_basis; j++) {
    fprintf(stderr, "\nBasis Metadata: Name: %s, Cardinality: %d\n", basis[j].name,
            basis[j].cardinality);
    fprintf(stderr,
            "ordinal,\t subc:  _dim\t_ordinal\t_dof_ordinal\t_num_dof\t xi      eta     zeta\n");
    for (int i = 0; i < basis[j].cardinality; i++) {
      fprintf(stderr, "%8d\t%8d\t%8d\t%8d\t%8d\t%6.3f\t%6.3f\t%6.3f\n", i, basis[j].subc_dim[i],
              basis[j].subc_ordinal[i], basis[j].subc_dof_ordinal[i], basis[j].subc_num_dof[i],
              basis[j].xi[i], basis[j].eta[i], basis[j].zeta[i]);
    }
  }
}

static char *get_basis_type(char *type_name, size_t which)
{
  if (type_name[0] != '\0') {
    char *string = strdup(type_name);
    char *tofree = string;
    char *token  = strsep(&string, ",");
    for (int i = 0; i < which; i++) {
      token = strsep(&string, ",");
    }
    if (token != NULL) {
      static char basis_type[256 + 1];
      ex_copy_string(basis_type, token, 256);
      free(tofree);
      return basis_type;
    }
    free(tofree);
  }
}

static void print_field_metadata(ex_field *field)
{
  fprintf(stderr, "\n");
  fprintf(stderr, "Field Metadata: Name: %s, Nesting: %d\n", field->name, field->nesting);
  for (int j = 0; j < field->nesting; j++) {
    char  sep        = field->component_separator[j] == 0 ? ' ' : field->component_separator[j];
    char *basis_type = NULL;
    if (field->type[j] == EX_BASIS) {
      basis_type = get_basis_type(field->type_name, j);
      fprintf(stderr, "\tNesting level: %d, Type: %s (%s), Cardinality: %d, Separator: \"%c\"\n", j,
              ex_field_type_enum_to_string(field->type[j]), basis_type, field->cardinality[j], sep);
    }
    else {
      fprintf(stderr, "\tNesting level: %d, Type: %s, Cardinality: %d, Separator: \"%c\"\n", j,
              ex_field_type_enum_to_string(field->type[j]), field->cardinality[j], sep);
    }
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
      fprintf(stderr, "\t\tComponent %d, Full name = %s\n", jj, name);
    }
  }
  else if (field->nesting == 2) {
    for (int kk = 1; kk <= field->cardinality[1]; kk++) {
      for (int jj = 1; jj <= field->cardinality[0]; jj++) {
        const char *name = ex_component_field_name(field, (int[]){jj, kk});
        fprintf(stderr, "\t\tComponent %d %d, Full name = %s\n", jj, kk, name);
      }
    }
  }
  else if (field->nesting == 3) {
    for (int ii = 1; ii <= field->cardinality[2]; ii++) {
      for (int kk = 1; kk <= field->cardinality[1]; kk++) {
        for (int jj = 1; jj <= field->cardinality[0]; jj++) {
          const char *name = ex_component_field_name(field, (int[]){jj, kk, ii});
          fprintf(stderr, "\t\tComponent %d %d %d, Full name = %s\n", jj, kk, ii, name);
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
            fprintf(stderr, "\t\tComponent %d %d %d %d, Full name = %s\n", jj, kk, ii, mm, name);
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

  int exoid = ex_open("test-field-metadata.exo", EX_READ, &CPU_word_size, &IO_word_size, &version);

  printf("\nafter ex_open\n");
  if (exoid < 0) {
    exit(1);
  }

  /* Check for nodal fields... */
  {
    int fld_cnt = ex_get_field_metadata_count(exoid, EX_NODAL, 0);
    assert(fld_cnt == 2);
    ex_field fields[2] = {{.entity_type = EX_NODAL}, {.entity_type = EX_NODAL}};
    EXCHECK(ex_get_field_metadata(exoid, fields));

    for (int i = 0; i < fld_cnt; i++) {
      get_field_cardinality(&fields[i], NULL, 0);
      print_field_metadata(&fields[i]);
      print_full_field_names(&fields[i]);
    }
  }

  {
    int fld_cnt = ex_get_field_metadata_count(exoid, EX_ELEM_BLOCK, 10);
    assert(fld_cnt == 2);
    ex_field fields[2] = {{.entity_id = 10, .entity_type = EX_ELEM_BLOCK},
                          {.entity_id = 10, .entity_type = EX_ELEM_BLOCK}};
    EXCHECK(ex_get_field_metadata(exoid, fields));

    for (int i = 0; i < fld_cnt; i++) {
      get_field_cardinality(&fields[i], NULL, 0);
      print_field_metadata(&fields[i]);
      print_full_field_names(&fields[i]);
    }
  }
  // ------------------------------------------------------------------------
  int bas_cnt = ex_get_basis_metadata_count(exoid);
  assert(bas_cnt == 2);
  struct ex_basis basis[2];
  EXCHECK(ex_initialize_basis_struct(basis, bas_cnt, 0));
  EXCHECK(ex_get_basis_metadata(exoid, basis));

  /*
   * Now, allocate memory for all pointer members of basis and call to populate...
   */
  EXCHECK(ex_initialize_basis_struct(basis, bas_cnt, 1));
  EXCHECK(ex_get_basis_metadata(exoid, basis));
  print_basis_metadata(basis, 2);

  {
    int fld_cnt = ex_get_field_metadata_count(exoid, EX_ELEM_BLOCK, 11);
    assert(fld_cnt == 3);
    ex_field fields[3] = {{.entity_id = 11, .entity_type = EX_ELEM_BLOCK},
                          {.entity_id = 11, .entity_type = EX_ELEM_BLOCK},
                          {.entity_id = 11, .entity_type = EX_ELEM_BLOCK}};
    EXCHECK(ex_get_field_metadata(exoid, fields));

    for (int i = 0; i < fld_cnt; i++) {
      get_field_cardinality(&fields[i], basis, bas_cnt);
      print_field_metadata(&fields[i]);
      print_full_field_names(&fields[i]);
    }
  }

  // Now, deallocate any memory allocated on the `basis` struct.
  EXCHECK(ex_initialize_basis_struct(basis, bas_cnt, -1));

  int fld_cnt = ex_get_field_metadata_count(exoid, EX_ELEM_BLOCK, 12);
  assert(fld_cnt == 0);

  int error = ex_close(exoid);
  printf("\nafter ex_close, error = %3d\n", error);
  return 0;
}
