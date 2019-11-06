/*
 * Copyright (c) 2005-2017 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of NTESS nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
/*****************************************************************************
 *
 * testrd - read exodus file test.exo created by testwt
 *
 * author - Sandia National Laboratories
 *          Larry A. Schoof - Original
 *
 *
 * environment - UNIX
 *
 * entry conditions -
 *   input parameters:
 *       int     exoid                   exodus file id
 *
 * exit conditions -
 *
 * revision history -
 *
 *
 *****************************************************************************/

#include "exodusII.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* #include "drmd.h" */

int main(int argc, char **argv)
{
  int  exoid, num_dim, num_nodes, num_elem, num_elem_blk, num_node_sets, num_assembly;
  int  num_side_sets, error;
  int  i;
  int *ids;
  int *num_elem_in_block  = NULL;
  int *num_nodes_per_elem = NULL;
  int *num_attr           = NULL;
  int  CPU_word_size, IO_word_size;
  int  idum;

  float version;
  float fdum;

  char *block_names[10];
  char  name[MAX_STR_LENGTH + 1];
  char  elem_type[MAX_STR_LENGTH + 1];
  char  title_chk[MAX_LINE_LENGTH + 1];
  char *cdum = 0;

  CPU_word_size = 0; /* sizeof(float) */
  IO_word_size  = 0; /* use what is stored in file */

  ex_opts(EX_VERBOSE | EX_ABORT);

  /* open EXODUS II files */
  exoid = ex_open("test-assembly.exo", /* filename path */
                  EX_READ,             /* access mode = READ */
                  &CPU_word_size,      /* CPU word size */
                  &IO_word_size,       /* IO word size */
                  &version);           /* ExodusII library version */

  printf("\nafter ex_open\n");
  if (exoid < 0) {
    exit(1);
  }

  printf("test-assembly.exo is an EXODUSII file; version %4.2f\n", version);
  /*   printf ("         CPU word size %1d\n",CPU_word_size);  */
  printf("         I/O word size %1d\n", IO_word_size);
  ex_inquire(exoid, EX_INQ_API_VERS, &idum, &version, cdum);
  printf("EXODUSII API; version %4.2f\n", version);

  ex_inquire(exoid, EX_INQ_LIB_VERS, &idum, &version, cdum);
  printf("EXODUSII Library API; version %4.2f (%d)\n", version, idum);

  /* read database parameters */
  {
    ex_init_params par;
    error = ex_get_init_ext(exoid, &par);

    printf("after ex_get_init, error = %3d\n", error);

    printf("database parameters:\n");
    printf("title =  '%s'\n", par.title);
    printf("num_dim = %" PRId64 "\n", par.num_dim);
    printf("num_assembly = %" PRId64 "\n", par.num_assembly);
    printf("num_nodes = %" PRId64 "\n", par.num_nodes);
    printf("num_edge = %" PRId64 "\n", par.num_edge);
    printf("num_face = %" PRId64 "\n", par.num_face);
    printf("num_elem = %" PRId64 "\n", par.num_elem);
    printf("num_elem_blk = %" PRId64 "\n", par.num_elem_blk);
    printf("num_node_sets = %" PRId64 "\n", par.num_node_sets);
    printf("num_side_sets = %" PRId64 "\n", par.num_side_sets);

    num_dim       = par.num_dim;
    num_elem      = par.num_elem;
    num_nodes     = par.num_nodes;
    num_elem_blk  = par.num_elem_blk;
    num_node_sets = par.num_node_sets;
    num_side_sets = par.num_side_sets;
    num_assembly  = par.num_assembly;

    /* Check that ex_inquire gives same title */
    error = ex_inquire(exoid, EX_INQ_TITLE, &idum, &fdum, title_chk);
    printf("after ex_inquire, error = %d\n", error);
    if (strcmp(par.title, title_chk) != 0) {
      printf("error in ex_inquire for EX_INQ_TITLE %s, vs %s\n", par.title, title_chk);
    }
  }

  /* read element block parameters */
  if (num_elem_blk > 0) {
    ids                = (int *)calloc(num_elem_blk, sizeof(int));
    num_elem_in_block  = (int *)calloc(num_elem_blk, sizeof(int));
    num_nodes_per_elem = (int *)calloc(num_elem_blk, sizeof(int));
    num_attr           = (int *)calloc(num_elem_blk, sizeof(int));

    error = ex_get_ids(exoid, EX_ELEM_BLOCK, ids);
    printf("\nafter ex_get_elem_blk_ids, error = %3d\n", error);

    for (i = 0; i < num_elem_blk; i++) {
      block_names[i] = (char *)calloc((MAX_STR_LENGTH + 1), sizeof(char));
    }

    error = ex_get_names(exoid, EX_ELEM_BLOCK, block_names);
    printf("\nafter ex_get_names, error = %3d\n", error);

    for (i = 0; i < num_elem_blk; i++) {
      ex_get_name(exoid, EX_ELEM_BLOCK, ids[i], name);
      if (strcmp(name, block_names[i]) != 0) {
        printf("error in ex_get_name for block id %d\n", ids[i]);
      }
      error = ex_get_block(exoid, EX_ELEM_BLOCK, ids[i], elem_type, &(num_elem_in_block[i]),
                           &(num_nodes_per_elem[i]), 0, 0, &(num_attr[i]));
      printf("\nafter ex_get_elem_block, error = %d\n", error);

      printf("element block id = %2d\n", ids[i]);
      printf("element type = '%s'\n", elem_type);
      printf("num_elem_in_block = %2d\n", num_elem_in_block[i]);
      printf("num_nodes_per_elem = %2d\n", num_nodes_per_elem[i]);
      printf("num_attr = %2d\n", num_attr[i]);
      printf("name = '%s'\n", block_names[i]);
    }
  }

  /* Read assembly information */
  /* Verify ex_inquire_int gives same value for assembly count... */
  int chk_num_assembly = ex_inquire_int(exoid, EX_INQ_ASSEMBLY);
  if (chk_num_assembly != num_assembly) {
    printf("error in ex_inquire_int for EX_INQ_ASSEMBLY: %d vs %d\n", chk_num_assembly,
           num_assembly);
  }
  if (num_assembly > 0) {
    int   assembly_ids[10];
    char *assembly_names[10];
    for (i = 0; i < num_assembly; i++) {
      assembly_names[i] = (char *)calloc((MAX_STR_LENGTH + 1), sizeof(char));
    }

    error = ex_get_ids(exoid, EX_ASSEMBLY, assembly_ids);
    printf("\nafter ex_get_ids(EX_ASSEMBLY), error = %3d\n", error);

    error = ex_get_names(exoid, EX_ASSEMBLY, assembly_names);
    printf("\nafter ex_get_names(EX_ASSEMBLY), error = %3d\n", error);

    ex_assembly assemblies[10];
    int         entity[10];
    for (i = 0; i < num_assembly; i++) {
      ex_get_name(exoid, EX_ASSEMBLY, assembly_ids[i], name);
      if (strcmp(name, assembly_names[i]) != 0) {
        printf("error in ex_get_name for assembly id %d\n", assembly_ids[i]);
      }

      assemblies[i].id   = assembly_ids[i];
      assemblies[i].name = assembly_names[i];
      /* Clear out name to make sure still getting same name */
      assemblies[i].name[0] = '\0';

      assemblies[i].entity_list = &entity;
      error                     = ex_get_assembly(exoid, &assemblies[i]);
      printf("\nafter ex_get_assembly, id=%d, error = %3d\n", assembly_ids[i], error);
      printf("Assembly named '%s' has id %lld. It contains %d entities of type '%s'\n\t",
             assemblies[i].name, assemblies[i].id, assemblies[i].entity_count,
             ex_name_of_object(assemblies[i].type));
      for (int j = 0; j < assemblies[i].entity_count; j++) {
        printf("%d, ", entity[j]);
      }
      printf("\n");
    }

    ex_assembly assmbly[10];
    for (i = 0; i < num_assembly; i++) {
      assmbly[i].name = NULL;
      assmbly[i].name = assembly_names[i];
      /* Clear out name to make sure still getting same name */
      assmbly[i].name[0]     = '\0';
      assmbly[i].entity_list = NULL;
    }
    error = ex_get_assemblies(exoid, assmbly);
    printf("\nafter ex_get_assemblies, error = %3d\n", error);
    for (i = 0; i < num_assembly; i++) {
      printf("Assembly named '%s' has id %lld. It contains %d entities of type '%s'\n",
             assmbly[i].name, assmbly[i].id, assmbly[i].entity_count,
             ex_name_of_object(assmbly[i].type));
    }
  }
  /*  free(block_names[i]); */
  error = ex_close(exoid);
  printf("\nafter ex_close, error = %3d\n", error);
  return 0;
}
