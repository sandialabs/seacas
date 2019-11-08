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
 * testwt - test write an ExodusII database file
 *
 * author - Sandia National Laboratories
 *          Larry A. Schoof - Original
 *          Vic Yarberry    - Added headers and error logging
 *               7/7/93          Modified for use with Exodus 2.00
 *
 *
 * environment - UNIX
 *
 * entry conditions -
 *
 * exit conditions -
 *
 * revision history -
 *
 *  This is a test program for the C binding of the EXODUS II
 *  database write routines.
 *
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "exodusII.h"

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    error = (funcall);                                                                             \
    printf("after %s, error = %d\n", TOSTRING(funcall), error);                                    \
    if (error != EX_NOERR) {                                                                       \
      fprintf(stderr, "Error calling %s\n", TOSTRING(funcall));                                    \
      ex_close(exoid);                                                                             \
      exit(-1);                                                                                    \
    }                                                                                              \
  } while (0)

int main(int argc, char **argv)
{
  int exoid, num_dim, num_nodes, num_elem, num_elem_blk;
  int num_node_sets, num_side_sets, num_assembly;
  int error;
  int CPU_word_size, IO_word_size;

  char *          title = "This is a test";
  char *          block_names[10];
  struct ex_block blocks[10];

  ex_opts(EX_VERBOSE);

  /* Specify compute and i/o word size */

  CPU_word_size = 8;
  IO_word_size  = 8;

  /* create EXODUS II file */

  exoid = ex_create("test-assembly.exo", /* filename path */
                    EX_CLOBBER,          /* create mode */
                    &CPU_word_size,      /* CPU double word size in bytes */
                    &IO_word_size);      /* I/O double word size in bytes */
  printf("after ex_create for test.exo, exoid = %d\n", exoid);
  printf(" cpu word size: %d io word size: %d\n", CPU_word_size, IO_word_size);

  ex_set_option(exoid, EX_OPT_ENABLE_FEATURE, EX_ENABLE_ASSEMBLY);
  /* initialize file with parameters */
  {
    num_dim       = 3;
    num_nodes     = 33;
    num_elem      = 7;
    num_elem_blk  = 7;
    num_node_sets = 2;
    num_side_sets = 5;
    num_assembly  = 4;

    ex_init_params par = {.num_dim       = num_dim,
                          .num_nodes     = num_nodes,
                          .num_face      = 5,
                          .num_face_blk  = 1,
                          .num_elem      = num_elem,
                          .num_elem_blk  = num_elem_blk,
                          .num_node_sets = num_node_sets,
                          .num_assembly  = num_assembly};

    ex_copy_string(par.title, title, MAX_LINE_LENGTH + 1);
    EXCHECK(ex_put_init_ext(exoid, &par));
  }

  /* write element block parameters */
  for (int i = 0; i < num_elem_blk; i++) {
    blocks[i] = (ex_block){.type = EX_ELEM_BLOCK, .num_entry = 1, .id = i + 10};
  }

  block_names[0] = "block_1";
  block_names[1] = "block_2";
  block_names[2] = "block_3";
  block_names[3] = "block_4";
  block_names[4] = "block_5";
  block_names[5] = "block_6";
  block_names[6] = "block_7";

  ex_copy_string(blocks[0].topology, "quad", MAX_STR_LENGTH + 1);
  ex_copy_string(blocks[1].topology, "quad", MAX_STR_LENGTH + 1);
  ex_copy_string(blocks[2].topology, "hex", MAX_STR_LENGTH + 1);
  ex_copy_string(blocks[3].topology, "tetra", MAX_STR_LENGTH + 1);
  ex_copy_string(blocks[4].topology, "wedge", MAX_STR_LENGTH + 1);
  ex_copy_string(blocks[5].topology, "tetra", MAX_STR_LENGTH + 1);
  ex_copy_string(blocks[6].topology, "tri", MAX_STR_LENGTH + 1);

  blocks[0].num_nodes_per_entry = 4; /* elements in block #1 are 4-node quads  */
  blocks[1].num_nodes_per_entry = 4; /* elements in block #2 are 4-node quads  */
  blocks[2].num_nodes_per_entry = 8; /* elements in block #3 are 8-node hexes  */
  blocks[3].num_nodes_per_entry = 4; /* elements in block #4 are 4-node tetras */
  blocks[4].num_nodes_per_entry = 6; /* elements in block #5 are 6-node wedges */
  blocks[5].num_nodes_per_entry = 8; /* elements in block #6 are 8-node tetras */
  blocks[6].num_nodes_per_entry = 3; /* elements in block #7 are 3-node tris   */

  EXCHECK(ex_put_block_params(exoid, num_elem_blk, blocks));

  /* Write element block names */
  for (int i = 0; i < num_elem_blk; i++) {
    EXCHECK(ex_put_name(exoid, EX_ELEM_BLOCK, blocks[i].id, block_names[i]));
  }

  int   assembly_ids[] = {100, 200, 300, 400};
  char *names[]        = {"Root", "Child1", "Child2", "Child3"};

  EXCHECK(ex_put_names(exoid, EX_ASSEMBLY, names));

  {
    int         list_100[] = {100, 200, 300, 400};
    ex_assembly assembly   = {assembly_ids[0], "RootName", EX_ASSEMBLY, 4, NULL};
    EXCHECK(ex_put_assembly(exoid, assembly));

    assembly.entity_list = list_100;
    EXCHECK(ex_put_assembly(exoid, assembly));
  }

  {
    int         list_200[] = {10, 11, 12, 13};
    ex_assembly assembly   = {assembly_ids[1], "ChildName_2", EX_ELEM_BLOCK, 4, list_200};
    EXCHECK(ex_put_assembly(exoid, assembly));
  }

  {
    int         list_300[] = {14, 15, 16};
    ex_assembly assembly   = {assembly_ids[2], "ChildName_3", EX_ELEM_BLOCK, 3, list_300};
    EXCHECK(ex_put_assembly(exoid, assembly));
  }

  {
    int         list_400[] = {10, 16};
    ex_assembly assembly   = {assembly_ids[3], "ChildName_4", EX_ELEM_BLOCK, 2, NULL};
    EXCHECK(ex_put_assembly(exoid, assembly));

    assembly.entity_list = list_400;
    EXCHECK(ex_put_assembly(exoid, assembly));
  }

  /* Add some arbitrary attributes to the assemblies */
  {
    double scale     = 1.5;
    double offset[]  = {1.1, 2.2, 3.3};
    char * dimension = "length";
    int    units[]   = {1, 0, 0, -1};

    EXCHECK(ex_put_double_attribute(exoid, EX_ASSEMBLY, 100, "Scale", 1, &scale));
    EXCHECK(ex_put_double_attribute(exoid, EX_ASSEMBLY, 200, "Offset", 3, offset));
    EXCHECK(ex_put_text_attribute(exoid, EX_ASSEMBLY, 300, "Dimension", dimension));
    EXCHECK(ex_put_integer_attribute(exoid, EX_ASSEMBLY, 400, "Units", 4, units));

    ex_attribute attribute = {.entity_type = EX_ASSEMBLY,
                              .entity_id   = 100,
                              .name        = {"Units"},
                              .type        = EX_INTEGER,
                              .value_count = 4,
                              .values      = units};
    EXCHECK(ex_put_attribute(exoid, attribute));

    ex_attribute attr_offset = {EX_ASSEMBLY, 300, "Offset", EX_DOUBLE, 3, offset};
    EXCHECK(ex_put_attribute(exoid, attr_offset));

    /* Make sure this works for non-assemblies also... */
    EXCHECK(ex_put_integer_attribute(exoid, EX_ELEM_BLOCK, 11, "Units", 4, units));
    EXCHECK(ex_put_text_attribute(exoid, EX_GLOBAL, 0, "ACIS", "STEP-X-43-1547836-Rev 0"));
  }

  EXCHECK(ex_put_variable_param(exoid, EX_ASSEMBLY, 4));

  {
    char *var_names[4] = {"Momentum_X", "Momentum_Y", "Momentum_Z", "Kinetic_Energy"};
    EXCHECK(ex_put_variable_names(exoid, EX_ASSEMBLY, 4, var_names));
  }

#if 0
  { /* Output time steps ... */
    double *var_vals = (double *)calloc(4, CPU_word_size);
    for (int i = 0; i < 10; i++) {
      double time_val = (double)(i+1) / 100.0f;

      EXCHECK(ex_put_time(exoid, i+1, &time_val));

      /* write assembly variables */
      for (int k = 0; k < num_assembly; k++) {
	for (int j = 0; j < 4; j++) {
	  var_vals[j] = (double)(j + 2) * time_val + k;
	  EXCHECK(ex_put_var(exoid, i+1, EX_ASSEMBLY, j, assembly_ids[k], 1, var_vals));
	}
      }

    }
    
  }
#endif

  /* close the EXODUS files
   */
  EXCHECK(ex_close(exoid));
  return 0;
}
