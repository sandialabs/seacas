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
#include "exodusII_int.h"

int main(int argc, char **argv)
{
  int exoid, num_dim, num_nodes, num_elem, num_elem_blk;
  int num_node_sets, num_side_sets, num_assembly;
  int error;
  int CPU_word_size, IO_word_size;

  char *          title = "This is a test";
  char *          block_names[7];
  struct ex_block blocks[7];

  ex_opts(EX_VERBOSE);

  /* Specify compute and i/o word size */

  CPU_word_size = 0; /* sizeof(float) */
  IO_word_size  = 4; /* (4 bytes) */

  /* create EXODUS II file */

  exoid = ex_create("test-assembly.exo", /* filename path */
                    EX_CLOBBER,          /* create mode */
                    &CPU_word_size,      /* CPU float word size in bytes */
                    &IO_word_size);      /* I/O float word size in bytes */
  printf("after ex_create for test.exo, exoid = %d\n", exoid);
  printf(" cpu word size: %d io word size: %d\n", CPU_word_size, IO_word_size);

  /* initialize file with parameters */
  {
    ex_init_params par;
    num_dim       = 3;
    num_nodes     = 33;
    num_elem      = 7;
    num_elem_blk  = 7;
    num_node_sets = 2;
    num_side_sets = 5;
    num_assembly  = 4;

    ex_copy_string(par.title, title, MAX_LINE_LENGTH + 1);
    par.num_dim       = num_dim;
    par.num_nodes     = num_nodes;
    par.num_edge      = 0;
    par.num_edge_blk  = 0;
    par.num_face      = 5;
    par.num_face_blk  = 1;
    par.num_elem      = num_elem;
    par.num_elem_blk  = num_elem_blk;
    par.num_node_sets = num_node_sets;
    par.num_edge_sets = 0;
    par.num_face_sets = 0;
    par.num_side_sets = 0;
    par.num_elem_sets = 0;
    par.num_node_maps = 0;
    par.num_edge_maps = 0;
    par.num_face_maps = 0;
    par.num_elem_maps = 0;
    par.num_assembly  = num_assembly;

    error = ex_put_init_ext(exoid, &par);
    printf("after ex_put_init_ext, error = %d\n", error);

    if (error) {
      ex_close(exoid);
      exit(-1);
    }
  }

  /* write element block parameters */
  for (int i = 0; i < 10; i++) {
    blocks[i].type                = EX_ELEM_BLOCK;
    blocks[i].id                  = 0;
    blocks[i].num_entry           = 0;
    blocks[i].num_nodes_per_entry = 0;
    blocks[i].num_edges_per_entry = 0;
    blocks[i].num_faces_per_entry = 0;
    blocks[i].num_attribute       = 0;
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

  blocks[0].num_entry = 1;
  blocks[1].num_entry = 1;
  blocks[2].num_entry = 1;
  blocks[3].num_entry = 1;
  blocks[4].num_entry = 1;
  blocks[5].num_entry = 1;
  blocks[6].num_entry = 1;

  blocks[0].num_nodes_per_entry = 4; /* elements in block #1 are 4-node quads  */
  blocks[1].num_nodes_per_entry = 4; /* elements in block #2 are 4-node quads  */
  blocks[2].num_nodes_per_entry = 8; /* elements in block #3 are 8-node hexes  */
  blocks[3].num_nodes_per_entry = 4; /* elements in block #4 are 4-node tetras */
  blocks[4].num_nodes_per_entry = 6; /* elements in block #5 are 6-node wedges */
  blocks[5].num_nodes_per_entry = 8; /* elements in block #6 are 8-node tetras */
  blocks[6].num_nodes_per_entry = 3; /* elements in block #7 are 3-node tris   */

  blocks[0].id = 10;
  blocks[1].id = 11;
  blocks[2].id = 12;
  blocks[3].id = 13;
  blocks[4].id = 14;
  blocks[5].id = 15;
  blocks[6].id = 16;

  error = ex_put_block_params(exoid, num_elem_blk, blocks);
  printf("after ex_put_block_params, error = %d\n", error);

  if (error) {
    ex_close(exoid);
    exit(-1);
  }

  /* Write element block names */
  for (int i = 0; i < num_elem_blk; i++) {
    error = ex_put_name(exoid, EX_ELEM_BLOCK, blocks[i].id, block_names[i]);
    printf("after ex_put_names, error = %d\n", error);
  }

  if (error) {
    ex_close(exoid);
    exit(-1);
  }

  int   assembly_ids[] = {100, 200, 300, 400};
  char *names[]        = {"Root", "Child1", "Child2", "Child3"};
  ex_put_names(exoid, EX_ASSEMBLY, names);

  int         list_100[] = {100, 200, 300, 400};
  ex_assembly assembly;
  assembly.id           = assembly_ids[0];
  assembly.name         = "RootName";
  assembly.type         = EX_ASSEMBLY;
  assembly.entity_count = 4;
  assembly.entity_list  = NULL;

  ex_put_assembly(exoid, assembly);

  assembly.entity_list = list_100;
  ex_put_assembly(exoid, assembly);

  int list_200[]        = {10, 11, 12, 13};
  assembly.id           = assembly_ids[1];
  assembly.name         = "ChildName_2";
  assembly.type         = EX_ELEM_BLOCK;
  assembly.entity_count = 4;
  assembly.entity_list  = list_200;

  ex_put_assembly(exoid, assembly);

  int list_300[]        = {14, 15, 16};
  assembly.id           = assembly_ids[2];
  assembly.name         = "ChildName_3";
  assembly.type         = EX_ELEM_BLOCK;
  assembly.entity_count = 3;
  assembly.entity_list  = list_300;

  ex_put_assembly(exoid, assembly);

  int list_400[]        = {10, 16};
  assembly.id           = assembly_ids[3];
  assembly.name         = "ChildName_4";
  assembly.type         = EX_ELEM_BLOCK;
  assembly.entity_count = 2;
  assembly.entity_list  = NULL;

  ex_put_assembly(exoid, assembly);

  assembly.entity_list = list_400;
  ex_put_assembly(exoid, assembly);

  /* Add some arbitrary attributes to the assemblies */
  {
    double scale     = 1.5;
    double offset[]  = {1.1, 2.2, 3.3};
    char * dimension = "length";
    int    units[]   = {1, 0, 0, -1};

    ex_put_double_attribute(exoid, EX_ASSEMBLY, 100, "Scale", 1, &scale);
    ex_put_double_attribute(exoid, EX_ASSEMBLY, 200, "Offset", 3, offset);
    ex_put_text_attribute(exoid, EX_ASSEMBLY, 300, "Dimension", dimension);
    ex_put_integer_attribute(exoid, EX_ASSEMBLY, 400, "Units", 4, units);

    /* Make sure this works for non-assemblies also... */
    ex_put_integer_attribute(exoid, EX_ELEM_BLOCK, 11, "Units", 4, units);
  }

  error = ex_put_variable_param(exoid, EX_ASSEMBLY, 4);
  printf("after ex_put_variable_param, error = %d\n", error);
  if (error) {
    ex_close(exoid);
    exit(-1);
  }

  {
    char *var_names[4];
    var_names[0] = "Momentum_X";
    var_names[1] = "Momentum_Y";
    var_names[2] = "Momentum_Z";
    var_names[3] = "Kinetic_Energy";

    error = ex_put_variable_names(exoid, EX_ASSEMBLY, 4, var_names);
    printf("after ex_put_variable_names, error = %d\n", error);
    if (error) {
      ex_close(exoid);
      exit(-1);
    }
  }

#if 0
  { /* Output time steps ... */
    float *var_vals = (float *)calloc(4, CPU_word_size);
    for (int i = 0; i < 10; i++) {
      float time_val = (float)(i+1) / 100.0f;

      error = ex_put_time(exoid, i+1, &time_val);
      printf("after ex_put_time, error = %d\n", error);
      if (error) {
	ex_close(exoid);
	exit(-1);
      }

      /* write assembly variables */
      for (int k = 0; k < num_assembly; k++) {
	for (int j = 0; j < 4; j++) {
	  var_vals[j] = (float)(j + 2) * time_val + k;
	  error = ex_put_var(exoid, i+1, EX_ASSEMBLY, j, assembly_ids[k], 1, var_vals);
	  printf("after ex_put_var, error = %d\n", error);
	  if (error) {
	    ex_close(exoid);
	    exit(-1);
	  }
	}
      }

    }
    
  }
#endif

  /* close the EXODUS files
   */
  error = ex_close(exoid);
  printf("after ex_close, error = %d\n", error);
  if (error) {
    ex_close(exoid);
    exit(-1);
  }
  return 0;
}
