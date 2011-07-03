/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: dr_output_const.h,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:37:57 $
 *    Revision: 1.3 $
 ****************************************************************************/


#ifndef _DR_OUTPUT_CONST_H_
#define _DR_OUTPUT_CONST_H_

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif


#include "dr_input_const.h"

extern void print_distributed_mesh(
  int Proc,
  int Num_Proc,
  MESH_INFO_PTR mesh
);

extern int output_results(
  char *cmd_file,
  char *tag,
  int Proc,
  int Num_Proc,
  PROB_INFO_PTR prob,
  PARIO_INFO_PTR pio_info,
  MESH_INFO_PTR mesh);

extern int output_gnu(char *cmd_file,
  char *tag,
  int Proc,
  int Num_Proc,
  PROB_INFO_PTR prob,
  PARIO_INFO_PTR pio_info,
  MESH_INFO_PTR mesh);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif /* _DR_OUTPUT_CONST_H_ */
