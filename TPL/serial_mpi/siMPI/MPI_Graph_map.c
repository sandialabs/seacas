/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Graph_map.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ************************  MPI_Graph_map.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Graph_map ( MPI_Comm comm_old, int nnodes, int *index, int *edges, 
                   int *newrank )
{
  _MPI_COVERAGE();
  return PMPI_Graph_map (comm_old, nnodes, index, edges, newrank);
}

