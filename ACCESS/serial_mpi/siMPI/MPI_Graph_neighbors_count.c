/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Graph_neighbors_count.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  *************** MPI_Graph_neighbors_count.c ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Graph_neighbors_count ( MPI_Comm comm, int rank, int *nneighbors )
{
  _MPI_COVERAGE();
  return PMPI_Graph_neighbors_count (comm, rank, nneighbors);
}

