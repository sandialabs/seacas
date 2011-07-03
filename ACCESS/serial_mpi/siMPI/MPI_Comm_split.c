/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Comm_split.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************    MPI_Comm_split.c      ************************/
/****************************************************************************/
/* Author : Lisa Alano July 19 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Comm_split ( MPI_Comm comm, int color, int key, MPI_Comm *comm_out )
{
  _MPI_COVERAGE();
  return PMPI_Comm_split (comm, color, key, comm_out); 
}

