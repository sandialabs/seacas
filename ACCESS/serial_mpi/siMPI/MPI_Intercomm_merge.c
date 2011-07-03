/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Intercomm_merge.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************  MPI_Intercomm_merge.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Intercomm_merge ( MPI_Comm comm, int high, MPI_Comm *comm_out )
{
  _MPI_COVERAGE();
  return PMPI_Intercomm_merge (comm, high, comm_out);
}

