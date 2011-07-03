/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Iprobe.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ***********************   MPI_Iprobe.c   ***************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Iprobe( int source, int tag, MPI_Comm comm, int *flag, 
               MPI_Status *status )
{
  _MPI_COVERAGE();
  return PMPI_Iprobe (source, tag, comm, flag, status);
}

