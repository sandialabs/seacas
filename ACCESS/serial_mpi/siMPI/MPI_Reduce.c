/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Reduce.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/* FILE  ******************       MPI_Reduce.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 1 2002                                          */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*==========================================================================*/
int MPI_Reduce ( void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm )
{
  _MPI_COVERAGE();
  return PMPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm);
}

/*==========================================================================*/
