/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Recv.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************        MPI_Recv.c        ************************/
/****************************************************************************/
/* Author : Lisa Alano June 27 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Recv (void* message, int count, MPI_Datatype datatype, int source,
        int tag, MPI_Comm comm, MPI_Status* status)
{
  _MPI_COVERAGE();
  return PMPI_Recv (message,count, datatype, source, tag, comm, status);
}
