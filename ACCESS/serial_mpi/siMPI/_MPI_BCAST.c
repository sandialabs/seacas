/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: _MPI_BCAST.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/* FILE  ******************      MPI_BCAST.c        ************************/
/****************************************************************************/
/* Author : Lisa Alano July 9 2002                                          */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*==========================================================================*/
int MPI_Bcast ( void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm ) {
  _MPI_COVERAGE();
  return PMPI_Bcast(buffer, count, datatype, root, comm);
}
/*==========================================================================*/

