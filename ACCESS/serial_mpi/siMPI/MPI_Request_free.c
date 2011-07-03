/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Request_free.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  *********************  MPI_Request_free.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"


int MPI_Request_free( MPI_Request *request )
{
  _MPI_COVERAGE();
  return PMPI_Request_free (request);
}

