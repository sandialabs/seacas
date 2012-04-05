/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Comm_remote_size.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ****************** MPI_Comm_remote_size.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 19 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Comm_remote_size ( MPI_Comm comm, int *size )
{
  _MPI_COVERAGE();
  return PMPI_Comm_remote_size (comm, size);
}

