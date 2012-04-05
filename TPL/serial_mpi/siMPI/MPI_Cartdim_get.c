/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Cartdim_get.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************     MPI_Cartdim_get.c    ************************/
/****************************************************************************/
/* Author : Lisa Alano July 18 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Cartdim_get ( MPI_Comm comm, int *ndims )
{
  _MPI_COVERAGE();
  return PMPI_Cartdim_get (comm, ndims); 
}

