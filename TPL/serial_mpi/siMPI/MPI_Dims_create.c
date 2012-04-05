/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Dims_create.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************  MPI_Dims_create.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 19 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Dims_create(
        int nnodes, 
        int ndims, 
        int *dims)
{
  _MPI_COVERAGE();
  return PMPI_Dims_create ( nnodes, ndims, dims); 
}

