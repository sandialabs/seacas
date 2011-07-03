/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Cart_create.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:39 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************    MPI_Cart_create.c     ************************/
/****************************************************************************/
/* Author : Lisa Alano July 18 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Cart_create ( MPI_Comm comm_old, int ndims, int *dims, int *periods,
                     int reorder, MPI_Comm *comm_cart )
{
  _MPI_COVERAGE();
  return PMPI_Cart_create (comm_old, ndims, dims, periods, reorder, comm_cart); 
}

