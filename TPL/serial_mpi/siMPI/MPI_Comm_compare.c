/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Comm_compare.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************    MPI_Comm_compare.c    ************************/
/****************************************************************************/
/* Author : Lisa Alano July 18 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Comm_compare ( 
        MPI_Comm  comm1,
        MPI_Comm  comm2,
        int *result)
{
  _MPI_COVERAGE();
  return PMPI_Comm_compare (comm1, comm2, result); 
}

