/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Get_count.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ************************  MPI_Get_count.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Get_count( 
        MPI_Status *status, 
        MPI_Datatype datatype, 
        int *count )
{
  _MPI_COVERAGE();
  return PMPI_Get_count (status, datatype, count);
}

