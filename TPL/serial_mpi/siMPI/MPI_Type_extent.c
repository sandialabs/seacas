/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Type_extent.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  **********************  MPI_Type_extent.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 24 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Type_extent( MPI_Datatype datatype, MPI_Aint *extent )
{
  _MPI_COVERAGE();
  return PMPI_Type_extent (datatype, extent);
}

