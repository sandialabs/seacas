/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Address.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************       MPI_Address       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 17 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Address( void *location, MPI_Aint *address)
{
  _MPI_COVERAGE();
  return PMPI_Address(location, address); 
}

