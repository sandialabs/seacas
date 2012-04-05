/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Startall.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  *********************  MPI_Startall.c   ****************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Startall( int count, MPI_Request array_of_requests[] )
{
  _MPI_COVERAGE();
  return PMPI_Startall (count, array_of_requests);
}

