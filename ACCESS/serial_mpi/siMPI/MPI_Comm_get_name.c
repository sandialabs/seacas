/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Comm_get_name.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************   MPI_Comm_get_name.c    ************************/
/****************************************************************************/
/* Author : Lisa Alano July 18 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Comm_get_name( MPI_Comm comm, char *namep, int *reslen )
{
  _MPI_COVERAGE();
  return PMPI_Comm_get_name (comm, namep, reslen); 
}

