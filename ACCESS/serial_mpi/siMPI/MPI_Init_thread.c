/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Init_thread.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  **********************  MPI_Init_thread.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Init_thread(int *argc, char ***argv, int required, int *provided )
{
  _MPI_COVERAGE();
  return PMPI_Init_thread (argc, argv, required, provided);
}

