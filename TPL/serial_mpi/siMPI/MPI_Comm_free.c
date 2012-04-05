/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Comm_free.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/******************************************************************/
/* FILE  ***********      MPI_Comm_free.c      ********************/
/******************************************************************/
/* Author : Lisa Alano June 24 2002                               */
/* Copyright (c) 2002 University of California Regents            */
/******************************************************************/

#include <stdio.h>
#include "mpi.h"

int MPI_Comm_free(MPI_Comm* comm)
{
  _MPI_COVERAGE();
  return  PMPI_Comm_free(comm);
}

