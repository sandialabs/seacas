/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Comm_rank.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/******************************************************************/
/* FILE  ***********      MPI_Comm_rank.c      ********************/
/******************************************************************/
/* Author : Lisa Alano June 21 2002                               */
/* Copyright (c) 2002 University of California Regents            */
/******************************************************************/

#include <stdio.h>
#include "mpi.h"

int MPI_Comm_rank(MPI_Comm comm, int* rank)
{
  _MPI_COVERAGE();
  return  PMPI_Comm_rank(comm, rank);
}

