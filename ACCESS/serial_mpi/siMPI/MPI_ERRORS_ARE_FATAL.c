/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_ERRORS_ARE_FATAL.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/******************************************************************/
/* FILE  ***********    MPI_ERRORS_ARE_FATAL.c ********************/
/******************************************************************/
/* Author : Lisa Alano June 24 2002                               */
/* Copyright (c) 2002 University of California Regents            */
/******************************************************************/

#include <stdio.h>
#include "mpi.h"

void MPI_ERRORS_ARE_FATAL (MPI_Comm* comm, int* error_code, ...)
{
  _MPI_COVERAGE();
  MPI_ERRORS_RETURN(comm, error_code);
  if (comm == (MPI_Comm)0) MPI_Abort(0, *error_code);
  MPI_Abort(*comm, *error_code);
}

