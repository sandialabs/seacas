/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Init.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/******************************************************************/
/* FILE  ***********        MPI_Init.c         ********************/
/******************************************************************/
/* Author : Lisa Alano June 18 2002                               */
/* Copyright (c) 2002 University of California Regents            */
/******************************************************************/

#include <stdio.h>
#include "mpi.h"

int MPI_Init( int *argc, char **argv[])
{
  _MPI_COVERAGE();
  return  PMPI_Init(argc, argv);
}

