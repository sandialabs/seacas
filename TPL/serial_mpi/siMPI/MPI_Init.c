/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Init.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
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

