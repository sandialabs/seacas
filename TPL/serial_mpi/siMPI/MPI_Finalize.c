/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Finalize.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/******************************************************************/
/* FILE  ***********        MPI_Finalize.c     ********************/
/******************************************************************/
/* Author : Lisa Alano June 18 2002                               */
/* Copyright (c) 2002 University of California Regents            */
/******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

int MPI_Finalize (void)
{  
  _MPI_COVERAGE();
  return  PMPI_Finalize();
  /* 
  #ifdef _MPI_DEBUG
  if (x == _MPI_NOT_OK)
    printf ("\n>>>>ERROR MPI_Finalize\n");
  else
    printf(">>MPI_Finalize was called.\n");
  #endif
  */
}

  
