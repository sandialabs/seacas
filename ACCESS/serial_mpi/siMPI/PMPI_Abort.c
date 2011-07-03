/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Abort.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/* FILE  ***********        PMPI_Abort.c       ********************/
/******************************************************************/
/* Author : Lisa Alano June 24 2002                               */
/* Copyright (c) 2002 University of California Regents            */
/******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "mpi.h"

int PMPI_Abort( MPI_Comm comm, int errorcode )
{
  _MPI_COVERAGE();
   #ifndef _MPI_DEBUG
   #define _MPIDEBUG 1
   #endif
   _MPI_ERR_ROUTINE(errorcode, "MPI aborting...");
   exit(-1);
   return -1;
}
