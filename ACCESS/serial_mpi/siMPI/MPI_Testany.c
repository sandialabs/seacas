/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Testany.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  *********************  MPI_Testany.c   *****************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Testany( 
        int count, 
        MPI_Request array_of_requests[], 
        int *index, int *flag, 
        MPI_Status *status )
{
  _MPI_COVERAGE();
  return PMPI_Testany (count, array_of_requests, index, flag, status);
}

