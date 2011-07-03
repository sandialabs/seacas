/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPIO_Test.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:39 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************        MPIO_Test.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 17 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPIO_Test(MPIO_Request *request, int *flag, MPI_Status *status)
{
  _MPI_COVERAGE();
  return PMPIO_Test(request, flag, status); 
}

