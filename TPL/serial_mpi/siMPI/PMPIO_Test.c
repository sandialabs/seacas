/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPIO_Test.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************       PMPIO_Test.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 17 2002                                          */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int PMPIO_Test(MPIO_Request *request, int *flag, MPI_Status *status)
{
  fprintf(stderr,"%s:%d: NOT IMPLEMENTED\n",__FILE__,__LINE__);
  return MPI_Abort((MPI_Comm)NULL, MPI_UNDEFINED); 
}

