/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPIO_Wait.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************      MPIO_Wait.c         ************************/
/****************************************************************************/
/* Author : Lisa Alano July 18 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPIO_Wait(MPIO_Request *request, MPI_Status *status)
{
  _MPI_COVERAGE();
  return PMPIO_Wait(request, status); 
}

