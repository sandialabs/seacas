/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Test.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  *********************  PMPI_Test.c   *******************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int PMPI_Test ( 
        MPI_Request  *request,
        int          *flag,
        MPI_Status   *status)
{
  /* Flag indicates message was sent */
  if ( flag ) *flag = 1;

  /* Just defer to the code in Wait() */
  return PMPI_Wait(request,status);
}

