/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Error_class.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  **********************  MPI_Error_class.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 24 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Error_class( 
        int errorcode, 
        int *errorclass)
{
  _MPI_COVERAGE();
  return PMPI_Error_class (errorcode, errorclass);
}

