/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Attr_put.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:39 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************     MPI_Attr_put.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 18 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Attr_put ( MPI_Comm comm, int keyval, void *attr_value )
{
  _MPI_COVERAGE();
  return PMPI_Attr_put (comm, keyval, attr_value); 
}

