/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Attr_delete.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************     MPI_Attr_delete.c    ************************/
/****************************************************************************/
/* Author : Lisa Alano July 17 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int MPI_Attr_delete ( MPI_Comm comm, int keyval )
{
  _MPI_COVERAGE();
  return PMPI_Attr_delete (comm, keyval); 
}

