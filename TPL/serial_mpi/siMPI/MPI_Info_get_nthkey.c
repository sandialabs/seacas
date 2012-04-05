/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Info_get_nthkey.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************   MPI_Info_get_nthkey.c  ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Info_get_nthkey(MPI_Info info, int n, char *key)
{
  _MPI_COVERAGE();
  return PMPI_Info_get_nthkey (info, n, key);
}

