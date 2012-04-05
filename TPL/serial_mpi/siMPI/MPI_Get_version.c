/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Get_version.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  **********************  MPI_Get_version.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Get_version( 
        int *version, 
        int *subversion )
{
  _MPI_COVERAGE();
  return PMPI_Get_version (version, subversion);
}

