/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Group_compare.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************    MPI_Group_compare.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Group_compare ( MPI_Group group1, MPI_Group group2, int *result )
{
  _MPI_COVERAGE();
  return PMPI_Group_compare (group1, group2, result);
}

