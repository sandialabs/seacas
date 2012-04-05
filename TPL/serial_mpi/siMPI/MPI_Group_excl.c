/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Group_excl.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ***********************  MPI_Group_excl.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Group_excl ( MPI_Group group, int n, int *ranks, MPI_Group *newgroup )
{
  _MPI_COVERAGE();
  return PMPI_Group_excl (group, n, ranks, newgroup);
}

