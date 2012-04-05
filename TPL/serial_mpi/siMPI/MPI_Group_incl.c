/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Group_incl.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ***********************  MPI_Group_incl.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Group_incl ( MPI_Group group, int n, int *ranks, MPI_Group *group_out )
{
  return PMPI_Group_incl (group, n, ranks, group_out);
}

