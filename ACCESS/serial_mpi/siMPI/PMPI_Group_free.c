/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Group_free.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ********************** PMPI_Group_free.c    ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int PMPI_Group_free ( MPI_Group *group )
{
  if (!group) return MPI_ERR_ARG;
  *group = MPI_GROUP_NULL;
  return MPI_SUCCESS;
}

