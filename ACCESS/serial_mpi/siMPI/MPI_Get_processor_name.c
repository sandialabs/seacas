/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Get_processor_name.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ****************  MPI_Get_processor_name.c  ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Get_processor_name( 
        char *name,
        int *resultlen)
{
  _MPI_COVERAGE();
  return PMPI_Get_processor_name (name, resultlen);
}

