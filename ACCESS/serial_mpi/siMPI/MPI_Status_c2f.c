/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_Status_c2f.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ***********************  MPI_Status_c2f.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_Status_c2f( MPI_Status *c_status, MPI_Fint *f_status )
{
  _MPI_COVERAGE();
  return PMPI_Status_c2f (c_status, f_status);
}

