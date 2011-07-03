/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Barrier.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************      PMPI_Barrier.c      ************************/
/****************************************************************************/
/* Author : Lisa Alano July 18 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int PMPI_Barrier ( MPI_Comm comm)
{
  int retval;
  _MPI_COVERAGE();
  retval = _MPI_checkCommunicator (comm);
  if (retval != MPI_SUCCESS)
  {
  _MPI_COVERAGE();
    _MPI_ERR_ROUTINE (retval, "MPI_Barrier: Invalid communicator.");
     MPI_Abort (comm, retval);
  }
  return retval;
}

