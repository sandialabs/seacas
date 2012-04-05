/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Request_free.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ********************  PMPI_Request_free.c   ************************/
/****************************************************************************/
/* Author : Lisa Alano July 23 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int PMPI_Request_free( MPI_Request *request )
{
  if (_MPI_checkRequest(*request)!=MPI_SUCCESS)
  {
    _MPI_ERR_ROUTINE (MPI_ERR_REQUEST, "MPI_REQUEST_FREE: argument error");
    MPI_Abort (MPI_COMM_NULL, MPI_ERR_REQUEST);
    return MPI_ERR_REQUEST;
  }
  return _MPI_Unset_Request(*request);
}

