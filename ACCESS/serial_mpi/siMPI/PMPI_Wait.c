/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Wait.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************        PMPI_Wait.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 9 2002                                          */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*==========================================================================*/
int PMPI_Wait ( MPI_Request* request, MPI_Status* status)
{
  int retval;

  if ( !request ) {
     _MPI_ERR_ROUTINE(MPI_ERR_REQUEST,"Request pointer is null");
    return _MPI_NOT_OK;
  }

  if (*request == MPI_REQUEST_NULL) {
    return MPI_SUCCESS;
  }

  retval = _MPI_checkStatus(status);
  if(retval != MPI_SUCCESS)
  {
    _MPI_ERR_ROUTINE (retval, "MPI_WAIT: MPI_Status error");
    MPI_Abort (MPI_COMM_NULL, retval);
    return retval;
  }

  if ((*request)->send) {
    status->MPI_SOURCE = _MPI_RANK; 
    status->MPI_TAG = (*request)->tag;
    status->MPI_ERROR = MPI_SUCCESS;
    _MPI_Unset_Request(*request);  /* KDDKDD Free request memory */
  }

  else {
    if ((*request)->valid !=_MPI_NOT_VALID) {
      retval = PMPI_Recv((*request)->buffer, (*request)->size, (*request)->type, _MPI_RANK, (*request)->tag, (*request)->comm, status); 
    }
    if (retval != MPI_SUCCESS) {
      return retval;
    }
  }  
  *request = MPI_REQUEST_NULL;
  return MPI_SUCCESS;
}
/*==========================================================================*/

