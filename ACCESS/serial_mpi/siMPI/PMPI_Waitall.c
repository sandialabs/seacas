/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Waitall.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************       PMPI_Waitall.c     ************************/
/****************************************************************************/
/* Author : Lisa Alano July 11 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*==========================================================================*/
int PMPI_Waitall(
        int count,
        MPI_Request array_of_requests[],
        MPI_Status array_of_statuses[] )
{
  int retval, i;
  MPI_Request request; 
  MPI_Status* status = &array_of_statuses[0];

  retval = _MPI_checkStatus(status);
  if(retval != MPI_SUCCESS) {
    return retval;
  }

  for(i=0; i<count; i++) {
    request = array_of_requests[i];
    /* MPI_REQUEST_NULL should be fine.
     *retval = _MPI_checkRequest(&array_of_requests[i]);
     *if(retval != MPI_SUCCESS)
     * return retval;
     */
    if (request != MPI_REQUEST_NULL) {
      if (request->send) {
        ;/*Should I check the buffer list? */
        array_of_requests[i] = MPI_REQUEST_NULL;
        status->MPI_SOURCE = _MPI_RANK; 
        status->MPI_TAG = request->tag;
        status->MPI_ERROR = MPI_SUCCESS;
        _MPI_Unset_Request(request);  /* KDDKDD Free request memory */

      } else {
        retval = PMPI_Recv(request->buffer, request->size, request->type, _MPI_RANK, request->tag, request->comm, status); 
        if (retval == MPI_SUCCESS) {
          array_of_requests[i] = MPI_REQUEST_NULL;
          status->MPI_SOURCE = _MPI_RANK;
          status->MPI_TAG = request->tag;
          status->MPI_ERROR = MPI_SUCCESS;
        } else {
          return retval;
	}
      }
    }
  }  
  return MPI_SUCCESS;
  
}
/*==========================================================================*/

