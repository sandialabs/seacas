/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Waitany.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************       PMPI_Waitany.c     ************************/
/****************************************************************************/
/* Author : Lisa Alano July 11 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*==========================================================================*/
int PMPI_Waitany( int count, MPI_Request array_of_requests[], int* index, MPI_Status* status)
{
  int retval, i;
  MPI_Request request;
  MPI_Status recv_status;

  retval = _MPI_checkStatus(status);
  if ( retval != MPI_SUCCESS ) return retval;

  retval = _MPI_Check_Request_Array(count, array_of_requests); 
  if ( retval != MPI_SUCCESS ) return MPI_UNDEFINED;

  /*POLL THROUGH REQUESTS*/
    for(i=0; i<count; i++) {
      request = (MPI_Request) &(array_of_requests[i]);
      if (request != MPI_REQUEST_NULL) {
        if (request->send) {/*SEND REQUEST*/
          ;/*Should I check the buffer list?*/
          /*ASK PAT*/
          array_of_requests[i] = MPI_REQUEST_NULL;
          status->MPI_SOURCE = _MPI_RANK; 
          status->MPI_TAG = request->tag;
          status->MPI_ERROR = MPI_SUCCESS;
          *index = i;
          _MPI_Unset_Request(request);  /* KDDKDD  Free request memory */
          return MPI_SUCCESS;

        }

	/*RECEIVE REQUEST*/
        else {
          retval = PMPI_Recv(request->buffer, request->size, request->type, _MPI_RANK, request->tag, request->comm, &recv_status); 
          if (retval == MPI_SUCCESS) {
            /*ASK PAT*/
            status->MPI_SOURCE = _MPI_RANK;
            status->MPI_TAG = request->tag;
            status->MPI_ERROR = MPI_SUCCESS;
            _MPI_Req_Invalid(i);
            array_of_requests[i] = MPI_REQUEST_NULL;
            *index = i;
            return MPI_SUCCESS;
          }

          else if (retval== MPI_ERR_PENDING) {
            ;
	  }

          else {
            return retval; /*ERROR IN MPI_Recv.c*/ 
	  }
        }
      }
    }  
  return MPI_ERR_OTHER;
  
}
/*==========================================================================*/

