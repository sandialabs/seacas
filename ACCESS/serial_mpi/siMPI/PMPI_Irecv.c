/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Irecv.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************       PMPI_Irecv.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano June 27 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*==========================================================================*/
int PMPI_Irecv (void* message, int count, MPI_Datatype datatype, int source,
        int tag, MPI_Comm comm, MPI_Request* request)
{
  int retval, req;
  retval=_MPI_checks(message, count, datatype, source, tag, comm);

  if (retval != MPI_SUCCESS) {
    _MPI_ERR_ROUTINE (retval, "MPI_IRECV: argument error");
    MPI_Abort (comm, retval);
    return retval;
  }

#if 0
  /* Not sure what this is doing.... */
  if (*request == MPI_REQUEST_NULL) {
    _MPI_Set_Request(request, message, count, datatype, _MPI_FALSE, tag, comm);/* KDDKDD Passing request pointer */
  }
#endif

  if (retval == MPI_SUCCESS) {
    /* Check for New Request and Insert it into the Request queque */

#if 0
    /* TODO: WHAT IS THIS DOING? */
    req = _MPI_Req_Find(tag, comm);
#else
    req = -1;
#endif

    if ( req < 0 ) { /* It is a new request */
      req = _MPI_Req_Post(message, count, datatype, tag, comm, _MPI_FALSE);
      *request = &_MPI_REQ_LIST[req];
    }

#if 0
    /* TODO: This should wait until for the "wait" */
    size = _MPI_calculateSize(count, datatype);
    index = _MPI_Buff_Ifind(tag, comm);

    if (index == _MPI_NOT_OK) {
       return MPI_ERR_PENDING;
    }
 
    /*MESSAGE IS THERE*/
    _MPI_Req_Invalid(req);
    if (size < _MPI_DATA_BUFF[index].size) { /*MESSAGE IS TRUNCATED */
      message = memcpy(message, _MPI_DATA_BUFF[index].buffer, size);
      printf("MPI_RECV : Message truncated.\n");
      MPI_Abort(comm, MPI_ERR_COUNT);
    } else {
      message = memcpy(message, _MPI_DATA_BUFF[index].buffer, size);
    }
    _MPI_Data_Invalid(index);
    return MPI_SUCCESS;
#endif
  }
  return retval;
}
/*==========================================================================*/


