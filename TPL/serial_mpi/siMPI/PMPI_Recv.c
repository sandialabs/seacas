/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Recv.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************       PMPI_Recv.c        ************************/
/****************************************************************************/
/* Author : Lisa Alano June 27 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*==========================================================================*/
int PMPI_Recv (void* message, int count, MPI_Datatype datatype, int source,
        int tag, MPI_Comm comm, MPI_Status* status)
{
  int retval, size, index, req;

  retval=_MPI_checks(message, count, datatype, source, tag, comm);
  if (retval == MPI_SUCCESS) {
    if(_MPI_checkStatus(status)==MPI_SUCCESS) {
      size = _MPI_calculateSize(count, datatype);

      /* TODO: KDDKDD  Why does PMPI_Recv post a request?  */
      req =_MPI_Req_Post(message, count, datatype, tag, comm, _MPI_FALSE);
      index = _MPI_Buff_Find(tag, comm);
      if (index == _MPI_NOT_OK) {
         return MPI_ERR_TAG;
      }
        
      /*MESSAGE IS THERE*/
      _MPI_Req_Invalid(req);
      status->MPI_SOURCE = _MPI_RANK;
      status->MPI_TAG = _MPI_DATA_BUFF[index].tag;
      if (size < _MPI_DATA_BUFF[index].size) /*MESSAGE IS TRUNCATED*/
      {
        message = memcpy(message, _MPI_DATA_BUFF[index].buffer, size);
        _MPI_ERR_ROUTINE(MPI_ERR_COUNT, "MPI_RECV : Message truncated.");
        status->MPI_ERROR = MPI_ERR_COUNT; 
        MPI_Abort(comm, MPI_ERR_COUNT);
        return MPI_ERR_COUNT;
      } else {
        /* printf("RECV size = %d | size = %d\n", _MPI_DATA_BUFF[index].size, size); */
        message = memcpy(message, _MPI_DATA_BUFF[index].buffer, size);
        status->MPI_ERROR = MPI_SUCCESS;
      }
      _MPI_Data_Invalid(index);
      return MPI_SUCCESS;
    }
  }

  else {
     _MPI_ERR_ROUTINE (retval, "MPI_RECV argument error");
     MPI_Abort (comm, retval);
  }

  return retval;  
}

/*==========================================================================*/
