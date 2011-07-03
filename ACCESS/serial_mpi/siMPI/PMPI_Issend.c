/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Issend.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/***********************************************************************************************/
/* FILE  **************************      PMPI_Issend.c         *********************************/
/***********************************************************************************************/
/* Author : Lisa Alano July 15 2002                                                            */
/* Copyright (c) 2002 University of California Regents                                         */
/***********************************************************************************************/

#include <string.h>
#include "mpi.h"

/*=============================================================================================*/
/* Same behavior as PMPI_Irsend.c */
int PMPI_Issend (void* message, int count, MPI_Datatype datatype, int dest,
        int tag, MPI_Comm comm, MPI_Request* request)
{
  int size, retval, index;
  char* p;

 _MPI_COVERAGE();
  _MPI_CHECK_STATUS(&comm);
  retval = _MPI_checkRequest(*request);
  if ( retval!=MPI_SUCCESS ) {
    _MPI_Set_Request(request, message, count, datatype, _MPI_TRUE, tag, comm);
  }
  retval = _MPI_checks(message, count, datatype, dest, tag, comm);

  if (retval == MPI_SUCCESS) {
    index = _MPI_Req_Find(tag, comm);
    if ( index >= 0 ) {
      size = _MPI_calculateSize(count, datatype);
      p = (char *)_MPI_safeMalloc(size, "Error with malloc for send buffer.");
      p = memcpy(p, message, size);
      retval =_MPI_Buff_Insert(p, count, datatype, tag, comm);
      return retval;
    }
    return MPI_ERR_PENDING;
  } else {
     _MPI_ERR_ROUTINE (retval, "MPI_ISSEND / MPI_IRSEND: argument error");
     MPI_Abort (comm, retval);
  }

 _MPI_COVERAGE();
  return retval; 
}
/*=============================================================================================*/

