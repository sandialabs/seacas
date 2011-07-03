/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Ibsend.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/***********************************************************************************************/
/* FILE  **************************      PMPI_Ibsend.c         *********************************/
/***********************************************************************************************/
/* Author : Lisa Alano July 15 2002                                                            */
/* Copyright (c) 2002 University of California Regents                                         */
/***********************************************************************************************/

#include "mpi.h"

/*=============================================================================================*/
int PMPI_Ibsend (void* message, int count, MPI_Datatype datatype, int dest,
        int tag, MPI_Comm comm, MPI_Request* request)
{
  int retval;
  retval = PMPI_Bsend(message, count, datatype, dest, tag, comm); 
  if (retval == MPI_SUCCESS) {
    _MPI_Set_Request(request, message, count, datatype, _MPI_TRUE, tag, comm); /* KDDKDD passing request pointer */
  } else {
    if ( request ) *request = MPI_REQUEST_NULL;
  }
  return retval;
}
/*=============================================================================================*/

