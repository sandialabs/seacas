/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Isend.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/***********************************************************************************************/
/* FILE  **************************      PMPI_Isend.c          *********************************/
/***********************************************************************************************/
/* Author : Lisa Alano June 27 2002                                                            */
/* Copyright (c) 2002 University of California Regents                                         */
/***********************************************************************************************/
#include "mpi.h"

/*=============================================================================================*/
int PMPI_Isend (void* message, int count, MPI_Datatype datatype, int dest,
        int tag, MPI_Comm comm, MPI_Request* request)
{
  int retval;
  retval = PMPI_Send(message, count, datatype, dest, tag, comm); 

  /* Fill in the request object so that we can query it later */
  if (retval == MPI_SUCCESS && request != 0) {
    _MPI_Set_Request(request, message, count, datatype, _MPI_TRUE, tag, comm);
  }
  return retval;
}
/*=============================================================================================*/

