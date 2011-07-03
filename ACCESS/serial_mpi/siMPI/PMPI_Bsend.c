/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Bsend.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/***********************************************************************************************/
/* FILE  **************************       PMPI_Bsend.c       ***********************************/
/***********************************************************************************************/
/* Author : Lisa Alano July 12 2002                                                            */
/* Copyright (c) 2002 University of California Regents                                         */
/***********************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*=============================================================================================*/
int PMPI_Bsend( void* buffer, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
{
  int size, retval;

 _MPI_COVERAGE();
  _MPI_CHECK_STATUS(&comm);
  retval = _MPI_checks(buffer, count, datatype, dest, tag, comm);
  
  if (retval == MPI_SUCCESS)
  {
  _MPI_COVERAGE();
    size = _MPI_calculateSize(count, datatype);  
    retval =_MPI_Buff_Insert(buffer, size, datatype, tag, comm);
    return retval;
  }
  else
  {
  _MPI_COVERAGE();
     _MPI_ERR_ROUTINE (retval, "MPI_BSEND: argument error");
     MPI_Abort (comm, retval);
  }
  

 _MPI_COVERAGE();
  return _MPI_NOT_OK;
}
/*=============================================================================================*/

