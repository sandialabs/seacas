/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Ssend.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/***********************************************************************************************/
/* FILE  **************************      PMPI_Ssend.c        ***********************************/
/***********************************************************************************************/
/* Author : Lisa Alano July 12 2002                                                            */
/* Copyright (c) 2002 University of California Regents                                         */
/***********************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*=============================================================================================*/
/* SAME as PMPI_Rsend.c */
int PMPI_Ssend (void* message, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
{
  int size, retval, index;
  char* p;

 _MPI_COVERAGE();
  _MPI_CHECK_STATUS(&comm);
  retval = _MPI_checks(message, count, datatype, dest, tag, comm);
  if (retval == MPI_SUCCESS)
  {
    while(1)
    {
      index = _MPI_Req_Find(tag, comm);
      if (index >=0)
      { 
        size = _MPI_calculateSize(count, datatype);  
        p = (char *)_MPI_safeMalloc(size, "Error with malloc for send buffer."); 
        p = memcpy(p, message, size);
        retval =_MPI_Buff_Insert(p, count, datatype, tag, comm);
        return retval;
      }
    }
  }  

 _MPI_COVERAGE();
  return _MPI_NOT_OK;
}
/*=============================================================================================*/
