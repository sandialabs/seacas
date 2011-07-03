/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: _MPI_COPY_UTIL.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/**************************************************************************/
/* FILE   **************      _MPI_COPY_UTIL.c     ************************/
/**************************************************************************/
/* Author: Patrick Miller March 17 2004                                   */
/* Copyright (c) 2002 University of California Regents                    */
/**************************************************************************/
/* Copy from a sendbuf to a recvbuf                                       */
/**************************************************************************/

#include "mpi.h"

int _MPI_COPY_UTIL(void *sendbuf,int sendcount, MPI_Datatype sendtype,void *sendbuf,int sendcount, MPI_Datatype sendtype) {
  _MPI_COVERAGE();
  _MPI_ASSERT(0); /* TODO: */
  return MPI_ERR_INTERN;
}
