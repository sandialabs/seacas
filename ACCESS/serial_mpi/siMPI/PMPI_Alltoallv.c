/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Alltoallv.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************     PMPI_Alltoallv.c     ************************/
/****************************************************************************/
/* Author : Lisa Alano July 17 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int PMPI_Alltoallv ( 
        void *sendbuf, 
        int *sendcnts, 
        int *sdispls, 
        MPI_Datatype sendtype, 
        void *recvbuf, 
        int *recvcnts, 
        int *rdispls, 
        MPI_Datatype recvtype, 
        MPI_Comm comm )
{
  _MPI_COVERAGE();
  fprintf(stderr,"%s:%d: NOT IMPLEMENTED\n",__FILE__,__LINE__);
  return MPI_Abort((MPI_Comm)NULL, MPI_UNDEFINED); 
}

