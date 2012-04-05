/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Reduce.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************      PMPI_Reduce.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 1 2002                                          */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/*==========================================================================*/
int PMPI_Reduce ( void *sendbuf, void *recvbuf, int count, 
   MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm )
{
  int size;
  int retval;

 _MPI_COVERAGE();

  /* ----------------------------------------------- */
  /* We shortcut on length 0 sends since there is no */
  /* work to do...                                   */
  /* ----------------------------------------------- */
  if ( count == 0 ) return MPI_SUCCESS;

  /* ----------------------------------------------- */
  /* First we verify that the sendbuf, recvbuf, and  */
  /* operator are OK.                                */
  /* ----------------------------------------------- */
  retval = _MPI_checks(sendbuf, count, datatype, root, 1, comm);
  if ( retval != MPI_SUCCESS ) {
    _MPI_ERR_ROUTINE (MPI_ERR_OTHER, "MPI_REDUCE : Invalid argument");
    MPI_Abort(comm, retval);
    return _MPI_NOT_OK;
  }
      
  if ( _MPI_checkBuffer(recvbuf) != MPI_SUCCESS ) {
    _MPI_ERR_ROUTINE (MPI_ERR_BUFFER, "MPI_REDUCE : Invalid buffer pointer");
    MPI_Abort(comm, MPI_ERR_BUFFER);
    return _MPI_NOT_OK;
  }
   
  if ( _MPI_checkOp(op) != MPI_SUCCESS ) {
    _MPI_ERR_ROUTINE(MPI_ERR_OP, "MPI_REDUCE : Invalid MPI_Op");
    MPI_Abort(comm, MPI_ERR_OP);
    return _MPI_NOT_OK;
  }

  /* ----------------------------------------------- */
  /* Guard against buffer overlap...                 */
  /* ----------------------------------------------- */
  size = _MPI_calculateSize(count, datatype);
  if (  _MPI_check_overlap(sendbuf, recvbuf, size) != MPI_SUCCESS ) {
    _MPI_ERR_ROUTINE (MPI_ERR_BUFFER, "MPI_REDUCE : Invalid buffer pointer: Arguments must specify different buffers (no aliasing)");
    MPI_Abort(comm, MPI_ERR_BUFFER);
    return _MPI_NOT_OK;
  }

#if 0
  /* KDDKDD:  This initialization isn't right, particularly for 
   * KDDKDD:  user-defined functions that do comparisons of in with inout.
   * KDDKDD:  I'm not sure what the correct initialization is, though.
   * KDDKDD:  Nor am I sure what the symantics of MPI require; that is,
   * KDDKDD:  should the user initialization inout (and we just got lucky
   * KDDKDD:  on other platforms)?  
   * KDDKDD:  Anyway, this initialization to zero causes 
   * KDDKDD:  ch_simple/zdrive.inp.rcb-partlocal2 to go infinite due to
   * KDDKDD:  faulty box merge.
   */
  /* ----------------------------------------------- */
  /* We zero out the buffer since some users expect  */
  /* it to be initialized.                           */
  /* ----------------------------------------------- */
  memset(recvbuf,0,size);
#endif

  /* ----------------------------------------------- */
  /* Now, we call the function                       */
  /* ----------------------------------------------- */
#if 0
  /* KDDKDD:  On one processor, MPICH and LAM both do only a copy of 
   * KDDKDD:  sendbuf to recvbuf.  They do not call user-defined functions.
   * KDDKDD:  This choice may not be correct on their part, but siMPI should
   * KDDKDD:  probably do the same (for consistency).
   */
  if ( op>MPI_MAXLOC ) {
    (_MPI_OP_LIST[op-_MPI_OP_OFFSET].function)(sendbuf, recvbuf, &count, &datatype); 
  } else 
#endif
  { 
    _MPI_Default_Op(sendbuf, recvbuf, &count, &datatype);
  }
  return MPI_SUCCESS;
}
/*==========================================================================*/

