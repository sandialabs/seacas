/*******************************************************************************
 * Copyright 1995, Sandia Corporation.  The United States Government retains a *
 * nonexclusive license in this software as prescribed in AL 88-1 and AL 91-7. *
 * Export of this program may require a license from the United States         *
 * Government.                                                                 *
 ******************************************************************************/


#ifdef MPI
#include <mpi.h>
#else
#define MPI_Request int
#endif

#include <stdlib.h>
#include <stdio.h>

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

void get_parallel_info(int *proc, int *nprocs, int *dim)

{
  *proc   = 0;
  *nprocs = 1;
  *dim    = 0;

} /* get_parallel_info */

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

int md_read(char *buf, int bytes, int *source, int *type, int *flag)

{
  return bytes;

} /* md_read */

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

int md_write(char *buf, int bytes, int dest, int type, int *flag)

{
  return 0;

} /* md_write */

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

int md_wrap_iread(void *buf, int bytes, int *source, int *type,
                  MPI_Request *request)

/*******************************************************************************

  Machine dependent wrapped message-reading communication routine for the
  Intel.  This routine is a simple no-op but is used in order to provide
  compatibility with the MPI communication routine order.

  Author:          Scott A. Hutchinson, SNL, 9221
  =======

  Return code:     int
  ============

  Parameter list:
  ===============

  buf:             Beginning address of data to be sent.

  bytes:           Length of message in bytes.

  source:          Source processor number.

  type:            Message type

*******************************************************************************/

{

  return 0;

} /* md_wrap_iread */

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

int md_wrap_write(void *buf, int bytes, int dest, int type, int *flag)

/*******************************************************************************

  Machine dependent wrapped message-sending communication routine for the
  Intel.  This routine is exactly the same as md_write.

  Author:          Scott A. Hutchinson, SNL, 9221
  =======

  Return code:     int
  ============

  Parameter list:
  ===============

  buf:             Beginning address of data to be sent.

  bytes:           Length of message in bytes.

  dest:            Destination processor number.

  type:            Message type

  flag:

*******************************************************************************/

{

  return 0;

} /* md_wrap_write */

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

int md_wrap_wait(void *buf, int bytes, int *source, int *type, int *flag,
                 MPI_Request *request)

/*******************************************************************************

  Machine dependent wrapped message-wait communication routine for the Intel.
  This routine is identical to md_read but is put here in order to be compatible
  with the order required to do MPI communication.

  Author:          Scott A. Hutchinson, SNL, 9221
  =======

  Return code:     int
  ============

  Parameter list:
  ===============

  buf:             Beginning address of data to be sent.

  bytes:           Length of message in bytes.
  dest:            Destination processor number.

  type:            Message type

  flag:

*******************************************************************************/

{

  return bytes;

} /* md_wrap_wait */
