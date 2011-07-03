/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Wtime.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/**************************************************************************/
/* FILE   **************        PMPI_Wtime.c       ************************/
/**************************************************************************/
/* Author: Patrick Miller March  5 2004                                   */
/* Copyright (C) 2004 University of California Regents                    */
/**************************************************************************/
/* Simple "high resolution timer                                          */
/**************************************************************************/

#include "mpi.h"

#include <sys/time.h>

/* STUB */
double PMPI_Wtime(void)
{
  struct timeval T;

  T.tv_sec = 0;
  T.tv_usec = 0;

  gettimeofday(&T,0);

  return T.tv_sec + 1e-6*T.tv_usec;
}
