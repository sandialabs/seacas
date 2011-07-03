/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_File_read_all_begin.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ****************** MPI_File_read_all_begin.c  **********************/
/****************************************************************************/
/* Author : Lisa Alano July 22 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_File_read_all_begin(MPI_File fh, void *buf, int count, 
                            MPI_Datatype datatype)
{
  _MPI_COVERAGE();
  return PMPI_File_read_all_begin(fh, buf, count, datatype); 
}

