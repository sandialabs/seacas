/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: MPI_File_seek.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  ******************    MPI_File_seek.c       ************************/
/****************************************************************************/
/* Author : Lisa Alano July 22 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

int MPI_File_seek(MPI_File fh, MPI_Offset offset, int whence)
{
  _MPI_COVERAGE();
  return PMPI_File_seek (fh, offset, whence); 
}

