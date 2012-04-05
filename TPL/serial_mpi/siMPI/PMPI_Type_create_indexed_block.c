/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: PMPI_Type_create_indexed_block.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/****************************************************************************/
/* FILE  *************  PMPI_Type_create_indexed_block.c  *******************/
/****************************************************************************/
/* Author : Lisa Alano July 24 2002                                         */
/* Copyright (c) 2002 University of California Regents                      */
/****************************************************************************/

#include "mpi.h"

/* STUB */
int PMPI_Type_create_indexed_block( 
        int count, 
        int blocklength, 
        int array_of_displacements[], 
        MPI_Datatype old_type, 
        MPI_Datatype *newtype )
{
  fprintf(stderr,"%s:%d: NOT IMPLEMENTED\n",__FILE__,__LINE__);
  return MPI_Abort((MPI_Comm)NULL, MPI_UNDEFINED); 
}

