/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: _MPI_UTILITY.h,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/***********************************************************************************************/
/* FILE  ***********                     _MPI_UTILITY.c                     ********************/
/***********************************************************************************************/
/* Author : Lisa Alano June 26 2002                                                            */
/* Copyright (c) 2002 University of California Regents                                         */
/***********************************************************************************************/

#ifndef _MPI_UTIL_H
#define _MPI_UTIL_H


void _MPI_checkMemAlloc (void* array, char* message);
void *_MPI_safeMalloc(int size, char* message);
void *_MPI_safeRealloc(void *oldBuffer, int size, char* message);
void _MPI_safeFree(void* buffer,char* message);
int _MPI_checkIntP (int *pt);


#endif
