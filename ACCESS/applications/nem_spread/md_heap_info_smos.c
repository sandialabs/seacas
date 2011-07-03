/*
 * Copyright (C) 2009 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
 * certain rights in this software
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 * 
 *     * Neither the name of Sandia Corporation nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */
/*====================================================================
 * ------------------------
 * | CVS File Information |
 * ------------------------
 *
 * $RCSfile: md_heap_info_smos.c,v $
 *
 * $Author: gdsjaar $
 *
 * $Date: 2009/06/09 13:32:43 $
 *
 * $Revision: 1.1 $
 *
 * $Name:  $
 *====================================================================*/

/*******************************************************************************
 * Copyright 1995, Sandia Corporation.  The United States Government retains a *
 * nonexclusive license in this software as prescribed in AL 88-1 and AL 91-7. *
 * Export of this program may require a license from the United States         *
 * Government.                                                                 *
 ******************************************************************************/


#ifdef DEBUG

#include <stdio.h>
#include "md_heap_info_smos.h"
#include "rf_mp_const.h"

/*****************  Utilities to print heap information  *******************/
/*****************  (useful for debugging memory leaks)  *******************/
/*
 *   Routines in this file:                Called by:
 *      int local_heap_info                   print_heap_info
 *      void print_heap_info                  any routine you want
 */
/***************************************************************************/

/***************************************************************************/

int local_heap_info(int *fragments, int *total_free, int *largest_free,
                    int *total_used)
{

  /*
   * This routine computes heap information under SUNMOS.  It was provided by
   * Rolf Riesen, Sandia National Laboratories.  It may be included as part of
   * PUMA, making this routine unnecessary once we begin running with PUMA.
   * We'll have to check once PUMA is released.
   */

  ALLOC_LINKHDR_TYPE *p;
  extern SHORT_PROCESS_PCB_TYPE *_my_pcb;

  if ((fragments == NULL) || (total_free == NULL)  ||
      (largest_free == NULL) || (total_used == NULL))   {
    return -1;
  }

  p = (ALLOC_LINKHDR_TYPE *)_my_pcb->heap_base;
  *fragments    = 0;
  *total_free   = 0;
  *largest_free = 0;
  *total_used   = 0;

  /* set_lock(&heap_lock); */

  do {
    if (p->free)   {
      if (p->size > *largest_free)   {
        *largest_free= p->size;
      }
      *total_free = p->size + *total_free;
    } else   {
      *total_used = p->size + *total_used;
    }
    (*fragments)++;
  } while (p = p->next);

  /* clr_lock(&heap_lock); */

  *total_free   = *total_free * 8;
  *largest_free = *largest_free * 8;
  *total_used   = *total_used * 8;

  return 0;

}  /* end of local_heap_info() */

/***************************************************************************/

void print_heap_info(
                     int proc_num, /* Processor number for which heap info is
                                      desired.                               */
                     char *s       /* Character string to be printed with the
                                      heap information.                      */
                     )

{

  /*
   *  Prints heap information in SUNMOS, including the number of bytes
   *  used by SALSA, the number of blocks allocated, the size of the
   *  largest contiguous free block (in bytes), the total number of free bytes,
   *  SUNMOS's overhead (in bytes), and the total heap size (in bytes).
   *
   *  This utility is useful for finding memory leaks.  Calls to print_heap_info
   *  can be placed before and after a suspect operation, and the number of
   *  bytes used by SALSA can be compared.  We've also used it to track
   *  memory fragmentation by monitoring the size of the largest contiguous
   *  free block.
   *
   *  Example usage:
   *      print_heap_info(Proc, "BEFORE NONLINEAR SOLVE");
   *  prints the heap information for all processors with a heading that
   *  includes "BEFORE NONLINEAR SOLVE."
   *      print_heap_info(0, "BEFORE NONLINEAR SOLVE");
   *  prints the heap information for only processor 0.
   *
   *  The total heap size tells you how much physical memory has been set
   *  aside for mallocing.  Before a program reaches main(), PUMA allocates
   *  some memory for file descriptors and such.
   *
   *  Every allocated block also uses 32 bytes of header.  This is shown
   *  in overhead.
   */

  extern SHORT_PROCESS_PCB_TYPE *_my_pcb;
  int tfree;                      /*  Total number of free bytes.            */
  int lfree;                      /*  Size of largest contiguous free block. */
  int frags;                      /*  Number of blocks allocated.            */
  int used;                       /*  Number of bytes used by SALSA.         */

  if (Proc == proc_num) {

    local_heap_info(&frags, &tfree, &lfree, &used);

    (void) printf("Heap Information for Processor %4d:  (%s)\n", Proc, s);
    (void) printf("   %9d  Total heap size\n", _my_pcb->heap_length * 8);
    (void) printf("   %9d  Number of blocks allocated\n", frags);
    (void) printf("   %9d  Total number of free bytes\n", tfree);
    (void) printf("   %9d  Largest free contiguous block\n", lfree);
    (void) printf("   %9d  Number of bytes used by SALSA\n", used);
    (void) printf("   %9d  SMOS overhead\n\n",
           (frags + 1) * sizeof(ALLOC_LINKHDR_TYPE));
  }

}

#endif
