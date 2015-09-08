/*******************************************************************************
 * Copyright 1995, Sandia Corporation.  The United States Government retains a *
 * nonexclusive license in this software as prescribed in AL 88-1 and AL 91-7. *
 * Export of this program may require a license from the United States         *
 * Government.                                                                 *
 ******************************************************************************/


typedef struct short_pcb_type{
  unsigned int        iregs[32];
  unsigned int        fregs[32];

  int *               text_base;
  unsigned int        text_length;

  int *               data_base;
  unsigned int        data_length;

  int *               comm_base;
  unsigned int        comm_length;

  int *               heap_base;
  unsigned int        heap_length;

  int *               stack_base1;
  unsigned int        stack_length1;

  int *               stack_base2;
  unsigned int        stack_length2;

  int *               stack_base3;
  unsigned int        stack_length3;

} SHORT_PROCESS_PCB_TYPE;


typedef struct alloc_sizehdr_type {

  /*
   * this should be a multiple of 32 bytes for the Paragon!
   */

  int                         size;
  int                         free;
  struct alloc_sizehdr_type   *prev;
  struct alloc_sizehdr_type   *next;
  int                         filler[4];
} ALLOC_LINKHDR_TYPE;
