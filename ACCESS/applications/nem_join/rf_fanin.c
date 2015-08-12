#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "rf_fem_const.h"
#include "rf_io_const.h"
#include "rf_mp_const.h"
#include "rf_message.h"
#include "rf_allo.h"
#include "rf_util.h"
#include "jo_const.h"

/*********** R O U T I N E S   I N   T H I S   F I L E  ***********************
*
*       NAME                    TYPE                    CALL BY
*----------------------------------------------------------------------
*  fanin_void_slab              int                     many places
*  fanin_int_slab               int                     many places 
*  fanin_int_void_slab          int                     many places 
*  fanin_void_2d_slab           int                     many places 
*  fanin_int_2d_slab            int                     many places 
*  fanin_iblk_slab              int                     many places 
*  fanin_iblk_ptr_slab          int                     many places 
*      fanin_float              void                    fanin_void_slab
*      fanin_double             void                    fanin_void_slab
*      fanin_int                void                    various
*      fanin_str                void                    various
******************************************************************************/

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int fanin_void_slab(int word_size, int gindex[], void *gvec, int gsize,
                    int start_pos, int end_pos, void *gvec_ord)

/*
 *   fanin_void_slab:
 *
 *       This routine gathers a contiguous chunk of an array, that is
 *  scattered across multiple processors, onto Processor 0.
 *
 *  Input:
 *
 *       gindex[]  ==    Index of values of the global vector that the
 *                       current processor owns.
 *
 *       gvec[]    ==    Floating point values for the global vector that
 *                       the current processor owns
 *
 *       gsize     ==    Size of the gindex and gvec vectors
 *
 *       start_pos ==    Starting global position for that part of the
 *                       global vector to be collected
 *
 *       end_pos   ==    Ending global position for that part of the
 *                       global vector to be collected
 *
 *  Output:
 *
 *       (return)  ==    0 - normal return status
 *                       -1 - error status
 *
 *       gvec_ord  ==    On Proc = 0, this is the address of the position
 *                       in memory where the global vector is storred
 *                       contiguously.  On other processors, this address just
 *                       points to work space that will be used in this
 *                       program.
 *                       NOTE: gvec_ord is assumed to have been already
 *                       malloced, with enough space to store (end_pos -
 *                       start_pos + 1) floating point variables.
 */

{
  int       i, start_g, end_g, len_range;
  int       len_vec = end_pos - start_pos + 1;

  float    *fptr1, *fptr2;
  double   *dptr1, *dptr2;

  /* Initialize the ordered vector to zero on all processors */

  /* determine the precision */
  if (word_size == sizeof(float)) {
    fptr1 = (float *) gvec;
    fptr2 = (float *) gvec_ord;
    for (i = 0; i < len_vec; i++) fptr2[i] = 0.0F;
  }
  else {
    dptr1 = (double *) gvec;
    dptr2 = (double *) gvec_ord;
    for (i = 0; i < len_vec; i++) dptr2[i] = 0.0;
  }

  /*
   * Find the range of the gindex values which overlap the desired global range
   */

  len_range = find_range(start_pos, end_pos, gindex, gsize, &start_g, &end_g);

  /* Fill the local ordered vector with local values */

  if (len_range > 0) {
    if (word_size == sizeof(float))
      for (i = start_g; i <= end_g; i++)
        fptr2[gindex[i] - start_pos] = fptr1[i];
    else
      for (i = start_g; i <= end_g; i++)
        dptr2[gindex[i] - start_pos] = dptr1[i];
  }

#ifdef DEBUG_FANNIN
  if (Proc == 0) {
    (void) printf("\n\nFanin_void_slab: Fanin of a chunk of a distributed "
                  "vector:\n");
    (void) printf("\tStarting position = %d\n", start_pos);
    (void) printf("\tEnding   position = %d\n", end_pos);
    (void) printf("\tSize of global vector on proc 0 = %d\n", len_vec);
  }
#endif

  /*
   * Do a fanin of a constant sized vector, using a vector sum to combine
   * solutions at each lowering of the dimension of the cube
   */

  if (word_size == sizeof(float))
    fanin_float(fptr2, len_vec, 0, Proc, Num_Proc);
  else
    fanin_double(dptr2, len_vec, 0, Proc, Num_Proc);

  /* Return a no error condition */

  return 0;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int fanin_int_slab(int gindex[], int *gvec, int gsize,
                   int start_pos, int end_pos, int *gvec_ord)

/*
 *   fanin_int_slab:
 *
 *       This routine gathers a contiguous chunk of an integer vector, that is
 *  scattered across multiple processors, onto Processor 0.
 *
 *  Input:
 *
 *       gindex[]  ==    Index of values of the global vector that the
 *                       current processor owns.
 *
 *       gvec[]    ==    Integer values for the global vector that
 *                       the current processor owns
 *
 *       gsize     ==    Size of the gindex and gvec vectors
 *
 *       start_pos ==    Starting global position for that part of the
 *                       global vector to be collected
 *
 *       end_pos   ==    Ending global position for that part of the
 *                       global vector to be collected
 *
 *  Output:
 *
 *       (return)  ==    0 - normal return status
 *                       -1 - error status
 *
 *       gvec_ord  ==    On Proc = 0, this is the address of the position
 *                       in memory where the global vector is storred
 *                       contiguously.  On other processors, this address just
 *                       points to work space that will be used in this
 *                       program.
 *                       NOTE: gvec_ord is assumed to have been already
 *                       malloced, with enough space to store (end_pos -
 *                       start_pos + 1) integer values.
 */

{
  int       i, start_g, end_g, len_range;
  int       len_vec = end_pos - start_pos + 1;

  /* Initialize the ordered vector to zero on all processors */
  for (i = 0; i < len_vec; i++) gvec_ord[i] = 0;

  /*
   * Find the range of the gindex values which overlap the desired global range
   */

  len_range = find_range(start_pos, end_pos, gindex, gsize, &start_g, &end_g);

  /* Fill the local ordered vector with local values */

  if (len_range > 0) {
    for (i = start_g; i <= end_g; i++)
      gvec_ord[gindex[i] - start_pos] = gvec[i];
  }

#ifdef DEBUG_FANNIN
  if (Proc == 0) {
    (void) printf("\n\nFanin_int_slab: Fanin of a chunk of a distributed "
                  "vector:\n");
    (void) printf("\tStarting position = %d\n", start_pos);
    (void) printf("\tEnding   position = %d\n", end_pos);
    (void) printf("\tSize of global vector on proc 0 = %d\n", len_vec);
  }
#endif

  /*
   * Do a fanin of a constant sized vector, using a vector sum to combine
   * solutions at each lowering of the dimension of the cube
   */

  fanin_int(gvec_ord, len_vec, 0, Proc, Num_Proc);

  /* Return a no error condition */
  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int fanin_int_void_slab(int word_size, int gindex[], int *gvec, void *fvec,
                        int gsize, int start_pos, int end_pos, int *gvec_ord,
                        void *fvec_ord)

/*
 *   fanin_int_void_slab:
 *
 *       This routine gathers a contiguous chunk of an integer vector, that is
 *  scattered across multiple processors, onto Processor 0. It also fan's in
 *  the coresponding values of a floating point vector.
 *
 *  Input:
 *
 *       gindex[]  ==    Index of values of the global vector that the
 *                       current processor owns.
 *
 *       gvec[]    ==    Integer values for the global vector that
 *                       the current processor owns
 *
 *       fvec[]    ==    Floating point values for the global vector that
 *                       the current processor owns
 *
 *       gsize     ==    Size of the gindex, gvec and fvec vectors
 *
 *       start_pos ==    Starting global position for that part of the
 *                       global vector to be collected
 *
 *       end_pos   ==    Ending global position for that part of the
 *                       global vector to be collected
 *
 *  Output:
 *
 *       (return)  ==    0 - normal return status
 *                       -1 - error status
 *
 *       gvec_ord  ==    On Proc = 0, this is the address of the position
 *                       in memory where the global vector is storred
 *                       contiguously.  On other processors, this address just
 *                       points to work space that will be used in this
 *                       program.
 *                       NOTE: gvec_ord is assumed to have been already
 *                       malloced, with enough space to store (end_pos -
 *                       start_pos + 1) integer values.
 *
 *       fvec_ord  ==    On Proc = 0, this is the address of the position
 *                       in memory where the global vector is storred
 *                       contiguously.  On other processors, this address just
 *                       points to work space that will be used in this
 *                       program.
 *                       NOTE: fvec_ord is assumed to have been already
 *                       malloced, with enough space to store (end_pos -
 *                       start_pos + 1) floating point values.
 */

{
  int       i, start_g, end_g, len_range;
  int       len_vec = end_pos - start_pos + 1;

  float    *fptr1, *fptr2;
  double   *dptr1, *dptr2;

  /* determine the precision */
  if (word_size == sizeof(float)) {
    fptr1 = (float *) fvec;
    fptr2 = (float *) fvec_ord;
  }
  else {
    dptr1 = (double *) fvec;
    dptr2 = (double *) fvec_ord;
  }

  /* Initialize the ordered vector to zero on all processors */
  for (i = 0; i < len_vec; i++) {
    gvec_ord[i] = 0;
    if (word_size == sizeof(float)) fptr2[i] = 0.0F;
    else                            dptr2[i] = 0.0;
  }

  /*
   * Find the range of the gindex values which overlap the desired global range
   */

  len_range = find_range(start_pos, end_pos, gindex, gsize, &start_g, &end_g);

  /* Fill the local ordered vector with local values */

  if (len_range > 0) {
    for (i = start_g; i <= end_g; i++) {
      gvec_ord[gindex[i] - start_pos] = gvec[i];
      if (word_size == sizeof(float))
        fptr2[gindex[i] - start_pos] = fptr1[i];
      else
        dptr2[gindex[i] - start_pos] = dptr1[i];
    }
  }

#ifdef DEBUG_FANNIN
  if (Proc == 0) {
    (void) printf("\n\nFanin_int_void_slab: Fanin of a chunk of a distributed "
                  "vector:\n");
    (void) printf("\tStarting position = %d\n", start_pos);
    (void) printf("\tEnding   position = %d\n", end_pos);
    (void) printf("\tSize of global vector on proc 0 = %d\n", len_vec);
  }
#endif

  /*
   * Do a fanin of a constant sized vector, using a vector sum to combine
   * solutions at each lowering of the dimension of the cube
   */

  fanin_int(gvec_ord, len_vec, 0, Proc, Num_Proc);

  /* now fanin the corresponding floating point values */
  if (word_size == sizeof(float))
    fanin_float(fptr2, len_vec, 0, Proc, Num_Proc);
  else
    fanin_double(dptr2, len_vec, 0, Proc, Num_Proc);

  /* Return a no error condition */
  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int fanin_void_2d_slab(int word_size, int gindex[], void *gvec, int gsize,
                       int block_size, int start_pos, int end_pos,
                       void *gvec_ord)

/*
 *   fanin_void_slab:
 *
 *       This routine gathers a contiguous chunk of a 2d floating point
 *  array, that is scattered across multiple processors, onto Processor 0.
 *
 *  Input:
 *
 *       gindex[]   ==    Index of values of the global vector that the
 *                        current processor owns.
 *
 *       gvec[]     ==    Floating point values for the global vector that
 *                        the current processor owns
 *
 *       gsize      ==    Size of the gindex and gvec vectors
 *
 *       block_size ==    Size of each block in gvec
 *
 *       start_pos  ==    Starting global position for that part of the
 *                        global vector to be collected
 *
 *       end_pos    ==    Ending global position for that part of the
 *                        global vector to be collected
 *
 *  Output:
 *
 *       (return)   ==    0 - normal return status
 *                        -1 - error status
 *
 *       gvec_ord   ==    On Proc = 0, this is the address of the position
 *                        in memory where the global vector is storred
 *                        contiguously.  On other processors, this address just
 *                        points to work space that will be used in this
 *                        program.
 *                        NOTE: gvec_ord is assumed to have been already
 *                        malloced, with enough space to store (end_pos -
 *                        start_pos + 1) floating point variables.
 */

{
  int       i, j, start_g, end_g, len_range;
  int       len_vec = (end_pos - start_pos + 1) * block_size;

  float   **fptr1, *fptr2;
  double  **dptr1, *dptr2;

  /* Initialize the ordered vector to zero on all processors */

  /* determine the precision */
  if (word_size == sizeof(float)) {
    fptr1 = (float **) gvec;
    fptr2 = (float *) gvec_ord;
    for (i = 0; i < len_vec; i++) fptr2[i] = 0.0F;
  }
  else {
    dptr1 = (double **) gvec;
    dptr2 = (double *) gvec_ord;
    for (i = 0; i < len_vec; i++) dptr2[i] = 0.0;
  }

  /*
   * Find the range of the gindex values which overlap the desired global range
   */

  len_range = find_range(start_pos, end_pos, gindex, gsize, &start_g, &end_g);

  /* Fill the local ordered vector with local values */

  if (len_range > 0) {
    if (word_size == sizeof(float))
      for (i = start_g; i <= end_g; i++)
        for (j = 0; j < block_size; j++)
          fptr2[(gindex[i] - start_pos) * block_size + j] = fptr1[i][j];
    else
      for (i = start_g; i <= end_g; i++)
        for (j = 0; j < block_size; j++)
          dptr2[(gindex[i] - start_pos) * block_size + j] = dptr1[i][j];
  }

#ifdef DEBUG_FANNIN
  if (Proc == 0) {
    (void) printf("\n\nFanin_void_2d_slab: Fanin of a chunk of a distributed "
                  "vector:\n");
    (void) printf("\tStarting position = %d\n", start_pos);
    (void) printf("\tEnding   position = %d\n", end_pos);
    (void) printf("\tSize of global vector on proc 0 = %d\n", len_vec);
  }
#endif

  /*
   * Do a fanin of a constant sized vector, using a vector sum to combine
   * solutions at each lowering of the dimension of the cube
   */

  if (word_size == sizeof(float))
    fanin_float(fptr2, len_vec, 0, Proc, Num_Proc);
  else
    fanin_double(dptr2, len_vec, 0, Proc, Num_Proc);

  /* Return a no error condition */

  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int fanin_int_2d_slab(int gindex[], int **gvec, int gsize, int block_size,
                      int start_pos, int end_pos, int *gvec_ord)

/*
 *   fanin_int_2d_slab:
 *
 *       This routine gathers a contiguous chunk of a 2d integer vector, that
 *  is scattered across multiple processors, onto Processor 0.
 *
 *  Input:
 *
 *       gindex[]   ==    Index of values of the global vector that the
 *                        current processor owns.
 *
 *       gvec       ==    Integer values for the global 2d vector that
 *                        the current processor owns
 *
 *       gsize      ==    Size of the gindex and gvec vectors
 *
 *       block_size ==    Size of each block in gvec
 *
 *       start_pos  ==    Starting global position for that part of the
 *                        global vector to be collected
 *
 *       end_pos    ==    Ending global position for that part of the
 *                        global vector to be collected
 *
 *  Output:
 *
 *       (return)   ==    0 - normal return status
 *                        -1 - error status
 *
 *       gvec_ord   ==    On Proc = 0, this is the address of the position
 *                        in memory where the global vector is storred
 *                        contiguously.  On other processors, this address just
 *                        points to work space that will be used in this
 *                        program.
 *                        NOTE: gvec_ord is assumed to have been already
 *                        malloced, with enough space to store ((end_pos -
 *                        start_pos + 1) * block_size) integer values.
 */

{
  int       i, j, start_g, end_g, len_range;
  int       len_vec = (end_pos - start_pos + 1) * block_size;

  /* Initialize the ordered vector to zero on all processors */
  for (i = 0; i < len_vec; i++) gvec_ord[i] = 0;

  /*
   * Find the range of the gindex values which overlap the desired global range
   */

  len_range = find_range(start_pos, end_pos, gindex, gsize, &start_g, &end_g);

  /* Fill the local ordered vector with local values */

  if (len_range > 0) {
    for (i = start_g; i <= end_g; i++)
      for (j = 0; j < block_size; j++)
        gvec_ord[(gindex[i] - start_pos) * block_size + j] = gvec[i][j];
  }

#ifdef DEBUG_FANNIN
  if (Proc == 0) {
    (void) printf("\n\nFanin_int_2d_slab: Fanin of a chunk of a distributed 2d"
                  " vector:\n");
    (void) printf("\tStarting position = %d\n", start_pos);
    (void) printf("\tEnding   position = %d\n", end_pos);
    (void) printf("\tSize of global vector on proc 0 = %d\n", len_vec);
  }
#endif

  /*
   * Do a fanin of a constant sized vector, using a vector sum to combine
   * solutions at each lowering of the dimension of the cube
   */

  fanin_int(gvec_ord, len_vec, 0, Proc, Num_Proc);

  /* Return a no error condition */
  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int fanin_iblk_slab(int gindex[], int *gvec, int gsize, int gblk_size,
                    int start_pos, int end_pos, int *gvec_ord)

/*
 *   fanin_iblk_slab:
 *
 *       This routine gathers a contiguous chunk of an integer vector, that is
 *  scattered across multiple processors, onto Processor 0. It also fan's in
 *  the coresponding values of a floating point vector.
 *
 *  Input:
 *
 *       gindex[]   ==    Index of values of the global vector that the
 *                        current processor owns.
 *
 *       gvec[]     ==    Integer values for the global vector that
 *                        the current processor owns
 *
 *       gsize      ==    Base size of the gindex, gvec and fvec vectors
 *
 *       gblk_size  ==    Size of the blocks in the gvec vector, the
 *                        vector length is (gsize * gblk_size)
 *
 *       start_pos  ==    Starting global position for that part of the
 *                        global vector to be collected
 *
 *       end_pos    ==    Ending global position for that part of the
 *                        global vector to be collected
 *
 *  Output:
 *
 *       (return)   ==    0 - normal return status
 *                        -1 - error status
 *
 *       gvec_ord   ==    On Proc = 0, this is the address of the position
 *                        in memory where the global vector is storred
 *                        contiguously.  On other processors, this address just
 *                        points to work space that will be used in this
 *                        program.
 *                        NOTE: gvec_ord is assumed to have been already
 *                        malloced, with enough space to store (end_pos -
 *                        start_pos + 1) * gblk_size integer values.
 *
 */

{
  int       i, j, start_g, end_g, len_range;
  int       len_vec1 = (end_pos - start_pos + 1) * gblk_size;
  int       pos;

  /* Initialize the ordered vector to zero on all processors */
  for (i = 0; i < len_vec1; i++)
    gvec_ord[i] = 0;

  /*
   * Find the range of the gindex values which overlap the desired global range
   */

  len_range = find_range(start_pos, end_pos, gindex, gsize, &start_g, &end_g);

  /* Fill the local ordered vector with local values */

  if (len_range > 0) {
    for (i = start_g; i <= end_g; i++) {
      pos = (gindex[i] - start_pos) * gblk_size;
      for (j = 0; j < gblk_size; j++)
        gvec_ord[pos + j] = gvec[(i * gblk_size) + j];
    }
  }

#ifdef DEBUG_FANNIN
  if (Proc == 0) {
    (void) printf("\n\nFanin_iblk_slab: Fanin of a chunk of a distributed "
                  "vector:\n");
    (void) printf("\tStarting position = %d\n", start_pos);
    (void) printf("\tEnding   position = %d\n", end_pos);
    (void) printf("\tSize of global vector on proc 0 = %d\n", len_vec1);
  }
#endif

  /*
   * Do a fanin of a constant sized vector, using a vector sum to combine
   * solutions at each lowering of the dimension of the cube
   */

  fanin_int(gvec_ord, len_vec1, 0, Proc, Num_Proc);

  /* Return a no error condition */
  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int fanin_iblk_ptr_slab(int word_size, int gindex[], int *gvec, void *fvec,
                        int gsize, int gblk_size, int fblk_size, int start_pos,
                        int end_pos, int *gvec_ord, void *fvec_ord)

/*
 *   fanin_iblk_ptr_slab:
 *
 *       This routine gathers a contiguous chunk of an integer vector, that is
 *  scattered across multiple processors, onto Processor 0. It also fan's in
 *  the coresponding values of a floating point vector.
 *
 *  Input:
 *
 *       gindex[]   ==    Index of values of the global vector that the
 *                        current processor owns.
 *
 *       gvec[]     ==    Integer values for the global vector that
 *                        the current processor owns
 *
 *       *fvec[]    ==    An array of pointers to blocks of floating
 *                        point values
 *
 *       gsize      ==    Size of the gindex vector
 *
 *       gblk_size  ==    Size of the blocks in the gvec vector, the
 *                        vector length is (gsize * gblk_size)
 *
 *       fblk_size  ==    Size of the blocks in the fvec vector, the
 *                        vector is 2d of length
 *                        (gsize * gblk_size) x fblk_size
 *
 *       start_pos  ==    Starting global position for that part of the
 *                        global vector to be collected
 *
 *       end_pos    ==    Ending global position for that part of the
 *                        global vector to be collected
 *
 *  Output:
 *
 *       (return)   ==    0 - normal return status
 *                        -1 - error status
 *
 *       gvec_ord   ==    On Proc = 0, this is the address of the position
 *                        in memory where the global vector is storred
 *                        contiguously.  On other processors, this address just
 *                        points to work space that will be used in this
 *                        program.
 *                        NOTE: gvec_ord is assumed to have been already
 *                        malloced, with enough space to store (end_pos -
 *                        start_pos + 1) * gblk_size integer values.
 *
 *       fvec_ord   ==    On Proc = 0, this is the address of the position
 *                        in memory where the global vector is storred
 *                        contiguously.  On other processors, this address just
 *                        points to work space that will be used in this
 *                        program.
 *                        NOTE: fvec_ord is assumed to have been already
 *                        malloced, with enough space to store (end_pos -
 *                        start_pos + 1) * gblk_size * fblk_size floating
 *                        point values.
 */

{
  int       i, j, k, start_g, end_g, len_range;
  int       len_vec1 = (end_pos - start_pos + 1) * gblk_size;
  int       len_vec2 = (end_pos - start_pos + 1) * gblk_size * fblk_size;
  int       pos;
  int off;

  float    **fptr1, *fptr2;
  double   **dptr1, *dptr2;

  /* determine the precision */
  if (word_size == sizeof(float)) {
    fptr1 = (float **) fvec;
    fptr2 = (float *) fvec_ord;
  }
  else {
    dptr1 = (double **) fvec;
    dptr2 = (double *) fvec_ord;
  }

  /* Initialize the ordered vector to zero on all processors */
  if (word_size == sizeof(float)) {
    for (i = 0; i < len_vec1; i++) {
      off = i * fblk_size;
      gvec_ord[i] = 0;
      for (j = 0; j < fblk_size; j++) {
	fptr2[off + j] = 0.0F;
      }
    }
  } else {
    for (i = 0; i < len_vec1; i++) {
      off = i * fblk_size;
      gvec_ord[i] = 0;
      for (j = 0; j < fblk_size; j++) {
	dptr2[off + j] = 0.0;
      }
    }
  }

  /*
   * Find the range of the gindex values which overlap the desired global range
   */

  len_range = find_range(start_pos, end_pos, gindex, gsize, &start_g, &end_g);

  /* Fill the local ordered vector with local values */

  if (len_range > 0) {
    if (word_size == sizeof(float)) {
      for (i = start_g; i <= end_g; i++) {
	pos = (gindex[i] - start_pos) * gblk_size;
	for (j = 0; j < gblk_size; j++) {
	  gvec_ord[pos + j] = gvec[(i * gblk_size) + j];
	  for (k = 0; k < fblk_size; k++)
            fptr2[(pos + j) * fblk_size + k] = fptr1[(i * gblk_size) + j][k];
	}
      }
    } else {
      for (i = start_g; i <= end_g; i++) {
	pos = (gindex[i] - start_pos) * gblk_size;
	for (j = 0; j < gblk_size; j++) {
	  gvec_ord[pos + j] = gvec[(i * gblk_size) + j];
	  for (k = 0; k < fblk_size; k++)
            dptr2[(pos + j) * fblk_size + k] = dptr1[(i * gblk_size) + j][k];
	}
      }
    }
  }

#ifdef DEBUG_FANNIN
  if (Proc == 0) {
    (void) printf("\n\nFanin_iblk_ptr_slab: Fanin of a chunk of a distributed "
                  "vector:\n");
    (void) printf("\tStarting position = %d\n", start_pos);
    (void) printf("\tEnding   position = %d\n", end_pos);
    (void) printf("\tSize of global vector on proc 0 = %d\n", len_vec1);
  }
#endif

  /*
   * Do a fanin of a constant sized vector, using a vector sum to combine
   * solutions at each lowering of the dimension of the cube
   */

  fanin_int(gvec_ord, len_vec1, 0, Proc, Num_Proc);

  /* now fanin the corresponding floating point values */
  if (word_size == sizeof(float))
    fanin_float(fptr2, len_vec2, 0, Proc, Num_Proc);
  else
    fanin_double(dptr2, len_vec2, 0, Proc, Num_Proc);

  /* Return a no error condition */
  return 0;

}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

void fanin_float(
                 float *vector, /* vector of values to sum */
                 int len,       /* length of vector */
                 int sum,       /* which type of fanin to do
                                    0 - replace (only if non-initialized value)
                                    1 - sum */
                 int node,      /* my processor id */
                 int nprocs     /* number of processors in machine, initially,
                                   and thereafter, the number of processors
                                   still participating in the fanin */
)

/*
 *  fanin_float:
 *      Routine to do a fanin to node 0, of a fixed vector length.
 * A vector sum is used to combine data at each dimension of the collection
 * algorithm.  The messages are broken into pieces to avoid overflowing the
 * message buffers.
 *     The algorithm users recursive calling of the fanin routine.
 * There are round_up [ log_2(nproc) ] levels or recursive calls in this
 * algorithm.  Therefore, it attempts to be speedy by reducing the
 * total amount of message traffic.
 *
 */

{
  char     *yo = "fanin_float";

  float    *fbuf;                      /* buffer for receiving messages */
  int       left_nprocs;               /* # processors in left half of
                                          machine                       */
  int       num_units_per_message;     /* default # values transmitted in
                                          a single message */
  int       unit_len;                  /* actual # values transmitted in
                                          the current message */
  int       byte_len;                  /* length of message in bytes */
  int       partner;                   /* processor I communicate with */
  int       start_pos;                 /* index into vector */
  int       cflag;                     /* dummy nread/nwrite parameter */
  int       max_in_cbuf;               /* max # messages allowed in buffer */
  int       type;                      /* type of data message */
  int       type_ack;                  /* type for handshaking messages */
  int       base_type;                 /* starting type for data messages */
  int       i, j;                      /* loop counters */

  /*
   *  Normal exit location for the recursive calls
   */

  if (nprocs <= 1) return;

  type_ack  = MT_FANNIN;
  base_type = MT_FANNIN + 1;

  max_in_cbuf = BRCST_COMM_BUFF_SIZE / MSIZE_FANNIN;

  num_units_per_message = MSIZE_FANNIN / sizeof(float);
  unit_len = num_units_per_message;

  left_nprocs = (nprocs + 1) / 2;

  /* Receive message */

  if (node < left_nprocs) {
    partner = node + left_nprocs;
    if (partner < nprocs) {

          /* NOTE - later optimizations could reduce the number of times this
             malloc is done during the recursive calls */

          fbuf = (float *) array_alloc(__FILE__, __LINE__, 1, unit_len,
                                       sizeof(float));
          start_pos = 0;
          type = base_type;
          for (j = 0; j*num_units_per_message < len; j++) {

            /* Handshake periodically to ensure message buffer not overflown. */

            if ((j % max_in_cbuf) == 0) {
              if (md_write((char *) NULL, 0, partner, type_ack, &cflag) != 0) {
                (void)fprintf(stderr, "%s: ERROR on node %d\n",
                              yo, node);
                (void)fprintf(stderr,
                              "\tmd_write ack failed, message type %d\n",
                              type_ack);
                exit(-1);
              }
            }
            if (start_pos + num_units_per_message > len)
              unit_len = len - start_pos;

            byte_len = sizeof(float) * unit_len;
            if (md_read((char *) fbuf, byte_len, &partner, &type,
                        &cflag) != byte_len) {
              (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
              (void) fprintf(stderr, "\tmd_read failed, message type %d\n",
                             type);
              exit(-1);
            }

            /*
             * Do the actual fsum operation incrementally so that it is
             * partially masked by the communication
             *
             * During the replacement, need to check to make sure that
             * an existing value is not being overwritten.
             */

            for (i = 0; i < unit_len; i++)
              if (sum)
                vector[start_pos+i] += fbuf[i];
              else
                if (!(vector[start_pos+i] > 0.0f
                      || vector[start_pos+i] < 0.0f))
                  vector[start_pos+i] = fbuf[i];
            start_pos += unit_len;
            type++;
          }
          safe_free((void **) &fbuf);
    }

      /*
       * Recursively call the subroutine again for the lower 1/2 rounded up
       * number of processors.  Don't need to call the routine again if
       * left_nprocs = 1 already
       */

    if (left_nprocs > 1)
      fanin_float(vector, len, sum, node, left_nprocs);
  }
  else {

    /*
     * Current processor is in the top half (rounded down) number of processors
     *  - Send the message, then exit at the bottom or routine
     */

    partner = node - left_nprocs;
    start_pos = 0;
    type = base_type;
    for (j = 0; j*num_units_per_message < len; j++) {
      if (start_pos + num_units_per_message > len)
        unit_len = len - start_pos;
      byte_len = sizeof(float) * unit_len;
      type = base_type + j;

      /*
       * Must read an acknowledgement periodically to ensure message buffer is
       * not overflown.  This is true especially for the first message, because
       * we don't know that the receiving processor has caught up to this one,
       * yet.
       */

      if ((j % max_in_cbuf) == 0) {
        if (md_read((char *) NULL, 0, &partner, &type_ack, &cflag) != 0) {
          (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
          (void) fprintf(stderr, "\tmd_read ack failed, message type %d\n",
                         type_ack);
          exit(-1);
        }
      }

      /* Start the transfer of data */

      if (md_write((char *) &vector[start_pos], byte_len, partner, type,
                   &cflag) != 0) {
        (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
        (void) fprintf(stderr, "\tmd_write failed, message type %d\n", type);
        exit(-1);
      }
      start_pos += unit_len;
      type++;
    }
  }

}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

void fanin_double(
                  double *vector, /* vector of values to sum */
                  int len,       /* length of vector */
                  int sum,      /* which type of fanin to do
                                    0 - replace (only if non-initialized value)
                                    1 - sum */
                  int node,      /* my processor id */
                  int nprocs     /* number of processors in machine, initially,
                                   and thereafter, the number of processors
                                   still participating in the fanin */
)

/*
 *  fanin_double:
 *      Routine to do a fanin to node 0, of a fixed vector length.
 * A vector sum is used to combine data at each dimension of the collection
 * algorithm.  The messages are broken into pieces to avoid overflowing the
 * message buffers.
 *     The algorithm users recursive calling of the fanin routine.
 * There are round_up [ log_2(nproc) ] levels or recursive calls in this
 * algorithm.  Therefore, it attempts to be speedy by reducing the
 * total amount of message traffic.
 *
 */

{
  char     *yo = "fanin_double";

  double   *fbuf;                      /* buffer for receiving messages */
  int       left_nprocs;               /* # processors in left half of
                                          machine                       */
  int       num_units_per_message;     /* default # values transmitted in
                                          a single message */
  int       unit_len;                  /* actual # values transmitted in
                                          the current message */
  int       byte_len;                  /* length of message in bytes */
  int       partner;                   /* processor I communicate with */
  int       start_pos;                 /* index into vector */
  int       cflag;                     /* dummy nread/nwrite parameter */
  int       max_in_cbuf;               /* max # messages allowed in buffer */
  int       type;                      /* type of data message */
  int       type_ack;                  /* type for handshaking messages */
  int       base_type;                 /* starting type for data messages */
  int       i, j;                      /* loop counters */

  /*
   *  Normal exit location for the recursive calls
   */

  if (nprocs <= 1) return;

  type_ack  = MT_FANNIN;
  base_type = MT_FANNIN + 1;

  max_in_cbuf = BRCST_COMM_BUFF_SIZE / MSIZE_FANNIN;

  num_units_per_message = MSIZE_FANNIN / sizeof(double);
  unit_len = num_units_per_message;

  left_nprocs = (nprocs + 1) / 2;

  /* Receive message */

  if (node < left_nprocs) {
    partner = node + left_nprocs;
    if (partner < nprocs) {

          /* NOTE - later optimizations could reduce the number of times this
             malloc is done during the recursive calls */

          fbuf = (double *) array_alloc(__FILE__, __LINE__, 1, unit_len,
                                        sizeof(double));
          start_pos = 0;
          type = base_type;
          for (j = 0; j*num_units_per_message < len; j++) {

            /* Handshake periodically to ensure message buffer not overflown. */

            if ((j % max_in_cbuf) == 0) {
              if (md_write((char *) NULL, 0, partner, type_ack, &cflag) != 0) {
                (void)fprintf(stderr, "%s: ERROR on node %d\n",
                              yo, node);
                (void)fprintf(stderr,
                              "\tmd_write ack failed, message type %d\n",
                              type_ack);
                exit(-1);
              }
            }
            if (start_pos + num_units_per_message > len)
              unit_len = len - start_pos;

            byte_len = sizeof(double) * unit_len;
            if (md_read((char *) fbuf, byte_len, &partner, &type,
                        &cflag) != byte_len) {
              (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
              (void) fprintf(stderr, "\tmd_read failed, message type %d\n",
                             type);
              exit(-1);
            }

            /*
             * Do the actual fsum operation incrementally so that it is
             * partially masked by the communication
             *
             * During the replacement, need to check to make sure that
             * an existing value is not being overwritten.
             */

            for (i = 0; i < unit_len; i++) {
              if (sum)
                vector[start_pos+i] += fbuf[i];
              else {
                if (!(vector[start_pos+i] > 0.0 || vector[start_pos+i] < 0.0))
                  vector[start_pos+i] = fbuf[i];
              }
            }
            start_pos += unit_len;
            type++;
          }
          safe_free((void **) &fbuf);
    }

      /*
       * Recursively call the subroutine again for the lower 1/2 rounded up
       * number of processors.  Don't need to call the routine again if
       * left_nprocs = 1 already
       */

    if (left_nprocs > 1)
      fanin_double(vector, len, sum, node, left_nprocs);
  }
  else {

    /*
     * Current processor is in the top half (rounded down) number of processors
     *  - Send the message, then exit at the bottom or routine
     */

    partner = node - left_nprocs;
    start_pos = 0;
    type = base_type;
    for (j = 0; j*num_units_per_message < len; j++) {
      if (start_pos + num_units_per_message > len)
        unit_len = len - start_pos;
      byte_len = sizeof(double) * unit_len;
      type = base_type + j;

      /*
       * Must read an acknowledgement periodically to ensure message buffer is
       * not overflown.  This is true especially for the first message, because
       * we don't know that the receiving processor has caught up to this one,
       * yet.
       */

      if ((j % max_in_cbuf) == 0) {
        if (md_read((char *) NULL, 0, &partner, &type_ack, &cflag) != 0) {
          (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
          (void) fprintf(stderr, "\tmd_read ack failed, message type %d\n",
                         type_ack);
          exit(-1);
        }
      }

      /* Start the transfer of data */

      if (md_write((char *) &vector[start_pos], byte_len, partner, type,
                   &cflag) != 0) {
        (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
        (void) fprintf(stderr, "\tmd_write failed, message type %d\n", type);
        exit(-1);
      }
      start_pos += unit_len;
      type++;
    }
  }
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

void fanin_int(
               int *vector,   /* vector of values to sum */
               int len,       /* length of vector */
               int sum,       /* which type of fanin to do
                                   0 - replace (only if non-initialized value)
                                   1 - sum */
               int node,      /* my processor id */
               int nprocs     /* number of processors in machine, initially,
                                 and thereafter, the number of processors
                                 still participating in the fanin */
)

/*
 *  fanin_int:
 *      Routine to do a fanin to node 0, of a fixed vector length.
 * A vector sum is used to combine data at each dimension of the collection
 * algorithm.  The messages are broken into pieces to avoid overflowing the
 * message buffers.
 *     The algorithm users recursive calling of the fanin routine.
 * There are round_up [ log_2(nproc) ] levels or recursive calls in this
 * algorithm.  Therefore, it attempts to be speedy by reducing the
 * total amount of message traffic.
 *
 */

{
  char     *yo = "fanin_int";

  int      *ibuf;                      /* buffer for receiving messages */
  int       left_nprocs;               /* # processors in left half of
                                          machine                       */
  int       num_units_per_message;     /* default # values transmitted in
                                          a single message */
  int       unit_len;                  /* actual # values transmitted in
                                          the current message */
  int       byte_len;                  /* length of message in bytes */
  int       partner;                   /* processor I communicate with */
  int       start_pos;                 /* index into vector */
  int       cflag;                     /* dummy nread/nwrite parameter */
  int       max_in_cbuf;               /* max # messages allowed in buffer */
  int       type;                      /* type of data message */
  int       type_ack;                  /* type for handshaking messages */
  int       base_type;                 /* starting type for data messages */
  int       i, j;                      /* loop counters */

  /*
   *  Normal exit location for the recursive calls
   */

  if (nprocs <= 1) return;

  type_ack  = MT_FANNIN;
  base_type = MT_FANNIN + 1;

  max_in_cbuf = BRCST_COMM_BUFF_SIZE / MSIZE_FANNIN;

  num_units_per_message = MSIZE_FANNIN / sizeof(int);
  unit_len = num_units_per_message;

  left_nprocs = (nprocs + 1) / 2;

  /* Receive message */

  if (node < left_nprocs) {
    partner = node + left_nprocs;
    if (partner < nprocs) {

          /* NOTE - later optimizations could reduce the number of times this
             malloc is done during the recursive calls */

          ibuf = (int *) array_alloc(__FILE__, __LINE__, 1, unit_len,
                                     sizeof(int));
          start_pos = 0;
          type = base_type;
          for (j = 0; j*num_units_per_message < len; j++) {

            /* Handshake periodically to ensure message buffer not overflown. */

            if ((j % max_in_cbuf) == 0) {
              if (md_write((char *) NULL, 0, partner, type_ack, &cflag) != 0) {
                (void)fprintf(stderr, "%s: ERROR on node %d\n",
                              yo, node);
                (void)fprintf(stderr,
                              "\tmd_write ack failed, message type %d\n",
                              type_ack);
                exit(-1);
              }
            }
            if (start_pos + num_units_per_message > len)
              unit_len = len - start_pos;

            byte_len = sizeof(int) * unit_len;
            if (md_read((char *) ibuf, byte_len, &partner, &type,
                        &cflag) != byte_len) {
              (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
              (void) fprintf(stderr, "\tmd_read failed, message type %d\n",
                             type);
              exit(-1);
            }

            /*
             * Do the actual fsum operation incrementally so that it is
             * partially masked by the communication
             *
             * During the replacement, need to check to make sure that
             * an existing value is not being overwritten.
             */

            for (i = 0; i < unit_len; i++) {
              if (sum)
                vector[start_pos+i] += ibuf[i];
              else {
                if (vector[start_pos+i] == 0)
                  vector[start_pos+i] = ibuf[i];
              }
            }
            start_pos += unit_len;
            type++;
          }
          safe_free((void **) &ibuf);
    }

      /*
       * Recursively call the subroutine again for the lower 1/2 rounded up
       * number of processors.  Don't need to call the routine again if
       * left_nprocs = 1 already
       */

    if (left_nprocs > 1)
      fanin_int(vector, len, sum, node, left_nprocs);
  }
  else {

    /*
     * Current processor is in the top half (rounded down) number of processors
     *  - Send the message, then exit at the bottom or routine
     */

    partner = node - left_nprocs;
    start_pos = 0;
    type = base_type;
    for (j = 0; j*num_units_per_message < len; j++) {
      if (start_pos + num_units_per_message > len)
        unit_len = len - start_pos;
      byte_len = sizeof(int) * unit_len;
      type = base_type + j;

      /*
       * Must read an acknowledgement periodically to ensure message buffer is
       * not overflown.  This is true especially for the first message, because
       * we don't know that the receiving processor has caught up to this one,
       * yet.
       */

      if ((j % max_in_cbuf) == 0) {
        if (md_read((char *) NULL, 0, &partner, &type_ack, &cflag) != 0) {
          (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
          (void) fprintf(stderr, "\tmd_read ack failed, message type %d\n",
                         type_ack);
          exit(-1);
        }
      }

      /* Start the transfer of data */

      if (md_write((char *) &vector[start_pos], byte_len, partner, type,
                   &cflag) != 0) {
        (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
        (void) fprintf(stderr, "\tmd_write failed, message type %d\n", type);
        exit(-1);
      }
      start_pos += unit_len;
      type++;
    }
  }

}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

void fanin_str(
               char *vector,   /* vector of values to sum */
               int  len,       /* length of vector */
               int  node,      /* my processor id */
               int  nprocs     /* number of processors in machine, initially,
                                  and thereafter, the number of processors
                                  still participating in the fanin */
)

/*
 *  fanin_char:
 *      Routine to do a fanin to node 0, of a fixed vector length.
 * 	The messages are broken into pieces to avoid overflowing the
 * message buffers.
 *     The algorithm users recursive calling of the fanin routine.
 * There are round_up [ log_2(nproc) ] levels or recursive calls in this
 * algorithm.  Therefore, it attempts to be speedy by reducing the
 * total amount of message traffic.
 *
 */

{
  char     *yo = "fanin_str";

  char     *cbuf;                      /* buffer for receiving messages */
  int       left_nprocs;               /* # processors in left half of
                                          machine                       */
  int       num_units_per_message;     /* default # values transmitted in
                                          a single message */
  int       unit_len;                  /* actual # values transmitted in
                                          the current message */
  int       byte_len;                  /* length of message in bytes */
  int       partner;                   /* processor I communicate with */
  int       start_pos;                 /* index into vector */
  int       cflag;                     /* dummy nread/nwrite parameter */
  int       max_in_cbuf;               /* max # messages allowed in buffer */
  int       type;                      /* type of data message */
  int       type_ack;                  /* type for handshaking messages */
  int       base_type;                 /* starting type for data messages */
  int       i, j;                      /* loop counters */

  /*
   *  Normal exit location for the recursive calls
   */

  if (nprocs <= 1) return;

  type_ack  = MT_FANNIN;
  base_type = MT_FANNIN + 1;

  max_in_cbuf = BRCST_COMM_BUFF_SIZE / MSIZE_FANNIN;

  num_units_per_message = MSIZE_FANNIN / sizeof(char);
  unit_len = num_units_per_message;

  left_nprocs = (nprocs + 1) / 2;

  /* Receive message */

  if (node < left_nprocs) {
    partner = node + left_nprocs;
    if (partner < nprocs) {

          /* NOTE - later optimizations could reduce the number of times this
             malloc is done during the recursive calls */

          cbuf = (char *) array_alloc(__FILE__, __LINE__, 1, unit_len,
                                      sizeof(char));
          start_pos = 0;
          type = base_type;
          for (j = 0; j*num_units_per_message < len; j++) {

            /* Handshake periodically to ensure message buffer not overflown. */

            if ((j % max_in_cbuf) == 0) {
              if (md_write((char *) NULL, 0, partner, type_ack, &cflag) != 0) {
                (void)fprintf(stderr, "%s: ERROR on node %d\n",
                              yo, node);
                (void)fprintf(stderr,
                              "\tmd_write ack failed, message type %d\n",
                              type_ack);
                exit(-1);
              }
            }
            if (start_pos + num_units_per_message > len)
              unit_len = len - start_pos;

            byte_len = sizeof(char) * unit_len;
            if (md_read(cbuf, byte_len, &partner, &type, &cflag) != byte_len) {
              (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
              (void) fprintf(stderr, "\tmd_read failed, message type %d\n",
                             type);
              exit(-1);
            }

            /*
             * Do the actual replacement operation incrementally so that it
             * is * partially masked by the communication
             *
             * During the replacement, need to check to make sure that
             * an existing value is not being overwritten.
             */

            for (i = 0; i < unit_len; i++) {
              if (vector[start_pos+i] == '\0')
                vector[start_pos+i] = cbuf[i];
            }
            start_pos += unit_len;
            type++;
          }
          safe_free((void **) &cbuf);
    }

      /*
       * Recursively call the subroutine again for the lower 1/2 rounded up
       * number of processors.  Don't need to call the routine again if
       * left_nprocs = 1 already
       */

    if (left_nprocs > 1)
      fanin_str(vector, len, node, left_nprocs);
  }
  else {

    /*
     * Current processor is in the top half (rounded down) number of processors
     *  - Send the message, then exit at the bottom or routine
     */

    partner = node - left_nprocs;
    start_pos = 0;
    type = base_type;
    for (j = 0; j*num_units_per_message < len; j++) {
      if (start_pos + num_units_per_message > len)
        unit_len = len - start_pos;
      byte_len = sizeof(char) * unit_len;
      type = base_type + j;

      /*
       * Must read an acknowledgement periodically to ensure message buffer is
       * not overflown.  This is true especially for the first message, because
       * we don't know that the receiving processor has caught up to this one,
       * yet.
       */

      if ((j % max_in_cbuf) == 0) {
        if (md_read((char *) NULL, 0, &partner, &type_ack, &cflag) != 0) {
          (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
          (void) fprintf(stderr, "\tmd_read ack failed, message type %d\n",
                         type_ack);
          exit(-1);
        }
      }

      /* Start the transfer of data */

      if (md_write(&vector[start_pos], byte_len, partner, type, &cflag) != 0) {
        (void) fprintf(stderr, "%s: ERROR on node %d\n", yo, node);
        (void) fprintf(stderr, "\tmd_write failed, message type %d\n", type);
        exit(-1);
      }
      start_pos += unit_len;
      type++;
    }
  }

}

/******************************************************************************/
