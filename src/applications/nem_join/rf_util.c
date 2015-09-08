#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
                                /* It would be nice if the routines in this
				   file didn't have include dependencies!   */
#include "pe_common.h"
#include "el_geom_const.h"
#include "rf_fem_const.h"
#include "rf_allo.h"

/************ R O U T I N E S   I N   T H I S   F I L E  **********************

       NAME            		        TYPE
----------------------------------------------------------------------
	sort_int_int		        void
        sort_int_float			void
        sort_int_double			void
	sort_int_ptr		        void
	sort_int_int_ptr	        void
	sort_int			void

        find_max			void
	find_min			int
	find_inter			int
	find_inter_pos			int
	exchange_pointers		void
        in_list_mono                    int
        bin_search2                     int
        find_range                      int
        bin_search_min                  int
 	print_line			void
        break_message_up                int

******************************************************************************/

/******************* PROTOTYPES FOR STATIC FUNCTIONS *************************/

static int bin_search_min(int list[], int num, int value);

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
#define SWAP(type, r,s)  do{type t=r; r=s; s=t; } while(0)

static void siftDown( int *a, int *b, int start, int count);

void sort_int_int(int count, int ra[], int rb[])
{
  int start, end;
 
  /* heapify */
  for (start = (count-2)/2; start >=0; start--) {
    siftDown( ra, rb, start, count);
  }
 
  for (end=count-1; end > 0; end--) {
    SWAP(int, ra[end],ra[0]);
    SWAP(int, rb[end],rb[0]);
    siftDown(ra, rb, 0, end);
  }
}
 
static void siftDown( int *a, int *b, int start, int end)
{
  int root = start;
 
  while ( root*2+1 < end ) {
    int child = 2*root + 1;
    if ((child + 1 < end) && (a[child] < a[child+1])) {
      child += 1;
    }
    if (a[root] < a[child]) {
      SWAP(int, a[child], a[root] );
      SWAP(int, b[child], b[root] );
      root = child;
    }
    else
      return;
  }
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static void siftDown_if( int *a, float *b, int start, int count);

void sort_int_float(int count, int ra[], float rb[])
{
  int start, end;
 
  /* heapify */
  for (start = (count-2)/2; start >=0; start--) {
    siftDown_if( ra, rb, start, count);
  }
 
  for (end=count-1; end > 0; end--) {
    SWAP(int,   ra[end],ra[0]);
    SWAP(float, rb[end],rb[0]);
    siftDown_if(ra, rb, 0, end);
  }
}
 
static void siftDown_if( int *a, float *b, int start, int end)
{
  int root = start;
 
  while ( root*2+1 < end ) {
    int child = 2*root + 1;
    if ((child + 1 < end) && (a[child] < a[child+1])) {
      child += 1;
    }
    if (a[root] < a[child]) {
      SWAP(int,   a[child], a[root] );
      SWAP(float, b[child], b[root] );
      root = child;
    }
    else
      return;
  }
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static void siftDown_id( int *a, double *b, int start, int count);

void sort_int_double(int count, int ra[], double rb[])
{
  int start, end;
 
  /* heapify */
  for (start = (count-2)/2; start >=0; start--) {
    siftDown_id( ra, rb, start, count);
  }
 
  for (end=count-1; end > 0; end--) {
    SWAP(int,   ra[end],ra[0]);
    SWAP(double, rb[end],rb[0]);
    siftDown_id(ra, rb, 0, end);
  }
}
 
static void siftDown_id( int *a, double *b, int start, int end)
{
  int root = start;
 
  while ( root*2+1 < end ) {
    int child = 2*root + 1;
    if ((child + 1 < end) && (a[child] < a[child+1])) {
      child += 1;
    }
    if (a[root] < a[child]) {
      SWAP(int,   a[child], a[root] );
      SWAP(double, b[child], b[root] );
      root = child;
    }
    else
      return;
  }
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static void siftDown_ip( int *a, char **b, int start, int count);

void sort_int_ptr(int count, int ra[], char *rb[])
{
  int start, end;
 
  /* heapify */
  for (start = (count-2)/2; start >=0; start--) {
    siftDown_ip( ra, rb, start, count);
  }
 
  for (end=count-1; end > 0; end--) {
    SWAP(int,   ra[end],ra[0]);
    SWAP(char*, rb[end],rb[0]);
    siftDown_ip(ra, rb, 0, end);
  }
}
 
static void siftDown_ip( int *a, char **b, int start, int end)
{
  int root = start;
 
  while ( root*2+1 < end ) {
    int child = 2*root + 1;
    if ((child + 1 < end) && (a[child] < a[child+1])) {
      child += 1;
    }
    if (a[root] < a[child]) {
      SWAP(int,   a[child], a[root] );
      SWAP(char*, b[child], b[root] );
      root = child;
    }
    else
      return;
  }
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static void siftDown_iip( int *a, int *b, char **c, int start, int count);

void sort_int_int_ptr(int count, int ra[], int rb[], char *rc[])
{
  int start, end;
 
  /* heapify */
  for (start = (count-2)/2; start >=0; start--) {
    siftDown_iip( ra, rb, rc, start, count);
  }
 
  for (end=count-1; end > 0; end--) {
    SWAP(int,   ra[end],ra[0]);
    SWAP(int,   rb[end],rb[0]);
    SWAP(char*, rc[end],rc[0]);
    siftDown_iip(ra, rb, rc, 0, end);
  }
}
 
static void siftDown_iip( int *a, int *b, char **c, int start, int end)
{
  int root = start;
 
  while ( root*2+1 < end ) {
    int child = 2*root + 1;
    if ((child + 1 < end) && (a[child] < a[child+1])) {
      child += 1;
    }
    if (a[root] < a[child]) {
      SWAP(int,   a[child], a[root] );
      SWAP(int,   b[child], b[root] );
      SWAP(char*, c[child], c[root] );
      root = child;
    }
    else
      return;
  }
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static void siftDown_i( int *a, int start, int count);

void sort_int(int count, int ra[])
{
  int start, end;
 
  /* heapify */
  for (start = (count-2)/2; start >=0; start--) {
    siftDown_i( ra, start, count);
  }
 
  for (end=count-1; end > 0; end--) {
    SWAP(int,   ra[end],ra[0]);
    siftDown_i(ra, 0, end);
  }
}
 
static void siftDown_i( int *a, int start, int end)
{
  int root = start;
 
  while ( root*2+1 < end ) {
    int child = 2*root + 1;
    if ((child + 1 < end) && (a[child] < a[child+1])) {
      child += 1;
    }
    if (a[root] < a[child]) {
      SWAP(int,   a[child], a[root] );
      root = child;
    }
    else
      return;
  }
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int find_max(int list_length, int *list)

     /*
       Function which finds the largest integer from a vector of integers

       Author:          Scott Hutchinson (1421)
       Date:            8 December 1992

       */

/*****************************************************************************/

{

  /* local variables */

  register int i, max;

 /*************************** execution begins *******************************/

  if (list_length > 0) {
    max = list[0];
    for (i = 1; i < list_length; i++)
      if (list[i] > max) max = list[i];
    return (max);
  } else
    return (INT_MIN);

} /* find_max */

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int find_min(int list_length, int list[])

/*
 *     Function which finds the smallest integer from a vector of integers
 *
 *      Author:          Scott Hutchinson (1421)
 *      Date:            8 December 1992
 *
 */

{

  /* local variables */

  register int i, min;

  /************************** execution begins *******************************/
  if (list_length > 0) {
    min = list[0];
    for (i = 1; i < list_length; i++)
      if (list[i] < min)  min = list[i];
    return min;
  } else
      return INT_MAX;

} /* find_min ****************************************************************/

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int find_inter(int inter_ptr[], int set1[], int set2[], int length1,
	       int length2, int prob_type)

/*
 *
 *      Function which finds the intersection of two integer lists.
 *      The points in set1 that belong in the intersection set are
 *      returned in the vector inter_pts, starting at position inter_pts[0].
 *      Enough space in inter_pts[] (min(length1, length2)) must
 *      have already been allocated in the calling program before this
 *      function is called.
 *
 *        prob_type defines the problem to be addressed:
 *            0 = don't know anything about set1 or set2.
 *            1 = Know that set2 is monotonically increasing.
 *            2 = Know that set1 and set2 are monotonically increasing
 *
 *      On return, find_inter returns 0 if there is no intersection.
 *      It returns the number of points in the intersection, if there
 *      is an intersection.
 */

{

  /* Local variables */

  register int    i, j, counter = 0;
  int             max_set1, min_set1, max_set2, min_set2;

  /* Extern Declarations */

  extern int      find_max (int, int*), find_min (int, int*);

/****************************** execution begins *****************************/

  /* Error check the arguments */
  if ((length1 <= 0) || (length2 <= 0)) return (counter);

  if (prob_type == 0 ) {

    /* find the maximum and the minimum of the two sets */
    max_set1 = find_max (length1, set1);
    min_set1 = find_min (length1, set1);
    max_set2 = find_max (length2, set2);
    min_set2 = find_min (length2, set2);

    /*  check for a possible overlaps in node numbers;
     *  If there is an overlap, then do the search
     */

    if ( (max_set2 >= min_set1) && (min_set2 <= max_set1) )   {

      for (i = 0; i < length1; i++)
	for (j = 0; j < length2; j++)
	  if (set1[i] == set2[j])  inter_ptr[counter++] = i;

    }
  } else if (prob_type == 1) {

    fprintf (stderr, "prob_type = 1 is unimplemented\n");
    exit(1);

  } else if (prob_type == 2) {
    /*
     *    Find the maximum and the minimum of the two sets
     */
    max_set1 =  set1[length1-1];
    min_set1 =  set1[0];
    max_set2 =  set2[length2-1];
    min_set2 =  set2[0];
    /*
     *    Check for a possible overlaps in node numbers;
     *    If there is an overlap, then do the search using a linearly
     *    scaled method
     */
    if ( (max_set2 >= min_set1) && (min_set2 <= max_set1) )   {

      for (i = 0, j = 0; i < length1; i++) {
	if (set1[i] > max_set2) {
	  break;
	}
	while ( (j < (length2-1)) && (set1[i] > set2[j]) ) 
	  j++;

	if (set1[i] == set2[j]) 
	  inter_ptr[counter++] = i;
      }
    }
  } else {

    fprintf (stderr, "prob_type = %d is unknown\n", prob_type);
    exit(1);

  }

  return counter;

} /* find_inter **************************************************************/


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int find_inter_pos(int intersect[], int length1, int set1[], int length2,
                   int set2[], int prob_type)

/*
 *
 *	Function which finds the intersection of two integer lists.
 *	inter_ptr[] must be allocated as at least the same length as
 *	set1[]. The values in inter_ptr[] correspond to the location
 *	in set2[] of the value in the same position in set2[]. If the
 *	value in set1[] is not in set2[], then inter_ptr is set to -1
 *	for that position.
 *
 *	    prob_type defines the problem to be addressed:
 *		0 = don't know anything about set1 or set2.
 *		1 = Know that set2 is monotonically increasing.
 *		2 = Know that set1 and set2 are monotonically increasing
 *
 *      On return, find_inter_pos returns 0 if there is no intersection.
 *      It returns the number of points in the intersection, if there
 *      is an intersection.
 */

{

  /* Local variables */

  register int    i, j, count = 0;
  int             max_set1, min_set1, max_set2, min_set2;

/****************************** execution begins *****************************/

  /* Error check the arguments */
  if ((length1 <= 0) || (length2 <= 0)) return (count);

  /* Initialize the intersect vector */
  for(i=0; i < length1; i++)
    intersect[i] = -1;

  if (prob_type == 0 ) {

    /* find the maximum and the minimum of the two sets */
    max_set1 = find_max (length1, set1);
    min_set1 = find_min (length1, set1);
    max_set2 = find_max (length2, set2);
    min_set2 = find_min (length2, set2);

    /*  check for a possible overlaps in node numbers;
     *  If there is an overlap, then do the search
     */

    if ( (max_set2 >= min_set1) && (min_set2 <= max_set1) )   {

      for (i = 0; i < length1; i++)
        for (j = 0; j < length2; j++)
          if (set1[i] == set2[j]) {
            intersect[i] = j;
            count++;
          }

    }
  } else if (prob_type == 1) {

    fprintf (stderr, "prob_type = 1 is unimplemented\n");
    exit(1);

  } else if (prob_type == 2) {

    /* Check for the possibility of an intersection */
    min_set1 = set1[0];
    max_set1 = set1[length1-1];
    min_set2 = set2[0];
    max_set2 = set2[length2-1];

    /* Search through the set */

    if( (max_set2 >= min_set1) && (min_set2 <= max_set1) ) {
      for(i=0, j=0; i < length1; i++) {
        while( (j < (length2-1)) && (set1[i] > set2[j]) ) j++;
        if (set1[i] == set2[j]) {
          intersect[i] = j;
          count++;
        }
      }
    }
  } else {

    fprintf (stderr, "prob_type = %d is unknown\n", prob_type);
    exit(1);

  }

  return count;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

void exchange_pointers(void **pointer1, void **pointer2)

/*
 *	This function can be used to exchange the addresses that two pointers
 *       point to in the calling routine.  It should be called in the
 *       following way:
 *
 *	    double *pt1, *pt2;
 *	    exchange_pointers ( (void **) &pt1,  (void **) &pt2 );
 *
 *
 */

{
  void *temp_pointer;

  temp_pointer = *pointer1;
  *pointer1    = *pointer2;
  *pointer2    = temp_pointer;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int in_list_mono(int ivalue, int *ibegin, int iend, int ivector[])

/*
 *     This function searches an integer vector, ivector(ibegin:iend-1),
 *     for the presence of a number, ivalue.  It returns the index of the
 *     value, or -1, if the number, ivalue, is not found in the list.
 *     ivector is assumed to be monotonic.  Therefore, the search is
 *     stopped when ivector[i] > ibegin.  It is also assumed that this
 *     function will be called repeatibly with monotonically increasing
 *     values of ivalue.  Therefore, *ibegin is incremented at the end of
 *     the routine to the last value in ivector that was checked.
 */

{
  register int i = *ibegin;

  if ((ivalue >= ivector[i]) && (ivalue <= ivector[iend-1])) {
    while (ivalue > ivector[i]) {
      if (i < iend-1) i++;
    }
    *ibegin = i;
    if (ivalue == ivector[i])
      return i;
    else
      return -1;
  } else
    return -1;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int bin_search2 (int value, int num, int List[])

/*
 * Searches a monotonic list of values for the value, value.
 * It returns the index of the first position found, which matches value.
 * The list is assumed to be monotonic, and
 * consist of elements list[0], ..., list[n-1].
 * If no position in list matches value, it returns the value -1.
 *
 */

{

 register int top, bottom = 0, middle, g_mid;

 /***** execution begins *****/

 top = num - 1;
 while (bottom <= top) {
   middle = (bottom + top) >> 1;
   g_mid = List[middle];
   if (value < g_mid)
     top = middle - 1;
   else if (value > g_mid)
     bottom = middle + 1;
   else
     return middle;     /* found */
 }

 return -1;

} /* bin_search2 */

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

int find_range (int start_value, int end_value, int list[], int num,
                int *start_pos, int *end_pos)

/*
 * find_range:
 *
 *      This routine searches a monotonically increasing list for the range
 * of values which fall between two given values, i.e.,
 *
 *  start_value <= list[i] <= end_value, for all i in the range
 *
 * The number of values which fall in the given range is returned.
 * The lowest value of i such that the above is true is returned in
 * start_pos, while the highest value of i is returned in end_pos
 *   If there is no overlap, find_range is set to 0, and *start_pos and
 * *end_pos will have unspecified values.
 *
 *   Input
 * ----------
 *  start_value = Lower value of the range sought
 *  end_value   = Upper value of the range sought
 *  list        = Vector of integer values to be searched
 *                (must be a monotonically increasing function)
 *                i.e., {list[0], ..., list[num-1]}
 *  num         = Length of the vector, list
 *
 *   Output
 * ----------
 *  (return)    =  Number of values in the vector list, which are in the
 *                 designated range.
 *  start_pos   =  Address of the value of the starting position within list[]
 *                 which is in the designated range.
 *   end_pos    =  Address of the value of the ending position within list[]
 *                 which is in the designated range.
 *
 */

{

  /* Local Variables: */

  int        start, end;

  /* Error check the argument list */

  if (end_value < start_value) return (0);
  if (num <= 0)                return (0);

  /* Check for Obvious Limits -saves doing a lot of special cases below */

  if (start_value > list[num - 1]) return (0);
  if (  end_value < list[0])       return (0);

  /* Find starting position in list */

  start = bin_search_min (list, num, start_value);

  /* Find ending position in list */

  end   = bin_search_min (&list[start], num - start, end_value);
  end  += start;

  if (list[start] < start_value) start++;
  if (start > end) return (0);

  *start_pos = start;
  *end_pos   = end;
  return (end - start + 1);

}

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

static int bin_search_min(int list[], int num, int value)

/*
 *     Searches a monotonic list of values for the value, value.
 * It returns the index of the first position found, which matches value.
 * The list is assumed to be monotonic, and
 * consist of elements list[0], ..., list[n-1].
 *   If no position in list matches value, it the next lowest position
 * in the list, or 0 if the value is lower than all values in list []
 * Error conditions are specified with a return value of -1.
 *
 *  example:
 * -------------
 *    list [] = {3, 5, 7, 9}, num = 4
 *
 *       value       return_value
 *      ---------   --------------
 *          1             0
 *          3             0
 *          6             1
 *          13            3
 * ---------------------------------------------
 */

{
  int top, bottom, diff, middle, g_mid, g_bot, g_top;

  if (num <= 0) return (-1);
  bottom = 0;
  g_bot = list[0];
  if (value <= g_bot)
    return bottom;
  else {
    top    = num - 1;
    g_top  = list[top];
    if (value >=  g_top)
      return top;
    else {
      while ( (diff = top - bottom) > 1) {
        middle = bottom + diff/2;
        g_mid = list[middle];
        if (value > g_mid) {
          bottom = middle;
        } else if (value < g_mid) {
          top    = middle;
        } else {
          return middle;
        }
      }
    }
  }

  return bottom;

}

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

void print_line (char *charstr, int ntimes)

{
  int i;
  for (i = 0; i < ntimes; i++) (void) printf("%c", *charstr);
  (void) printf("\n");
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int break_message_up (unsigned int unit_size, int num_units, int max_bytes,
		      int *start_pos[])

/*
 *   break_message_up:
 *
 *         This routine will break a long message up into chunks.  It returns
 * the number of chunks and the starting and ending positions, in terms of
 * unit numbers, of each chunk.
 * It assumes that the first unit of the message has a positional value of 0.
 *
 * NOTE:
 *   This routine does malloc memory.  This memory must be freed in the
 * calling routine, when it is no longer needed.  Use the following statement
 * to free the memory:
 *
 *       safe_free ((void **) &start_pos);
 *
 * Input
 * -------
 *
 * unit_size      = Size in bytes of each "unit" in the message.
 * num_units      = Number of units in the message.
 * max_bytes      = Max_bytes allowed in each message.
 *
 * Output
 * -------
 *
 * return_value   = Number of messages
 * start_pos []   = Starting positions of each message
 *                      (note: start_pos[0] = 0, by definition).
 *                    It is defined to have a length of num_mesg+1
 *                    start_pos[num_mesg] = num_units
 *
 *
 * Usage
 *----------
 *       To get the length of message i:
 *
 *                   mesg_length = start_pos[i+1] - start_pos[i]
 *
 */

{
  int           i, num_units_per_message, remainder, num_mesg;

 /*------------------------- begin execution --------------------------------*/

  if (num_units <= 0) {
    num_mesg = 0;
    *start_pos = NULL;
    return (num_mesg);
  }

  num_units_per_message = max_bytes / unit_size ;
  if (num_units < num_units_per_message) num_units_per_message = num_units;
  num_mesg   = num_units / num_units_per_message;
  remainder  = num_units % num_units_per_message;
  if (remainder != 0) num_mesg++;

  *start_pos = (int *) array_alloc (__FILE__, __LINE__, 1, (num_mesg + 1),
                                    sizeof (int));

  for (i = 0; i < num_mesg; i++) {
    (*start_pos)[i] =  i * num_units_per_message;
  }
  (*start_pos) [num_mesg] = num_units;

  return num_mesg;
}

/*****************************************************************************/
/*                END OF FILE rf_util.c					     */
/*****************************************************************************/
