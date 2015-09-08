/*******************************************************************************
 * Copyright 1995, Sandia Corporation.  The United States Government retains a *
 * nonexclusive license in this software as prescribed in AL 88-1 and AL 91-7. *
 * Export of this program may require a license from the United States         *
 * Government.                                                                 *
 ******************************************************************************/

/*
 * HP process timer:
 *
 *   This timer is suppose to return the time in seconds taken up by the user
 * process (i.e., user time plus system time spent doing user process work).
 * This time is most naturally returned by the function clock().  However, has
 * the unfortunate property that it wraps arounds every 36 minutes. On the sun,
 * the function getrusage() can be used to return the time without this
 * wraparound problem.  However, I haven't found anything equivalent on the HP.
 * Note that the function gettimeofday() returns a very precise definition of
 * the time.  However, it is strictly a wall clock time.
 */

#include <time.h>

double second (void)

{
  extern clock_t clock(void);

  return ( ((double) clock()) / (double) CLOCKS_PER_SEC);
}
