/*******************************************************************************
 * Copyright 1995, Sandia Corporation.  The United States Government retains a *
 * nonexclusive license in this software as prescribed in AL 88-1 and AL 91-7. *
 * Export of this program may require a license from the United States         *
 * Government.                                                                 *
 ******************************************************************************/


#include <sys/time.h>
#include <sys/resource.h>

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

double second(void)

{
  double time;                  /* elapsed time in seconds */

  struct rusage rusage;

  extern int getrusage();

  getrusage(RUSAGE_SELF, &rusage);

  time = ((rusage.ru_utime.tv_sec + rusage.ru_stime.tv_sec) +
          1.0e-6 * (rusage.ru_utime.tv_usec + rusage.ru_stime.tv_usec));

  return time;

} /* second */
