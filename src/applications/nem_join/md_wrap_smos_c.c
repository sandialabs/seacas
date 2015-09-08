/******************************************************************************
 * Copyright 1995, Sandia Corporation.  The United States Government retains  *
 * a nonexclusive license in this software as prescribed in AL 88-1 and       *
 * AL 91-7. Export of this program may require a license from the United      *
 * States Government.                                                         *
 *****************************************************************************/

#include <stdio.h>
#include <string.h>

#ifdef MPI
#include <mpi.h>
#else
#define MPI_Request int
#endif

#define nCUBE 1
#define INTEL 2
#define SUN   3
#define DELTA 4
#define MACHINE INTEL
#define CUBESIZ 128 /* a power of two >= number of processors */

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void get_parallel_info(int *proc, int *nprocs, int *dim)

{

  int dummy1, dummy2, dummy3;
  int host, nodeID;

  *proc   = mynode();
  *nprocs = numnodes();
  *dim    = 0;

} /* get_parallel_info */

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int md_read(char *buf, int bytes, int *source, int *type, int *flag)

{
  int i;

  i = nread(buf, bytes, source, type, flag);
  return i;

} /* md_read */

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int md_write(char *buf, int bytes, int dest, int type, int *flag)

{
  int i;
  i = nwrite(buf, bytes, dest, type, flag);
  return i;

} /* md_write */

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

/*
 * This section is used to kill the program on all nodes when an "exit" is
 * called from the program.  The "exit" call must be redefined in a header file
 * (e.g., rf_salsa.h) to mean "smos_exit" when being compiled for SMOS on the
 * Intel.  Make sure -DSMOS is used when compiling.
 */

#include <signal.h>

int smos_exit(int ignore, char *fname, int lno)

{
  fprintf(stderr, "global exit called from %s at line %d\n",
          fname, lno);
  nodekill(-1, 1, SIGKILL);
}
