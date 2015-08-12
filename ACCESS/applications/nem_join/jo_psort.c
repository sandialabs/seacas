#include <stdlib.h>
#include <stdio.h>

#include <mpi.h>

/******************** ROUTINES IN THIS FILE ***********************************
*
*     Name                      Type                  Called By
*   --------                 ---------              ---------------
*
******************************************************************************/

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

void psort_int(int node, int nprocs, int *nval, int *values, int imax)
{
  /* Local Variables */
  char       *yo = "psort_int";

  int        *buf, ibuf;
  int         val_hi, val_lo, val_mid, val_old;
  int         proc_hi, proc_lo, proc_mid;
  int         partner[2], target, nlocal;
  int         i, ncnt, ntotal, iflag, sflag, rflag;

  MPI_Comm    local_comm, tmp_comm;
  MPI_Status  status;
  MPI_Request request;

  /*------------------- begin execution -------------------------------------*/

  if (nprocs <= 1) return;

  /* create a temporary duplicate communicator */
  if (MPI_Comm_dup(MPI_COMM_WORLD, &local_comm) != MPI_SUCCESS) {
    fprintf(stderr, "%s: MPI Error: unable to duplicate communicator\n", yo);
    exit (-1);
  }

  nlocal = *nval;   /* don't want to have to deal with a pointer */
  proc_lo = 0;
  proc_hi = nprocs - 1;

  do {

    proc_mid = ((proc_lo + proc_hi) / 2) + 1;
    if (node < proc_mid) partner[0] = node + (proc_mid - proc_lo);
    else                 partner[0] = node - (proc_mid - proc_lo);

    /*
     * need to put a check here in case there are an
     * odd number of processors. Note: if there are
     * an odd number of processors, then the extra proc
     * is in the lower half. Change its partner to be
     * the last processor in the upper half.
     *
     * Also, the last processor will then have two partners.
     * It's second partner will be the last node in the lower
     * half (which is proc_mid - 1).
     */
    if (partner[0] > proc_hi) {
      partner[0] = proc_hi;
      sflag = 1;
    }
    else
      sflag = 0;

    if ((node == proc_hi) && ((proc_hi - proc_lo + 1)%2 != 0)) {
      partner[1] = proc_mid - 1;
      rflag = 1;
    }
    else
      rflag = 0;

    /* find the total number of values in this communicator */
    if (MPI_Allreduce(&nlocal, &ntotal, 1, MPI_INT, MPI_SUM,
                      local_comm) != MPI_SUCCESS) {
      fprintf(stderr, "%s: MPI Error: unable to get global sum\n", yo);
      exit (-1);
    }

    /* find the high and low values in this partition */
    val_lo = values[0];
    for (i = 1; i < nlocal; i++)
      if (values[i] < val_lo) val_lo = values[i];
    if (MPI_Allreduce(&val_lo, &ncnt, 1, MPI_INT, MPI_MIN,
                      local_comm) != MPI_SUCCESS) {
      fprintf(stderr, "%s: MPI Error: unable to get global min\n", yo);
      exit (-1);
    }
    val_lo = ncnt;

    val_hi = values[0];
    for (i = 1; i < nlocal; i++)
      if (values[i] > val_hi) val_hi = values[i];
    if (MPI_Allreduce(&val_hi, &ncnt, 1, MPI_INT, MPI_MAX,
                      local_comm) != MPI_SUCCESS) {
      fprintf(stderr, "%s: MPI Error: unable to get global max\n", yo);
      exit (-1);
    }
    val_hi = ncnt;

    /*
     * Now find the number of values that should be in the
     * lower half. If there are an odd number of processors,
     * then this will not simply be half of the values, since
     * there may be more than half of the processors in the
     * lower half.
     */
    target = ((proc_mid - proc_lo) * ntotal) / (proc_hi - proc_lo + 1);

#ifdef DEBUG_PSORT
    if (node == 0) {
      printf ("\n\n[%d]: Sorting parameters\n", node);
      printf ("\t[%d]Hi Processor:\t%d\n", node, proc_hi);
      printf ("\t[%d]Low Processor:\t%d\n", node, proc_lo);
      printf ("\t[%d]Middle Processor:\t%d\n", node, proc_mid);
      printf ("\t[%d]Number of Values:\t%d\n", node, ntotal);
      printf ("\t[%d]Partner:\t\t%d\n", node, partner[0]);
      if (rflag)
        printf ("\t[%d]Second Partner:\t%d\n", node, partner[1]);
      printf ("\t[%d]sflag:\t\t%d\n", node, sflag);
      printf ("\t[%d]Target Number:\t%d\n", node, target);
    }
#endif

    /*
     * median search loop
     * val_mid - current guess for cut
     * ntotal  - # of values less than the cut
     * compare ntotal to target, & adjust search bounds accordingly
     *
     * Since there can be duplicates, it is possible that there
     * is not an exact cut.
     */
    ntotal = -1;
    val_mid = 0;
    do {
      val_old = val_mid;

      val_mid = ((val_lo + val_hi) / 2) + 1;

      ncnt = 0;
      for (i = 0; i < nlocal; i++)
        if (values[i] < val_mid) ncnt++;

      if (MPI_Allreduce(&ncnt, &ntotal, 1, MPI_INT, MPI_SUM,
                        local_comm) != MPI_SUCCESS) {
        fprintf(stderr, "%s: MPI Error: unable to get global sum\n", yo);
        exit (-1);
      }

      if (ntotal < target)      val_lo = val_mid;
      else if (ntotal > target) val_hi = val_mid;

    } while (ntotal != target && val_mid != val_old);

#ifdef DEBUG_PSORT
    if (node == 0) {
      printf ("\t[%d]Value Mid:\t\t%d\n", node, val_mid);
      printf ("\t[%d]Count Actual:\t%d\n", node, ntotal);
    }
#endif

    /* allocate space for the send buffer, and pack the values */
    buf = (int *) malloc(nlocal * sizeof(int));
    if (!buf) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }

    ibuf = 0;
    i = 0;
    while (i < nlocal) {

#ifdef DEBUG_PSORT
      if (node == 0) {
        printf("\n[%d]Packing buffer\n", node);
        printf("\ti = %d\n", i);
        printf("\tvalues[i] = %d\n", values[i]);
        printf("\tibuf = %d\n", ibuf);
        printf("\tnlocal = %d\n", nlocal);
      }
#endif

      if ((node < proc_mid && values[i] >= val_mid) ||
            (node >= proc_mid && values[i] < val_mid)) {
        buf[ibuf++] = values[i];
        values[i] = values[--nlocal];
      }
      else i++;
    }

#ifdef DEBUG_PSORT
    if (node == 0) {
      printf ("\n\n[%d]: Exported Values\n", node);
      printf("\t");
      for (i = 0; i < ibuf; i++)
        printf("  %d", buf[i]);
      printf("\n");
    }
#endif

    /*
     * post recieve for incoming message from partner[0]
     * send buffer to partner[0]
     * wait until message from partner[0] recieved
     * increment nlocal accordingly
     *
     * if sflag is set, then this processor is extra in
     * the lower half, and is sharing a processor. Then,
     * it does not recieve any information. It only send
     * information.
     */
    if (!sflag)
      if (MPI_Irecv(&(values[nlocal]), (imax - nlocal), MPI_INT, partner[0], 0,
                    MPI_COMM_WORLD, &request) != MPI_SUCCESS) {
        fprintf(stderr, "%s: MPI Error: recieve request failed\n", yo);
        exit (-1);
      }
    if (MPI_Send(buf, ibuf, MPI_INT, partner[0], 0,
                 MPI_COMM_WORLD) != MPI_SUCCESS) {
      fprintf(stderr, "%s: MPI Error: send failed\n", yo);
      exit (-1);
    }
    if (!sflag) {
      if (MPI_Wait(&request, &status) != MPI_SUCCESS) {
        fprintf(stderr, "%s: MPI Error: wait for recieve failed\n", yo);
        exit (-1);
      }
      if (MPI_Get_count(&status, MPI_INT, &ncnt) != MPI_SUCCESS) {
        fprintf(stderr, "%s: MPI Error: unable to get count\n", yo);
        exit (-1);
      }
      nlocal += ncnt;
    }

    if (rflag) {
      if (MPI_Irecv(&(values[nlocal]), (imax - nlocal), MPI_INT, partner[1], 0,
                    MPI_COMM_WORLD, &request) != MPI_SUCCESS) {
        fprintf(stderr, "%s: MPI Error: recieve request failed\n", yo);
        exit (-1);
      }

      if (MPI_Wait(&request, &status) != MPI_SUCCESS) {
        fprintf(stderr, "%s: MPI Error: wait for recieve failed\n", yo);
        exit (-1);
      }
      if (MPI_Get_count(&status, MPI_INT, &ncnt) != MPI_SUCCESS) {
        fprintf(stderr, "%s: MPI Error: unable to get count\n", yo);
        exit (-1);
      }
      nlocal += ncnt;
    }

    /* free the buffer space */
    free (buf);

    /* now recurse into smaller partitions */
    if (node < proc_mid) {
      proc_hi = proc_mid - 1;
      iflag = 0;
    }
    else {
      proc_lo = proc_mid;
      iflag = 1;
    }

    if (MPI_Comm_split(local_comm, iflag, 0, &tmp_comm) != MPI_SUCCESS) {
      fprintf(stderr, "%s: MPI Error: unable to split communicator\n", yo);
      exit (-1);
    }
    if (MPI_Comm_free(&local_comm) != MPI_SUCCESS) {
      fprintf(stderr, "%s: MPI Error: unable to free communicator\n", yo);
      exit (-1);
    }
    local_comm = tmp_comm;

  } while (proc_lo < proc_hi);

  if (MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS) {
    fprintf(stderr, "%s: MPI Error: barrier failed\n", yo);
    exit (-1);
  }

  *nval = nlocal; /* be sure to update this value */

}
