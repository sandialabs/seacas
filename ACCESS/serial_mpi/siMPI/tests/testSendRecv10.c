
/*TEST
PATH='tests/testSendRecv10.c'
CCFLAGS=""
INPUT=""
OUTPUT='0 1 2 3 4 5 6 7 8 9 \n--\n0 1 2 3 1 2809400 1 0 1 2807328 '
STATUS=0
TEST*/

/******************************************************************/
/* FILE  ***********    testSendRecv10.c       ********************/
/******************************************************************/
/* Author : Lisa Alano June 5 2002                                */
/* Copyright (c) 2002 University of California Regents            */
/******************************************************************/
/******************************************************************/

#if 0
CCFLAGS = None 
ARGS = None
INPUT = EOF 
OUTPUT = 0 1 2 3 4 5 6 7 8 9 
--
0 1 2 3 1073824416 1 537107616 1 -1073726720 1023
         MPI_Send/Recv buffer type mismatch. - Junk.
STATUS = 0 
#endif

#include <stdio.h>
#include <string.h>
#include "mpi.h"

int main(int argc, char**argv) 
{
  int my_rank;
  int p, i;
  int message1[15];     /* Should be char */
  int message2[15];
  int source, dest, tag; 
  MPI_Status status;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &p);

  for(i = 0 ; i < 10 ; i++)
  {
      message1[i] = i;
  }
  for(i=0; i<10; i++)
  {
     printf("%d ", message1[i]);
  }
  printf("\n--\n");
  source = tag = dest = 0;
  MPI_Send(message1, 15, MPI_CHAR, dest, tag, MPI_COMM_WORLD);
  MPI_Recv(message2, 15, MPI_CHAR, source, tag, MPI_COMM_WORLD, &status);

  for(i=0; i<10; i++)
  {
    printf("%d ", message2[i]);
  }
  MPI_Finalize();
  return 0;
}
