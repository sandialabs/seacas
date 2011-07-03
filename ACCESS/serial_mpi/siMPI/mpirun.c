/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: mpirun.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:50:40 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/**************************************************************************/
/* FILE   **************          mpirun.c         ************************/
/**************************************************************************/
/* Author: Patrick Miller July 19 2002					  */
/* Copyright (C) 2002 University of California Regents			  */
/**************************************************************************/
/*                                                                        */
/**************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char** argv) {
  char* arguments[1000];
  int i;

  /* ----------------------------------------------- */
  /* Copy over original arguments, replacing mpirun  */
  /* with the executable                             */
  /* ----------------------------------------------- */
  arguments[0] = argv[3];
  for(i=1;i<argc;++i) arguments[i] = argv[i+3];

  execvp(arguments[0],arguments);
  perror(arguments[0]);
  return 1;
}
