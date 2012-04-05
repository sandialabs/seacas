/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: mpicc.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/09/06 19:14:55 $
 *    $Revision: 1.4 $
 ****************************************************************************/
/**************************************************************************/
/* FILE   **************          mpicc.c          ************************/
/**************************************************************************/
/* Author: Patrick Miller July 15 2002					  */
/* Copyright (C) 2002 University of California Regents			  */
/**************************************************************************/
/*  */
/**************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "mpi_path.h"

#ifdef COMPILER_PATH
char* cxx = COMPILER_PATH;
#else
char* cxx = CXXPATH;
#endif

char* include_directory = IFLAG;
char* lib_directory = LFLAG;
char* lib_library = "-lsimpi";

int main(int argc, char** argv) {
  char* arguments[1000];
  int i, j;
  int verbose = 0;
  int dash_c = 0;
  char *token = NULL;
  char command[100];

  /* ----------------------------------------------- */
  /* Copy over original arguments, replacing mpicxx   */
  /* with real C compiler                            */
  /* ----------------------------------------------- */
  token = strchr(cxx, ' ');

  if (token != NULL) {
    strncpy(command, cxx, strlen(cxx)-strlen(token));
    command[strlen(cxx)-strlen(token)] = '\0';
    arguments[0] = command;
    j = 1;
    arguments[1] = token;
  } else {
    arguments[0] = cxx;
    j = 0;
  }
  
  for(i=1;i<argc;++i) arguments[j+i] = argv[i];


  /* ----------------------------------------------- */
  /* Check for special flags                         */
  /* ----------------------------------------------- */
  for(i=1;i<argc;++i) {
    if ( strcmp(argv[i],"-c") == 0 ) dash_c = 1;
    if ( strcmp(argv[i],"-M") == 0 ) dash_c = 1;
    if ( strcmp(argv[i],"-v") == 0 ) verbose = 1;
    if ( strcmp(argv[i],"--verbose") == 0 ) verbose = 1;
  }


  /* ----------------------------------------------- */
  /* Add in include directories....                  */
  /* ----------------------------------------------- */
  arguments[j+argc++] = "-I.";
  arguments[j+argc++] = include_directory;


  /* ----------------------------------------------- */
  /* If the -c switch is not there, add in link flag */
  /* ----------------------------------------------- */
  if ( !dash_c ) {
    arguments[j+argc++] = "-L.";
    arguments[j+argc++] = lib_directory;
    arguments[j+argc++] = lib_library;
  }
  arguments[j+argc] = 0;
  

  /* ----------------------------------------------- */
  /* In verbose mode, echo the command               */
  /* ----------------------------------------------- */
  if ( verbose ) {
    for(i=0;i<j+argc;++i) {
      fprintf(stderr,"%s ",arguments[i]);
    }
    fprintf(stderr,"\n");
  }

  execvp(arguments[0],arguments);
  perror(arguments[0]);
  return 1;
}
