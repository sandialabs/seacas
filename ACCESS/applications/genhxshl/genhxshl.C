// $Id: genhxshl.C,v 1.5 2004/03/19 15:39:45 gdsjaar Exp $
/* This program reads a genesis mesh and allows the user to convert
   hex element blocks to hexshell element blocks.  The program uses
   the ExodusII file format for the input and output files.  */

#include <iostream>
#include <stdlib.h>
#include "banner.h"
#include "Mesh.h"
#include "UserInput.h"
#include "exodusII.h"
#include <assert.h>

int main(int argc , char *argv[] )
{

  ex_opts(EX_ABORT);

// Parse the input options
  if( argc != 3){
    std::cout << "\n\nUsage:  genhxshl Input_Mesh.g Output_Mesh.g\n\n";
    exit(0);
  }
  char * Input_Mesh = argv[1];
  char * Output_Mesh = argv[2];

// Announce the Program 
  banner( Input_Mesh , Output_Mesh );

// Read the initial mesh
  Mesh mesh1( Input_Mesh );

// Get the user commands
  UserInput( & mesh1 );

// Write the new mesh
  mesh1.write_exodus( Output_Mesh );

  return(0);   
}


