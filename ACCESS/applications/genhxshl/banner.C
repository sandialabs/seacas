// $Id: banner.C,v 1.5 2004/03/19 15:39:45 gdsjaar Exp $
// banner.c

#include <iostream>
#include "banner.h"
#include "Version.h"
#include <string.h>

void banner(char * Input_Mesh , char * Output_Mesh ) 

{
  char version[81];
  strcpy( version , &(VERSION[11]) );
  int len = strlen( version );
  strcpy( &(version[len - 1]) , "\0" );
  std::cout << "\n\n\n";
  std::cout << "      GGGGG   EEEEEEE N    NN HH   HH XX   XX  SSSSSS HH   HH LL\n";
  std::cout << "     GG   GG  EE      NN   NN HH   HH  XX XX  SS      HH   HH LL\n";
  std::cout << "     GG       EE      NNN  NN HH   HH   XXX   SS      HH   HH LL\n";
  std::cout << "     GG       EEEEE   NN N NN HHHHHHH   XXX    SSSSS  HHHHHHH LL\n";
  std::cout << "     GG  GGG  EE      NN  NNN HH   HH   XXX        SS HH   HH LL\n";
  std::cout << "     GG   GG  EE      NN   NN HH   HH  XX XX       SS HH   HH LL\n";
  std::cout << "      GGGGG   EEEEEEE NN    N HH   HH XX   XX SSSSSS  HH   HH LLLLLLL\n";
  std::cout << "\n\n\n";
  std::cout << "          A HEX to HEXSHELL Mesh Conversion Program\n";
  std::cout << "          Version: " << version  << "\n\n";
  std::cout << "          Sponsor:  SEACAS@sandia.gov\n";
  std::cout << "\n\n";
  std::cout << "          Input Mesh:  " << Input_Mesh << "\n";
  std::cout << "          Output Mesh: " << Output_Mesh << "\n";
  std::cout << "\n\n\n";
}
