// $Id: Command.h,v 1.1 1998/05/20 18:28:49 khbrown Exp $
//

#ifndef _COMMAND_H_
#define _COMMAND_H_

#include "Hexshell.h"
#include "Mesh.h"

enum{ HEXSHELL,HSJOINT,THICKNESS,NORMALIZE };

class Command{
public:
  Command( Mesh * );
  ~Command();
  void store( int , int=0 , int=0 , float=0 );
  void process( Mesh * );
private:
  int Number_of_Element_Blocks;
  int Hexshell_Blocks;
  int Hexshell_Joints;
  int Normalize;
  Hexshell * hs;
  int * HS_EB;
  int * HS_SS;
  float * HS_TH;
  int * HS_JT;
};

#endif

