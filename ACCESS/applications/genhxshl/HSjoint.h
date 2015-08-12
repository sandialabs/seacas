// $Id: HSjoint.h,v 1.4 1998/06/29 17:38:33 khbrown Exp $

#ifndef _HSjoint_H_
#define _HSjoint_H_

#include "Hexshell.h"
#include "Node.h"
#include "Combine.h"

class HSjoint{
 public:
  HSjoint();
  HSjoint(int NNOD);
  ~HSjoint(void);
  int process_element( int,int,int,int,int,int,int,int,Hexshell * );
  void process_all( Node * nodes );
 private:
  int edge_status[12];
  int Shell_Nodes[8];
  int Hex_Nodes_Average[8];
  int Hex_Nodes[8];
  Combine * combine;
  void Process_L_Joint( int,int,int,int,int,int,int,int,int);
  void Process_T_Joint( int,int,int,int,int,int,int,int,int);
  void Process_Plus_Joint( int,int,int,int,int,int,int,int);
  void Process_Corner_Joint(int,int,int,int,int,int,int,int,
                            int,int,int,int,int,int,int);
  void Process_4Corner_Joint(int,int,int,int,int,int,int,int,int,int,int,int,
                             int,int,int,int,int,int,int,int);
  int get_case_id( );
};

#endif
