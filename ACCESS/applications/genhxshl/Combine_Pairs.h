// $Id: Combine_Pairs.h,v 1.2 2004/03/19 15:39:45 gdsjaar Exp $

#ifndef _COMBINE_PAIRS_H_
#define _COMBINE_PAIRS_H_

#include <iostream>
#include "Node.h"

class Combine_Pairs{
 private:
  int METHOD;
  int Number_SNodes_to_Combine;
  int SNodes_to_Combine[8];
  int Number_to_Average_for_Shells;
  int Nodes_to_Average_for_Shells[8];
  int Number_HNodes_to_Combine;
  int HNodes_to_Combine[8];
  int HNodes_Position;
 public:
  Combine_Pairs(int Method , int * Shell_Nodes , int * Hex_Nodes_Average , 
          int * Hex_Nodes );
  ~Combine_Pairs(void);
  void fill_arrays( int * Shell_Nodes, int * Hex_Nodes_Average , 
                                 int * Hex_Nodes );
  void process(Node * nodes );
  friend std::ostream& operator<<(std::ostream& os, const Combine_Pairs & combine_pairs);
};
#endif
