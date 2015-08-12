// $Id: Combine.h,v 1.3 2004/03/19 15:39:45 gdsjaar Exp $

#ifndef _COMBINE_H_
#define _COMBINE_H_

#include <iostream>
#include <stdlib.h>
#include "Combine_Pairs.h"
#include "Node.h"

enum{ L_Joint,T_Joint,Plus_Joint,Corner_Joint,Four_Corner_Joint };

class Combine{
 private:
  int Number_of_Pairs;
  int NP_Allocated;
  Combine_Pairs ** Pairs;
  int * Node_Pair_Index;
  void reorganize( int , int * );
 public:
  Combine();
  Combine( int Number_of_Nodes,int Size = 1000 );
    ~Combine(void);
  void process_case( int, int * , int * , int *);
  void increase_memory( int Size = 1000 );
  void process( Node * nodes );
  friend std::ostream& operator<<(std::ostream& os, const Combine & combine);
};
#endif
