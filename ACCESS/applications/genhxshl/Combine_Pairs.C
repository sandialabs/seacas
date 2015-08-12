// $Id: Combine_Pairs.C,v 1.4 2004/03/19 15:39:45 gdsjaar Exp $

#include <iostream>
#include "Combine.h"
#include "Combine_Pairs.h"

Combine_Pairs::Combine_Pairs(int Method, int * Shell_Nodes , 
                             int * Hex_Nodes_Average , int * Hex_Nodes ){

  METHOD = Method;
  switch (METHOD){
  case L_Joint:
    Number_SNodes_to_Combine = 2;
    Number_to_Average_for_Shells = 4;
    Number_HNodes_to_Combine = 3;
    break;
  case T_Joint:
    Number_SNodes_to_Combine = 3;
    Number_to_Average_for_Shells = 4;
    Number_HNodes_to_Combine = 2;
    break;
  case Plus_Joint:
    Number_SNodes_to_Combine = 4;
    Number_to_Average_for_Shells = 4;
    Number_HNodes_to_Combine = 0;
    break;
  case Corner_Joint:
    Number_SNodes_to_Combine = 3;
    Number_to_Average_for_Shells = 8;
    Number_HNodes_to_Combine = 4;
    break;
  case Four_Corner_Joint:
    Number_SNodes_to_Combine = 8;
    Number_to_Average_for_Shells = 8;
    Number_HNodes_to_Combine = 4;
    break;
  default:
    std::cout << "Unsupported Case in Combine_Pairs\n";
    exit(1);
  }
  fill_arrays( Shell_Nodes,Hex_Nodes_Average,Hex_Nodes );
}




Combine_Pairs::~Combine_Pairs(){}




void Combine_Pairs::fill_arrays( int * Shell_Nodes, int * Hex_Nodes_Average , 
                                 int * Hex_Nodes ){

  int i;

  for( i=0;i<Number_SNodes_to_Combine;i++ )
    SNodes_to_Combine[i] = Shell_Nodes[i];
  for( i=0;i<Number_to_Average_for_Shells;i++)
    Nodes_to_Average_for_Shells[i] = Hex_Nodes_Average[i];
  for( i=0;i<Number_HNodes_to_Combine;i++)
    HNodes_to_Combine[i] = Hex_Nodes[i];

}


void Combine_Pairs::process( Node * nodes ){

  switch( METHOD ){
  case L_Joint:
    nodes->merge_nodes_average( Number_SNodes_to_Combine,SNodes_to_Combine,
               Number_to_Average_for_Shells,Nodes_to_Average_for_Shells);
    nodes->merge_nodes_delete( HNodes_to_Combine[0],HNodes_to_Combine[1],
                               HNodes_to_Combine[2] );
    break;
  case T_Joint:
    nodes->merge_nodes_average( Number_SNodes_to_Combine,SNodes_to_Combine,
               Number_to_Average_for_Shells,Nodes_to_Average_for_Shells);
    nodes->merge_nodes_average( Number_HNodes_to_Combine,HNodes_to_Combine,
                                Number_HNodes_to_Combine,HNodes_to_Combine);
    break;
  case Plus_Joint:
    nodes->merge_nodes_average( Number_SNodes_to_Combine,SNodes_to_Combine,
               Number_to_Average_for_Shells,Nodes_to_Average_for_Shells);
    break;
  case Corner_Joint:
    nodes->merge_nodes_average( Number_SNodes_to_Combine,SNodes_to_Combine,
               Number_to_Average_for_Shells,Nodes_to_Average_for_Shells);
    nodes->merge_nodes_set(Number_HNodes_to_Combine,HNodes_to_Combine);
    break;
  case Four_Corner_Joint:
    nodes->merge_nodes_average( Number_SNodes_to_Combine,SNodes_to_Combine,
               Number_to_Average_for_Shells,Nodes_to_Average_for_Shells);
    nodes->merge_nodes_average( Number_HNodes_to_Combine,HNodes_to_Combine,
               Number_HNodes_to_Combine,HNodes_to_Combine);
    break;
  default:
    std::cout << "Unsupported Case in Combine_Pairs\n";
    exit(1);
  }

}


std::ostream& operator<<(std::ostream &os,const Combine_Pairs & pairs ){

  int i;

  os << "Shell Nodes to Combine: ";
  for( i=0;i<pairs.Number_SNodes_to_Combine;i++)
    os << pairs.SNodes_to_Combine[i] << " ";
  os << "\n";
 
  os << "Node to average for shell position: ";
  for( i=0;i<pairs.Number_to_Average_for_Shells;i++ )
    os <<  pairs.Nodes_to_Average_for_Shells[i] << " ";
  os << "\n";

  os << "Hex Nodes to Combine: ";
  for( i=0;i<pairs.Number_HNodes_to_Combine;i++ )
    os << pairs.HNodes_to_Combine[i] << " ";
  os << "\n\n";

  return(os);

}
