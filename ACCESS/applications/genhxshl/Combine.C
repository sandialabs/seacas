// $Id: Combine.C,v 1.4 2004/03/19 15:39:45 gdsjaar Exp $

#include <iostream>
#include <stdlib.h>
#include "Combine.h"
#include "Combine_Pairs.h"
#include "Node.h"

Combine::Combine(){}

Combine::Combine(int Number_of_Nodes,int Size ){

  Number_of_Pairs = 0;
  NP_Allocated = 1000;
  Pairs = new Combine_Pairs * [Size];
  Node_Pair_Index = new int[2*Number_of_Nodes];
  for( int i=0;i<2*Number_of_Nodes;i++ )
    Node_Pair_Index[i] = 0;
}

Combine::~Combine(){
  for( int i=0;i<Number_of_Pairs;i++)
    delete Pairs[i];
  delete [] Pairs;
  delete [] Node_Pair_Index;
}

void Combine::process_case( int Method, int * Shell_Nodes , 
                            int * Hex_Nodes_Average ,int * Hex_Nodes  ){

  int i,add,all;

  switch(Method){
  case L_Joint:
    // Determine if any of the shell nodes have already been merged
    add = 1;
    for( i=0;i<2;i++ )
      if( Node_Pair_Index[Shell_Nodes[i]-1] != 0 ) add = 0; 
    if( add == 1 ){
      // These shell node haven't been stored so store them
      if( Number_of_Pairs == NP_Allocated-1) increase_memory();
      Pairs[Number_of_Pairs] = new Combine_Pairs( Method , Shell_Nodes , 
                                 Hex_Nodes_Average , Hex_Nodes );
      // Note that we store pair+1 for the entry in Node_Pair_Index
      Number_of_Pairs++;
      for( i=0;i<2;i++ )
        Node_Pair_Index[Shell_Nodes[i]-1] = Number_of_Pairs; // -1 for exodus
    }
    else{
    all = 1;
    for( i=0;i<2;i++ )
      if( Node_Pair_Index[Shell_Nodes[i]-1] == 0 ) all = 0; 
    if( all == 0 )
      std::cout << "Warning L_Joint:  Not all nodes have been merged\n";
    }
    break;
  case T_Joint:
    // Determine if any of the shell nodes have already been merged
    add = 1;
    for( i=0;i<3;i++ )
      if( Node_Pair_Index[Shell_Nodes[i]-1] != 0 ) add = 0; 
    if( add == 1 ){
      // These shell node haven't been stored so store them
      if( Number_of_Pairs == NP_Allocated-1) increase_memory();
      Pairs[Number_of_Pairs] = new Combine_Pairs( Method , Shell_Nodes , 
                                 Hex_Nodes_Average , Hex_Nodes );
      // Note that we store pair+1 for the entry in Node_Pair_Index
      Number_of_Pairs++;
      for( i=0;i<3;i++ )
        Node_Pair_Index[Shell_Nodes[i]-1] = Number_of_Pairs; // -1 for exodus
    }
    else{
    all = 1;
    for( i=0;i<2;i++ )
      if( Node_Pair_Index[Shell_Nodes[i]-1] == 0 ) all = 0; 
    if( all == 0 )
      std::cout << "Warning T_Joint:  Not all nodes have been merged\n";
    }
    break;  
  case Plus_Joint:
    // Determine if any of the shell nodes have already been merged
    add = 1;
    for( i=0;i<4;i++ )
      if( Node_Pair_Index[Shell_Nodes[i]-1] != 0 ) add = 0; 
    if( add == 1 ){
      // These shell node haven't been stored so store them
      if( Number_of_Pairs == NP_Allocated-1) increase_memory();
      Pairs[Number_of_Pairs] = new Combine_Pairs( Method , Shell_Nodes , 
                                 Hex_Nodes_Average , Hex_Nodes );
      // Note that we store pair+1 for the entry in Node_Pair_Index
      Number_of_Pairs++;
      for( i=0;i<4;i++ )
        Node_Pair_Index[Shell_Nodes[i]-1] = Number_of_Pairs; // -1 for exodus
    }
    else{
      all = 1;
      for( i=0;i<2;i++ )
        if( Node_Pair_Index[Shell_Nodes[i]-1] == 0 ) all = 0; 
      if( all == 0 )
        std::cout << "Warning +_Joint:  Not all nodes have been merged\n";
    }
    break;   
  case Corner_Joint:
    // Determine if any of the shell nodes have already been merged
    add = 1;
    for( i=0;i<3;i++ )
      if( Node_Pair_Index[Shell_Nodes[i]-1] != 0 ) add = 0; 
    if( add == 1 ){
      // These shell node haven't been stored so store them
      if( Number_of_Pairs == NP_Allocated-1) increase_memory();
      Pairs[Number_of_Pairs] = new Combine_Pairs( Method , Shell_Nodes , 
                                 Hex_Nodes_Average , Hex_Nodes );
      // Note that we store pair+1 for the entry in Node_Pair_Index
      Number_of_Pairs++;
      for( i=0;i<3;i++ )
        Node_Pair_Index[Shell_Nodes[i]-1] = Number_of_Pairs; // -1 for exodus
    }
    else{
#ifdef DEBUG
      std::cout << "Corner_Joint of previous Pair\n";
#endif
      //Overwrite any stored pair with this information
      int NPI = 0;
      for( i=0;i<3;i++ ){
        if( Node_Pair_Index[Shell_Nodes[i]-1] != 0 ) {
          if( NPI == 0 ){
            NPI = Node_Pair_Index[Shell_Nodes[i]-1];
          }
          else{
            if( NPI != Node_Pair_Index[Shell_Nodes[i]-1] )
              std::cerr << "Error: Corner_Joint Error!!!\n:";
          }
	}
      }
#ifdef DEBUG
      std::cout << "Node Pair being Overwritten = " << NPI-1 << "\n";
#endif
      Pairs[NPI-1] = new Combine_Pairs( Method , Shell_Nodes , 
                                 Hex_Nodes_Average , Hex_Nodes );
    }
    break;
  case Four_Corner_Joint:
    // Determine if any of the shell nodes have already been merged
    add = 1;
    for( i=0;i<8;i++ )
      if( Node_Pair_Index[Shell_Nodes[i]-1] != 0 ) add = 0; 
    if( add == 1 ){
      // These shell node haven't been stored so store them
      if( Number_of_Pairs == NP_Allocated-1) increase_memory();
      Pairs[Number_of_Pairs] = new Combine_Pairs( Method , Shell_Nodes , 
                                 Hex_Nodes_Average , Hex_Nodes );
      // Note that we store pair+1 for the entry in Node_Pair_Index
      Number_of_Pairs++;
      for( i=0;i<8;i++ )
        Node_Pair_Index[Shell_Nodes[i]-1] = Number_of_Pairs; // -1 for exodus
    }
    else{
      //Eliminate pairs that have one of these shell nodes and reorganize
      reorganize( 8,Shell_Nodes );
      Pairs[Number_of_Pairs] = new Combine_Pairs( Method , Shell_Nodes , 
                                 Hex_Nodes_Average , Hex_Nodes );
      // Note that we store pair+1 for the entry in Node_Pair_Index
      Number_of_Pairs++;
      for( i=0;i<8;i++ )
        Node_Pair_Index[Shell_Nodes[i]-1] = Number_of_Pairs; // -1 for exodus
    }
    break;  
  default:
    std::cout << "Unsupported Method in Combine\n";
    exit(1);
  }

}



void Combine::reorganize( int Number_of_Shell_Nodes , int * Shell_Nodes ){

  int i,j,Accounted_For;
  int * Pair = new int [Number_of_Shell_Nodes];
  
  for( i=0;i<Number_of_Shell_Nodes;i++ )
    Pair[i] = Node_Pair_Index[Shell_Nodes[i]-1] ;

  for( i=0;i<Number_of_Shell_Nodes;i++ ){
    if( Pair[i] > 0 ){
      Accounted_For = 0;
      for( j=i;j>=0;j-- ){
        if( Pair[i] == Pair[j] ) {
          Accounted_For = 1;
	  break;
	}
      }
      if( Accounted_For == 0 ){
	//Delete this pair and compact pointer array 
        delete Pairs[Node_Pair_Index[Pair[i] - 1]];
        Pairs[Pair[i]-1] = Pairs[--Number_of_Pairs];
      }
    }
  }
}

void Combine::increase_memory( int Size ){

  int Old_Size = NP_Allocated;
  Combine_Pairs ** Old_Pairs = Pairs;

  NP_Allocated = Old_Size + Size; 
  Pairs = new Combine_Pairs * [NP_Allocated];

  for( int i=0;i<Old_Size;i++)
    Pairs[i] = Old_Pairs[i];

  delete [] Old_Pairs;

}



void Combine::process( Node * nodes ){
 
  for( int i=0;i<Number_of_Pairs;i++ )
    Pairs[i]->process( nodes );
}  


std::ostream& operator<<( std::ostream& os, const Combine& combine){

  for( int i=0;i<combine.Number_of_Pairs;i++ )
    os << *combine.Pairs[i];

  return(os);
}
