// $Id: Hexshell.C,v 1.5 2004/03/19 15:39:45 gdsjaar Exp $
// Hexshell Class //

#include <stdlib.h>
#include <assert.h>
#include <iostream>
#include "Hexshell.h"
#include "Node.h"


Hexshell::Hexshell(void) {

  Number_of_HS_Pairs = Memory_Length = Number_of_Nodes = 0;
  Number_of_New_Nodes = 0;
  Hex_Node1 = Hex_Node2 = HS_Node = NULL;

}

Hexshell::~Hexshell(void) {

  delete [] Hex_Node1;
  delete [] Hex_Node2;
  delete [] HS_Node;

}

Hexshell::Hexshell( int NNOD , int Initial_Memory_Length ) {
  
  Number_of_Nodes = NNOD;
  Number_of_HS_Pairs =0;
  Number_of_New_Nodes = 0;
  Memory_Length = Initial_Memory_Length ;
  
  Hex_Node1 = new int [Memory_Length];
  assert( Hex_Node1 != NULL ); 
  Hex_Node2 = new int [Memory_Length];
  assert( Hex_Node2 != NULL ); 
  HS_Node   = new int [Memory_Length];
  assert( HS_Node != NULL ); 

}

void Hexshell::Increase_Memory( void ){

  int Old_Length = Memory_Length;

  Memory_Length = Memory_Length + 1000;

  int * HXN1 = Hex_Node1;
  int * HXN2 = Hex_Node2;
  int * HSN  = HS_Node;

  int * to1 = HXN1;
  int * to2 = HXN2;
  int * tos = HSN;

  int * t1;
  int * t2;
  int * ts;

  t1 = Hex_Node1 = new int [Memory_Length];
  assert( t1 != NULL );
  t2 = Hex_Node2 = new int [Memory_Length];
  assert( t2 != NULL );
  ts = HS_Node   = new int [Memory_Length];
  assert( ts != NULL );

// Replace this with a memcopy later
  
  for( int i=0 ; i< Old_Length ; i++ ){
    *t1++ = *to1++;
    *t2++ = *to2++;
    *ts++ = *tos++;
  }

  delete [] HXN1;
  delete [] HXN2;
  delete [] HSN;
}

int Hexshell::Add_Node( int Node1 , int Node2 ){

//For convenience, make n1 = min(Node1,Node2) and n2 = max(Node1,Node2)
  int n1 = Node1 < Node2 ? Node1 : Node2;
  int n2 = Node1 < Node2 ? Node2 : Node1;
  

//See if the pair n1,n2 already has been defined
  for( int i=0 ; i<Number_of_HS_Pairs ; i++){
    if( n1 == Hex_Node1[i] ){
      if( n2 == Hex_Node2[i] ){
        return( HS_Node[i] );
      }
    }
  }

//This is a new pair, make sure we have enough memory
  if( Number_of_HS_Pairs == Memory_Length)
    Increase_Memory();

//Add this pair
  Hex_Node1[Number_of_HS_Pairs] = n1;
  Hex_Node2[Number_of_HS_Pairs] = n2;
  HS_Node[Number_of_HS_Pairs]   = ++Number_of_Nodes;
  Number_of_New_Nodes++;
  return( HS_Node[Number_of_HS_Pairs++] );

}




void Hexshell::get_pair( int i , int * nodeh1 , int * nodeh2 , int * nodesh ){

  *nodeh1 = Hex_Node1[ i ];
  *nodeh2 = Hex_Node2[ i ];
  *nodesh = HS_Node[ i ];

}


void Hexshell::get_new_pair( int * nodeh1 , int * nodeh2 , int * nodesh ){

  if( Number_of_New_Nodes == 0 ){
    std::cerr << "No new hexshell nodes need to be created\n";
    exit(1);
  }
  int index = Number_of_HS_Pairs - Number_of_New_Nodes--;
  *nodeh1 = Hex_Node1[ index ];
  *nodeh2 = Hex_Node2[ index ];
  *nodesh = HS_Node[ index ];

}

int Hexshell::get_number( void ) {
 
  return( Number_of_HS_Pairs );

}


int Hexshell::get_number_of_new_nodes( void ) {
  
  return( Number_of_New_Nodes );

}

int Hexshell::check_pair( int Node1 , int Node2 ){

  int n1 = Node1 < Node2 ? Node1 : Node2;
  int n2 = Node1 < Node2 ? Node2 : Node1;

  for( int i=0 ; i<Number_of_HS_Pairs ; i++ ){
    if( Hex_Node1[i] == n1 ){
      if( Hex_Node2[i] == n2 )
// Return Shell Node
        return( HS_Node[i] );
    }
  }
// No match found, return 0
  return(0);
}

void Hexshell::update_ids( Node * nodes ){

  for( int i=0 ; i<Number_of_HS_Pairs ; i++ ){
    HS_Node[i]   = nodes->get_new_id( HS_Node[i] );
    Hex_Node1[i] = nodes->get_new_id( Hex_Node1[i] );
    Hex_Node2[i] = nodes->get_new_id( Hex_Node2[i] );
  }

}
