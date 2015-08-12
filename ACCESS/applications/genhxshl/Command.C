// $Id: Command.C,v 1.4 2004/03/19 15:39:45 gdsjaar Exp $
//

#include <stdlib.h>
#include <iostream>
#include <assert.h>
#include "Command.h"
#include "Mesh.h"
#include "Hexshell.h"

Command::Command( Mesh * mesh){
  Normalize = 0;
  Hexshell_Blocks = 0;
  Hexshell_Joints = 0;
  Number_of_Element_Blocks = mesh->get_Num_EBs();
  HS_EB = new int [ Number_of_Element_Blocks ];
  HS_SS = new int [ Number_of_Element_Blocks ];
  HS_TH = new float [ Number_of_Element_Blocks ];
  HS_JT = new int [ Number_of_Element_Blocks ];
  assert( HS_EB != NULL );
  assert( HS_SS != NULL );
  assert( HS_TH != NULL );
  assert( HS_JT != NULL );
  for( int i=0 ; i<Number_of_Element_Blocks ; i++ )
    HS_TH[i] = -1.0;
  
  hs = new Hexshell( mesh->get_Num_Nodes() );
  assert( hs != NULL );
}

Command::~Command(){

  delete [] HS_EB;
  delete [] HS_SS;
  delete [] HS_TH;
  delete [] HS_JT;
  delete hs;
}

void Command::store( int command , int value1 , int value2 , float value3 ){

  int i,add;

  enum {True,False};

  if( command==HEXSHELL ){
    add = True;
    for( i=0 ; i<Hexshell_Blocks ; i++ ){
      if( HS_EB[i]==value1 )
         add = False;
    }
    if( add==True ){
      HS_EB[Hexshell_Blocks] = value1;
      HS_SS[Hexshell_Blocks++] = value2;
    }
    assert( Hexshell_Blocks <= Number_of_Element_Blocks);
  }
  else if( command==HSJOINT ){
    add = True;
    for( i=0 ; i<Hexshell_Joints ; i++ ){
      if( HS_JT[i]==value1 )
         add = False;
    }
    if( add==True ){
      HS_JT[Hexshell_Joints++] = value1;
    }
    assert( Hexshell_Joints <= Number_of_Element_Blocks);
  }
  else if( command==THICKNESS ){
    for( i=0 ; i<Hexshell_Blocks ; i++ ){
      if( HS_EB[i] == value1 ){
        HS_TH[i] = value3 ;
        return;
      }
    }
    std::cout << "Element Block " << value1 << "is not a Hexshell Block\n\n.";
  }
  else if( command==NORMALIZE ){
    Normalize = 1;
  }
  else{
    std::cerr << "Internal Error in Command Parsing./n";
    exit(1);
  }
} 

void Command::process( Mesh * mesh ){

  int i,elb,ss;
  float thick;

  for( i=0 ; i<Hexshell_Blocks ; i++ ){
    elb = HS_EB[i];
    ss  = HS_SS[i];
#ifdef DEBUG
    std::cout << "Converting Block " << elb << " to Hexshell using Sideset " << ss << "\n";
#endif
    mesh->convert_2_hexshell( elb , ss , hs );
  }

  for( i=0 ; i<Hexshell_Blocks ; i++ ){
    elb   = HS_EB[i];
    thick = HS_TH[i];
    if( thick > 0 )
      mesh->set_thickness( elb , thick );
  }

  mesh->add_hs_nodes_to_nodeset( hs );
  mesh->update_side_sets();

  for( i=0 ; i<Hexshell_Joints ; i++){
    elb = HS_JT[i];
#ifdef DEBUG
    std::cout << "Converting Block " << elb << " to Hexjoint\n";
#endif
    mesh->EB_status( elb , 0 );
    mesh->make_hsjoint( elb , hs );
  }

  if( Hexshell_Joints > 0 ) mesh->process_hsjoint( hs );

  if( Normalize==1 )
     mesh->normalize_hexshell_nodes( hs );

}
