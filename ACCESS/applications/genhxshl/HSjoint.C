// $Id: HSjoint.C,v 1.5 2004/03/19 15:39:45 gdsjaar Exp $
// HSjoint Class //

#include <assert.h>
#include <iostream>
#include <stdlib.h>
#include "HSjoint.h"
#include "Hexshell.h"
#include "Node.h"
#include "Combine.h"


HSjoint::HSjoint(){ combine = NULL; }


HSjoint::HSjoint( int NNOD ){ 

  combine = new Combine( NNOD ); 

}

HSjoint::~HSjoint(){ delete combine;}

int HSjoint::process_element( int N1, int N2, int N3, int N4, int N5, int N6,
                              int N7, int N8 , Hexshell * hs  ){

  edge_status[ 0] = hs->check_pair( N1,N2 );
  edge_status[ 1] = hs->check_pair( N2,N3 );
  edge_status[ 2] = hs->check_pair( N3,N4 );
  edge_status[ 3] = hs->check_pair( N4,N1 );
  edge_status[ 4] = hs->check_pair( N5,N6 );
  edge_status[ 5] = hs->check_pair( N6,N7 );
  edge_status[ 6] = hs->check_pair( N7,N8 );
  edge_status[ 7] = hs->check_pair( N8,N5 );
  edge_status[ 8] = hs->check_pair( N1,N5 );
  edge_status[ 9] = hs->check_pair( N2,N6 );
  edge_status[10] = hs->check_pair( N3,N7 );
  edge_status[11] = hs->check_pair( N4,N8 );

// Figure out which case we have by determining an unique case ID
  int case_id = get_case_id( );

  switch( case_id ){
  case 2384:    // "L" case 1
    Process_L_Joint( edge_status[4] ,edge_status[8] ,N1,N2,N5,N6,N2,N1,N6 );
    Process_L_Joint( edge_status[6] ,edge_status[11],N3,N4,N7,N8,N3,N4,N7 );
    break;
  case 2309:     // "L" case 2
    Process_L_Joint( edge_status[0] ,edge_status[8] ,N1,N2,N5,N6,N6,N2,N5 );
    Process_L_Joint( edge_status[2] ,edge_status[11],N3,N4,N7,N8,N7,N3,N8 );
    break;
  case 1541:     // "L" case 3
    Process_L_Joint( edge_status[0] ,edge_status[9] ,N1,N2,N5,N6,N5,N1,N6 );
    Process_L_Joint( edge_status[2] ,edge_status[10],N3,N4,N7,N8,N8,N4,N7 );
    break;
  case 1616:    // "L" case 4
    Process_L_Joint( edge_status[4] ,edge_status[9] ,N1,N2,N5,N6,N1,N2,N5 );
    Process_L_Joint( edge_status[6] ,edge_status[10],N3,N4,N7,N8,N4,N3,N8 );
    break;
  case 928:    // "L" case 5
    Process_L_Joint( edge_status[5] ,edge_status[9] ,N2,N3,N6,N7,N3,N2,N7 );
    Process_L_Joint( edge_status[7] ,edge_status[8] ,N1,N4,N5,N8,N4,N1,N8 );
    break;
  case 3232:  // "L" case 6
    Process_L_Joint( edge_status[5] ,edge_status[10],N2,N3,N6,N7,N2,N3,N6 );
    Process_L_Joint( edge_status[7] ,edge_status[11],N1,N4,N5,N8,N1,N4,N5 );
    break;
  case 778:    // "L" case 7
    Process_L_Joint( edge_status[1] ,edge_status[9] ,N2,N3,N6,N7,N7,N3,N6 );
    Process_L_Joint( edge_status[3] ,edge_status[8] ,N1,N4,N5,N8,N8,N4,N5 );
    break;
  case 3082:   // "L" case 8
    Process_L_Joint( edge_status[1] ,edge_status[10],N2,N3,N6,N7,N6,N2,N7 );
    Process_L_Joint( edge_status[3] ,edge_status[11],N1,N4,N5,N8,N5,N1,N8 );
    break;
  case 51:     // "L" case 9
    Process_L_Joint( edge_status[0] ,edge_status[1] ,N1,N2,N3,N4,N4,N1,N3 );
    Process_L_Joint( edge_status[4] ,edge_status[5] ,N5,N6,N7,N8,N8,N5,N7 );
    break;
  case 153:    // "L" case 10
    Process_L_Joint( edge_status[0] ,edge_status[3] ,N1,N2,N3,N4,N3,N2,N4 );
    Process_L_Joint( edge_status[4] ,edge_status[7] ,N5,N6,N7,N8,N7,N6,N8 );
    break;
  case 102:    // "L" case 11
    Process_L_Joint( edge_status[1] ,edge_status[2] ,N1,N2,N3,N4,N1,N2,N4 );
    Process_L_Joint( edge_status[5] ,edge_status[6] ,N5,N6,N7,N8,N5,N6,N8 );
    break;
  case 204:     // "L" case 12
    Process_L_Joint( edge_status[2] ,edge_status[3] ,N1,N2,N3,N4,N2,N1,N3 );
    Process_L_Joint( edge_status[6] ,edge_status[7] ,N5,N6,N7,N8,N6,N5,N7 );
    break;
  case 3920:  // "T" case 1
    Process_T_Joint( edge_status[4],edge_status[ 8],edge_status[ 9],
                     N1,N2,N5,N6,N1,N2);
    Process_T_Joint( edge_status[6],edge_status[10],edge_status[11],
                     N3,N4,N7,N8,N3,N4);
    break;
  case 2389:   // "T" case 2
    Process_T_Joint( edge_status[0],edge_status[ 4],edge_status[ 8],
                     N1,N2,N5,N6,N2,N6);
    Process_T_Joint( edge_status[6],edge_status[10],edge_status[11],
                     N3,N4,N7,N8,N3,N7);
    break;
  case 3845:   // "T" case 3
    Process_T_Joint( edge_status[0],edge_status[ 8],edge_status[ 9],
                     N1,N2,N5,N6,N1,N2);
    Process_T_Joint( edge_status[2],edge_status[10],edge_status[11],
                     N3,N4,N7,N8,N3,N4);
  case 1621:   // "T" case 4
    Process_T_Joint( edge_status[0],edge_status[ 4],edge_status[ 9],
                     N1,N2,N5,N6,N1,N5);
    Process_T_Joint( edge_status[2],edge_status[ 6],edge_status[10],
                     N3,N4,N7,N8,N4,N8);
    break;
  case 938:   // "T" case 5
    Process_T_Joint( edge_status[1],edge_status[ 5],edge_status[ 9],
                     N2,N3,N6,N7,N3,N7);
    Process_T_Joint( edge_status[3],edge_status[ 7],edge_status[ 8],
                     N1,N4,N5,N8,N4,N8);
    break;
  case 3850:   // "T" case 6
    Process_T_Joint( edge_status[1],edge_status[ 9],edge_status[10],
                     N2,N3,N6,N7,N6,N7);
    Process_T_Joint( edge_status[3],edge_status[ 8],edge_status[11],
                     N1,N4,N5,N8,N5,N8);
    break;
  case 3242:   // "T" case 7
    Process_T_Joint( edge_status[1],edge_status[ 5],edge_status[10],
                     N2,N3,N6,N7,N1,N5);
    Process_T_Joint( edge_status[3],edge_status[ 7],edge_status[11],
                     N1,N4,N5,N8,N2,N6);
    break;
  case 4000:  // "T" case 8
    Process_T_Joint( edge_status[5],edge_status[ 9],edge_status[10],
                     N2,N3,N6,N7,N2,N3);
    Process_T_Joint( edge_status[7],edge_status[ 8],edge_status[11],
                     N1,N4,N5,N8,N1,N4);
    break;
  case 119:    // "T" case 9
    Process_T_Joint( edge_status[0],edge_status[ 1],edge_status[ 2],
                     N1,N2,N3,N4,N1,N4);
    Process_T_Joint( edge_status[4],edge_status[ 5],edge_status[ 6],
                     N5,N6,N7,N8,N5,N8);
    break;
  case 187:    // "T" case 10
    Process_T_Joint( edge_status[0],edge_status[ 1],edge_status[ 3],
                     N1,N2,N3,N4,N3,N4);
    Process_T_Joint( edge_status[4],edge_status[ 5],edge_status[ 7],
                     N5,N6,N7,N8,N7,N8);
    break;
  case 221:    // "T" case 11
    Process_T_Joint( edge_status[0],edge_status[ 2],edge_status[ 3],
                     N1,N2,N3,N4,N2,N3);
    Process_T_Joint( edge_status[4],edge_status[ 6],edge_status[ 7],
                     N5,N6,N7,N8,N6,N7);
    break;
  case 238:    // "T" case 12
    Process_T_Joint( edge_status[1],edge_status[ 2],edge_status[ 3],
                     N1,N2,N3,N4,N1,N2);
    Process_T_Joint( edge_status[5],edge_status[ 6],edge_status[ 7],
                     N5,N6,N7,N8,N5,N6);
    break;
  case 3925: // "+" case 1
    Process_Plus_Joint( edge_status[0],edge_status[4],edge_status[8],
                        edge_status[9],N1,N2,N5,N6);
    Process_Plus_Joint( edge_status[2],edge_status[6],edge_status[10],
                        edge_status[11],N3,N4,N7,N8);
    break;
  case 4010: // "+" case 2
    Process_Plus_Joint( edge_status[3],edge_status[7],edge_status[8],
                        edge_status[11],N1,N4,N5,N8);
    Process_Plus_Joint( edge_status[1],edge_status[5],edge_status[9],
                        edge_status[10],N2,N3,N6,N7);
    break;
  case 255:   // "+" case 3
    Process_Plus_Joint( edge_status[4],edge_status[5],edge_status[6],
                        edge_status[7],N5,N6,N7,N8);
    Process_Plus_Joint( edge_status[0],edge_status[1],edge_status[2],
                        edge_status[3],N1,N2,N3,N4);
    break;
  case 515:     // "Corner" case 1
    Process_Corner_Joint( edge_status[0],edge_status[1],edge_status[9],
                          N1,N2,N3,N4,N5,N6,N7,N8,N8,N1,N3,N6);
    break;
  case 265:     // "Corner" case 2
    Process_Corner_Joint( edge_status[0],edge_status[3],edge_status[8],
                          N1,N2,N3,N4,N5,N6,N7,N8,N7,N2,N4,N5);
    break;
  case 2060:    // "Corner" case 3
    Process_Corner_Joint( edge_status[2],edge_status[3],edge_status[11],
                          N1,N2,N3,N4,N5,N6,N7,N8,N6,N1,N3,N8);
    break;
  case 1030:    //  "Corner" case 4
    Process_Corner_Joint( edge_status[1],edge_status[2],edge_status[10],
                          N1,N2,N3,N4,N5,N6,N7,N8,N5,N2,N4,N7);
    break;
  case 560:    // "Corner" case 5
    Process_Corner_Joint( edge_status[4],edge_status[5],edge_status[9],
                          N1,N2,N3,N4,N5,N6,N7,N8,N4,N2,N5,N7);
    break;
  case 1120:    // "Corner" case 6
    Process_Corner_Joint( edge_status[5],edge_status[6],edge_status[10],
                          N1,N2,N3,N4,N5,N6,N7,N8,N1,N3,N6,N8);
    break;
  case 2240:    // "Corner" case 7
    Process_Corner_Joint( edge_status[6],edge_status[7],edge_status[11],
                          N1,N2,N3,N4,N5,N6,N7,N8,N2,N4,N5,N7);
    break;
  case 400:    // "Corner" case 8
    Process_Corner_Joint( edge_status[4],edge_status[7],edge_status[8],
                          N1,N2,N3,N4,N5,N6,N7,N8,N3,N1,N6,N8);
    break;
  case 3855:   // "4Corner" case 1
    Process_4Corner_Joint( edge_status[0],edge_status[1],edge_status[2],
                           edge_status[3],edge_status[8],edge_status[9],
                           edge_status[10],edge_status[11],
                           N1,N2,N3,N4,N5,N6,N7,N8,N5,N6,N7,N8 );
    break;
  case 4080:   // "4Corner" case 2
    Process_4Corner_Joint( edge_status[4],edge_status[5],edge_status[6],
                           edge_status[7],edge_status[8],edge_status[9],
                           edge_status[10],edge_status[11],
                           N1,N2,N3,N4,N5,N6,N7,N8,N1,N2,N3,N4 );
    break;
  case 1655:   // "4Corner" case 3
    Process_4Corner_Joint( edge_status[0],edge_status[1],edge_status[2],
                           edge_status[4],edge_status[5],edge_status[6],
                           edge_status[9],edge_status[10],
                           N1,N2,N3,N4,N5,N6,N7,N8,N1,N4,N5,N8 );
    break;
  case 2525:   // "4Corner" case 4
    Process_4Corner_Joint( edge_status[0],edge_status[2],edge_status[3],
                           edge_status[4],edge_status[6],edge_status[7],
                           edge_status[8],edge_status[11],
                           N1,N2,N3,N4,N5,N6,N7,N8,N2,N3,N6,N7 );
    break;
  case 3310:   // "4Corner" case 5
    Process_4Corner_Joint( edge_status[1],edge_status[2],edge_status[3],
                           edge_status[5],edge_status[6],edge_status[7],
                           edge_status[10],edge_status[11],
                           N1,N2,N3,N4,N5,N6,N7,N8,N1,N2,N5,N6 );
    break;
  case 955:    // "4Corner" case 6
    Process_4Corner_Joint( edge_status[0],edge_status[1],edge_status[3],
                           edge_status[4],edge_status[5],edge_status[7],
                           edge_status[8],edge_status[9],
                           N1,N2,N3,N4,N5,N6,N7,N8,N1,N2,N5,N6 );
    break;
  default: // Error //
    std::cout << "Unsupported case in HSjoint\n";
    exit(1);
  }

  return(0);

}




void HSjoint::Process_L_Joint( int es1 , int es2 , int NA1 , int NA2 , int NA3 ,
                               int NA4 , int HN1 , int HN2 , int HN3 ){

    Shell_Nodes[0] = es1;
    Shell_Nodes[1] = es2;
    Hex_Nodes_Average[0] = NA1;
    Hex_Nodes_Average[1] = NA2;
    Hex_Nodes_Average[2] = NA3;
    Hex_Nodes_Average[3] = NA4;
    Hex_Nodes[0] = HN1;
    Hex_Nodes[1] = HN2;
    Hex_Nodes[2] = HN3;
    combine->process_case( L_Joint,Shell_Nodes,Hex_Nodes_Average,Hex_Nodes );

}



void HSjoint::Process_T_Joint( int es1 , int es2 , int es3 , int NA1 , int NA2 ,
                               int NA3 , int NA4 , int HN1 , int HN2 ){

    Shell_Nodes[0] = es1;
    Shell_Nodes[1] = es2;
    Shell_Nodes[2] = es3;
    Hex_Nodes_Average[0] = NA1;
    Hex_Nodes_Average[1] = NA2;
    Hex_Nodes_Average[2] = NA3;
    Hex_Nodes_Average[3] = NA4;
    Hex_Nodes[0] = HN1;
    Hex_Nodes[1] = HN2;
    combine->process_case( T_Joint,Shell_Nodes,Hex_Nodes_Average,Hex_Nodes );

}




void HSjoint::Process_Plus_Joint( int es1 , int es2 , int es3 , int es4 , 
                                  int NA1 , int NA2 , int NA3 , int NA4 ){

    Shell_Nodes[0] = es1;
    Shell_Nodes[1] = es2;
    Shell_Nodes[2] = es3;
    Shell_Nodes[3] = es4;
    Hex_Nodes_Average[0] = NA1;
    Hex_Nodes_Average[1] = NA2;
    Hex_Nodes_Average[2] = NA3;
    Hex_Nodes_Average[3] = NA4;
    combine->process_case( Plus_Joint,Shell_Nodes,Hex_Nodes_Average,Hex_Nodes );

}




void HSjoint::Process_Corner_Joint( int es1 , int es2 , int es3 , 
                                    int NA1 , int NA2 , int NA3 , int NA4 ,
                                    int NA5 , int NA6 , int NA7 , int NA8 ,
                                    int NH1 , int NH2 , int NH3 , int NH4 ){
 

    Shell_Nodes[0] = es1;
    Shell_Nodes[1] = es2;
    Shell_Nodes[2] = es3;
    Hex_Nodes_Average[0] = NA1;
    Hex_Nodes_Average[1] = NA2;
    Hex_Nodes_Average[2] = NA3;
    Hex_Nodes_Average[3] = NA4;
    Hex_Nodes_Average[4] = NA5;
    Hex_Nodes_Average[5] = NA6;
    Hex_Nodes_Average[6] = NA7;
    Hex_Nodes_Average[7] = NA8;
    Hex_Nodes[0] = NH1;
    Hex_Nodes[1] = NH2;
    Hex_Nodes[2] = NH3;
    Hex_Nodes[3] = NH4;
    combine->process_case( Corner_Joint,Shell_Nodes,Hex_Nodes_Average,Hex_Nodes );

}


void HSjoint::Process_4Corner_Joint( int es1 , int es2 , int es3 , int es4 ,
                                     int es5 , int es6 , int es7 , int es8 ,
                                     int NA1 , int NA2 , int NA3 , int NA4 ,
                                     int NA5 , int NA6 , int NA7 , int NA8 ,
                                     int NH1 , int NH2 , int NH3 , int NH4 ){
 

    Shell_Nodes[0] = es1;
    Shell_Nodes[1] = es2;
    Shell_Nodes[2] = es3;
    Shell_Nodes[3] = es4;
    Shell_Nodes[4] = es5;
    Shell_Nodes[5] = es6;
    Shell_Nodes[6] = es7;
    Shell_Nodes[7] = es8;
    Hex_Nodes_Average[0] = NA1;
    Hex_Nodes_Average[1] = NA2;
    Hex_Nodes_Average[2] = NA3;
    Hex_Nodes_Average[3] = NA4;
    Hex_Nodes_Average[4] = NA5;
    Hex_Nodes_Average[5] = NA6;
    Hex_Nodes_Average[6] = NA7;
    Hex_Nodes_Average[7] = NA8;
    Hex_Nodes[0] = NH1;
    Hex_Nodes[1] = NH2;
    Hex_Nodes[2] = NH3;
    Hex_Nodes[3] = NH4;
    combine->process_case( Four_Corner_Joint,Shell_Nodes,Hex_Nodes_Average,
                           Hex_Nodes );

}


void HSjoint::process_all( Node * nodes ){
  
#ifdef DEBUG
   std::cout << *combine;
#endif
   combine->process( nodes );

}


int HSjoint::get_case_id( ){

  /* This member function returns a unique case ID for all possible
     combinations of Edges having a hexshell node on them.  To limit
     the size of the return value,  the edge status array is treated
     as a base 2 number and converted to a base 10 number */

  static int Powers_of_Two[12] = { 1,2,4,8,16,32,64,128,256,512,1024,2048 };

  int case_id = 0;

  for( int i=0;i<12;i++ ){
    if( edge_status[i] != 0 )
      case_id += Powers_of_Two[i];
  }
  return(case_id);

}
