// $Id: Element_Block.C,v 1.12 2004/03/19 15:39:45 gdsjaar Exp $
// Element Block Class //

#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <string.h>
#include "Element_Block.h"
#include "exodusII.h"
#include "Sideset.h"
#include "Hexshell.h"
#include "HSjoint.h"

Element_Block::Element_Block() {
  Status = 1;
  material_id = 0;
  element_connectivity = NULL;
  new_face_id = NULL;
  element_attributes = NULL;
  strcpy( element_name , "Null_Block");
  num_elements = nodes_per_element = attributes_per_element = faces_per_element = 0;
}



Element_Block::Element_Block(int EXOid,int mat_id) {
  
   int ierr = 0;

   material_id = mat_id;
   Status = 1;

// Get the element block parameters
      ierr = ex_get_elem_block( EXOid,material_id,element_name,&num_elements,
                  &nodes_per_element,&attributes_per_element);
   if( ierr > 0 ) {
     std::cerr << "Error Reading Element Block Parameters \n";
     exit(0);
   } 
// Set the number of faces per element
   if( strcmp( element_name,"HEX" ) )
     faces_per_element = 6;
   else if( strcmp( element_name,"HEX8" ) )
     faces_per_element = 6;
   else if( strcmp( element_name,"HEXSHELL" ) )
     faces_per_element = 6;
   else if( strcmp( element_name,"SHELL" ) )
     faces_per_element = 2;
   else if( strcmp( element_name,"SPHERE" ) )
     faces_per_element = 0;
// Allocate memory for this element block
   element_connectivity = new int [ num_elements*nodes_per_element ];
   assert( element_connectivity != NULL );
   element_attributes = new float [ num_elements*attributes_per_element ];
   assert( element_attributes != NULL );
   if( faces_per_element != 0 ) {
     new_face_id = new int [ num_elements*faces_per_element ];  
     assert( new_face_id != NULL );
   }
   else {
     new_face_id = NULL;
   }

// Read the link array for this element block
   ierr = ex_get_elem_conn( EXOid, material_id, element_connectivity );
   if( ierr > 0 ) {
     std::cerr << "Error Reading Link Array\n";
     exit(0);
   }
// Read the attributes
   if( attributes_per_element > 0 ) {
     ierr = ex_get_elem_attr( EXOid, material_id, element_attributes);
     if( ierr > 0 ) {
       std::cerr << "Error Reading Element Attributes \n'";
       exit(0);
     }
   }
// Set up new_face_id
   for( int i=0 ; i<num_elements ; i++){
     for( int j=0 ; j<faces_per_element ; j++){
       int index = i*faces_per_element + j;
       new_face_id[index] = j + 1;
     }
   }
}





void Element_Block::write_exodus( int EXOid ){

  int ierr;

// Write element block parameters
  ierr = ex_put_elem_block( EXOid , material_id , element_name ,
                            num_elements , nodes_per_element ,
                            attributes_per_element );
  if( ierr > 0 ) {
    std::cerr << "**ERROR** EX_PUT_ELEM_BLOCK";
    exit(1);
  }

// Write element connectivity
  ierr = ex_put_elem_conn( EXOid , material_id , element_connectivity );
  if( ierr > 0 ) {
    std::cerr << "**ERROR** EX_PUT_ELEM_CONN";
    exit(1);
  }

// Write element connectivity
  if( attributes_per_element > 0 ) {
    ierr = ex_put_elem_attr( EXOid , material_id , element_attributes );
    if( ierr > 0 ) {
      std::cerr << "**ERROR** EX_PUT_ELEM_ATTR";
      exit(1);
    }
  }
}




Element_Block::~Element_Block() {
  delete [] element_connectivity;
  delete [] element_attributes;
  delete [] new_face_id;
}



std::ostream& operator<<(std::ostream& os, const Element_Block& eb)
{
  int j = 0;
  int i,k;
  int * connectivity = eb.element_connectivity;
  float * attributes = eb.element_attributes;

  os << "Element Block: " << eb.material_id << "\n";
  os << "  Name of Element:    " << eb.element_name << "\n";
  os << "  Number of Elements: " << eb.num_elements << "\n";
  os << "  Nodes per Element:  " << eb.nodes_per_element << "\n";
  os << "  Atribs per Element: " << eb.attributes_per_element << "\n";

  for( i=0 ; i<eb.num_elements ; i++) {
    j++;
    os << "      Element : " << j << "\n";
    os << "        Nodes : ";
    for( k=0 ; k<eb.nodes_per_element ; k++ ){
      os << *connectivity++ << " ";
    }
    os << "\n";
    if( eb.attributes_per_element > 0 ) {
      os << "        Atribs: ";
      for( k=0 ; k<eb.attributes_per_element ; k++ ){
        os << *attributes++ ;
      }
      os << "\n";
    }
  }
  os << "\n\n";
  return(os);
}


void Element_Block::Make_Hexshell( Sideset * ss , Hexshell * hs , Node * nodes, int istart ) {

  int Element,Side1,Side2,l1,l2,node1,node2,i;
  int Number_of_Sides = ss->get_number();
  static int face[6][4] = { {0,1,5,4} , {1,2,6,5} , {3,7,6,2} , {0,4,7,3} ,
                            {0,3,2,1} , {4,5,6,7} };
  int * link_old = element_connectivity;
  nodes_per_element = 12;
  strcpy( element_name,"HEXSHELL");


  element_connectivity = new int [12*num_elements];
  assert( element_connectivity != NULL );
  for( i=0 ; i<12*num_elements ; i++){
    element_connectivity[i] = 0;
  }
  if( attributes_per_element == 0 ){
    attributes_per_element = 1;
    element_attributes = new float [num_elements];
    assert( element_attributes != NULL );
  }
  else{
    std::cerr << "**Error** A hex block can not have any attributes\n";
    exit(1);
  }

  for( i=0 ; i<Number_of_Sides ; i++ ){
    int Element1 = ss->get_element(i);
    Element = Element1 - istart;
// Don't process if this element isn't part of this element block
    if( Element >= 0  && Element < num_elements ){  
      Side1 = ss->get_side(i) - 1;
      switch( Side1 ){
        case 0:
          Side2 = 2;
          new_face_id[ 6*Element + 0] = 5;
          new_face_id[ 6*Element + 1] = 3;
          new_face_id[ 6*Element + 2] = 6;
          new_face_id[ 6*Element + 3] = 1;
          new_face_id[ 6*Element + 4] = 4;
          new_face_id[ 6*Element + 5] = 2;
          break;
        case 1:
          Side2 = 3;
          new_face_id[ 6*Element + 0] = 1;
          new_face_id[ 6*Element + 1] = 5;
          new_face_id[ 6*Element + 2] = 3;
          new_face_id[ 6*Element + 3] = 6;
          new_face_id[ 6*Element + 4] = 4;
          new_face_id[ 6*Element + 5] = 2;
          break;
        case 2:
          Side2 = 0;
          new_face_id[ 6*Element + 0] = 6;
          new_face_id[ 6*Element + 1] = 2;
          new_face_id[ 6*Element + 2] = 5;
          new_face_id[ 6*Element + 3] = 4;
          new_face_id[ 6*Element + 4] = 1;
          new_face_id[ 6*Element + 5] = 3;
          break;
        case 3:
          Side2 = 1;
          new_face_id[ 6*Element + 0] = 4;
          new_face_id[ 6*Element + 1] = 6;
          new_face_id[ 6*Element + 2] = 2;
          new_face_id[ 6*Element + 3] = 5;
          new_face_id[ 6*Element + 4] = 1;
          new_face_id[ 6*Element + 5] = 3;
          break;
        case 4:
          Side2 = 5;
          new_face_id[ 6*Element + 0] = 1;
          new_face_id[ 6*Element + 1] = 2;
          new_face_id[ 6*Element + 2] = 3;
          new_face_id[ 6*Element + 3] = 4;
          new_face_id[ 6*Element + 4] = 5;
          new_face_id[ 6*Element + 5] = 6;
          break;
        case 5:
          Side2 = 4;
          new_face_id[ 6*Element + 0] = 4;
          new_face_id[ 6*Element + 1] = 3;
          new_face_id[ 6*Element + 2] = 2;
          new_face_id[ 6*Element + 3] = 1;
          new_face_id[ 6*Element + 4] = 6;
          new_face_id[ 6*Element + 5] = 5;
          break;
      }
      l1 = Element*8;
      l2 = Element*12;
// Define the new link array
      element_connectivity[ 0 + l2 ] = link_old[ face[Side1][0] + l1];
      element_connectivity[ 3 + l2 ] = link_old[ face[Side1][1] + l1];
      element_connectivity[ 2 + l2 ] = link_old[ face[Side1][2] + l1];
      element_connectivity[ 1 + l2 ] = link_old[ face[Side1][3] + l1];
      element_connectivity[ 4 + l2 ] = link_old[ face[Side2][0] + l1];
      element_connectivity[ 5 + l2 ] = link_old[ face[Side2][1] + l1];
      element_connectivity[ 6 + l2 ] = link_old[ face[Side2][2] + l1];
      element_connectivity[ 7 + l2 ] = link_old[ face[Side2][3] + l1];
// Generate the New Nodes
      node1 = element_connectivity[ 0 + l2 ];
      node2 = element_connectivity[ 4 + l2 ];
      element_connectivity[ l2 +  8 ] = hs->Add_Node( node1 , node2 );
      node1 = element_connectivity[ 1 + l2 ];
      node2 = element_connectivity[ 5 + l2 ];
      element_connectivity[ l2 +  9 ] = hs->Add_Node( node1 , node2 );
      node1 = element_connectivity[ 2 + l2 ];
      node2 = element_connectivity[ 6 + l2 ];
      element_connectivity[ l2 + 10 ] = hs->Add_Node( node1 , node2 );
      node1 = element_connectivity[ 3 + l2 ];
      node2 = element_connectivity[ 7 + l2 ];
      element_connectivity[ l2 + 11 ] = hs->Add_Node( node1 , node2 );
    }
  }
   nodes->Add_Nodes( hs );
// Calculate the thickness
   hs_thickness( nodes );
// Make sure every element was converted
   int index = 0;
   int error = 0;
   for( i=0 ; i<num_elements ; i++){
     for( int j=0 ; j<nodes_per_element ; j++ ){
       if( element_connectivity[index++] == 0 ){
         std::cerr << "**ERROR** Element " << i+1 << 
                 " not converted to a valid Hexshell\n";
         error++;
       }
     }
   }
   if( error > 0 ){
     std::cerr << "\n\n**ERROR** " << error << " Elements not converted\n";
     exit(1);
   }
}




int Element_Block::get_num_elements( void ) {

  return( num_elements );

}



int Element_Block::get_new_face_id( int element , int old_face ){

  return( new_face_id[ element*faces_per_element + old_face - 1] );

}



void Element_Block::hs_thickness( Node * nodes ){
  
// Get the Node ID's
  for ( int Element=0 ; Element<num_elements ; Element++ ){
    int index = Element*12;
    int NH1 = element_connectivity[ index++ ];
    int NH2 = element_connectivity[ index++ ];
    int NH3 = element_connectivity[ index++ ];
    int NH4 = element_connectivity[ index++ ];
    int NH5 = element_connectivity[ index++ ];
    int NH6 = element_connectivity[ index++ ];
    int NH7 = element_connectivity[ index++ ];
    int NH8 = element_connectivity[ index++ ];
    int NS1 = element_connectivity[ index++ ];
    int NS2 = element_connectivity[ index++ ];
    int NS3 = element_connectivity[ index++ ];
    int NS4 = element_connectivity[ index++ ];
//
// Get the Coordinates
//
    float XH1,XH2,XH3,XH4,XH5,XH6,XH7,XH8,XS1,XS2,XS3,XS4;
    float YH1,YH2,YH3,YH4,YH5,YH6,YH7,YH8,YS1,YS2,YS3,YS4;
    float ZH1,ZH2,ZH3,ZH4,ZH5,ZH6,ZH7,ZH8,ZS1,ZS2,ZS3,ZS4;
    nodes->get_coordinates( NH1 , &XH1 , &YH1 , &ZH1 );
    nodes->get_coordinates( NH2 , &XH2 , &YH2 , &ZH2 );
    nodes->get_coordinates( NH3 , &XH3 , &YH3 , &ZH3 );
    nodes->get_coordinates( NH4 , &XH4 , &YH4 , &ZH4 );
    nodes->get_coordinates( NH5 , &XH5 , &YH5 , &ZH5 );
    nodes->get_coordinates( NH6 , &XH6 , &YH6 , &ZH6 );
    nodes->get_coordinates( NH7 , &XH7 , &YH7 , &ZH7 );
    nodes->get_coordinates( NH8 , &XH8 , &YH8 , &ZH8 );
    nodes->get_coordinates( NS1 , &XS1 , &YS1 , &ZS1 );
    nodes->get_coordinates( NS2 , &XS2 , &YS2 , &ZS2 );
    nodes->get_coordinates( NS3 , &XS3 , &YS3 , &ZS3 );
    nodes->get_coordinates( NS4 , &XS4 , &YS4 , &ZS4 );
//
// Calculate the HEX volume
//
    float Z24 = ZH2 - ZH4;
    float Z52 = ZH5 - ZH2;
    float Z45 = ZH4 - ZH5;
    float GRADOPH1 = ( YH2*(ZH6-ZH3-Z45) + YH3*Z24 + YH4*(ZH3-ZH8-Z52) +
		       YH5*(ZH8-ZH6-Z24) + YH6*Z52 + YH8*Z45 ) / 12;

    float Z31 = ZH3 - ZH1;
    float Z63 = ZH6 - ZH3;
    float Z16 = ZH1 - ZH6;
    float GRADOPH2 = ( YH3*(ZH7-ZH4-Z16) + YH4*Z31 + YH1*(ZH4-ZH5-Z63) +
		       YH6*(ZH5-ZH7-Z31) + YH7*Z63 + YH5*Z16 ) / 12;

    float Z42 = ZH4 - ZH2;
    float Z74 = ZH7 - ZH4;
    float Z27 = ZH2 - ZH7;
    float GRADOPH3 = ( YH4*(ZH8-ZH1-Z27) + YH1*Z42 + YH2*(ZH1-ZH6-Z74) +
		       YH7*(ZH6-ZH8-Z42) + YH8*Z74 + YH6*Z27 ) / 12;

    float Z13 = ZH1 - ZH3;
    float Z81 = ZH8 - ZH1;
    float Z38 = ZH3 - ZH8;
    float GRADOPH4 = ( YH1*(ZH5-ZH2-Z38) + YH2*Z13 + YH3*(ZH2-ZH7-Z81) +
		       YH8*(ZH7-ZH5-Z13) + YH5*Z81 + YH7*Z38 ) / 12;

    float Z86 = ZH8 - ZH6;
    float Z18 = ZH1 - ZH8;
    float Z61 = ZH6 - ZH1;
    float GRADOPH5 = ( YH8*(ZH4-ZH7-Z61) + YH7*Z86 + YH6*(ZH7-ZH2-Z18) +
		       YH1*(ZH2-ZH4-Z86) + YH4*Z18 + YH2*Z61 ) / 12;

    float Z57 = ZH5 - ZH7;
    float Z25 = ZH2 - ZH5;
    float Z72 = ZH7 - ZH2;
    float GRADOPH6 = ( YH5*(ZH1-ZH8-Z72) + YH8*Z57 + YH7*(ZH8-ZH3-Z25) +
		       YH2*(ZH3-ZH1-Z57) + YH1*Z25 + YH3*Z72 ) / 12 ;

    float Z68 = ZH6 - ZH8;
    float Z36 = ZH3 - ZH6;
    float Z83 = ZH8 - ZH3;
    float GRADOPH7 = ( YH6*(ZH2-ZH5-Z83) + YH5*Z68 + YH8*(ZH5-ZH4-Z36) +
		       YH3*(ZH4-ZH2-Z68) + YH2*Z36 + YH4*Z83 ) / 12;
  
    float Z75 = ZH7 - ZH5;
    float Z47 = ZH4 - ZH7;
    float Z54 = ZH5 - ZH4;
    float GRADOPH8 = ( YH7*(ZH3-ZH6-Z54) + YH6*Z75 + YH5*(ZH6-ZH1-Z47) +
		       YH4*(ZH1-ZH3-Z75) + YH3*Z47 + YH1*Z54 ) / 12;

    float VOLUME = XH1*GRADOPH1 + XH2*GRADOPH2 + XH3*GRADOPH3 + XH4*GRADOPH4 +
                   XH5*GRADOPH5 + XH6*GRADOPH6 + XH7*GRADOPH7 + XH8*GRADOPH8;

//
// Calculate the Shell Area
//
// Line from midpoint of side 4 (between points 1 and 4)
//        to midpoint of side 2 (between points 2 and 3)
    float XX1 = 0.5*(XS2+XS3) - 0.5*(XS1+XS4);
    float YY1 = 0.5*(YS2+YS3) - 0.5*(YS1+YS4);
    float ZZ1 = 0.5*(ZS2+ZS3) - 0.5*(ZS1+ZS4);
// Line from midpoint of side 1 (between points 1 and 2)
//        to midpoint of side 3 (between points 3 and 4)
    float XX2 = 0.5*(XS4+XS3) - 0.5*(XS1+XS2);
    float YY2 = 0.5*(YS4+YS3) - 0.5*(YS1+YS2);
    float ZZ2 = 0.5*(ZS4+ZS3) - 0.5*(ZS1+ZS2);
//
// E3 = L1 X L2
//
    float BASEL1_3 = YY1*ZZ2 - ZZ1*YY2;
    float BASEL2_3 = ZZ1*XX2 - XX1*ZZ2;
    float BASEL3_3 = XX1*YY2 - YY1*XX2;
//
// Normalize E3 vector
//
    float E3LEN = sqrt( BASEL1_3*BASEL1_3 +
			BASEL2_3*BASEL2_3 +
			BASEL3_3*BASEL3_3 );
    BASEL1_3 = BASEL1_3 / E3LEN;
    BASEL2_3 = BASEL2_3 / E3LEN;
    BASEL3_3 = BASEL3_3 / E3LEN;
//
// Define local E1 axis as the line
// from midpoint of side 4 (between points 1 and 4)
//   to midpoint of side 2 (between points 2 and 3)
    float BASEL1_1 = XX1;
    float BASEL2_1 = YY1;
    float BASEL3_1 = ZZ1;
//
// Normalize E1 vector
//
    float E1LEN = sqrt( BASEL1_1*BASEL1_1 +
			BASEL2_1*BASEL2_1 +
			BASEL3_1*BASEL3_1 );
    BASEL1_1 = BASEL1_1 / E1LEN;
    BASEL2_1 = BASEL2_1 / E1LEN;
    BASEL3_1 = BASEL3_1 / E1LEN;
//
// Define local element y axis  (E_2)
//      E_2 = E_3 X E_1
    float BASEL1_2 = BASEL2_3*BASEL3_1 - BASEL3_3*BASEL2_1;
    float BASEL2_2 = BASEL3_3*BASEL1_1 - BASEL1_3*BASEL3_1;
    float BASEL3_2 = BASEL1_3*BASEL2_1 - BASEL2_3*BASEL1_1;         
//
// Transform to a coordinate system with origin at Shell Node 1
//
    XS2 -= XS1;
    XS3 -= XS1;
    XS4 -= XS1;
    YS2 -= YS1;
    YS3 -= YS1;
    YS4 -= YS1;
    ZS2 -= ZS1;
    ZS3 -= ZS1;
    ZS4 -= ZS1;
//
// Transform to a coordinate system with origin at Shell Node 1
//

    float X2 = BASEL1_1*XS2 + BASEL2_1*YS2 + BASEL3_1*ZS2;
    float X3 = BASEL1_1*XS3 + BASEL2_1*YS3 + BASEL3_1*ZS3;
    float X4 = BASEL1_1*XS4 + BASEL2_1*YS4 + BASEL3_1*ZS4;
    float Y2 = BASEL1_2*XS2 + BASEL2_2*YS2 + BASEL3_2*ZS2;
    float Y3 = BASEL1_2*XS3 + BASEL2_2*YS3 + BASEL3_2*ZS3;
    float Y4 = BASEL1_2*XS4 + BASEL2_2*YS4 + BASEL3_2*ZS4;

//  float GRADOPS1_1 =  0.5*( Y2 - Y4 ) ;
    float GRADOPS2_1 =  0.5*Y3;
    float GRADOPS3_1 = -0.5*( Y2 - Y4 );
//  float GRADOPS4_1 = -0.5*Y3;
//  float GRADOPS1_2 =  0.5*( X4 - X2);
//  float GRADOPS2_2 = -0.5*X3;
    float GRADOPS3_2 = -0.5*( X4 - X2);
    float GRADOPS4_2 =  0.5*X3;

    float AREA = 2*( GRADOPS4_2*GRADOPS3_1 + GRADOPS3_2*GRADOPS2_1 );

  
    element_attributes[Element] = VOLUME/AREA;
  }
}




void Element_Block::set_thickness( float thickness ){

  if( strcmp( element_name , "HEXSHELL" ) != 0 ){
    std::cout << "Element Block: " << material_id << " is not a HEXSHELL Block\n";
    return;
  }
  
  float * old_attributes = element_attributes;
  element_attributes = new float [num_elements];
  assert( element_attributes != NULL );

  for( int i=0 ; i<num_elements ; i++ ){
    float difference = (old_attributes[i] - thickness ) / 
                            old_attributes[i];
    if( fabs(difference) <= 0.05 ){
      element_attributes[i] = thickness;
    }
    else {
      std::cout << "Thickness varies by more than 5% of calculated value.  " <<
              "Thickness not reset.\n";
      delete [] element_attributes;
      element_attributes = old_attributes;
      return;
    }
  }

  delete [] old_attributes;
}


void Element_Block::assemble_normals( float * normal , Node * nodes ){


  for( int Element=0 ; Element<num_elements ; Element++ ){
    int index = Element*12;
    int NS1 = element_connectivity[ index+ 8 ];
    int NS2 = element_connectivity[ index+ 9 ];
    int NS3 = element_connectivity[ index+10 ];
    int NS4 = element_connectivity[ index+11 ];
//
// Get the Coordinates
//
    float XS1,XS2,XS3,XS4;
    float YS1,YS2,YS3,YS4;
    float ZS1,ZS2,ZS3,ZS4;
    nodes->get_coordinates( NS1 , &XS1 , &YS1 , &ZS1 );
    nodes->get_coordinates( NS2 , &XS2 , &YS2 , &ZS2 );
    nodes->get_coordinates( NS3 , &XS3 , &YS3 , &ZS3 );
    nodes->get_coordinates( NS4 , &XS4 , &YS4 , &ZS4 );
//
// Calculate the normal
//
// Line from midpoint of side 4 (between points 1 and 4)
//        to midpoint of side 2 (between points 2 and 3)
    float XX1 = 0.5*(XS2+XS3) - 0.5*(XS1+XS4);
    float YY1 = 0.5*(YS2+YS3) - 0.5*(YS1+YS4);
    float ZZ1 = 0.5*(ZS2+ZS3) - 0.5*(ZS1+ZS4);
// Line from midpoint of side 1 (between points 1 and 2)
//        to midpoint of side 3 (between points 3 and 4)
    float XX2 = 0.5*(XS4+XS3) - 0.5*(XS1+XS2);
    float YY2 = 0.5*(YS4+YS3) - 0.5*(YS1+YS2);
    float ZZ2 = 0.5*(ZS4+ZS3) - 0.5*(ZS1+ZS2);
//
// E3 = L1 X L2
//
    float BASEL1_3 = YY1*ZZ2 - ZZ1*YY2;
    float BASEL2_3 = ZZ1*XX2 - XX1*ZZ2;
    float BASEL3_3 = XX1*YY2 - YY1*XX2;
//
// Normalize E3 vector
//
    float E3LEN = sqrt( BASEL1_3*BASEL1_3 +
			BASEL2_3*BASEL2_3 +
			BASEL3_3*BASEL3_3 );
    normal[3*(NS1-1)]   = BASEL1_3 / E3LEN * element_attributes[Element];
    normal[3*(NS1-1)+1] = BASEL2_3 / E3LEN * element_attributes[Element];
    normal[3*(NS1-1)+2] = BASEL3_3 / E3LEN * element_attributes[Element];
    normal[3*(NS2-1)]   = BASEL1_3 / E3LEN * element_attributes[Element];
    normal[3*(NS2-1)+1] = BASEL2_3 / E3LEN * element_attributes[Element];
    normal[3*(NS2-1)+2] = BASEL3_3 / E3LEN * element_attributes[Element];
    normal[3*(NS3-1)]   = BASEL1_3 / E3LEN * element_attributes[Element];
    normal[3*(NS3-1)+1] = BASEL2_3 / E3LEN * element_attributes[Element];
    normal[3*(NS3-1)+2] = BASEL3_3 / E3LEN * element_attributes[Element];
    normal[3*(NS4-1)]   = BASEL1_3 / E3LEN * element_attributes[Element];
    normal[3*(NS4-1)+1] = BASEL2_3 / E3LEN * element_attributes[Element];
    normal[3*(NS4-1)+2] = BASEL3_3 / E3LEN * element_attributes[Element];
  }
}



void Element_Block::update_link( Node * nodes ){

  int length = nodes_per_element*num_elements;

  for( int i=0 ; i<length ; i++ )
    element_connectivity[i] = nodes->get_new_id(element_connectivity[i]);

}


void Element_Block::make_hsjoint( Hexshell * hs , HSjoint* HSj){

  for( int i=0 ; i<num_elements ; i++ ){
    int N1 = element_connectivity[i*nodes_per_element + 0];
    int N2 = element_connectivity[i*nodes_per_element + 1];
    int N3 = element_connectivity[i*nodes_per_element + 2];
    int N4 = element_connectivity[i*nodes_per_element + 3];
    int N5 = element_connectivity[i*nodes_per_element + 4];
    int N6 = element_connectivity[i*nodes_per_element + 5];
    int N7 = element_connectivity[i*nodes_per_element + 6];
    int N8 = element_connectivity[i*nodes_per_element + 7];
    int status;
    status = HSj->process_element( N1,N2,N3,N4,N5,N6,N7,N8,hs );
    if( status != 0 ){
      std::cerr << "Can not convert hex element to hsjoint\n";
      exit(1);
    }
  }  
}


void Element_Block::set_status( int Stat ){

  Status = Stat;

}



int Element_Block::get_status( void ){
  
  return( Status );

}
