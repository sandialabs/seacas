// $Id: Mesh.C,v 1.12 2004/03/19 15:39:45 gdsjaar Exp $
// Mesh Class //

#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <string.h>
#include "exodusII.h"
#include "Nodeset.h"
#include "Sideset.h"
#include "Element_Block.h"
#include "Mesh.h"
#include "Node.h"
#include "QA_Records.h"
#include "Info_Records.h"


Mesh::Mesh( void ) {

  Number_of_Dimensions = Number_of_Nodes = Number_of_Elements = 0;
  Number_of_Element_Blocks = Number_of_Node_Sets = Number_of_Side_Sets = 0;
  Number_of_Dimensions = Number_of_Nodes = Number_of_Elements = 0;
  Number_of_Element_Blocks = Number_of_Node_Sets = Number_of_Side_Sets = 0;
  Element_Block_IDs = Node_Set_IDs = Side_Set_IDs = NULL;
  eb_pointers = NULL;
  node_set_pointers = NULL;
  side_set_pointers = NULL;
  QA_Recs = NULL;
  Info_Recs = NULL;
  eb_start = NULL;

}





Mesh::Mesh( char * Input_Mesh ) {
  
  int EXOid , cpu_ws = 0 , io_ws = 0;
  int i,j,sum;
  float vers;

// Open the mesh file
  if((EXOid=ex_open( Input_Mesh , EX_READ , &cpu_ws , &io_ws, &vers)) < 0){
    std::cerr << "\n\n**ERROR** Opening Input File: " << Input_Mesh << "\n\n";
    exit(1);
  }

// Get the title, dimension and number of nodes
  if( ex_get_init( EXOid, Title , &Number_of_Dimensions , &Number_of_Nodes ,
                   &Number_of_Elements , &Number_of_Element_Blocks ,
                   &Number_of_Node_Sets , &Number_of_Side_Sets ) < 0 ) {
    std::cerr << "\n\n**ERROR** EX_GET_INIT \n";
    exit(1);
  }

  float fdum;
  char * cdum = NULL;
  ex_inquire( EXOid,EX_INQ_QA,&Number_of_QA_Recs,&fdum,cdum );
  ex_inquire( EXOid,EX_INQ_INFO,&Number_of_Info_Recs,&fdum,cdum );

// Allocate Memory 
  eb_pointers = new Element_Block * [Number_of_Element_Blocks];
  assert(eb_pointers != NULL);
  Element_Block_IDs = new int [Number_of_Element_Blocks];
  assert(Element_Block_IDs != NULL);
  Node_Set_IDs = new int [Number_of_Node_Sets];
  assert(Node_Set_IDs != NULL);
  node_set_pointers = new Nodeset * [Number_of_Node_Sets];
  assert(node_set_pointers != NULL);
  Side_Set_IDs = new int [Number_of_Side_Sets];
  assert(Side_Set_IDs != NULL);
  side_set_pointers = new Sideset * [Number_of_Side_Sets];
  assert(side_set_pointers != NULL);
  eb_start = new int [Number_of_Element_Blocks];
  assert(eb_start != NULL );
  nodes = new Node( EXOid , Number_of_Nodes , Number_of_Dimensions );
  assert( nodes != NULL );
  QA_Recs = new QA_Records( EXOid );
  assert( QA_Recs != NULL );
  Info_Recs = new Info_Records( EXOid );
  assert( Info_Recs != NULL );

// Get the Element Block IDs
  if( ex_get_elem_blk_ids( EXOid , Element_Block_IDs ) < 0 ){
    std::cerr << "\n\n**ERROR** EX_GET_ELEM_BLK_IDS\n";
    exit(1);
  }
// Construct Element Blocks
  for( i=0 ; i<Number_of_Element_Blocks ; i++ ){
    eb_pointers[i] = new Element_Block( EXOid , Element_Block_IDs[i] );
    assert( eb_pointers[i] != NULL );
    sum = 1;
    for( j=0 ; j<i ; j++ ){
      sum += eb_pointers[j]->get_num_elements();
    }
    eb_start[i] = sum;
  }

// Get Node Set
  if( Number_of_Node_Sets > 0 ){
// Get Node Set IDs
    if( ex_get_node_set_ids( EXOid , Node_Set_IDs ) > 0){
      std::cerr << "\n\n**ERROR** EX_GET_NODE_SET_IDS\n";
      exit(1);
    }
// Construct Node Sets
    for( int i=0 ; i<Number_of_Node_Sets ; i++ ){
      node_set_pointers[i] = new Nodeset( EXOid , Node_Set_IDs[i] );
      assert(node_set_pointers[i] != NULL);
    }
  }

// Get Side Set
  if( Number_of_Side_Sets > 0 ){
// Get Side Set IDs
    if( ex_get_side_set_ids( EXOid , Side_Set_IDs ) > 0){
      std::cerr << "\n\n**ERROR** EX_GET_SIDE_SET_IDS\n";
      exit(1);
    }
// Construct Side Sets
    for( int i=0 ; i<Number_of_Side_Sets ; i++ ){
      side_set_pointers[i] = new Sideset( EXOid , Side_Set_IDs[i] );
      assert(side_set_pointers[i] != NULL);
    }
  }  
// Close the database
  if( ex_close( EXOid ) > 0 ){
    std::cerr << "**ERROR** EX_CLOSE\n";
    exit(1);
  }
// Make the Hexjoint Object
     HSj = new HSjoint( nodes->get_number_nodes() );
}



void Mesh::write_exodus( char * OutputFile){

  int i,EXOid,ierr=0;
  int iows = 0;
  int compws = 0;

  EXOid = ex_create( OutputFile,EX_CLOBBER,&compws,&iows);
  if( ierr > 0 ){
    std::cerr << "**ERROR** EX_CREATE \n";
    exit(1);
  }
  
// Count active Element Blocks (those not deleted by the user)
  int num_eb = 0;
  Number_of_Elements = 0;
  for( i=0 ; i<Number_of_Element_Blocks ; i++ ){
    if( eb_pointers[i]->get_status() == 1 ){
      num_eb++;
      Number_of_Elements += eb_pointers[i]->get_num_elements();
    }
  }

// Count active Node Sets (those not deleted by the user)
  int num_node_sets = 0;
  for( i=0 ; i<Number_of_Node_Sets ; i++ )
    num_node_sets += node_set_pointers[i]->get_status();

// Count active Side Sets (those not deleted by the user)
  int num_side_sets = 0;
  for( i=0 ; i<Number_of_Side_Sets ; i++ )
    num_side_sets += side_set_pointers[i]->get_status();

  ierr = ex_put_init( EXOid , Title , Number_of_Dimensions , Number_of_Nodes ,
                      Number_of_Elements , num_eb , 
                      num_node_sets , num_side_sets);

  if( ierr > 0 ){
    std::cerr << "**ERROR** EX_PUT_INIT \n";
    exit(1);
  }

  nodes->write_exodus( EXOid );

  for( i=0 ; i<Number_of_Element_Blocks ; i++ ){
    if( eb_pointers[i]->get_status() == 1 )
      eb_pointers[i]->write_exodus( EXOid );
  }

  for( i=0 ; i<Number_of_Node_Sets ; i++ ){
    if( node_set_pointers[i]->get_status() )
      node_set_pointers[i]->write_exodus( EXOid);
  }

  for( i=0 ; i<Number_of_Side_Sets ; i++ ){
    if( side_set_pointers[i]->get_status() ){
      side_set_pointers[i]->write_exodus( EXOid );
    }
  }

// Write the QA and Info records
  QA_Recs->Save( EXOid );
  Info_Recs->Save( EXOid ); 
// Close the Exodus database 
  ierr = ex_close( EXOid );
  if( ierr != 0) {
    std::cerr << "**ERROR** EX_CLOSE\n";
    exit(1);
  }
}




Mesh::~Mesh(void){

  int i;
// Delete Element Blocks
  for( i=0 ; i<Number_of_Element_Blocks ; i++ ){
    delete eb_pointers[i];
  }
  delete [] eb_pointers;
  delete [] Element_Block_IDs;
// Delete Node Sets
  for( i=0 ; i<Number_of_Node_Sets ; i++ ){
    delete node_set_pointers[i];
  }
  delete [] Node_Set_IDs;
  delete [] node_set_pointers;
// Delete Side Sets
  for( i=0 ; i<Number_of_Side_Sets ; i++ ){
    delete side_set_pointers[i];
  }
  delete [] Side_Set_IDs;
  delete [] side_set_pointers;
  delete HSj;
// Delete QA Records
//   for( i=0 ; i<Number_of_QA_Recs ; i++ )
//     delete [] QA_Records[i];
//   delete [] QA_Records;
// Delete Info Records
//   for( i=0 ; i<Number_of_Info_Recs ; i++ )
//     delete [] Info_Records[i];
//   delete [] Info_Records;

}



int Mesh::Valid_EB( const int ID ){

  int valid = 0;
  for( int i=0 ; i<Number_of_Element_Blocks ; i++){
    if( ID == Element_Block_IDs[i]) {
      valid = 1;
      break;
    }
  }
  return(valid);
}




int Mesh::Valid_NS( const int ID ){

  int valid = 0;
  for( int i=0 ; i<Number_of_Node_Sets ; i++ ){
    if( ID == Node_Set_IDs[i] ){
       valid = 1;
       break;
    }
  }
  return(valid);
}



int Mesh::Valid_SS( const int ID){

  int valid=0;
  for( int i=0 ; i<Number_of_Side_Sets ; i++ ){
    if( ID == Side_Set_IDs[i] ){
       valid = 1;
       break;
    }
  }
  return(valid);
}



void Mesh::EB_status( const int ID , const int status ) {
  for( int i=0 ; i<Number_of_Element_Blocks ; i++ ){
    if( ID == Element_Block_IDs[i] ) {
      eb_pointers[i]->set_status( status );
      break;
    }
  }
}



void Mesh::NS_status( const int ID , const int status ) {
  for( int i=0 ; i<Number_of_Node_Sets ; i++ ){
    if( ID == Node_Set_IDs[i] ) {
      node_set_pointers[i]->set_status( status );
      break;
    }
  }
}  

void Mesh::SS_status( const int ID , const int status ) {
  for( int i=0 ; i<Number_of_Side_Sets ; i++ ){
    if( ID == Side_Set_IDs[i] ) {
      side_set_pointers[i]->set_status( status );
      break;
    }
  }
}



int Mesh::get_Num_Nodes( void ){

  return( Number_of_Nodes );

}

int Mesh::get_Num_EBs( void ){

  return( Number_of_Element_Blocks );

}


void Mesh::convert_2_hexshell( int eb_id , int ss_id , Hexshell * hs){

  int ebpt,sspt,i;

  for( i=0 ; i<Number_of_Side_Sets ; i++ ){
    if( Side_Set_IDs[i] == ss_id ){
       sspt = i;
       break;
    }
  }

  for( i=0 ; i<Number_of_Element_Blocks ; i++ ){
    if( Element_Block_IDs[i] == eb_id ){
       ebpt = i;
       break;
    }
  }
  
  eb_pointers[ebpt]->Make_Hexshell( side_set_pointers[sspt] , hs , 
				    nodes , eb_start[i] );

  Number_of_Nodes = nodes->get_number_nodes();
}


void Mesh::update_side_sets(){
  for( int i=0 ; i<Number_of_Side_Sets ; i++ ){
    if( side_set_pointers[i]->get_status() ){
      int num = side_set_pointers[i]->get_number();
      for( int j=0 ; j<num ; j++ ){
        int old_id = side_set_pointers[i]->get_side( j );
        int element = side_set_pointers[i]->get_element( j );
        int eb = get_element_block( element );
        int new_id = eb_pointers[eb]->
                     get_new_face_id( element - eb_start[eb] , old_id );
        side_set_pointers[i]->set_new_face_id( j , new_id );
      }
    }
  }
}



void Mesh::set_thickness( int ID , float thick ){

  for( int i=0 ; i<Number_of_Element_Blocks ; i++ ){
    if( ID == Element_Block_IDs[i] ) {
      eb_pointers[i]->set_thickness( thick );
      break;
    }
  }
}




int Mesh::get_element_block( int element ){

  int i;
  for( i=Number_of_Element_Blocks-1 ; i>=0 ; i-- ){
    if( element >= eb_start[i] )
       return( i );
  }
  return(-1);
}
     

void Mesh::normalize_hexshell_nodes( Hexshell * hs ){
  int i;
  float * rpnt;
  float * normal;
  rpnt = normal = new float [Number_of_Nodes*Number_of_Dimensions];
  assert( rpnt != NULL );

  for( i=0 ; i<Number_of_Nodes*Number_of_Dimensions ; i++ ){
    *rpnt++ = 0;
  }

// Loop over the element blocks and assemble thickness weighted Normals
// to the nodes.
  for( i=0 ; i<Number_of_Element_Blocks ; i++ ){
    if( strcmp(eb_pointers[i]->element_name,"HEXSHELL") == 0 ){
      eb_pointers[i]->assemble_normals( normal , nodes );
    }
  }

// make the sum of the thickness weighted normals be a unit vector
  int Num_HS = hs->get_number();
  int nodeh1,nodeh2,shell_node;

  for( i=0 ; i<Num_HS ; i++ ){
    hs->get_pair( i , &nodeh1 , &nodeh2 , &shell_node ); 
    float normx = normal[(shell_node-1)*Number_of_Dimensions];
    float normy = normal[(shell_node-1)*Number_of_Dimensions+1];
    float normz = normal[(shell_node-1)*Number_of_Dimensions+2];
    float rmag =  sqrt( normx*normx + normy*normy + normz*normz );
    assert( rmag != 0 );
    // shell_node - 1
    normal[(shell_node-1)*Number_of_Dimensions]   = normx/rmag;
    normal[(shell_node-1)*Number_of_Dimensions+1] = normy/rmag; 
    normal[(shell_node-1)*Number_of_Dimensions+2] = normz/rmag;
// Get the coordinates of the nodes
    float XH1,XH2,XS,YH1,YH2,YS,ZH1,ZH2,ZS;
    nodes->get_coordinates( nodeh1     , &XH1 , &YH1 , &ZH1 );
    nodes->get_coordinates( nodeh2     , &XH2 , &YH2 , &ZH2 );
    nodes->get_coordinates( shell_node , &XS  , &YS  , &ZS );
// Calculate the current distance between the nodes
// dot the normal with the current distance and use normal direction
    rmag = 0.5*(XH2 - XH1)*normal[(shell_node-1)*Number_of_Dimensions] +
           0.5*(YH2 - YH1)*normal[(shell_node-1)*Number_of_Dimensions+1] +
           0.5*(ZH2 - ZH1)*normal[(shell_node-1)*Number_of_Dimensions+2];
// Set the coordinates of the nodes
    XH1 = XS - rmag*normal[(shell_node-1)*3];
    YH1 = YS - rmag*normal[(shell_node-1)*3+1];
    ZH1 = ZS - rmag*normal[(shell_node-1)*3+2];
    nodes->set_coordinates( nodeh1 , &XH1 , &YH1 , &ZH1 );
    XH2 = XS + rmag*normal[(shell_node-1)*3];
    YH2 = YS + rmag*normal[(shell_node-1)*3+1];
    ZH2 = ZS + rmag*normal[(shell_node-1)*3+2];
    nodes->set_coordinates( nodeh2 , &XH2 , &YH2 , &ZH2 );
  }

  delete [] normal;
}



void Mesh::add_hs_nodes_to_nodeset( Hexshell * hs ){

  for( int i=0 ; i<Number_of_Node_Sets ; i++ ){
    node_set_pointers[i]->add_hexshell_nodes( Number_of_Nodes , hs );
  }
}


void Mesh::make_hsjoint( int eb_id , Hexshell * hs ){

  int i,ebpt;

// Find the element block
  for( i=0 ; i<Number_of_Element_Blocks ; i++ ){
    if( Element_Block_IDs[i] == eb_id ){
       ebpt = i;
       break;
    }
  }
  eb_pointers[ebpt]->make_hsjoint( hs,HSj );
}

void Mesh::process_hsjoint( Hexshell * hs ){

    int i;

    HSj->process_all( nodes );

    nodes->update_new_id();


// Update the Hexshell pairs with the new_ids
    hs->update_ids( nodes );

// Loop through all nodesets and get the New_IDs
  for( i=0 ; i<Number_of_Node_Sets ; i++ )
    node_set_pointers[i]->update_ids( nodes );

// Loop through all the element blocks and get the New_IDs for each node
  for( i=0 ; i<Number_of_Element_Blocks ; i++ )
    eb_pointers[i]->update_link( nodes );

// Compress the nodes
  nodes->compress_nodes();
  Number_of_Nodes = nodes->get_number_nodes();

// Loop over the side sets and update the element ID //
  for( i=0 ; i<Number_of_Side_Sets ; i++ ){
    if( side_set_pointers[i]->get_status() ){
      int num = side_set_pointers[i]->get_number();
      for( int j=0 ; j<num ; j++ ){
        int element = side_set_pointers[i]->get_element( j );
        int new_id = get_new_element_id( element );
        assert(new_id<=Number_of_Elements);
        assert(new_id>0);
        side_set_pointers[i]->set_new_elem_id( j , new_id );
      }
    }
  }
}


int Mesh::get_new_element_id( int element ){

  
  int i;
  int eb,offset;
  for( i=Number_of_Element_Blocks-1 ; i>=0 ; i-- ){
    if( element >= eb_start[i] ){
      eb = i;
      offset = element - eb_start[i] + 1;
      break;
    }
  }

  int new_start = 0;
  for( int j=0 ; j<=eb-1 ; j++ ){
    if( eb_pointers[j]->Status == 1 ){
      new_start += eb_pointers[j]->num_elements;
    }
  }
  return( new_start + offset );
}
