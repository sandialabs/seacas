// $Id: Nodeset.C,v 1.6 2004/03/19 15:39:45 gdsjaar Exp $
// Nodeset Class //

#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include "Nodeset.h"
#include "exodusII.h"
#include "Hexshell.h"
#include "Node.h"



Nodeset::Nodeset() {

  nodeset_id = 0;
  number_nodes = 0;
  number_df = 0;
  nodeset_list = NULL;
  nodeset_df = NULL;
  status = 0;
}




Nodeset::Nodeset( int EXOid , int ns_id )
  : nodeset_id(ns_id)
{
  int ierr;

  ierr = ex_get_node_set_param( EXOid , ns_id , &number_nodes , &number_df );
  if( ierr < 0 ){
    std::cerr << "**ERROR** EX_GET_NODE_SET_PARAM\n";
    exit(1);
  }


  if (number_nodes > 0) {
    status = 1;
    nodeset_list = new int [number_nodes];
    assert( nodeset_list != NULL );

    ierr = ex_get_node_set( EXOid , ns_id , nodeset_list );
    if( ierr > 0 ) {
      std::cerr << "**ERROR**  EX_GET_NODE_SET\n";
      exit(1);
    }
  } else {
    status = 0;
  }

  if (number_df > 0) {
    nodeset_df = new float [number_df];
    assert( nodeset_df != NULL );

    ierr = ex_get_node_set_dist_fact( EXOid , ns_id , nodeset_df );
    if( ierr > 0 ) {
      std::cerr << "**ERROR** EX_GET_NODE_SET_DIST_FACT\n";
      exit(1);
    }
  }
}




Nodeset::~Nodeset(void) {

  delete [] nodeset_list;
  delete [] nodeset_df;
}



void Nodeset::set_status( int stat ) {

  if( stat == 1 )
    status = stat;
  else
    status = 0;
}



int Nodeset::get_status( void ){

  int tmp = status;
  return(tmp);
}


void Nodeset::write_exodus( int EXOid ){

  int ierr;

  ierr = ex_put_node_set_param( EXOid,nodeset_id,number_nodes,number_df );
  if( ierr > 0 ){
    std::cerr << "**ERROR** EX_PUT_NODE_SET_PARAM\n";
    return;
  }

  ierr = ex_put_node_set( EXOid,nodeset_id,nodeset_list );
  if( ierr > 0 ){
    std::cerr << "**ERROR** EX_PUT_NODE_SET\n";
    return;
  }
}



void Nodeset::add_hexshell_nodes( int Number_of_Nodes , Hexshell * hs ){

  int i;

  int * list;
  list = new int [Number_of_Nodes];

// Set all nodes to zero 
  for( i=0 ; i<Number_of_Nodes ; i++ )
    list[i] = 0;
// Set nodes in the Nodeset to 1
  for( i=0 ; i<number_nodes ; i++ )
    list[nodeset_list[i]-1] = 1;

// Set the shell node to one if both hex nodes are part of the nodeset
  int num_hs = hs->get_number();
  for( i=0 ; i<num_hs ; i++ ){
    int nodeh1,nodeh2,nodesh;
    hs->get_pair( i , &nodeh1 , &nodeh2 , &nodesh );
// Convert to C numbering
    nodeh1-- ; nodeh2-- ; nodesh-- ;
    if( list[nodeh1]==1 && list[nodeh2]==1 ){
      list[nodesh] = 1;
    }
  }

// Calculate the number of nodes in the Nodeset
  number_nodes = 0;
  for( i=0 ; i<Number_of_Nodes ; i++ ){
    if( list[i]==1 ){
      number_nodes++;
    }
  }

// Build the new nodeset list and delete the previous list
  delete [] nodeset_list;
  nodeset_list = new int [number_nodes];
  assert( nodeset_list != NULL );
  int icount=0;
  for( i=0 ; i<Number_of_Nodes ; i++ ){
    if( list[i] == 1 ){
      nodeset_list[icount++] = i+1; // +1 for Exodus Numbering
    }
  }
  assert( icount == number_nodes );

// Build the new distribution list
  number_df = number_nodes;
  delete [] nodeset_df;
  nodeset_df = new float [number_df];
  assert( nodeset_df != NULL );
  for( i=0 ; i<number_df ; i++){
    nodeset_df[i]=1;
  }
}




std::ostream& operator<<(std::ostream& os, const Nodeset & ns) 
{
  int * node_ptr = ns.nodeset_list;
  int icount = 0;

  os << "Node Set         : " << ns.nodeset_id << "\n";
  os << "  Number of Nodes: " << ns.number_nodes << "\n";
  os << "  List of Nodes  :";
  for( int i=0 ; i<ns.number_nodes ; i++ ){
    os << " " << *node_ptr++;
    if( ++icount == 10) {
      icount = 0;
      os << "\n                  " ;
    }
  }
  os << "\n";
  if( icount != 0 ) {
    os << "\n";
  }
  return(os);
}



void Nodeset::update_ids( Node * nodes ){

  for( int i=0 ; i<number_nodes ; i++ )
    nodeset_list[i] = nodes->get_new_id(nodeset_list[i]);

}


