// $Id: Sideset.C,v 1.5 2004/03/19 15:39:45 gdsjaar Exp $
// Sideset Class //

#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include "Sideset.h"
#include "exodusII.h"


Sideset::Sideset() {

  sideset_id = 0;
  number_sides = 0;
  element_list = NULL;
  side_list = NULL;
  side_df = NULL;
}




Sideset::Sideset( int EXOid , int ss_id ) {

  int ierr;

  sideset_id = ss_id;
  status = 1;

  ierr = ex_get_side_set_param( EXOid , ss_id , &number_sides , 
                                &number_df );
  if( ierr < 0 ){
    std::cerr << "**ERROR** EX_GET_SIDE_SET_PARAM\n";
    exit(1);
  }

  if (number_sides > 0) {
    element_list = new int [number_sides];
    assert( element_list != NULL );
    side_list    = new int [number_sides];
    assert( side_list != NULL );
    
    ierr = ex_get_side_set( EXOid , ss_id , element_list , side_list );
    if( ierr < 0 ){
      std::cerr << "**ERROR** EX_GET_SIDE_SET\n";
      exit(1);
    }
  } else {
    status = 0;
  }

  if (number_df > 0) {
    side_df      = new float [number_df];
    assert( side_df != NULL );
    ierr = ex_get_side_set_dist_fact( EXOid , ss_id , side_df );
    if( ierr < 0 ){
      std::cerr << "**ERROR** EX_GET_SIDE_SET_DIST\n";
      exit(1);
    }
  }
}

void Sideset::write_exodus( int EXOid ) {
 
  int ierr;

  ierr = ex_put_side_set_param( EXOid , sideset_id , number_sides ,
                                number_df );
  if( ierr < 0 ) {
    std::cerr << "**ERROR** EX_PUT_SIDE_SET_PARAM\n";
    exit(1);
  }

  if (number_sides > 0) {
    ierr = ex_put_side_set( EXOid , sideset_id , element_list , side_list );
    if( ierr != 0 ) {
      std::cerr << "**ERROR** EX_PUT_SIDE_SET\n";
      exit(1);
    }
  }

  if (number_df > 0) {
    ierr = ex_put_side_set_dist_fact( EXOid , sideset_id , side_df );
    if( ierr != 0 ) {
      std::cerr << "**ERROR** EX_PUT_SIDE_SET_DIST_FACT\n";
      exit(1);
    }
  }
}



Sideset::~Sideset() {

  delete [] element_list;
  delete [] side_list;
  delete [] side_df;

}




void Sideset::set_status( int stat ) {

  if( stat == 1 )
    status = stat;
  else
    status = 0;

}




int Sideset::get_status( void ) {

  return(status);

}





int Sideset::get_number( void ) {
  
  return( number_sides );

}






int Sideset::get_element( int Num ){

  return( element_list[Num] );

}






int Sideset::get_side( int Num ){

  return( side_list[Num]  );

}




void Sideset::set_new_face_id( int Num , int New_ID ){

  side_list[Num] = New_ID;

}

void Sideset::set_new_elem_id( int Num , int New_ID ){

  element_list[Num] = New_ID;

}




std::ostream& operator<<(std::ostream& os, const Sideset & ss)
{
  int * elem_ptr = ss.element_list;
  int * side_ptr = ss.side_list;
  
  os << "Side Set         : " << ss.sideset_id << "\n";
  os << "  Number of Sides: " << ss.number_sides << "\n";
  for( int i=0 ; i<ss.number_sides ; i++) {
    os << "    Element: " << *elem_ptr++ << "  Side: " << *side_ptr++ << "\n";
  }
  os << " \n";

  return(os);
}
