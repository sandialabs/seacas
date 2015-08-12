// $Id: Node.C,v 1.8 2004/03/19 15:39:45 gdsjaar Exp $
// Node Class //

#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include "Node.h"
#include "exodusII.h"
#include "netcdf.h"
#include "Hexshell.h"


Node::Node() :
  x_coordinates(NULL), y_coordinates(NULL), z_coordinates(NULL),
  coordinate_names(NULL), Number_of_Nodes(0), Number_of_Dimensions(0),
  Status(NULL), New_ID(NULL)
{}

Node::Node ( int EXOid , int Num_Nodes , int Num_Dim ) :
  x_coordinates(NULL), y_coordinates(NULL), z_coordinates(NULL),
  coordinate_names(NULL), Number_of_Nodes(Num_Nodes), Number_of_Dimensions(Num_Dim),
  Status(NULL), New_ID(NULL)
{
  int i,ierr;

  x_coordinates = new float [Number_of_Nodes];
  assert( x_coordinates != NULL );
  y_coordinates = new float [Number_of_Nodes];
  assert( y_coordinates != NULL );

  if( Num_Dim == 3 ) {
    z_coordinates = new float [Number_of_Nodes];
    assert( z_coordinates != NULL );
  }
  else
    z_coordinates = NULL;

  Status = new int [Number_of_Nodes];
  assert( Status != NULL );
  for( i=0 ; i<Number_of_Nodes ; i++ )
    Status[i] = active;

  New_ID = new int [Number_of_Nodes];
  assert( Status != NULL );
  for( i=0 ; i<Number_of_Nodes ; i++ )
    New_ID[i] = i+1;

  char *temp_space;
  coordinate_names = new char* [Number_of_Dimensions];
  assert( coordinate_names != NULL );
  temp_space       = new char[81*Number_of_Dimensions];
  assert( temp_space != NULL );
  for(i=0 ; i<Number_of_Dimensions ; i++)
    coordinate_names[i] = &(temp_space[i*81]);


// Read the Coordinate Names
  ierr = ex_get_coord_names( EXOid , coordinate_names );
  if( ierr>0 ){
    std::cerr << "**ERROR** EX_GET_COORD_NAMES\n";
    exit(1);
  }

// Read the Coordinates
  ierr = ex_get_coord( EXOid , x_coordinates , y_coordinates , z_coordinates );
  if( ierr>0 ){
    std::cerr << "**ERROR** EX_GET_COORD\n";
    exit(1);
  }

}


Node::~Node(void) {

  delete [] x_coordinates;
  delete [] y_coordinates;
  delete [] z_coordinates;
  delete [] Status;
  delete [] coordinate_names[0];
  delete [] coordinate_names;
  delete [] New_ID;

}


int Node::get_number_nodes(void){return(Number_of_Nodes);}

  
void Node::write_exodus( int EXOid ){

  int ierr;

  ierr = ex_put_coord_names( EXOid , coordinate_names );
  if( ierr > 0 ){
    std::cerr << "**ERROR** EX_PUT_COORD_NAMES\n";
    exit(1);
  }

  ierr = ex_put_coord( EXOid , x_coordinates , y_coordinates , z_coordinates );
  if( ierr > 0 ){
    std::cerr << "**ERROR** EX_PUT_COORD\n";
    exit(1);
  }



}




void Node::Add_Nodes( Hexshell * hs ) {

  int i;
  int nodeh1 = 0;
  int nodeh2 = 0;
  int nodesh = 0;

  int Num_New_Nodes = hs->get_number_of_new_nodes();
  
  int Number_Nodes_Old = Number_of_Nodes;
  Number_of_Nodes += Num_New_Nodes;

  float * x_old = x_coordinates;
  float * y_old = y_coordinates;
  float * z_old = z_coordinates;
  int * Status_old = Status;
  int * New_ID_old = New_ID;

  x_coordinates = new float [Number_of_Nodes];
  assert( x_coordinates != NULL );
  y_coordinates = new float [Number_of_Nodes];
  assert( y_coordinates != NULL );
  z_coordinates = new float [Number_of_Nodes];
  assert( z_coordinates != NULL );
  Status = new int [Number_of_Nodes];
  assert( Status != NULL );
  New_ID = new int [Number_of_Nodes];
  assert( New_ID != NULL );
  
  for( i=0 ; i<Number_Nodes_Old ; i++ ){
    x_coordinates[i] = x_old[i];
    y_coordinates[i] = y_old[i];
    z_coordinates[i] = z_old[i];
    Status[i] = Status_old[i];
    New_ID[i] = New_ID_old[i];
  }

  delete [] x_old;
  delete [] y_old;
  delete [] z_old;
  delete [] Status_old;
  delete [] New_ID_old;

  for( i=0 ; i<Num_New_Nodes ; i++ ) {
    hs->get_new_pair( &nodeh1 , &nodeh2 , &nodesh );
    x_coordinates[ nodesh - 1 ] = 0.5*( x_coordinates[ nodeh1 - 1] +
                                        x_coordinates[ nodeh2 - 1] );
    y_coordinates[ nodesh - 1 ] = 0.5*( y_coordinates[ nodeh1 - 1] +
                                        y_coordinates[ nodeh2 - 1] );
    z_coordinates[ nodesh - 1 ] = 0.5*( z_coordinates[ nodeh1 - 1] +
                                        z_coordinates[ nodeh2 - 1] );
    Status[ nodesh - 1 ] = active;
    New_ID[ nodesh - 1 ] = nodesh;
  }
}


void Node::get_coordinates( int Node_Id , float * x , float * y , float * z ){

  int ID = Node_Id - 1;
  *x = x_coordinates[ ID ];
  *y = y_coordinates[ ID ];
  *z = z_coordinates[ ID ];

}

void Node::set_coordinates( int Node_Id , float * x , float * y , float * z ){

  int ID = Node_Id - 1;
  x_coordinates[ ID ] = *x;
  y_coordinates[ ID ] = *y;
  z_coordinates[ ID ] = *z;

}

int Node::get_new_id( int Old_ID ){

  if( Old_ID > 0 && Old_ID <= Number_of_Nodes ){
    return( New_ID[Old_ID-1] );
  }
  else{
    std::cerr << "Invalid ID requested in Node::get_new_id\n";
    exit(1);
    return(1);
  }
}

void Node::update_new_id( void ){

  int i;
  int * new_id_old = New_ID;
  
// Start from 1 for exodus numbering
  int ID = 1;
  for( i=0 ; i<Number_of_Nodes ; i++ )
    if( Status[i] == active ) New_ID[i]=ID++;

// Now set the New_ID for those nodes that will be removed
  for( i=0 ; i<Number_of_Nodes ; i++ ){
    ID = new_id_old[i]-1;
    assert( ID >= 0 && ID < Number_of_Nodes);
    ID = New_ID[ID];
    assert( ID >0 && ID <= Number_of_Nodes);
    if( Status[i] == deactive ) New_ID[i]=ID;
  }
}


void Node::compress_nodes( void ){

  int i;

// Count the number of Active Nodes
  int Active_Nodes=0;
  for( i=0 ; i<Number_of_Nodes ; i++ )
    if( Status[i] == active ) Active_Nodes++;

  float * x_old = x_coordinates;
  float * y_old = y_coordinates;
  float * z_old = z_coordinates;
  int * Status_old = Status;
  int * New_ID_old = New_ID;

// Allocate the new memory
  x_coordinates = new float [Active_Nodes];
  y_coordinates = new float [Active_Nodes];
  z_coordinates = new float [Active_Nodes];
  Status = new int [Active_Nodes];
  New_ID = new int [Active_Nodes];

// Copy information to new arrays
  int index = 0;
  for( i=0 ; i<Number_of_Nodes ; i++ ){
    if( Status_old[i] == active ){
      x_coordinates[index] = x_old[i];
      y_coordinates[index] = y_old[i];
      z_coordinates[index] = z_old[i];
      Status[index] = active;
      New_ID[index] = index+1;
      index++;
    }
  }
  assert( index == Active_Nodes );

  Number_of_Nodes = Active_Nodes;

  delete [] x_old;
  delete [] y_old;
  delete [] z_old;
  delete [] Status_old;
  delete [] New_ID_old;

}



void Node::delete_node( int ID ){

  Status[ID-1] = deactive;

}



void Node::merge_nodes_delete( int N1 , int N2 , int N3 ){
  
  Status[N2-1] = deactive;
  Status[N3-1] = deactive;
  New_ID[N2-1] = New_ID[N1-1];
  New_ID[N3-1] = New_ID[N1-1];

}


void Node::merge_nodes_set( int Num_Nodes_to_Merge,int * Nodes_to_Merge ){

  combine_nodes( Num_Nodes_to_Merge,Nodes_to_Merge );
  for( int i=1;i<Num_Nodes_to_Merge;i++ ){
    x_coordinates[Nodes_to_Merge[i]-1]=x_coordinates[Nodes_to_Merge[0]-1];
    y_coordinates[Nodes_to_Merge[i]-1]=y_coordinates[Nodes_to_Merge[0]-1];
    z_coordinates[Nodes_to_Merge[i]-1]=z_coordinates[Nodes_to_Merge[0]-1];
  }
}


void Node::merge_nodes_average( int Num_Nodes_to_Merge,int * Nodes_to_Merge, 
			int Num_Nodes_to_Average,int * Nodes_to_Average ){

  float x,y,z;
  x=y=z=(float) 0;
  combine_nodes(Num_Nodes_to_Merge, Nodes_to_Merge);
  get_average( Num_Nodes_to_Average,Nodes_to_Average,&x,&y,&z );
  for( int i=0;i<Num_Nodes_to_Merge;i++ ){
    x_coordinates[Nodes_to_Merge[i]-1] = x;
    y_coordinates[Nodes_to_Merge[i]-1] = y;
    z_coordinates[Nodes_to_Merge[i]-1] = z;
  }
}



void Node::get_average( int Number_to_Average,int * Nodes_to_Average,
                      float * xcoord,float * ycoord,float * zcoord){

  *xcoord = 0;
  *ycoord = 0;
  *zcoord = 0;

  for( int i=0;i<Number_to_Average;i++ ){
    *xcoord+=x_coordinates[Nodes_to_Average[i]-1]; 
    *ycoord+=y_coordinates[Nodes_to_Average[i]-1]; 
    *zcoord+=z_coordinates[Nodes_to_Average[i]-1];
  }
  *xcoord /= (float) Number_to_Average; 
  *ycoord /= (float) Number_to_Average; 
  *zcoord /= (float) Number_to_Average;

}


void Node::combine_nodes( int Num_Nodes_to_Merge,int * Nodes_to_Merge){

  int i,j;

  // Check to ensure we don't have the same node in the list multiple times
  int Same_Node = 0;
  for( i=0;i<Num_Nodes_to_Merge;i++ ){
    for ( j=i+1;j<Num_Nodes_to_Merge;j++ ){
      if( Nodes_to_Merge[i] == Nodes_to_Merge[j] ) 
        Same_Node = 1;
    }
  }
  if( Same_Node == 1 ){
    std::cerr << "Warning Duplicate IDs in Node::combine_nodes\n";
    exit(1);
  }

  // Find the minimum ID
  int MIN_ID = Nodes_to_Merge[0];
  for( i=1;i<Num_Nodes_to_Merge;i++ )
    MIN_ID = MIN_ID > Nodes_to_Merge[i] ? Nodes_to_Merge[i] : MIN_ID;

  // Combine the nodes
  for( i=0;i<Num_Nodes_to_Merge;i++ ){
    if( MIN_ID != Nodes_to_Merge[i] ){
      Status[Nodes_to_Merge[i]-1] = deactive;
      New_ID[Nodes_to_Merge[i]-1] = New_ID[MIN_ID-1];
    }
  }
}
