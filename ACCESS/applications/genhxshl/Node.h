// $Id: Node.h,v 1.8 1998/06/26 20:53:03 khbrown Exp $
// Node.h //

#ifndef _NODE_H_
#define _NODE_H_

class Hexshell;

enum{ active,deactive };

class Node {
 private:
  float * x_coordinates;
  float * y_coordinates;
  float * z_coordinates;
  char ** coordinate_names;
  int Number_of_Nodes;
  int Number_of_Dimensions;
  int * Status;
  int * New_ID;
 public:
  Node(void);
  Node( int EXOid , int Num_Nodes , int Num_Dim );
  ~Node(void);
  int get_number_nodes();
  void write_exodus( int EXOid );
  void Add_Nodes( Hexshell * hs ); 
  void get_coordinates( int node_id , float * x , float * y , float * z);
  void set_coordinates( int node_id , float * x , float * y , float * z);
  int get_new_id( int Old_ID );
  void update_new_id( void );
  void compress_nodes( void );
  void combine_nodes( int , int*  );
  void delete_node( int ID );
  void merge_nodes_delete( int N1 , int N2 , int N3 );
  void merge_nodes_average( int,int*,int,int*);
  void merge_nodes_set( int,int *);
  void get_average( int,int*,float*,float*,float*);
};

#endif
