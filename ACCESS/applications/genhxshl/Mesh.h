// $Id: Mesh.h,v 1.8 1998/06/25 20:46:55 khbrown Exp $
// Mesh.h //

#ifndef _MESH_H_
#define _MESH_H_

#include "Element_Block.h"
#include "Nodeset.h"
#include "Sideset.h"
#include "Mesh.h"
#include "Node.h"
#include "Hexshell.h"
#include "QA_Records.h"
#include "Info_Records.h"
#include "HSjoint.h"

class Mesh
{
 private:
  int Number_of_Dimensions;
  int Number_of_Nodes;
  int Number_of_Elements;
  int Number_of_Element_Blocks;
  int Number_of_Node_Sets;
  int Number_of_Side_Sets;
  int Number_of_QA_Recs;
  int Number_of_Info_Recs;
  char Title[80+1];
  Node * nodes;
  int * Element_Block_IDs;
  int * Node_Set_IDs;
  int * Side_Set_IDs;
  int * eb_start;
  QA_Records * QA_Recs;
  Info_Records * Info_Recs;   
  Element_Block ** eb_pointers;
  Nodeset ** node_set_pointers;
  Sideset ** side_set_pointers;
  int get_element_block( int element );
  int get_new_element_id( int element );
  HSjoint * HSj;
 public:
Mesh( void );
Mesh( char * Input_Mesh );
~Mesh( void );
int Valid_EB( const int ID);
int Valid_NS( const int ID);
int Valid_SS( const int ID);
void EB_status( const int ID , const int status );
void NS_status( const int ID , const int status );
void SS_status( const int ID , const int status );
void write_exodus( char * OutputFile);
int get_Num_Nodes( void );
int get_Num_EBs( void );
void convert_2_hexshell( int eb_id , int ss_id , Hexshell * hs);
void set_thickness( int eb_id , float thick );
void normalize_hexshell_nodes( Hexshell * hs );
void add_hs_nodes_to_nodeset( Hexshell * hs );
void make_hsjoint( int EB_ID , Hexshell * hs );
void process_hsjoint( Hexshell * hs);
void update_side_sets();
};


#endif
