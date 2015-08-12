// $Id: Element_Block.h,v 1.8 2004/03/19 15:39:45 gdsjaar Exp $
// Element_Block.h //
#ifndef _ELEMENT_BLOCK_H_
#define _ELEMENT_BLOCK_H_
#include <iostream>

#include "Sideset.h"
#include "Hexshell.h"
#include "Node.h"
#include "HSjoint.h"

class Element_Block
{
 private:
  int * element_connectivity;
  int * new_face_id;                     // Used to reset sideset face id
  float * element_attributes;
  int material_id;
  int nodes_per_element; 
  int faces_per_element;
  int attributes_per_element;
  void hs_thickness( Node * nodes );
  int get_element_block( int element );
 public:
  char element_name[80+1];
  Element_Block(void);                        // Default Constructor //
  Element_Block(int EXOid, int material_id);  // Constructor to read block //
  void write_exodus(int EXOid);               // Write block to ExodusII file //
  ~Element_Block(void);                       // Destructor //
  friend std::ostream& operator<<(std::ostream& os, const Element_Block & eb);
  void Make_Hexshell( Sideset * ss , Hexshell * hs ,Node * nodes , int istart );
  int get_num_elements( void );
  int get_new_face_id( int element , int old_face);
  int get_status( void );
  void set_status( int );
  void set_thickness( float thickness );
  void assemble_normals( float * normal , Node * nodes);
  void update_link( Node * nodes );
  void make_hsjoint( Hexshell * hs , HSjoint * HSj );
  int Status;
  int num_elements;
};
#endif
