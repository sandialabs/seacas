// $Id: Hexshell.h,v 1.5 1998/05/20 22:48:54 khbrown Exp $
// Hexshell Class for Hexshells //

#ifndef _HEXSHELL_H_
#define _HEXSHELL_H_

class Node;

class Hexshell{

 private:
  int Number_of_Nodes;
  int * Hex_Node1;
  int * Hex_Node2;
  int * HS_Node;
  int Memory_Length;
  int Number_of_HS_Pairs;
  int Number_of_New_Nodes;
  void Increase_Memory( void );
 public:
  Hexshell(void);
  Hexshell( int NNOD , int Initial_Memory_Length = 1000 );
  ~Hexshell(void);
  int Add_Node( int Node1 , int Node2 );
  int get_number( void );
  int get_number_of_new_nodes( void );
  void get_pair( int i , int* nodeh1 , int* nodeh2 , int* nodesh );
  void get_new_pair( int * nodeh1 , int * nodeh2 , int * nodesh );
  int check_pair( int Node1 , int Node2 );
  void update_ids( Node * nodes );
};
#endif
