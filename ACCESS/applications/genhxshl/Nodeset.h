// $Id: Nodeset.h,v 1.5 2004/03/19 15:39:45 gdsjaar Exp $
// Nodeset.h //
#ifndef _NODESET_H_
#define _NODESET_H_

#include <iostream>
#include "Hexshell.h"
#include "Node.h"

class Nodeset
{
 private:
  int number_nodes;
  int number_df;
  int * nodeset_list;
  int status;
  int nodeset_id;
  float * nodeset_df;
 public:
  Nodeset(void);
  Nodeset( int EXOid , int ns_id );
  ~Nodeset(void);
  int get_status(void);
  void set_status( int stat );
  void write_exodus( int EXOid );
  void add_hexshell_nodes( int Number_of_Nodes , Hexshell * hs );
  friend std::ostream& operator<<(std::ostream& os, const Nodeset & ns);
  void update_ids( Node * nodes );
};
#endif
