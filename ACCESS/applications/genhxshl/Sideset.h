// $Id: Sideset.h,v 1.5 2004/03/19 15:39:45 gdsjaar Exp $
// Sideset.h //
#ifndef _SIDESET_H_
#define _SIDESET_H_

#include <iostream>
class Sideset 
{
 private:
   int number_sides;
   int number_df;
   int * element_list;
   int * side_list;
   float * side_df;
   int status;
 public:
   int sideset_id;

Sideset();
Sideset( int EXOid , int SS_ID );
~Sideset();
void write_exodus( int EXOid );
void set_status( int stat );
int get_status( void );
int get_number( void );
int get_element( int Num );
int get_side( int Num );
void set_new_face_id( int Num , int New_ID );
void set_new_elem_id( int Num , int New_ID );
friend std::ostream& operator<<(std::ostream & os , const Sideset & ss);
};
#endif
