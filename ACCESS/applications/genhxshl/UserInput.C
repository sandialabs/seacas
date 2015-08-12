// $Id: UserInput.C,v 1.15 2004/03/19 15:39:45 gdsjaar Exp $

#include <iostream>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h> /* for tolower */

#include "Nodeset.h"
#include "Sideset.h"
#include "Mesh.h"
#include "Command.h"

static int case_compare(const char *s, const char *t, int n)
{
  assert(s != NULL && t != NULL);
  while (*s != '\0' && n-- > 0) {
    if (*t == '\0' || tolower(*s++) != tolower(*t++)) {
      return false;
    }
  }
  return n==0 || *s == *t;
}

void strip_word( char* line , char* word , int* start , int* end , int num );
void print_help(char ** , int);

void UserInput( Mesh * mesh1 ) {

#define max_len 80

  int i,elb,ns,ss,end[80],start[80];
  int nwords,valid;
  int Number_of_Invalid_Commands = 0;
  char line[max_len+1];
  static char command[max_len+1] = "Begin";
  const char *space = " ,";
  const char *endofstring = "";
  Command commands( mesh1 );
  

/* 
   Adding a new command requires increasing the number of commands on
   the line below and adding the name of the command to the enum.  You
   should include the format of your command in the usage below so it
   will be described with the help command.  Inside the if-elseif construct
   add the following for the new command

   else if( case_compare( command,comand[________],3 )){

   and insert the necessary logic. 
*/
   
  const int Number_Commands=9;
  enum {help,hexshell,thickness,hsjoint,dlete,normalize,exit,end_,quit} ;

  char * comand[Number_Commands];
  for( i=0;i<Number_Commands;i++ )
    comand[i] = new char [max_len+1];
  strcpy( comand[help]      , "help"      );
  strcpy( comand[hexshell]  , "hexshell"  );
  strcpy( comand[thickness] , "thickness" );
  strcpy( comand[hsjoint]   , "hsjoint"   );
  strcpy( comand[dlete]     , "delete"    );
  strcpy( comand[normalize] , "normalize" );
  strcpy( comand[exit]      , "exit"      );
  strcpy( comand[end_]      , "end"       );
  strcpy( comand[quit]      , "quit"      );

  const int Number_Usage=9;
  char * usage[Number_Usage];
  for( i=0;i<Number_Commands;i++ )
    usage[i] = new char [max_len+1];
  i=0;
  strcpy( usage[i++],"hexshell ELEMENT_BLOCK_ID SIDE_SET_ID" );
  strcpy( usage[i++],"thickness ELEMENT_BLOCK_ID THICKNESS" );
  strcpy( usage[i++],"hsjoint ELEMENT_BLOCK_ID" );
  strcpy( usage[i++],"delete sideset SIDE_SET_ID" );
  strcpy( usage[i++],"delete nodeset NODE_SET_ID" );
  strcpy( usage[i++],"normalize" );
  strcpy( usage[i++],"exit" );
  strcpy( usage[i++],"end" );
  strcpy( usage[i++],"quit" );

  const char *subcom1 = "nodeset";
  const char *subcom2 = "sideset";

  while( std::cin.good() && !case_compare( command,comand[quit],3 )){
    std::cout << "genhxshl> ";
    std::cin.getline( line , max_len );
    
    nwords = 0;
    start[0] = 0;
    for( i=0 ; i<max_len ; i++ ){
      if( char(line[i])==char(space[0]) || char(line[i])==char(space[1]) ){
        end[nwords++] = i-1;
        start[nwords] = i+1;
      }
      if( char(line[i]) == char(endofstring[0]) ){
        end[nwords++] = i-1;
        break;
      }
    }

    strip_word( line , command , start , end , 0 );

    if( strlen(command) == 0 ){
      /* Blank Line: Do Nothing */
    }
    else if( case_compare( command,"$",1 )){
      /* Comment from Aprepro: Do Nothing */
    }
    else if( case_compare( command,comand[help],3 )){
      print_help( usage,Number_Usage );
    }
    else if( case_compare( command,comand[hexshell],3 )){
      valid = 1;
      strip_word( line , command , start , end , 1 );
      elb = atoi( command );
      if( !mesh1->Valid_EB(elb) ){
        std::cout << "Invalid Element Block ID: " << elb << "\n";
        valid = 0;
      }
      strip_word( line , command , start , end , 2 );
      ss  = atoi( command );
      if( !mesh1->Valid_SS(ss) ){
        std::cout << "Invalid Side Set: " << ss << "\n";
        valid = 0;
      }
      if( valid == 1 ){
        commands.store( HEXSHELL , elb , ss );
      }
    }
    else if( case_compare( command,comand[thickness],3 )) {
      valid = 1;
      strip_word( line , command , start , end , 1 );
      elb = atoi( command );
      if( !mesh1->Valid_EB(elb) ){
        std::cout << "Invalid Element Block ID: " << elb << "\n";
        valid = 0;
      }
      if( valid == 1 ){
        strip_word( line , command , start , end , 2 );
        float thick = float( atof( command ) );
        commands.store( THICKNESS , elb, 0 ,thick );
      }
    }
    else if( case_compare( command,comand[hsjoint],3 )) {
      strip_word( line , command , start , end , 1 );
      int elb = int( atoi( command ) );
      commands.store( HSJOINT , elb );
    }
    else if( case_compare( command,comand[dlete],3   )) {
      strip_word( line , command , start , end , 1);
      if( case_compare( command,subcom1,3 )){
        strip_word( line , command , start , end , 2 );
        ns  = atoi( command );
        if( mesh1->Valid_NS(ns) )
          mesh1->NS_status(ns,0);
        else
          std::cout << "Invalid Nodeset : " << ns << "\n";
      }
      else if( case_compare( command,subcom2,3 )){
        strip_word( line , command , start , end , 2 );
        ss  = atoi( command );
        if( mesh1->Valid_SS(ss) )
          mesh1->SS_status(ss,0);
        else
          std::cout << "Invalid Sideset : " << ss << "\n";
      }
      else{
        std::cout << "Invalid Command\n";
      }
    }
    else if( case_compare( command,comand[normalize],3   )) {
// Normalize Command
      commands.store( NORMALIZE );
    }
    else if( case_compare(command,comand[end_],3) || 
             case_compare(command,comand[exit],3)) {
// End or Exit Command
      strcpy( command,comand[quit] );
    }
    else if( !case_compare(command,comand[quit],3)) {
// Invalid Command
      std::cout << "Invalid Command\n";
      Number_of_Invalid_Commands++;
      if( Number_of_Invalid_Commands >= 3 ){
        print_help( usage,Number_Usage);
        Number_of_Invalid_Commands = 0;
      }
    }
  }

// Proccess all commands
  commands.process( mesh1 );
}

void strip_word( char* line , char* word , int* start , int* end , int num)

{
  static char endofstring[1] = "";
  int j = 0;
  for( int i=start[num] ; i<=end[num] ; i++ ) 
    word[j++] = line[i];
  word[j] = endofstring[0];

}
  

void print_help(char ** usage , int Number_Commands )

{

  std::cout << "\n";
  std::cout << " Supported Commands: " << "\n\n";
  for( int i=0 ; i<Number_Commands ; i++ )
    std::cout << "      " << usage[i] << "\n";
  std::cout << "\n NOTES: \n";
  std::cout << "   GENHXSHL Allows all commands to be input with 3 letter abbreviations.\n";
  std::cout << "   All quantities in UPPER CASE are the numerical ID's of\n";
  std::cout << "       the give quantity from the Exodus database.\n\n";

}
