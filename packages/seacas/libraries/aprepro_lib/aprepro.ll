s"}/* -*- Mode: c++ -*- */

/*
 * Copyright(C) 1999-2025 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */


%{

#include <assert.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stack>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "apr_scanner.h"
#include "aprepro.h"
#include "apr_util.h"
#include "apr_getline.h"
#include "apr_tokenize.h"
#include "fmt/format.h"
#include "fmt/ostream.h"
#include "fmt/ranges.h"

#define register /*as nothing*/
#define YY_NO_UNISTD_H
/* import the parser's token type into a local typedef */
typedef SEAMS::Parser::token token;
typedef SEAMS::Parser::token_type token_type;

/* By default yylex returns int, we use token_type. Unfortunately yyterminate
 * by default returns 0, which is not of token_type. */
#define yyterminate() return token::END

 namespace SEAMS {
   extern bool echo;
   void yyerror(const char *s);
 }

namespace {
  bool begin_double_brace = false;
  bool end_double_brace = false;
  bool string_is_ascii(const char *line, size_t len)
  {
    for (size_t i = 0; i < len; i++) {
      if (!(std::isspace(static_cast<unsigned char>(line[i])) || std::isprint(static_cast<unsigned char>(line[i])))) {
        return false;
      }
    }
    return true;
  }
} // namespace

int file_must_exist = 0; /* Global used by include/conditional include */

/* Global variables used by the looping mechanism */
 SEAMS::file_rec *outer_file = nullptr;
int loop_lvl = 0;
std::fstream *tmp_file;
const char  *temp_f;

#if defined __NVCC__
#pragma diag_suppress code_is_unreachable
#endif

#define MAX_IF_NESTING 1024

 int if_state[MAX_IF_NESTING] = {0}; // INITIAL
 int if_case_run[MAX_IF_NESTING] = {false}; /* Has any if or elseif condition executed */
 int if_lvl = 0;
 int if_skip_level = 0;
bool suppress_nl = false;
 bool switch_active = false;   // Are we in a switch
 bool switch_case_run = false; // has there been a case which matched condition run?
 bool switch_skip_to_endcase = false;
 double switch_condition = 0.0; // Value specified in "switch(condition)"

// For substitution history
int curr_index = 0;
std::string history_string;
size_t hist_start = 0;

#define YY_USER_ACTION curr_index += yyleng;

%}
/*** Flex Declarations and Options ***/

/* enable c++ scanner class generation */
%option c++

/* change the name of the scanner class. results in "SEAMSFlexLexer" */
%option prefix="SEAMS"

/* enable scanner to generate debug output. disable this for release
 * versions. */
%option debug

/* enables the use of start condition stacks */
%option stack

qstring \"[^\"\n]*[\"\n]
mlstring \'[^\']*[\']
COM   "#"|"$"|"//"
D     [0-9]
E     [Ee][+-]?{D}+
L     [A-Za-z_]
id    {L}({L}|{D}|:)*
WS    [ \t\f]*
NL    "\n"
number {D}*\.({D}+)?({E})?
integer {D}+({E})?

%START PARSING GET_FILENAME IF_SKIP GET_VAR VERBATIM IF_WHILE_SKIP GET_LOOP_VAR LOOP LOOP_SKIP END_CASE_SKIP

%%
<VERBATIM>{
  "{VERBATIM(OFF)}" { BEGIN(INITIAL);   }
  "{APREPRO(ON)}".*"\n" { echo = true; BEGIN(INITIAL);   }
  [A-Za-z0-9_ ]* |
    .               { if (echo) ECHO; }
    "\n"            { if (echo) ECHO; aprepro.ap_file_list.top().lineno++; }
}

<INITIAL>{
  /* Aprepro "comment" -- skip entire line */
  "{#}".*"\n"       { yytext++; yytext[1]=' '; if (echo) ECHO; aprepro.ap_file_list.top().lineno++; }
  
  "{VERBATIM(ON)}"   { BEGIN(VERBATIM);  }
  "{APREPRO(OFF)}"   { echo = false; BEGIN(VERBATIM);  }
  {WS}"{ECHO}" |
  {WS}"{ECHO(ON)}"          { echo = true;      }
  {WS}"{NOECHO}" |
  {WS}"{ECHO(OFF)}"         { echo = false;     }

  {WS}"{IMMUTABLE(ON)}"     { aprepro.stateImmutable = true;    }
  {WS}"{IMMUTABLE(OFF)}"            { aprepro.stateImmutable = aprepro.ap_options.immutable; }

  (?i:{WS}"{loop"{WS}"(") {
    BEGIN(GET_LOOP_VAR);
  }
}

<GET_LOOP_VAR>{
  .+")}".*"\n"  {
    aprepro.ap_file_list.top().lineno++;
    /* `yytext` includes closing `)}` and newline...  Strip these */
    char *pt = strchr(yytext, ')');
    *pt = '\0';
    auto tokens = tokenize(yytext, " ,\t");
    if (aprepro.ap_options.debugging) {
      fmt::print(stderr, "DEBUG LOOP: tokens = {}\n",
		 fmt::join(tokens, ", "));
    }

    /* Determine if the first token is a symbol or an explicit number... */
    const auto &count = tokens[0];
    bool all_dig = count.find_first_not_of("0123456789") == std::string::npos;
    int loop_iterations = 0;
    if (all_dig) {
      loop_iterations = std::stoi(count);
    }
    else {
      symrec *s;
      if (!check_valid_var(tokens[0].c_str())) {
	aprepro.warning("Invalid variable name syntax '" + tokens[0] + "'");
	BEGIN(LOOP_SKIP);
      } else {
	s = aprepro.getsym(tokens[0]);
	if (s == nullptr || (s->type != token::SVAR && s->type != token::IMMSVAR && s->value.var == 0.)) {
	  BEGIN(LOOP_SKIP);
	}
	else {
	  loop_iterations = (int)s->value.var;
	}
      }
    }

    if (loop_iterations <= 0) {
      BEGIN(LOOP_SKIP);
      if (aprepro.ap_options.debugging) {
	fmt::print(
		   stderr,
		   "DEBUG LOOP: iteration count = {}, Skipping loop...\n", loop_iterations);
      }
    }
    else {
      temp_f = get_temp_filename();
      SEAMS::file_rec new_file(temp_f, 0, true, loop_iterations);
      outer_file = &aprepro.ap_file_list.top();
      new_file.loop_level = outer_file->loop_level + 1;

      // Get optional loop index...
      std::string sym_name;
      if (tokens.size() == 1) {
	// Default loop index variable name if not specified in loop command.
	sym_name = fmt::format("__loop_{}", new_file.loop_level);
      }
      else {
	sym_name = tokens[1];
      }
      SEAMS::symrec *li = aprepro.getsym(sym_name);
      if (li == nullptr) {
	li = aprepro.putsym(sym_name, SEAMS::Aprepro::SYMBOL_TYPE::VARIABLE, true);
      }

      // Get optional loop index initial value.  Default to 0 if not specified.
      double init = 0.0;
      if (tokens.size() >= 3) {
	init = std::stod(tokens[2]);
      }
      li->value.var       = init;

      // Get optional loop index increment value.  Default to 1 if not specified.
      if (tokens.size() >= 4) {
	double increment = std::stod(tokens[3]);
	new_file.loop_increment = increment;
      }

      new_file.loop_index = li;
      aprepro.ap_file_list.push(new_file);

      tmp_file = new std::fstream(temp_f, std::ios::out);
      loop_lvl++;
      BEGIN(LOOP);
      aprepro.isCollectingLoop = true;
      if (aprepro.ap_options.debugging) {
	fmt::print(stderr, "DEBUG LOOP: iteration count = {}, loop_index variable = {}, initial value = {}, increment = {}\n",
		   loop_iterations, sym_name, init, new_file.loop_increment);
      }
    }
  }
}

<LOOP>{
  (?i-s:{COM}*{WS}"{endloop".*"\n") {
    outer_file->lineno++;
    if(loop_lvl > 0)
      --loop_lvl;

    if (loop_lvl == 0) {
      BEGIN(INITIAL);
      tmp_file->close();
      delete tmp_file;

      if(!aprepro.doLoopSubstitution)
        yy_push_state(VERBATIM);

      aprepro.isCollectingLoop = false;

      yyin = aprepro.open_file(aprepro.ap_file_list.top().name, "r");
      yyFlexLexer::yypush_buffer_state (yyFlexLexer::yy_create_buffer( yyin, YY_BUF_SIZE));
      curr_index = 0;
    }
    else {
      (*tmp_file) << yytext;
    }
  }

  (?i-s:{COM}*{WS}"{loop"{WS}"(".*"\n")  {
    loop_lvl++; /* Nested Loop */
    (*tmp_file) << yytext;
    outer_file->lineno++;
  }

  (?i-s:{COM}*{WS}"{abortloop".*"\n") {
    if(aprepro.ap_options.interactive ||
       aprepro.string_interactive())
    {
      aprepro.warning("Aborting loop(s).", false);

      // Leave the looping state and remove the loop file
      BEGIN(INITIAL);
      tmp_file->close();
      delete tmp_file;

      if(aprepro.ap_file_list.top().tmp_file) {
        remove(aprepro.ap_file_list.top().name.c_str());
        aprepro.ap_file_list.pop();
      }

      loop_lvl = 0;
      aprepro.isCollectingLoop = false;
    }
  }

  .*"\n" {
    (*tmp_file) << yytext;
    outer_file->lineno++;
  }
}

<LOOP_SKIP>{
  (?i-s:{COM}*{WS}"{endloop".*"\n") {
    aprepro.ap_file_list.top().lineno++;
    if(loop_lvl > 0)
      --loop_lvl;

    if (loop_lvl == 0) {
      BEGIN(INITIAL);
      aprepro.isCollectingLoop = false;
    }
  }

  (?i-s:{COM}*{WS}"{loop"{WS}"(".*"\n") {
    loop_lvl++; /* Nested Loop */
    aprepro.ap_file_list.top().lineno++;
  }

  (?i-s:{COM}*{WS}"{abortloop".*"\n") {
    if(aprepro.ap_options.interactive ||
       aprepro.string_interactive())
    {
      aprepro.warning("Aborting loops(s).", false);

      // Leave the looping state
      BEGIN(INITIAL);

      loop_lvl = 0;
      aprepro.isCollectingLoop = false;
    }
  }

  .*"\n" { /* Do not increment line count */
    ;
  }
}

<END_CASE_SKIP>{WS}"{"{WS}"case".*"\n"  {
  yyless(0);
  curr_index = 0;
  BEGIN(INITIAL);
  switch_skip_to_endcase = false;
}

<INITIAL,END_CASE_SKIP>{WS}"{"{WS}"default"{WS}"}".*"\n"     {
 if (!switch_active) {
    yyerror("default statement found outside switch statement.");
  }

  if (!switch_case_run) {
    switch_case_run = true;
    BEGIN(INITIAL);
    switch_skip_to_endcase = false;
    if (aprepro.ap_options.debugging)
      fprintf (stderr, "DEBUG SWITCH: 'default' code executing at line %d\n",
               aprepro.ap_file_list.top().lineno);
  }
  else {
    if (aprepro.ap_options.debugging)
      fprintf (stderr, "DEBUG SWITCH: 'default' not executing since a previous case already ran at line %d\n",
               aprepro.ap_file_list.top().lineno);

    /* Need to skip all code until end of case */
    BEGIN(END_CASE_SKIP);
  }
}

<END_CASE_SKIP>{WS}"{"{WS}"endswitch"{WS}"}".*"\n"        {
  aprepro.ap_file_list.top().lineno++;
  BEGIN(INITIAL);
  switch_active = false;
  switch_skip_to_endcase = false;
  suppress_nl = false;
  if (aprepro.ap_options.debugging)
    fprintf (stderr, "DEBUG SWITCH: 'endswitch' at line %d\n",
             aprepro.ap_file_list.top().lineno);
}

<END_CASE_SKIP>.*"\n" {  aprepro.ap_file_list.top().lineno++; }

<INITIAL>{WS}"{"{WS}"endswitch"{WS}"}".*"\n"        {
  aprepro.ap_file_list.top().lineno++;
  if (!switch_active) {
    yyerror("endswitch statement found without matching switch.");
  }
  switch_active = false;
  switch_skip_to_endcase = false;
}

<INITIAL>{
  /* This restores the old behavior of ifdef and ifndef
   * where they would eat up any leading whitespace on
   * a line.
   */
  (?i:{WS}"{ifdef"{WS}"(") {
    // Used to avoid undefined variable warnings in old ifdef/ifndef construct
    aprepro.inIfdefGetvar = true;
    unput('(');
    unput('f');
    unput('e');
    unput('d');
    unput('f');
    unput('i');
    unput('_');
    unput('{');
    curr_index = 0;
  }

  (?i:{WS}"{ifndef"{WS}"(") {
    // Used to avoid undefined variable warnings in old ifdef/ifndef construct
    aprepro.inIfdefGetvar = true;
    unput('(');
    unput('f');
    unput('e');
    unput('d');
    unput('n');
    unput('f');
    unput('i');
    unput('_');
    unput('{');
    curr_index = 0;
  }

  (?i:{COM}*{WS}"{"{WS}"if"{WS}"(") {
    // This lets us strip leading optional '#' and spaces
    unput('(');
    unput('f');
    unput('i');
    unput('_');
    unput('{');
    curr_index = 0;
  }

  (?i:{COM}*{WS}"{"{WS}"elseif"{WS}"(") {
    // This lets us strip leading optional '#' and spaces
    unput('(');
    unput('f');
    unput('i');
    unput('e');
    unput('s');
    unput('l');
    unput('e');
    unput('_');
    unput('{');
    curr_index = 0;
  }
}

<IF_WHILE_SKIP>{
  /* If an if was found while skipping, then eat
   * that entire if block until endif
   * found since there is no way that
   * any of the code in that if block could be executed.
   * Make sure to handle multiple levels of skipped ifs...
   *
   * NOTE: if_lvl was not incremented, so don't need to decrement when
   *       endif found.
   */
  (?i-s:{COM}*{WS}"{endif}".*"\n")     {
    aprepro.ap_file_list.top().lineno++;
    if (--if_skip_level == 0)
      BEGIN(IF_SKIP);
  }

  (?i-s:{WS}"{ifdef"{WS}"(".*"\n")  {
    aprepro.ap_file_list.top().lineno++;
    if_skip_level++;
  }

  (?i-s:{COM}*{WS}"{"{WS}"if"{WS}"(".*"\n")  {
    aprepro.ap_file_list.top().lineno++;
    if_skip_level++;
  }

  (?i-s:{WS}"{ifndef"{WS}"(".*"\n") {
    aprepro.ap_file_list.top().lineno++;
    if_skip_level++;
  }

  .*"\n" {
    aprepro.ap_file_list.top().lineno++;
  }
}

<IF_SKIP>{
  /* IF an if, ifdef, or ifndef found while skipping, then
   * skip the entire block up and including the endif.
   * The (IF_WHILE_SKIP) start condition handles this skipping.
   */
  (?i-s:{WS}"{ifdef"{WS}"(")  {
    if (aprepro.ap_options.debugging)
      fprintf (stderr, "DEBUG IF: 'ifdef'  found while skipping at line %d\n",
               aprepro.ap_file_list.top().lineno);
    if_skip_level = 1;
    BEGIN(IF_WHILE_SKIP);
  }

  (?i-s:{COM}*{WS}"{"{WS}"if"{WS}"(")  {
    if (aprepro.ap_options.debugging)
      fprintf (stderr, "DEBUG IF: 'if'  found while skipping at line %d\n",
               aprepro.ap_file_list.top().lineno);
    if_skip_level = 1;
    BEGIN(IF_WHILE_SKIP);
  }

  (?i-s:{WS}"{ifndef"{WS}"(") {
    if (aprepro.ap_options.debugging)
      fprintf (stderr, "DEBUG IF: 'ifndef'  found while skipping at line %d\n",
               aprepro.ap_file_list.top().lineno);
    if_skip_level = 1;
    BEGIN(IF_WHILE_SKIP);
  }
}

(?i-s:{COM}*{WS}"{else}".*"\n")  {
  aprepro.ap_file_list.top().lineno++;
  if (aprepro.ap_options.debugging)
    fprintf (stderr, "DEBUG IF: 'else'   at level = %d at line %d\n",
             if_lvl, aprepro.ap_file_list.top().lineno);
  if(YY_START == VERBATIM) {
    if(echo) ECHO;
  }
  else if (if_state[if_lvl] == IF_SKIP) {
    if (!if_case_run[if_lvl]) {
      BEGIN(INITIAL);
      if_state[if_lvl] = INITIAL;
      if_case_run[if_lvl] = true;
    } else {
      BEGIN(IF_SKIP);
      if_state[if_lvl] = IF_SKIP;
    }
  }
  else if (if_state[if_lvl] == INITIAL) {
    BEGIN(IF_SKIP);
    if_state[if_lvl] = IF_SKIP;
  }

  /* If neither is true, this is a nested
     if that should be skipped */
}

<IF_SKIP>{
  (?i-s:{COM}*{WS}"{"{WS}"elseif".*"\n")  {
    /* If any previous 'block' of this if has executed, then
     * just skip this block; otherwise see if condition is
     * true and execute this block
     */
    if (aprepro.ap_options.debugging)
      fprintf (stderr, "DEBUG IF: 'elseif'   at level = %d at line %d\n",
               if_lvl, aprepro.ap_file_list.top().lineno);

    if (if_case_run[if_lvl]) { /* A previous else/elseif has run */
      aprepro.ap_file_list.top().lineno++;
      /* Already in IF_SKIP, so don't need to change state */
    } else {
      /* Need to check the elseif condition; push back and parse */
      yyless(0);
      curr_index = 0;
      BEGIN(INITIAL);
      if_state[if_lvl] = INITIAL;
    }
  }

  [A-Za-z0-9_ ]* |
   \\\{          |
   \\\}          |
   .                 { ; }

   "\n" {
     aprepro.ap_file_list.top().lineno++;
   }
}

(?i-s:{COM}*{WS}"{endif}".*"\n")     {

    if(YY_START == VERBATIM) {
      if(echo) ECHO;
    }
    else {
      if (if_state[if_lvl] == IF_SKIP ||
          if_state[if_lvl] == INITIAL) {
            BEGIN(INITIAL);
            suppress_nl = false;
      }
                           /* If neither is true, this is a nested
                              if that should be skipped */
      if (aprepro.ap_options.debugging)
        printf ("DEBUG IF: 'endif'  at level = %d at line %d\n",
                if_lvl, aprepro.ap_file_list.top().lineno);
      if (--if_lvl < 0) {
        if_lvl = 0;
        yyerror("Improperly Nested ifdef/ifndef statements");
      }
      /* Ignore endif if not skipping */
    }
    aprepro.ap_file_list.top().lineno++;
  }

<INITIAL>(?i:{WS}"{include"{WS}"(")           { BEGIN(GET_FILENAME);
                             file_must_exist = true; }
<INITIAL>(?i:{WS}"{cinclude"){WS}"("          { BEGIN(GET_FILENAME);
                             file_must_exist = false; }
<GET_FILENAME>.+")"{WS}"}"[^\n]*{NL}  {
  aprepro.ap_file_list.top().lineno++;
  BEGIN(INITIAL);
  {
    symrec *s;
    int quoted = 0;
    char *pt = strchr(yytext, ')');
    *pt = '\0';
    /* Check to see if surrounded by double quote */
    if ((pt = strchr(yytext, '"')) != nullptr) {
      yytext++;
      quoted = 1;
    }
    if ((pt = strrchr(yytext, '"')) != nullptr) {
      *pt = '\0';
      quoted = 1;
    }

    if (quoted == 0) {
      /* See if this is an aprepro variable referring to a name */
      s = aprepro.getsym(yytext);
      if (s == nullptr || (s->type != token::SVAR && s->type != token::IMMSVAR)) {
        pt = yytext;
      } else {
        pt = (char*)s->value.svar.c_str();
      }
    } else {
      pt = yytext;
    }

    bool added = add_include_file(pt, file_must_exist);

    if(added && !aprepro.doIncludeSubstitution)
      yy_push_state(VERBATIM);

    aprepro.ap_file_list.top().lineno++;
  }
}

<PARSING>{integer}  |
<PARSING>{number}          { sscanf (yytext, "%lf", &yylval->val);
                       return(token::NUM); }

<PARSING>{WS}          ; // Empty rule

<PARSING>{id} {
           symrec *s;
                             s = aprepro.getsym(yytext);
                             if (s == nullptr)
                               s = aprepro.putsym (yytext, SEAMS::Aprepro::SYMBOL_TYPE::UNDEFINED_VARIABLE, false);
                             yylval->tptr = s;
                             return((token::yytokentype)s->type);
                           }
<PARSING>"="               return(token::EQUAL);
<PARSING>"+="              return(token::EQ_PLUS);
<PARSING>"-="              return(token::EQ_MINUS);
<PARSING>"*="              return(token::EQ_TIME);
<PARSING>"/="              return(token::EQ_DIV);
<PARSING>"^="              return(token::EQ_POW);
<PARSING>"**="             return(token::EQ_POW);
<PARSING>"++"              return(token::INC);
<PARSING>"--"              return(token::DEC);
<PARSING>"+"               return(token::PLU);
<PARSING>"-"               return(token::SUB);
<PARSING>"*"               return(token::TIM);
<PARSING>"~"               return(token::TIM);          /* ~ is same as multiply */
<PARSING>"//"              return(token::CONCAT);       /* String concatenation */
<PARSING>"/"               return(token::DIV);
<PARSING>"%"               return(token::MOD);
<PARSING>"^"               return(token::POW);
<PARSING>"**"              return(token::POW);
<PARSING>"\n"              aprepro.ap_file_list.top().lineno++;
<PARSING>"("               return(token::LPAR);
<PARSING>")"               return(token::RPAR);
<PARSING>","               return(token::COMMA);
<PARSING>";"               return(token::SEMI);
<PARSING>":"               return(token::COLON);
<PARSING>"?"               return(token::QUEST);
<PARSING>"<"               return(token::LT);
<PARSING>">"               return(token::GT);
<PARSING>"<="              return(token::LE);
<PARSING>">="              return(token::GE);
<PARSING>"=="              return(token::EQ);
<PARSING>"!="              return(token::NE);
<PARSING>"&&"              return(token::LAND);
<PARSING>"||"              return(token::LOR);
<PARSING>"!"               return(token::NOT);
<PARSING>"["               return(token::LBRACK);
<PARSING>"]"               return(token::RBRACK);
<PARSING>{qstring}         {
           char *pt = strrchr(yytext, '"');
                             *pt = '\0';
                             new_string(yytext+1, &yylval->string);
                             return token::QSTRING; }

<PARSING>{mlstring}        {
           char *pt = strrchr(yytext, '\'');
                             *pt = '\0';
                             new_string(yytext+1, &yylval->string);
                             return token::QSTRING; }

<PARSING>"}" {
  // Add to the history string
  save_history_string();

  if (switch_skip_to_endcase)
    BEGIN(END_CASE_SKIP);
  else
    BEGIN(if_state[if_lvl]);
  return(token::RBRACE);
}


<PARSING>"}}" {
  if (begin_double_brace) {
    end_double_brace = true;
  }
  else {
    yyerror("Found an unexpected double end brace ('}}').\n\t"
            "It can only end an expression started with a double brace ('{{').\n\tCheck syntax.");
  }

  // Add to the history string
  save_history_string();

  if (switch_skip_to_endcase)
    BEGIN(END_CASE_SKIP);
  else
    BEGIN(if_state[if_lvl]);
    unput('}');
    unput('O');
    unput('H');
    unput('C');
    unput('E');
    unput('{');
  return(token::RBRACE);
}


\\\{                      { if (echo) LexerOutput("{", 1); }

\\\}                      { if (echo) LexerOutput("}", 1); }

"{{"  {
    // Check if we need to save the substitution history first.
    if(aprepro.ap_options.keep_history &&
            (aprepro.ap_file_list.top().name != "_string_"))
    {
      if (curr_index > yyleng)
        hist_start = curr_index - yyleng;
      else
        hist_start = 0;
    }

    BEGIN(PARSING);
    echo = false;
    begin_double_brace = true;
    return(token::LBRACE);
  }

"{"  {
    // Check if we need to save the substitution history first.
    if(aprepro.ap_options.keep_history &&
            (aprepro.ap_file_list.top().name != "_string_"))
    {
      if (curr_index > yyleng)
        hist_start = curr_index - yyleng;
      else
        hist_start = 0;
    }

    BEGIN(PARSING);

    return(token::LBRACE);
  }

(?i:exit) |
(?i:quit) {
  if (aprepro.ap_options.end_on_exit) {
    if (echo) ECHO;
    return((token::yytokentype)-1);
  }
  else
    if (echo) ECHO;
}


\$                         { if (echo) ECHO; }


{id} |
.                          { if (echo && if_state[if_lvl] != IF_SKIP) ECHO; }

"\n"                       { if (echo && !suppress_nl) ECHO; suppress_nl = false;
                             if (end_double_brace) {
                                echo = true;
                                begin_double_brace = false;
                                end_double_brace = false;
                             }
                             aprepro.ap_file_list.top().lineno++;
                           }

%%

    /* When the scanner receives an end-of-file indication from YY_INPUT, it then
     * checks the yywrap() function. If yywrap() returns false (zero), then it is
     * assumed that the function has gone ahead and set up `yyin' to point to
     * another input file, and scanning continues. If it returns true (non-zero),
     * then the scanner terminates, returning 0 to its caller. */

    namespace SEAMS
{

  Scanner::Scanner(Aprepro & aprepro_yyarg, std::istream * in, std::ostream * out)
      : SEAMSFlexLexer(in, out), aprepro(aprepro_yyarg)
  {
    aprepro.outputStream.push(out);
  }

  Scanner::~Scanner() {
    while (aprepro.ap_file_list.size() > 1) {
      const auto &kk = aprepro.ap_file_list.top();
      if (kk.name != "STDIN") {
        yyFlexLexer::yy_load_buffer_state();
        delete yyin;
        yyin = nullptr;
      }
      aprepro.ap_file_list.pop();
      yyFlexLexer::yypop_buffer_state();
    };
  }

  bool Scanner::add_include_file(const std::string &filename, bool must_exist)
  {
    std::fstream *yytmp = nullptr;
    if (must_exist)
      yytmp = aprepro.open_file(filename, "r");
    else
      yytmp = aprepro.check_open_file(filename, "r");

    if (yytmp) {
      if (yyin && !yy_init) {
        yyFlexLexer::yypush_buffer_state(yyFlexLexer::yy_create_buffer(yyin, YY_BUF_SIZE));
      }

      yyin = yytmp;
      aprepro.info("Included File: '" + filename + "'", true);

      SEAMS::file_rec new_file(filename.c_str(), 0, false, 0);
      aprepro.ap_file_list.push(new_file);

      yyFlexLexer::yypush_buffer_state(yyFlexLexer::yy_create_buffer(yytmp, YY_BUF_SIZE));
      curr_index = 0;
    }
    return yytmp != nullptr;
  }

  void Scanner::LexerOutput(const char *buf, int size)
  {
    // Do this before writing so that we have the correct index in the
    // output stream.
    if (aprepro.ap_options.keep_history) {
      aprepro.add_history(history_string, buf);
      history_string.clear();
      hist_start = 0;
    }

    aprepro.outputStream.top()->write(buf, size);
    if (aprepro.ap_options.interactive && aprepro.outputStream.size() == 1) {
      // In interactive mode, output to stdout in addition to the
      // output stream, unless user has redirected output...
      std::cout << buf;
    }
  }

  int Scanner::LexerInput(char *buf, int max_size)
  {
    if (yyin->eof() || yyin->fail()) {
      return 0;
    }

    if (aprepro.ap_options.interactive && yyin == &std::cin && isatty(0) != 0 && isatty(1) != 0) {
      char *line = SEAMS::getline_int(nullptr);

      if (strlen(line) == 0) {
        return 0;
      }

      if (!string_is_ascii(line, strlen(line))) {
        aprepro.warning("input line contains non-ASCII (probably UTF-8) characters which might be parsed incorrectly.");
      }

      SEAMS::gl_histadd(line);

      if (strlen(line) > (size_t)max_size - 2) {
        yyerror("input line is too long");
        return 0;
      }

      strcpy(buf, line);
      strcat(buf, "\n");

      return strlen(buf);
    }
    else {
      (void)yyin->read(buf, max_size);

      if (yyin->bad()) {
        return -1;
      }
      else {
        if (!string_is_ascii(buf, yyin->gcount())) {
          aprepro.warning("input file contains non-ASCII (probably UTF-8) characters which will might be parsed incorrectly.");
        }
        return yyin->gcount();
      }
    }
  }

  int Scanner::yywrap()
  {
    // Clear the history string.
    history_string.clear();
    hist_start = 0;
    curr_index = 0;

    // If we are using the string interactive method, we want to return to
    // our original state if parsing was cutoff prematurely.
    if (aprepro.string_interactive() && YY_START == PARSING) {
      if (switch_skip_to_endcase) {
        BEGIN(END_CASE_SKIP);
      }
      else {
        BEGIN(if_state[if_lvl]);
      }
    }

    if (aprepro.ap_file_list.size() <= 1) { /* End of main file, not in nested include */
      return (1);
    }
    else if (aprepro.string_interactive() && loop_lvl) {
      return (1);
    }
    else if (aprepro.isCollectingLoop) {
      yyerror("End-of-file detected inside loop. Check loop syntax. {endloop} must be on line by "
              "itself.");
      return (1);
    }
    else {
      /* We are in an included or looping file */
      if (aprepro.ap_file_list.top().tmp_file) {
        if (aprepro.ap_options.debugging) {
          std::cerr << "DEBUG LOOP: Loop count = " << aprepro.ap_file_list.top().loop_count << "\n";
        }
        if (--aprepro.ap_file_list.top().loop_count <= 0) {
          // On Windows, you can't remove the temp file until all the references to the
          // file object have been released, so we will delete it here.
          delete yyin;
          yyin = nullptr;

          if (aprepro.ap_file_list.top().name != "_string_") {
            if (!aprepro.ap_options.debugging) {
              remove(aprepro.ap_file_list.top().name.c_str()); /* Delete file if temporary */
            }
            if (!aprepro.doLoopSubstitution) {
              yy_pop_state();
            }
          }

          aprepro.ap_file_list.pop();
          yyFlexLexer::yypop_buffer_state();
        }
        else {
          // Do not pop ap_file_list; we are rereading that file...
          delete yyin;
          yyin = nullptr;
          yyFlexLexer::yypop_buffer_state();
          yyin = aprepro.open_file(aprepro.ap_file_list.top().name, "r");
          yyFlexLexer::yypush_buffer_state(yyFlexLexer::yy_create_buffer(yyin, YY_BUF_SIZE));
          aprepro.ap_file_list.top().lineno = 0;

          if (aprepro.ap_file_list.top().loop_index != nullptr) {
            aprepro.ap_file_list.top().loop_index->value.var += aprepro.ap_file_list.top().loop_increment;
          }
        }
      }
      else {
        delete yyin;
        yyin = nullptr;
        aprepro.ap_file_list.pop();
        yyFlexLexer::yypop_buffer_state();

        if (aprepro.ap_file_list.top().name == "standard input") {
          yyin = &std::cin;
        }

        /* Turn echoing back on at end of included files. */
        echo = true;

        // If we are not doing aprepro substitutions for the included file, but
        // just collecting lines, pop the state from VERBATIM back to what it
        // was previously.
        if (!aprepro.doIncludeSubstitution) {
          yy_pop_state();
        }

        /* Set immutable mode back to global immutable
         * state at end of included file*/
        aprepro.stateImmutable = aprepro.ap_options.immutable;
      }

      // Reset the current character index.
      curr_index = 0;
      if (yyin != nullptr) {
        curr_index = yyin->tellg();
      }

      return (0);
    }
  }

  /* Print error message to standard error and return.  Note: internally
   *   'lineno' starts at zero.  To avoid confusion, we add 1 to value
   *   when it is output.
   */

  void Scanner::yyerror(const char *s) { aprepro.error(s); }

  char *Scanner::execute(char string[])
  {
    /* Push the contents of 'string' onto the stack to be reread.
     * 'string' will be surrounded by {} so it must be a valid expression.
     */

    /*
     * NOTE: The closing } has not yet been scanned in the call to execute();
     *       therefore, we read it ourselves using input(), then we push:
     *       '}{' + our string + '}'
     */
    int i;
    while ((i = yyFlexLexer::yyinput()) != '}' && i != EOF)
      curr_index++; /* eat up values */

    // Increment curr_index to account for the '}' and save history
    curr_index++;
    save_history_string();

    /* Allocate space for string + '}' + '{' + end_of_string */
    std::string new_string;
    new_string += "}{";
    new_string += string;
    new_string += "}";

    aprepro.ap_file_list.push(SEAMS::file_rec("_string_", 0, true, -1));

    auto ins = new std::istringstream(new_string); // Declare an input string stream.
    yyFlexLexer::yypush_buffer_state(yyFlexLexer::yy_create_buffer(ins, new_string.size()));
    return (nullptr);
  }

  /* Push the contents of 'string' onto the stack to be reread.
   * 'string' will not be surrounded by {}.
   */

  char *Scanner::rescan(char *string)
  {
    int i;
    /*
     * NOTE: The closing } has not yet been scanned in the call to rescan();
     *       therefore, we read it ourselves using input(), then we push our
     *       string and then put the closing } back on the stack last
     *       (to be read first),
     */
    while ((i = yyFlexLexer::yyinput()) != '}' && i != EOF)
      curr_index++; /* eat up values */

    // Increment curr_index to account for the '}' and save history
    curr_index++;
    save_history_string();

    {
      aprepro.ap_file_list.push(SEAMS::file_rec("_string_", 0, true, -1));
      std::string new_string("}");
      new_string += string;

      auto ins = new std::istringstream(new_string); // Declare an input string stream.
      yyFlexLexer::yypush_buffer_state(yyFlexLexer::yy_create_buffer(ins, new_string.size()));
    }
    return (nullptr);
  }

  char *Scanner::import_handler(char *string)
  {
    /*
     * NOTE: The closing } has not yet been scanned in the call to rescan();
     *       therefore, we read it ourselves using input().
     */
    int i = 0;
    while ((i = yyFlexLexer::yyinput()) != '}' && i != EOF)
      curr_index++; /* eat up values */

    add_include_file(string, true);
    std::string info_string = std::string("Imported File: '") + string + "'";
    aprepro.info(info_string, true);

    if (!aprepro.doIncludeSubstitution) {
      yy_push_state(VERBATIM);
    }

    /*
     * Now we need to push back the closing } so it is the first thing read.
     * We no longer have the initial file stream (is is pushed down on stack)
     * so we need to add a new file stream consisting of just a single character.
     * Wasteful, but best I can come up with at this time.
     */
    aprepro.ap_file_list.push(SEAMS::file_rec("_string_", 0, true, -1));
    std::string new_string("}");
    auto        ins = new std::istringstream(new_string); // Declare an input string stream.
    yyFlexLexer::yypush_buffer_state(yyFlexLexer::yy_create_buffer(ins, new_string.size()));

    if (aprepro.ap_options.debugging) {
      std::cerr << "DEBUG IMPORT: " << string << "\n";
    }
    return (nullptr);
  }

  char *Scanner::if_handler(double x)
  {
    if_lvl++;
    if (if_lvl >= MAX_IF_NESTING) {
      yyerror("Too many nested if statements");
    }
    else {
      if (x == 0) {
        if_state[if_lvl]    = IF_SKIP;
        if_case_run[if_lvl] = false;
      }
      else {
        suppress_nl         = true;
        if_state[if_lvl]    = INITIAL;
        if_case_run[if_lvl] = true;
      }
      if (aprepro.ap_options.debugging) {
        std::cerr << "DEBUG IF: If level " << if_lvl << " " << if_state[if_lvl] << "\n";
      }
    }
    return (nullptr);
  }

  char *Scanner::elseif_handler(double x)
  {
    if (x == 0 || if_case_run[if_lvl]) {
      if_state[if_lvl] = IF_SKIP;
    }
    else {
      suppress_nl         = true;
      if_state[if_lvl]    = INITIAL;
      if_case_run[if_lvl] = true;
    }
    if (aprepro.ap_options.debugging) {
      std::cerr << "DEBUG IF: elseif at level " << if_lvl << " " << if_state[if_lvl] << "\n";
    }
    return (nullptr);
  }

  char *Scanner::switch_handler(double x)
  {
    // save that we are in a switch statement
    // save the value of 'x' for use in deciding which case to execute
    if (switch_active) {
      yyerror("switch statement found while switch already active. Nested switch not supported.");
    }

    switch_active          = true;
    switch_case_run        = false;
    switch_condition       = x;
    switch_skip_to_endcase = true; /* Skip everything until first case */
    suppress_nl            = true;

    if (aprepro.ap_options.debugging) {
      std::cerr << "DEBUG SWITCH: 'switch' with condition = " << switch_condition << " at line "
                << aprepro.ap_file_list.top().lineno << "\n";
    }
    return (nullptr);
  }

  char *Scanner::case_handler(double x)
  {
    // make sure we are in a switch statement
    // if 'x' matches the value saved in the switch statement
    // and no other case has been executed, then
    // execute the code in the case and set a flag indicating
    // the switch has run;
    // if 'x' does not match the value saved, then skip to endcase
    suppress_nl = true;

    if (!switch_active) {
      yyerror("case statement found outside switch statement.");
    }

    if (!switch_case_run && x == switch_condition) {
      switch_case_run = true;
      if (aprepro.ap_options.debugging) {
        fprintf(stderr,
                "DEBUG SWITCH: 'case' condition = %g matches switch condition = %g at line %d\n", x,
                switch_condition, aprepro.ap_file_list.top().lineno);
      }
    }
    else {
      if (aprepro.ap_options.debugging) {
        fprintf(stderr, "DEBUG SWITCH: 'case' condition = %g does not match switch condition = %g "
                        "(or case already matched) at line %d\n",
                x, switch_condition, aprepro.ap_file_list.top().lineno);
      }

      // Need to skip all code until end of case
      switch_skip_to_endcase = true;
    }
    return (nullptr);
  }

  void Scanner::save_history_string()
  {
    if (!aprepro.ap_options.keep_history) {
      return;
    }

    // Don't do it if the file is the one used by execute and rescan.
    if (aprepro.ap_file_list.top().name == "_string_" ||
        aprepro.ap_file_list.top().name == "standard input") {
      return;
    }

    size_t hist_end = curr_index;
    size_t len      = hist_end - hist_start;

    if (len <= 0)
      return;

    // Clear any possible end-of-stream if e.g., reading from a istringstream.
    std::ios::iostate state = yyin->rdstate();
    size_t loc = yyin->tellg();
    yyin->clear();

    // Go back in the stream to where we started keeping history.
    yyin->seekg(hist_start);
    if (!yyin->good()) {
      yyerror("Stream state bad in `save_history_string` seekg");
      return;
    }

    // Read everything up to this point again and save it.
    auto tmp = new char[len + 1];
    yyin->read(tmp, len);
    if (!yyin->good()) {
      yyerror("Stream state bad in `save_history_string` read");
      return;
    }
    tmp[len] = '\0';

    history_string = tmp;
    delete[] tmp;
    hist_start = 0;

    // restore stream state
    yyin->seekg(loc);
    yyin->setstate(state);
  }
}

/* This implementation of SEAMSFlexLexer::yylex() is required to fill the
 * vtable of the class SEAMSFlexLexer. We define the scanner's main yylex
 * function via YY_DECL to reside in the Scanner class instead. */

#ifdef yylex
#undef yylex
#endif
int SEAMSFlexLexer::yylex()
{
  std::cerr << "in SEAMSFlexLexer::yylex() !" << '\n';
  return 0;
}

/* When the scanner receives an end-of-file indication from YY_INPUT, it then
 * checks the yywrap() function. If yywrap() returns false (zero), then it is
 * assumed that the function has gone ahead and set up `yyin' to point to
 * another input file, and scanning continues. If it returns true (non-zero),
 * then the scanner terminates, returning 0 to its caller. */

int SEAMSFlexLexer::yywrap() { return 1; }
