/* 
 * Copyright 2007 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Governement
 * retains certain rights in this software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of Sandia Corporation nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/* $Id: main.c,v 1.2 2009/06/10 04:30:12 gdsjaar Exp $ */

/* Aprepro: Algebraic Preprocessor for Text files.
 *
 * Author:  Greg Sjaardema,
 *          Division 1521
 *          Applied Mechanics Division I
 *          Sandia National Laboratories
 *
 * History: 5/01/90: Initial Version
 */

/* NOTE: Must update version number manually; not done via cvs anymore */
static char *qainfo[] =
{
  "Aprepro ",
  "$Date: 2009/06/10 04:30:12 $",
  "Revision: 2.06",
};

#include <stdlib.h>
#include <ctype.h>
#include "getopt.h"
#include "aprepro.h"
#include "y.tab.h"
#include <sys/types.h>
#include <time.h>

/* Default value of comment character */
char comment = '$';
char *include_path = NULL;

int end_on_exit;
int warning_msg = True;
int info_msg = False;
int copyright = False;
int quiet = False;
int debugging = False;
int statistics = False;

extern void yyparse(void);
static void usage(void);
extern void dumpsym(void);
extern void pstats(void);
extern void init_table(char comment);
static void copyright_output(void);
extern FILE *open_file(char *file, char *mode);
extern void add_to_log(char *my_name);

/* The name the program was invoked under, for error messages */
char *myname;

int main (int argc, char *argv[])
{
  char *version_string = "Algebraic Preprocessor (Aprepro)";
  int interactive = False;
  int c;
  time_t time_val;
  struct tm *time_structure;
  char *asc_time;

  static struct option long_options[] =
  {
    {"debug", 0, 0, 'd'},
    {"statistics", 0, 0, 's'},
    {"copyright", 0, 0, 'C'},
    {"comment", 1, 0, 'c'},
    {"version", 0, 0, 'v'},
    {"interactive", 0, 0, 'i'},
    {"include", 0, 0, 'I'},
    {"exit_on", 0, 0, 'e'},
    {"help", 0, 0, 'h'},
    {"nowarning", 0, 0, 'W'},
    {"messages", 0, 0, 'M'},
    {"quiet", 0, 0, 'q'},
    {NULL, 0, NULL, 0}
  };

  int  option_index = 0;
  extern int optind;
  extern char *optarg;

#ifdef DEBUG_MALLOC
  unsigned long histid1, histid2, orig_size, current_size;
  union dbmalloptarg m;
  m.i = M_HANDLE_CORE | M_HANDLE_DUMP;
  dbmallopt (MALLOC_WARN, &m);

  m.i = M_HANDLE_ABORT;
  dbmallopt (MALLOC_FATAL, &m);

  m.str = "malloc_log";
  dbmallopt (MALLOC_ERRFILE, &m);
#endif

  myname = strrchr (argv[0], '/');
  if (myname == NULL)
    myname = argv[0];
  else
    myname++;

/* Process command line options */
  end_on_exit = False;
  while ((c = getopt_long (argc, argv, "c:dDsSvViI:eEwWmMhHCq",
			   long_options, &option_index)) != EOF)
    {
      switch (c)
	{
	case 'c':
	  comment = *optarg;
	  break;

	case 'd':
	case 'D':
	  debugging = True;
	  info_msg = True;
	  warning_msg = True;
	  break;

	case 's':
	case 'S':		/* Print hash statistics */
	  statistics = True;
	  break;

	case 'C':		/* Print copyright message */
	  copyright = True;
	  break;

	case 'v':
	case 'V':
	  fprintf (stderr, "%s: (%s) %s\n", version_string, qainfo[2], qainfo[1]);
	  break;

	case 'i':
	  interactive = True;
	  break;

	case 'I':
	  NEWSTR(include_path, optarg);
	  break;
	  
	case 'e':
	case 'E':
	  end_on_exit = True;
	  break;

	case 'W':
	  warning_msg = False;
	  break;

	case 'q':
	  quiet = True;
	  break;

	case 'M':
	  info_msg = True;
	  break;

	case 'h':
	case 'H':
	  usage();
	  exit(EXIT_SUCCESS);
	  break;
	  
	case '?':
	default:
	  /* getopt will print a message for us */
	  usage ();
	  exit(EXIT_FAILURE);
	  break;
	}
    }

/* Process remaining options.  If '=' in word, then it is of the form
 * var=value.  Set the value.  If '=' not found, process remaining
 * options as input and output files
 */
  while (optind < argc && strchr(argv[optind], '=') && !strchr(argv[optind], '/'))
    {
      char *var, *val;
      double value;
      symrec *s;

      var = argv[optind++];
      val = strchr (var, '=');
      *val++ = '\0';
      if (strchr(val, '"') != NULL) /* Should be a string variable */
      {
        char *pt = strrchr(val, '"');
	val++;
	*pt = '\0';
	s = putsym(var, SVAR);
	NEWSTR(val, s->value.svar);
      }
      else
	{
	  sscanf (val, "%lf", &value);
	  s = putsym (var, VAR);
	  s->value.var = value;
	}
    }
  if (copyright == True)
    copyright_output();
  /* Assume stdin, recopy if and when it is changed */
  yyin = stdin;
  yyout = stdout;

  if (argc > optind)
    {
      yyin = open_file(argv[optind], "r");
      NEWSTR (argv[optind], ap_file_list[0].name);
      SET_FILE_LIST (0, 0, False, 1);
    }
  else
    {
      NEWSTR ("stdin", ap_file_list[0].name);
      SET_FILE_LIST (0, 0, False, 1);
    }
  if (argc > ++optind)
    {
      yyout = open_file(argv[optind], "w");
    }
  else
    /* Writing to stdout */
    {
      if (interactive)
	setbuf (yyout, (char *) NULL);
    }

  time_val = time ((time_t*)NULL);
  time_structure = localtime (&time_val);
  asc_time = asctime (time_structure);

  /* NOTE: asc_time includes \n at end of string */
  if (!quiet)
    fprintf (yyout, "%c Aprepro (%s) %s", comment, qainfo[2], asc_time);

  init_table (comment);
#ifdef DEBUG_MALLOC
  orig_size = malloc_inuse (&histid1);
#endif
  yyparse ();
#ifdef DEBUG_MALLOC
  current_size = malloc_inuse (&histid2);
  if (current_size != orig_size)
    malloc_list (2, histid1, histid2);
  malloc_chain_check (0);
#endif
  if (debugging > 0)
    dumpsym ();
  if (statistics > 0)
    pstats ();
  add_to_log(myname);
  return (EXIT_SUCCESS);
}				/* NOTREACHED */


#define ECHO(s) fprintf(stderr, s)
#define ECHOC(s) fprintf(stderr, s, comment)
static void 
usage (void)
{
  fprintf (stderr,
	   "\nusage: %s [-dsviehMWCq] [-I path] [-c char] [var=val] filein fileout\n",
	   myname);
   ECHO("        --debug or -d: Dump all variables, debug loops/if/endif\n");
   ECHO("   --statistics or -s: Print hash statistics at end of run     \n");
   ECHO("      --version or -v: Print version number to stderr          \n");
   ECHO("--comment or -c  char: Change comment character to 'char'      \n");
   ECHO("  --interactive or -i: Interactive use, no buffering           \n");
   ECHO("      --include or -I: Include path                            \n");
   ECHO("      --exit_on or -e: End when 'Exit|EXIT|exit' entered       \n");
   ECHO("         --help or -h: Print this list                         \n");
   ECHO("      --message or -M: Print INFO messages                     \n");
   ECHO("    --nowarning or -W: Do not print WARN messages	        \n");
   ECHO("    --copyright or -C: Print copyright message                 \n");
   ECHO("        --quiet or -q: Do not anything extra to stdout         \n");
   ECHO("              var=val: Assign value 'val' to variable 'var'  \n\n");
 } 

static void 
copyright_output (void)
{
  ECHOC("%c -------------------------------------------------------------------------\n");
  ECHOC("%c Copyright 2007 Sandia Corporation. Under the terms of Contract\n");
  ECHOC("%c DE-AC04-94AL85000 with Sandia Corporation, the U.S. Governement\n");
  ECHOC("%c retains certain rights in this software.\n");
  ECHOC("%c\n");
  ECHOC("%c Redistribution and use in source and binary forms, with or without\n");
  ECHOC("%c modification, are permitted provided that the following conditions\n");
  ECHOC("%c are met:\n");
  ECHOC("%c\n");
  ECHOC("%c    * Redistributions of source code must retain the above copyright\n");
  ECHOC("%c      notice, this list of conditions and the following disclaimer.\n");
  ECHOC("%c    * Redistributions in binary form must reproduce the above\n");
  ECHOC("%c      copyright notice, this list of conditions and the following\n");
  ECHOC("%c      disclaimer in the documentation and/or other materials provided\n");
  ECHOC("%c      with the distribution.\n");
  ECHOC("%c    * Neither the name of Sandia Corporation nor the names of its\n");
  ECHOC("%c      contributors may be used to endorse or promote products derived\n");
  ECHOC("%c      from this software without specific prior written permission.\n");
  ECHOC("%c\n");
  ECHOC("%c THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n");
  ECHOC("%c 'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n");
  ECHOC("%c LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n");
  ECHOC("%c A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n");
  ECHOC("%c OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\n");
  ECHOC("%c SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT\n");
  ECHOC("%c LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,\n");
  ECHOC("%c DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\n");
  ECHOC("%c THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n");
  ECHOC("%c (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\n");
  ECHOC("%c OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n");
  ECHOC("%c -------------------------------------------------------------------------\n");
  ECHOC("%c\n");
 } 
 
/* 
 * Copies the numerical portion of the version string into the 'vstring' variable
 * Assumes that vstring is large enough.
 */
void version(char *vstring)
{
/* 
 * NOTE: There is a problem if the version drops to a single digit since, for example,
 * the string "1.9" will compare GREATER than "1.10". This also affects versions > 99.
 */
	int i;
	int j = 0;
	
	for (i=0; i < strlen(qainfo[2]); i++) {
		if (isdigit((int)qainfo[2][i]) || '.' == qainfo[2][i]) {
			vstring[j++] = qainfo[2][i];
		}
	}
	vstring[j] = '\0';
}
 
/*-dummy to get version updated
 *-debug for ifs and loops
 *-added nint
 *-added include(string_variable)
 *-fixed last fix
 *-change date() and time() functions to get_date() and get_time()
 *-add exodus capability
 *-add exodus timestep capability
 *-can now compile without exodusII capability
 *-update lex.yy.c and y.tab.c to remove alloca problems
 *-update statistic calculations
 *-use stdc function definitions
 *-add parsing of escaped braces
 *-change to use mkstemp instead of tmpnam
 *-units changes
 *-minor change to extern char comment
 *-fix ifdef loop control to correctly handle strings
 *-cygwin/no_cygwin_option mkstemp/mkstemps fix
 *-add ternary operator for string expressions
 *-fix so getenv with null string doesn't seg fault
 *-add rand_normal, rand_lognormal, rand_weibull
 *-fix parsing of \{ and \} in skipped if block
 */
