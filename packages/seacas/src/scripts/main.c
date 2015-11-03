/* $Id: main.c,v 1.48 2008/02/07 15:27:53 gdsjaar Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include "getopt.h"

char *getenv();

int
main (argc, argv)
     int argc;
     char **argv;
{
  int error = 0;
  int c;
  int digit_optind = 0;

  while (1)
    {
      int this_option_optind = optind ? optind : 1;
      int option_index = 0;

#define NO_ARG 0
#define IS_ARG 1
#define OP_ARG 2

      static struct option long_options[] =
      {
        {"EXOII",         IS_ARG, 0, 0},
        {"EXOII1",        IS_ARG, 0, 0},
        {"EXOII2",        IS_ARG, 0, 0},           
        {"IEEE",          NO_ARG, 0, 0},
        {"Include",       IS_ARG, 0, 0},
        {"Library_path",  IS_ARG, 0, 0},
        {"MANUAL",        NO_ARG, 0, 0},
        {"Material",      IS_ARG, 0, 0},
        {"ROOTDIR",       IS_ARG, 0, 0},
        {"SUBDIR",        IS_ARG, 0, 0},
        {"aprepro",       OP_ARG, 0, 0},
        {"background",    NO_ARG, 0, 0},
        {"command",       IS_ARG, 0, 0},
        {"concat",        NO_ARG, 0, 0},
        {"config",        IS_ARG, 0, 0},
        {"constraint",    IS_ARG, 0, 0},
        {"cpus",          IS_ARG, 0, 0},
        {"debug",         OP_ARG, 0, 0},
        {"device",        IS_ARG, 0, 0},
        {"dimension",     IS_ARG, 0, 0},
        {"distributed",   IS_ARG, 0, 0},
        {"double",        NO_ARG, 0, 0},
        {"dump",          IS_ARG, 0, 0},
        {"epu",           NO_ARG, 0, 0},
        {"executable",    IS_ARG, 0, 0},
        {"external",      IS_ARG, 0, 0},
	{"exodus",        IS_ARG, 0, 0},
	{"force",         NO_ARG, 0, 0},
	{"ftp",           NO_ARG, 0, 0},
	{"passive",       NO_ARG, 0, 0},
        {"grepos",        NO_ARG, 0, 0},
        {"hardcopy",      IS_ARG, 0, 0},
        {"help",          NO_ARG, 0, 0},
        {"input",         IS_ARG, 0, 0},
        {"interpolated",  IS_ARG, 0, 0},
        {"list",          IS_ARG, 0, 0},
        {"machine",       IS_ARG, 0, 0},
        {"mesh",          IS_ARG, 0, 0},
        {"mesh_opt",      IS_ARG, 0, 0},
	{"nem_join",      NO_ARG, 0, 0},
	{"nem_slice_flag",IS_ARG, 0, 0},
        {"neutral",       IS_ARG, 0, 0},
        {"noexecute",     NO_ARG, 0, 0},
        {"nographics",    NO_ARG, 0, 0},
        {"nomotif",       NO_ARG, 0, 0},
        {"nresults",      NO_ARG, 0, 0},
        {"offset",        IS_ARG, 0, 0},
        {"output",        IS_ARG, 0, 0},
        {"parallel",      OP_ARG, 0, 0},        
        {"plot",          IS_ARG, 0, 0},
        {"prefix",        IS_ARG, 0, 0},
        {"processors",    IS_ARG, 0, 0},
        {"radiation",     IS_ARG, 0, 0},
        {"raids",         IS_ARG, 0, 0},
        {"renumber",      NO_ARG, 0, 0},        
        {"restart",       IS_ARG, 0, 0},
        {"rsin",          IS_ARG, 0, 0},
        {"rsout",         IS_ARG, 0, 0},
        {"scalar_results",IS_ARG, 0, 0},
	{"scp",           NO_ARG, 0, 0},
        {"shutdown",      IS_ARG, 0, 0},
	{"single",        NO_ARG, 0, 0},
	{"slit",          NO_ARG, 0, 0},
	{"spread",        NO_ARG, 0, 0},
        {"step",          IS_ARG, 0, 0},
        {"subroutine",    IS_ARG, 0, 0},
	{"suffix_mesh",   IS_ARG, 0, 0},
	{"suffix_spread", IS_ARG, 0, 0},
	{"suffix_transient",IS_ARG, 0, 0},
	{"summary",       IS_ARG, 0, 0},
        {"thermal",       IS_ARG, 0, 0},
        {"time",          IS_ARG, 0, 0},
        {"transform",     IS_ARG, 0, 0},
        {"update",        IS_ARG, 0, 0},
        {"userplot",      IS_ARG, 0, 0},
        {"verbose",       NO_ARG, 0, 0},
        {"v",             NO_ARG, 0, 0}, /* Shortcut for verbose */
        {"viewfactor",    IS_ARG, 0, 0},
        {"volume_sources",IS_ARG, 0, 0},
        {"xcode",         IS_ARG, 0, 0},
	{"multikl",       NO_ARG, 0, 0}, /* Following are loadbal options.*/
	{"spectral",      NO_ARG, 0, 0},
	{"inertial",      NO_ARG, 0, 0},
	{"linear",        NO_ARG, 0, 0},
	{"random",        NO_ARG, 0, 0},
	{"rcb",           NO_ARG, 0, 0},
	{"rib",           NO_ARG, 0, 0},
	{"hsfc",          NO_ARG, 0, 0},
	{"scattered",     NO_ARG, 0, 0},
	{"zpinch",        NO_ARG, 0, 0},
	{"brick",         NO_ARG, 0, 0},
	{"No_scalar_mesh",NO_ARG, 0, 0},
        {"No_subdirectory",NO_ARG, 0, 0},
        {"No_01",          NO_ARG, 0, 0},
        { 0,              0,      0, 0}
      };

      c = getopt_long_only (argc, argv, "0123456789",
                       long_options, &option_index);
      if (c == EOF)
        break;

      switch (c)
	{
	case 0:
	  printf (" -%s", (long_options[option_index]).name);
	  if (optarg)
	    {
	    printf (" %s", optarg);
	  }
	  break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  if (digit_optind != 0 && digit_optind != this_option_optind)
	    printf ("digits occur in two different argv-elements.\n");
	  digit_optind = this_option_optind;
	  printf ("option %c\n", c);
	  break;

	case '?':
	default:
	  error++;
/* 	  printf ("?? getopt returned character code 0%o ??\n", c); */
	}
    }

  printf (" -- ");

  if (optind < argc)
    {
      while (optind < argc)
	printf ("%s ", argv[optind++]);
      printf ("\n");
    }
  if (error)
    exit (1);
  else
    exit (0);
}

