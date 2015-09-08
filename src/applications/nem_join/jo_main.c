/*--------------------------------------------------------------------------*/
/* Purpose: Parallel I/O utility for SALSA.                                 */
/*                                                                          */
/*    This program takes the output that has been written to parallel       */
/* Exodus II files, combines it, and writes it to an existing Exodus II     */
/* FEM database file.                                                       */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "jo_const.h"

#include "rf_allo.h"
#include "rf_comm.h"

#include "el_geom_const.h"
#include "el_geom.h"

#include "rf_io_const.h"
#include "ps_pario_const.h"
#include "rf_io.h"
#include "ps_pario.h"

#include "rf_mp.h"

#include "rf_fem_const.h"
#include "rf_fem.h"

#include "rf_util.h"

#include "jo_map_const.h"
#include "jo_map.h"

#include "exodusII.h"

#if defined(USE_MPI)
#include <mpi.h>
#endif

#include "add_to_log.h"

int main(int argc, char *argv[])
{
/* Local declarations. */
  static char  yo[] = "nem_join";

  double       g_start_t, g_end_t, start_t, end_t;

  char        *cmd_file;

  int          io_ws_sc;     /* I/O word size for scalar exodusII file */
  int          i;
  int          num_qa, num_info;
  int          res_exoid;   /* ID for scalar exodusII results file */

  void        *time_vals;    /* time values for the results */

/***************************** BEGIN EXECUTION ******************************/

#if defined(USE_MPI)
  MPI_Init(&argc, &argv);
#endif

  g_start_t = second();

  /* Determine Processor Number and size of Parallel Machine */
  get_parallel_info(&Proc, &Num_Proc, &Dim);

  /* Scan the command line arguments for a version flag */
  for(i=0; i < argc; i++) {
    if(strcmp(argv[i],"-V") == 0) {
      printf("%s version %s\n", UTIL_NAME, VER_STR);
      exit(0);
    }
  }

  /* Interpret the command line */
  switch(argc)
  {
  case 1:
    cmd_file = "nem_join.inp";
    break;

  case 2:
    cmd_file = argv[1];
    break;

  default:
    fprintf(stderr, "%s MAIN: ERROR in command line,", yo);
    if(Proc == 0)
    {
      fprintf(stderr, " usage:\n");
      fprintf(stderr, "\tnem_join [salsa command file] [<-p Proc> ");
      fprintf(stderr, "<-r raid #>]");
    }
    exit(1);
    break;
  }

  if(Proc == 0) {
    printf("%s version %s\n", UTIL_NAME, VER_STR);
    printf("%s: Total number of Processors = %d\n\n",
           yo, Num_Proc);
  }

  /* initialize some variables */
  ExoFile[0]			= '\0';
  Exo_LB_File[0]		= '\0';
  Exo_Res_File[0]		= '\0';
  Debug_Flag			= -1;
  Gen_Flag			= -1;

  Restart_Info.Num_Times	= -1;  /* default is all time steps */
  Restart_Info.NVar_Node_Out    = -1;  /* default is all nodal variables */
  Restart_Info.NVar_Elem_Out    = -1;  /* default is all elemental variables */

  Num_Nod_Var			= -1;
  Num_Elem_Var			= -1;
  Num_Glob_Var			= -1;

  PIO_Info.Dsk_List_Cnt		= -1;
  PIO_Info.Num_Dsk_Ctrlrs	= -1;
  PIO_Info.PDsk_Add_Fact	= -1;
  PIO_Info.Zeros		= -1;
  PIO_Info.Par_Dsk_Root[0]	= '\0';
  PIO_Info.Par_Dsk_SubDirec[0]	= '\0';
  PIO_Info.Staged_Writes[0]	= '\0';

  Proc_Info[0]			= -1;

  /* Read in the ASCII input file from the front end. */
  if(Proc == 0) {
    printf("%s: Reading the command file, %s\n", yo, cmd_file);
    if(read_pexoII_info(cmd_file) < 0)
    {
      fprintf(stderr,"%s ERROR: Could not read in the the I/O command file"
              " \"%s\"!\n", yo, cmd_file);
      exit(1);
    }

    if (!check_inp()) {
      fprintf(stderr, "%s ERROR: Error in user specified parameters.\n", yo);
      exit(1);
    }
  }

  /*
   * If the number of processors is greater than 1 then broadcast the
   * needed information read by processor 0 from the ASCII command
   * files to the other processors.
   */
  if(Num_Proc > 1)
    brdcst_command_info();
  else {
    strcpy(PIO_Info.Scalar_Exo_File_Name, ExoFile);
    strcpy(PIO_Info.Par_Exo_Res_File_Name, Par_Nem_File_Name);
  }

  /*
   * read initial global information from processor 0 file,
   * and broadcast it to all of the processors. Then read
   * initial information from each parallel file.
   */
  if (Proc == 0)
    printf("%s: Reading initial parameters from parallel files.\n\n", yo);

  /* read and broadcast from proc0 file */
  brdcst_init_global(&io_ws_sc, &time_vals, &num_qa, &num_info);

  /* If debug is on the turn on netCDF/Exodus information as well */
  if(Debug_Flag > 0)
     ex_opts(EX_VERBOSE);
  else
    ex_opts(0);

  /* now read from each parallel file */
  read_init_proc(io_ws_sc);

  if (Proc == 0) {
    start_t = second ();
    printf ("%s: Creating/Copying scalar results file, %s.\n",
            yo, Exo_Res_File);
    if (setup_scalar_file(&res_exoid, io_ws_sc, num_qa, num_info) < 0) {
      fprintf(stderr, "%s: Error returned while creating scalar results"
                      "  file.\n", yo);
      exit(1);
    }
    end_t   = second () - start_t;
    printf ("%s:\t\tTime to create/copy scalar file: %f (sec.)\n\n",
            yo, end_t);
  }

  /* get and write out the global element and node maps */
  if (Proc == 0)
    printf("%s: Getting the global maps.\n", yo);
  start_t = second ();
  get_global_maps(res_exoid);
  end_t   = second () - start_t;
  if (Proc == 0)
    printf("%s:\t\tTime to get global maps: %f (sec.)\n\n", yo, end_t);

  /* If a mesh file was not provided, then build one */
  if (!Gen_Flag) {
    if (Proc == 0)
      printf("\n\n%s: Generating scalar mesh file.\n", yo);
    start_t = second ();
    put_mesh(res_exoid, io_ws_sc);
    end_t   = second () - start_t;
    if (Proc == 0)
      printf("\n%s:\t\tTime to generate mesh file: %f (sec.)\n\n", yo, end_t);
  }


  /*
   * check to make sure that there are results, and that
   * the user doesn't just want the mesh to be generated
   */
  if (Restart_Info.Num_Times > 0) {
    /* put the results in the mesh file */
    if (Proc == 0)
      printf("%s: Putting results in mesh file.\n", yo);
    start_t = second ();
    put_results(res_exoid, io_ws_sc, time_vals);
    end_t   = second () - start_t;
    if (Proc == 0)
      printf("\n%s:\t\tTime to put results: %f (sec.)\n\n", yo, end_t);

    if (Proc == 0) {
      safe_free((void **) &time_vals);
    }

  }

  /* now close the results file */
  if (Proc == 0)
    if(ex_close (res_exoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing scalar results file\n",
               yo);
      exit(1);
    }


  if (Proc == 0)
    printf ("%s: write of parallel exodus complete\n", yo);

  g_end_t = second() - g_start_t;

  g_start_t = gsum_double(g_end_t, Proc, Num_Proc);

  g_end_t = g_start_t / (float)Num_Proc;

  if(Proc == 0)
    printf("The average run time was: %.2fs\n", g_end_t);

#if defined(USE_MPI)
  MPI_Finalize();
#endif

  add_to_log(argv[0], g_end_t);
  exit (0);

}
