#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "exodusII.h"

#include "jo_const.h"
#include "el_geom_const.h"
#include "ps_pario_const.h"
#include "rf_allo.h"
#include "rf_comm.h"
#include "rf_mp_const.h"
#include "rf_io_const.h"
#include "jo_map_const.h"
#include "pe_str_util_const.h"


static int get_var_info (int, int, void **);
static int get_eb_info (int);
static int get_ns_info (int);
static int get_ss_info (int);
static int get_qa_records (int, int *);
static int get_info_records (int, int *);

extern void check_exodus_error (int, int error, char *);

extern char Proc0File[];  /* proc 0 filename, defined in jo_check_inp.c */

/************ R O U T I N E S   I N   T H I S   F I L E ***********************
*
*  Name_of_Routine              type                 Called by
*  ---------------------------------------------------------------
*
*  brdcst_init_global ()                       main:jo_main.c
*  get_var_info ()               int                brdcst_init_global
*  get_eb_info ()                int                brdcst_init_global
*  get_ns_info ()                int                brdcst_init_global
*  get_ss_info ()                int                brdcst_init_global
*  get_qa_records ()             int                brdcst_init_global
*  get_info_records ()           int                brdcst_init_global
*
******************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void brdcst_init_global (int *io_ws_sc, void **time_vals,
                         int *num_qa, int *num_info)

/*
 * This function opens the parallel Nemesis file for processor 0
 * and retrieve some information that all of the processors will need.
 */
{

  char  *yo = "brdcst_init_global";
  int    cpu_ws, i, info[16];
  float  ver;
  int pexoid;

/**************************** execution begins *******************************/

  cpu_ws = sizeof(float);
  *io_ws_sc = 0;

  if (Proc == 0) {

    /*
     * open the parallel results file in order to find
     * the precision; the precision of the scalar results
     * file will be based on the precision of the parallel
     * results files
     */

    pexoid = ex_open (Proc0File, EX_READ, &cpu_ws, io_ws_sc, &ver);

    if (pexoid == -1) {
      fprintf (stderr,
               "%s: ERROR openning up the proc 0 exoII results file, %s\n", yo,
               Proc0File);
      exit (1);
    }

    /* close the processor 0 file */
    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing proc 0 exoII results"
               " file\n", yo);
      exit(1);
    }
  

    /*
     * need a check in case we are on a machine that has
     * double precision floats (ie - CRAY) and have a parallel
     * file that has single precision floats
     */
    if (*io_ws_sc < sizeof(float)) {
      fprintf (stderr,
               "%s: Fatal: Precision of parallel file less than precision of"
               " machine.\n", yo);
      exit (1);
    }

    /* now set the cpu word size and re-open the results file */
    cpu_ws = *io_ws_sc;

    pexoid = ex_open (Proc0File, EX_READ, &cpu_ws, io_ws_sc, &ver);
    if (pexoid == -1) {
      fprintf (stderr,
               "%s: ERROR openning up the proc 0 exoII results file, %s\n", yo,
               Proc0File);
      exit (1);
    }

    /* read the global initialization parameters */
    check_exodus_error(Proc,ex_get_init_global (pexoid, &Num_Node, &Num_Elem,
						&Num_Elem_Blk, &Num_Node_Set,
						&Num_Side_Set),
                       "ex_get_init_global");

    /* need global element block information */
    if (get_eb_info(pexoid) < 0) {
      fprintf (stderr, "%s: ERROR: Error while getting global element block"
               " information.\n", yo);
      exit(1);
    }

    /* get other global information if mesh file not supplied */
    if (!Gen_Flag) {
      if (get_ns_info(pexoid) < 0) {
        fprintf (stderr, "%s: ERROR: Error while getting global node set"
                 " information.\n", yo);
        exit(1);
      }

      if (get_ss_info(pexoid) < 0) {
        fprintf (stderr, "%s: ERROR: Error while getting global side set"
                 " information.\n", yo);
        exit(1);
      }
    }

    /* get the variable information */
    if (get_var_info(pexoid, *io_ws_sc, time_vals) < 0) { 
      fprintf (stderr, "%s: ERROR: Error while getting initial variable"
               " information.\n", yo);
      exit(1);
    }

    /* now get the QA records */
    if (get_qa_records(pexoid, num_qa) < 0) {
      fprintf (stderr, "%s: ERROR: Error while getting QA records.\n", yo);
      exit(1);
    }

    /* and the Info records */
    if (get_info_records(pexoid, num_info) < 0) {
      fprintf (stderr, "%s: ERROR: Error while getting Info records.\n", yo);
      exit(1);
    }

    /* close the processor 0 file for now */
    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing proc 0 exoII results"
               " file\n", yo);
      exit(1);
    }


#ifdef DEBUG
    if (Debug_Flag > 4) {
      printf ("\nKey definitions from parallel exodus file (%s)\n",
              Proc0File);
      printf ("\tNumber of nodes           = %d\n",   Num_Node);
      printf ("\tNumber of elements        = %d\n",   Num_Elem);
      printf ("\tNumber of element blocks  = %d\n",   Num_Elem_Blk);
      printf ("\tNumber of node sets       = %d\n",   Num_Node_Set);
      printf ("\tNumber of side sets       = %d\n\n", Num_Side_Set);
      printf ("\tNumber of processors      = %d\n\n", Proc_Info[0]);
    }
#endif

    /* now set up the information to pass to the other processors */
    info[0]  = *io_ws_sc;
    info[1]  = Num_Node;
    info[2]  = Num_Elem;
    info[3]  = Num_Elem_Blk;
    info[4]  = Num_Node_Set;
    info[5]  = Num_Side_Set;
    info[6]  = Proc_Info[0];
    info[7]  = Restart_Info.Num_Times;
    info[8]  = Restart_Info.NVar_Glob;
    info[9]  = Restart_Info.NVar_Node;
    info[10] = Restart_Info.NVar_Elem;
    info[11] = Gen_Flag;
    info[12] = Debug_Flag;
    info[13] = Subset_Flag;
    info[14] = Restart_Info.NVar_Node_Out;
    info[15] = Restart_Info.NVar_Elem_Out;
  }

  brdcst (Proc, Num_Proc, (char *) info, (16 * sizeof(int)), 0);

  *io_ws_sc              = info[0];
  Num_Node               = info[1];
  Num_Elem               = info[2];
  Num_Elem_Blk           = info[3];
  Num_Node_Set           = info[4];
  Num_Side_Set           = info[5];
  Proc_Info[0]           = info[6];
  Restart_Info.Num_Times = info[7];
  Restart_Info.NVar_Glob = info[8];
  Restart_Info.NVar_Node = info[9];
  Restart_Info.NVar_Elem = info[10];
  Gen_Flag               = info[11];
  Debug_Flag             = info[12];
  Subset_Flag 		 = info[13];
  Restart_Info.NVar_Node_Out = info[14];
  Restart_Info.NVar_Elem_Out = info[15];

  /* if reading a subset, get the time indices */
  if (Subset_Flag) {

    if (Proc != 0) {
      Restart_Info.Time_Ind  = (int *) array_alloc(__FILE__, __LINE__, 1,
                                         Restart_Info.Num_Times, sizeof(int));
    }

    brdcst (Proc, Num_Proc, (char *) Restart_Info.Time_Ind,
            (Restart_Info.Num_Times * sizeof(int)), 0);
  }

  /* share the nodal variable index list */
  if ((Proc != 0) && (Restart_Info.NVar_Node_Out > 0)) {
    Restart_Info.NVar_Index  = (int *) array_alloc(__FILE__, __LINE__, 1,
                                           Restart_Info.NVar_Node_Out, 
                                           sizeof(int));
    if(!(Restart_Info.NVar_Index)) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }
  }

  brdcst (Proc, Num_Proc, (char *) Restart_Info.NVar_Index,
          (Restart_Info.NVar_Node_Out * sizeof(int)), 0);

  /* share the elemental variable index list */
  if ((Proc != 0) && (Restart_Info.NVar_Elem_Out > 0)) {
    Restart_Info.EVar_Index  = (int *) array_alloc(__FILE__, __LINE__, 1,
                                           Restart_Info.NVar_Elem_Out, 
                                           sizeof(int));
    if(!(Restart_Info.EVar_Index)) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }
  }

  brdcst (Proc, Num_Proc, (char *) Restart_Info.EVar_Index,
          (Restart_Info.NVar_Elem_Out * sizeof(int)), 0);

  if (Proc != 0) {
    EB_Cnts = (int *) array_alloc(__FILE__, __LINE__, 1, (2 * Num_Elem_Blk),
                                  sizeof(int));
    if (!EB_Cnts) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
    EB_Ids = EB_Cnts + Num_Elem_Blk;
  }

  brdcst (Proc, Num_Proc, (char *) EB_Cnts,
          (2 * Num_Elem_Blk * sizeof(int)), 0);

  /* if there isn't a mesh file, then all processors need this info */
  if (!Gen_Flag) {
    if (Num_Node_Set > 0) {
      if (Proc != 0) {
        Proc_NS_Ids = (int **) array_alloc(__FILE__, __LINE__, 2, 1,
                                           Num_Node_Set, sizeof(int));
        if (!Proc_NS_Ids) {
          fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
          exit (1);
        }
      }

      brdcst (Proc, Num_Proc, (char *) Proc_NS_Ids[0],
              (Num_Node_Set * sizeof(int)), 0);
    }

    if (Num_Side_Set > 0) {
      if (Proc != 0) {
        Proc_SS_Ids = (int **) array_alloc(__FILE__, __LINE__, 2, 1,
                                           Num_Side_Set, sizeof(int));
        if (!Proc_SS_Ids) {
          fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
          exit (1);
        }
      }

      brdcst (Proc, Num_Proc, (char *) Proc_SS_Ids[0],
              (Num_Side_Set * sizeof(int)), 0);
    }

  }

  /* Calculate which processor is responsible for what */
  Proc_Info[2] = Proc_Info[0] / Num_Proc;
  Proc_Info[3]  = Proc_Info[0] % Num_Proc;
  if(Proc <= (Proc_Info[3]-1))
    Proc_Info[2]++;

  Proc_Ids = (int *)array_alloc(__FILE__, __LINE__, 1, Proc_Info[2],
                                sizeof(int));

  for(i=0; i < Proc_Info[2]; i++)
    Proc_Ids[i] = Proc + i*Num_Proc;

  /* Generate the processor to disk map */

  gen_disk_map(&PIO_Info, Proc_Info, Proc, Num_Proc);

  return;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int get_var_info (int pexoid, int word_size, void **time_vals)

/*
 * This function retrieves the variable information from the
 * processor 0 parallel file. This information is used to
 * check the results file, if it already has result information
 * in it, or to setup variable information in it, if it does
 * not.
 *
 * Use the Restart_Info struct to hold the variable information.
 */
{
  char   *yo = "get_var_info"; 

  int     i, j;

  int     num_times = 0;

  int     nvars = 0;

/**************************** execution begins *******************************/

  /* Get the number of time indices contained in the file */
  if (ex_inquire(pexoid, EX_INQ_TIME, &num_times, NULL,
                 NULL) < 0) {
    fprintf(stderr, "%s: Could not get number of time steps from file\n", yo);
    return -1;
  }

  /*
   * check to see if there are results in the file
   * if there are none, then check if a mesh is being generated
   */
  if (num_times <= 0) {
    if (!Gen_Flag) {
      fprintf(stderr, "%s: warning: No results found; just generating mesh.\n",
              yo);
      return 0;
    }
    else { /* if not generating mesh, this is an error */
      fprintf(stderr, "%s: fatal: No results found in the parallel files.\n",
              yo);
      return -1;
    }
  }

  if (Restart_Info.Num_Times == -1) {

    /* user wants to read all the time indices */
    Restart_Info.Num_Times = num_times;
  }
  else if (Restart_Info.Num_Times == -2) {
    /* user wants to read the last time index only */
    Restart_Info.Num_Times = 1;
    Restart_Info.Time_Ind[0] = num_times;
    Subset_Flag = 1;
  }
  else {
    /* user wants to read a subset of time indices */
    Subset_Flag = 1;
  }

  /* now get the time values -- more efficient to get all time values,
     even if reading a subset of time step results */
  *time_vals = (void *) array_alloc(__FILE__, __LINE__, 1,
                                    num_times, word_size);

  if (!(*time_vals)) {
    fprintf(stderr, "Insufficient memory\n");
    exit(1);
  }

  if (ex_get_all_times(pexoid, *time_vals) < 0) {
    fprintf(stderr, "%s: Could not get the time values from file\n", yo);
    return -1;
  }

  /***************** Global Variables ********************/
  if (ex_get_var_param(pexoid, "g", &(Restart_Info.NVar_Glob)) < 0) {
    fprintf(stderr, "%s: Could not get global variable parameter from file\n",
            yo);
    return -1;
  }

  /* allocate space for the global variable names */
  if (Restart_Info.NVar_Glob > 0) {
    Restart_Info.GV_Name = (char **) array_alloc(__FILE__, __LINE__, 2,
                                                 Restart_Info.NVar_Glob,
                                                 MAX_STR_LENGTH+1,
                                                 sizeof(char));
    if (!Restart_Info.GV_Name) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }


    /* get the global variable names */
    if (ex_get_var_names(pexoid, "g", Restart_Info.NVar_Glob,
                         Restart_Info.GV_Name) < 0) {
      fprintf(stderr, "%s: Could not get global variable names from file\n",
              yo);
      return -1;
    }
  }


  /***************** Elemental Variables ********************/
  if (ex_get_var_param(pexoid, "e", &(Restart_Info.NVar_Elem)) < 0) {
    fprintf(stderr, "%s: Could not get elemental variable param from file\n",
            yo);
    return -1;
  }

  /* allocate space for the elemental variable names */
  if (Restart_Info.NVar_Elem > 0) {

    Restart_Info.EV_Name = (char **) array_alloc(__FILE__, __LINE__, 2,
                                                 Restart_Info.NVar_Elem,
                                                 MAX_STR_LENGTH+1,
                                                 sizeof(char));
    if (!Restart_Info.EV_Name) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }

    /* get the elemental variable names */
    if (ex_get_var_names(pexoid, "e", Restart_Info.NVar_Elem,
                         Restart_Info.EV_Name) < 0) {
      fprintf(stderr, "%s: Could not get elemental variable names from file\n",
              yo);
      return -1;
    }

  }

  /* Allocate the elemental variable index list */
  if (Restart_Info.NVar_Elem_Out > 0) {
      nvars = Restart_Info.NVar_Elem_Out;
  } else {
      nvars = Restart_Info.NVar_Elem;
  }

  if (nvars > 0) {
    Restart_Info.EVar_Index  = (int *) array_alloc(__FILE__, __LINE__, 1,
                                           nvars, sizeof(int));
    if(!(Restart_Info.EVar_Index)) {
        fprintf(stderr, "Insufficient memory\n");
        exit(1);
    }
  }

  /* set up the elemental variable index & output name list */
  if (Restart_Info.NVar_Elem_Out == -1) {

    /* writing all elemental variables */
    if (Restart_Info.NVar_Elem > 0) {

      for (i = 0; i < Restart_Info.NVar_Elem; i ++) {
        Restart_Info.EVar_Index[i] = i + 1;
      }

      Restart_Info.EV_Name_Out = (char **) 
        malloc(Restart_Info.NVar_Elem * (sizeof (char *)));

      for (i = 0; i < Restart_Info.NVar_Elem; i++) {
        Restart_Info.EV_Name_Out[i] = (char *) 
          malloc((MAX_STR_LENGTH+1) * (sizeof (char)));
        if (!Restart_Info.EV_Name_Out[i]) {
          fprintf(stderr, "Insufficient memory\n");
          exit(1);
        }
      }

      if (!Restart_Info.EV_Name_Out) {
        fprintf(stderr, "Insufficient memory\n");
        exit(1);
      }

      for (i = 0; i < Restart_Info.NVar_Elem; i ++) {
        strncpy (Restart_Info.EV_Name_Out[i],
                 Restart_Info.EV_Name[i],
                 string_length(Restart_Info.EV_Name[i])+1);
	Restart_Info.EV_Name_Out[i][MAX_STR_LENGTH] = '\0';
      }
    }

    Restart_Info.NVar_Elem_Out = Restart_Info.NVar_Elem;

  } else {

    /* writing a subset of elemental variables */
    for (i = 0; i < Restart_Info.NVar_Elem_Out; i ++) {

      for (j = 0; j < Restart_Info.NVar_Elem; j++) {
        strip_string(Restart_Info.EV_Name[j], " \t\n\r");
        if (token_case_compare (Restart_Info.EV_Name_Out[i], 
                                Restart_Info.EV_Name[j])) {
          Restart_Info.EVar_Index[i] = j + 1;
          break;
        }
      }
    }
  }

  /******************* Nodal Variables **********************/
  if (ex_get_var_param(pexoid, "n", &(Restart_Info.NVar_Node)) < 0) {
    fprintf(stderr, "%s: Could not get nodal variable param from file\n",
            yo);
    return -1;
  }

  /* allocate space for the nodal variable names */
  if (Restart_Info.NVar_Node > 0) {
    Restart_Info.NV_Name = (char **) array_alloc(__FILE__, __LINE__, 2,
                                                 Restart_Info.NVar_Node,
                                                 MAX_STR_LENGTH+1,
                                                 sizeof(char));
    if (!Restart_Info.NV_Name) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }

    /* get the nodal variable names */
    if (ex_get_var_names(pexoid, "n", Restart_Info.NVar_Node,
                         Restart_Info.NV_Name) < 0) {
      fprintf(stderr, "%s: Could not get nodal variable names from file\n",
              yo);
      return -1;
    }
  }

  /* Allocate the nodal variable index list */
  if (Restart_Info.NVar_Node_Out > 0) {
      nvars = Restart_Info.NVar_Node_Out;
  } else {
      nvars = Restart_Info.NVar_Node;
  }

  if (nvars > 0) {
    Restart_Info.NVar_Index  = (int *) array_alloc(__FILE__, __LINE__, 1,
                                           nvars, sizeof(int));
    if(!(Restart_Info.NVar_Index)) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }
  }

  /* set up the nodal variable index & output name list */
  if (Restart_Info.NVar_Node_Out == -1) {

    /* writing all nodal variables */
    if (Restart_Info.NVar_Node > 0) {

      for (i = 0; i < Restart_Info.NVar_Node; i ++) {
        Restart_Info.NVar_Index[i] = i + 1;
      }

      Restart_Info.NV_Name_Out = (char **) 
        malloc(Restart_Info.NVar_Node * (sizeof (char *)));

      for (i = 0; i < Restart_Info.NVar_Node; i++) {
        Restart_Info.NV_Name_Out[i] = (char *) 
          malloc((MAX_STR_LENGTH+1) * (sizeof (char)));
        if (!Restart_Info.NV_Name_Out[i]) {
          fprintf(stderr, "Insufficient memory\n");
          exit(1);
        }
      }

      if (!Restart_Info.NV_Name_Out) {
        fprintf(stderr, "Insufficient memory\n");
        exit(1);
      }

      for (i = 0; i < Restart_Info.NVar_Node; i ++) {
        strncpy (Restart_Info.NV_Name_Out[i],
               Restart_Info.NV_Name[i],
               string_length(Restart_Info.NV_Name[i])+1);
	Restart_Info.NV_Name_Out[i][MAX_STR_LENGTH] = '\0';
      }
    }

    Restart_Info.NVar_Node_Out = Restart_Info.NVar_Node;

  } else {

    /* writing a subset of nodal variables */
    for (i = 0; i < Restart_Info.NVar_Node_Out; i ++) {

      for (j = 0; j < Restart_Info.NVar_Node; j++) {
        strip_string(Restart_Info.NV_Name[j], " \t\n\r");
        if (token_case_compare (Restart_Info.NV_Name_Out[i], 
                                Restart_Info.NV_Name[j])) {
          Restart_Info.NVar_Index[i] = j + 1;
          break;
        }
      }
    }
  }

#ifdef DEBUG
  if ((Debug_Flag >= 2) && (Proc == 0)) {
    printf("\n\nResult Parameters:\n");
    printf("\tNumber of time indices: %d\n", Restart_Info.Num_Times);
    printf("\tNumber of global variables: %d\n", Restart_Info.NVar_Glob);
    for (cnt = 0; cnt < Restart_Info.NVar_Glob; cnt++)
      printf("\t\tGlobal variable %d: %s\n", (cnt+1),
             Restart_Info.GV_Name[cnt]);
    printf("\tNumber of elental variables: %d\n", Restart_Info.NVar_Elem);
    for (cnt = 0; cnt < Restart_Info.NVar_Elem; cnt++)
      printf("\t\tElemental variable %d: %s\n", (cnt+1),
             Restart_Info.EV_Name[cnt]);
    printf("\tNumber of nodal variables: %d\n", Restart_Info.NVar_Node);
    for (cnt = 0; cnt < Restart_Info.NVar_Node; cnt++)
      printf("\t\tNodal variable %d: %s\n", (cnt+1), Restart_Info.NV_Name[cnt]);  }
#endif


  return 0;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int get_eb_info(int pexoid)

/*
 * This function gets the global element block information
 * from the processor 0 file. This information is only
 * needed if a mesh file is being generated.
 */
{
  char   *yo = "get_eb_info";

/**************************** execution begins *******************************/

  /* safety check */
  if (Num_Elem_Blk <= 0) return 0;

  /* allocate memory */
  EB_Cnts = (int *) array_alloc(__FILE__, __LINE__, 1, (2 * Num_Elem_Blk),
                                sizeof(int));
  if (!EB_Cnts) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }
  EB_Ids = EB_Cnts + Num_Elem_Blk;

  if (ex_get_eb_info_global(pexoid, EB_Ids, EB_Cnts) < 0) {
    fprintf(stderr, "%s: error while trying to read global element "
                    "block information.\n", yo);
    return -1;
  }

  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int get_ns_info(int pexoid)

/*
 * This function gets the global node set information
 * from the processor 0 file. This information is only
 * needed if a mesh file is being generated.
 *
 * the global distribution factor counts are not currently
 * distributed by nem_spread
 */
{
  char   *yo = "get_ns_info";

  int    *hold_df;

/**************************** execution begins *******************************/

  /* safety check */
  if (Num_Node_Set <= 0) return 0;

  /* allocate memory */
  hold_df = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Node_Set,
                                sizeof(int));
  Proc_NS_Ids = (int **) array_alloc(__FILE__, __LINE__, 2, 1, Num_Node_Set,
                                     sizeof(int));
  NS_Cnts = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Node_Set,
                                sizeof(int));
  if ((!hold_df) || (!Proc_NS_Ids) || (!NS_Cnts)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  if (ex_get_ns_param_global(pexoid, Proc_NS_Ids[0], NS_Cnts, hold_df) < 0) {
    fprintf(stderr, "%s: error while trying to read global node set "
                    "information.\n", yo);
    return -1;
  }

  safe_free ((void **) &hold_df);

  return 0;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int get_ss_info(int pexoid)

/*
 * This function gets the global side set information
 * from the processor 0 file. This information is only
 * needed if a mesh file is being generated.
 *
 * the global distribution factor counts are not currently
 * distributed by nem_spread
 */
{
  char   *yo = "get_ss_info";

  int    *hold_df;

/**************************** execution begins *******************************/

  /* safety check */
  if (Num_Side_Set <= 0) return 0;

  /* allocate memory */
  hold_df = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Side_Set,
                                sizeof(int));
  Proc_SS_Ids = (int **) array_alloc(__FILE__, __LINE__, 2, 1, Num_Side_Set,
                                     sizeof(int));
  SS_Cnts = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Side_Set,
                                sizeof(int));
  if ((!hold_df) || (!Proc_SS_Ids) || (!SS_Cnts)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  if (ex_get_ss_param_global(pexoid, Proc_SS_Ids[0], SS_Cnts, hold_df) < 0) {
    fprintf(stderr, "%s: error while trying to read global side set "
                    "information.\n", yo);
    return -1;
  }

  safe_free ((void **) &hold_df);

  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int get_qa_records(int pexoid, int *num_qa)

/*
 * This function gets the QA information from the processor 0
 * file. It assumes that all QA information is saved in this
 * file. It allocates space to hold this and adds the information
 * for this utility.
 */
{
  char   *yo = "get_qa_records";

  char   *ct_ptr, tm_date[30];
  char    qa_date[15], qa_time[10], qa_name[MAX_STR_LENGTH];
  char    qa_vers[10];
  int     length_qa, hold_num, i;
  time_t  time_val;

/**************************** execution begins *******************************/

  /* first find out how many QA records there are in the file */
  if (ex_inquire (pexoid, EX_INQ_QA, &hold_num, NULL, NULL) < 0) {
    fprintf(stderr, "%s: Could not get number of QA records from file\n", yo);
    return -1;
  }

  /*
   * allocate space to hold the records;
   * there will be at least one for this program
   */
  length_qa = 4 * (hold_num + 1);
  QA_Record = (char **)array_alloc(__FILE__, __LINE__, 2, length_qa,
                                   (MAX_STR_LENGTH + 1), sizeof(char));
  if (!QA_Record) {
    fprintf(stderr, "Insufficient memory\n");
    exit(1);
  }

  if (hold_num > 0) {
    /* get the qa records from the parallel file */
    if (ex_get_qa (pexoid, (char *(*)[]) &(QA_Record[0])) < 0) {
      fprintf(stderr, "%s: Could not get QA records from file\n", yo);
      return -1;
    }
  }

  /* Generate a QA record for the utility */
  time_val = time(NULL);
  ct_ptr   = asctime(localtime(&time_val));
  strcpy(tm_date, ct_ptr);

  /* Break string with null characters */
  tm_date[3]  = '\0';
  tm_date[7]  = '\0';
  tm_date[10] = '\0';
  tm_date[19] = '\0';

  sprintf(qa_date, "%s %s %s", &tm_date[8], &tm_date[4], &tm_date[20]);
  sprintf(qa_time, "%s", &tm_date[11]);
  strcpy(qa_name, UTIL_NAME);
  strcpy(qa_vers, VER_STR);

  if(qa_date[strlen(qa_date)-1] == '\n')
    qa_date[strlen(qa_date)-1] = '\0';

  strcpy(QA_Record[4*hold_num],     qa_name);
  strcpy(QA_Record[(4*hold_num)+1], qa_vers);
  strcpy(QA_Record[(4*hold_num)+2], qa_date);
  strcpy(QA_Record[(4*hold_num)+3], qa_time);

  /* increment for the record for this utility */
  hold_num++;

  /* Output QA records to screen */
  if(Debug_Flag >= 7) {
    printf("\n\n[%d]: Number of QA records: %d\n", Proc, hold_num);
    printf("QA Records:\n");
    for(i=0; i < 4*(hold_num); i++) {
      printf("\t%s\n", QA_Record[i]);
    }
  }

  /* set the number of records before returning */
  *num_qa = hold_num;

  return 0;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int get_info_records(int pexoid, int *num_info)

/*
 * This function gets the information records from the processor 0
 * file. It assumes that all of the information records are saved
 * in this file. No information records are stored for this utility.
 */
{
  char   *yo = "get_info_records";

  int     hold_num, i;

/**************************** execution begins *******************************/
  /* Read the Info Records */
  if (ex_inquire(pexoid, EX_INQ_INFO, &hold_num, NULL,  NULL) < 0) {
    fprintf(stderr, "%s: Could not get number of info records from file\n",
            yo);
    return -1;
  }

  if(hold_num > 0) {
    Info_Record = (char **)array_alloc(__FILE__, __LINE__, 2, hold_num,
                                       (MAX_LINE_LENGTH + 1), sizeof(char));
    if (!Info_Record) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }

    if (ex_get_info(pexoid, Info_Record) < 0) {
      fprintf(stderr, "%s: Could not get information records from file\n", yo);
      return -1;
    }
  }

  /* Output Information records to screen */
  if(Debug_Flag >= 7) {
    printf("\n\n[%d]: Number of information records: %d\n", Proc, hold_num);
    printf("Info Records:\n");
    for(i=0; i < hold_num; i++) {
      printf("\t%s\n", Info_Record[i]);
    }
  }

  /* set the number of records before returning */
  *num_info = hold_num;

  return 0;

}
