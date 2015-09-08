#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "rf_allo.h"
#include "rf_message.h"

#include "pe_common.h"
#include "el_geom_const.h"
#include "ps_pario_const.h"
#include "rf_io_const.h"
#include "rf_mp_const.h"
#include "rf_util.h"
#include "jo_map_const.h"
#include "jo_util.h"

#include "exodusII.h"

/******************** ROUTINES IN THIS FILE ***********************************
*
*     Name                      Type                  Called By
*   --------                 ---------              ---------------
*
*  put_results			void            main:jo_main.c
*  put_records                  void            main:jo_main.c
*  check_serial_var             int                  put_results
*  gather_elem_tt		int                  put_results
*  read_vars			int                  put_results
*  gather_vars			int                  put_results
*
******************************************************************************/

static int check_serial_var (int , char *, int , char **);
static int gather_elem_tt   (int, int);
static int read_vars        (int, void *, void *, void *, int);
static int gather_vars      (int, int, void *, void *, void *, void *, int);

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

void put_results(int exoid_out, int word_size, void *time_vals)
{
  /* Local Variables */
  char   *yo = "put_results";

  int	  i, index, cnt, vartab=0;
  int     time_step;
  float  *tv_sp;
  double *tv_dp;
  void   *evar_sorted = NULL, *nvar_sorted = NULL;
  void   *global_var = NULL, *evar_gross = NULL, *nvar_gross = NULL;
  void   *time_ptr;

  /*------------------- begin execution -------------------------------------*/

  /* setup the results file for the variables */
  if (Proc == 0) {

    /* global variables */
    if (check_serial_var(exoid_out, "global", Restart_Info.NVar_Glob,
                         Restart_Info.GV_Name) < 0) {
      fprintf(stderr, "%s: error returned while checking global variables.\n",
              yo);
      exit (1);
    }

    /* elemental variables
     *
     * if there are elemental variables already defined in the scalar file,
     * then it is expected that there is already an elemental truth table
     * defined.
     */
    if ((vartab = check_serial_var(exoid_out, "elemental",
                                   Restart_Info.NVar_Elem_Out,
                                   Restart_Info.EV_Name_Out)) < 0) {
      fprintf(stderr, "%s: error returned while checking elemental var.\n",
              yo);
      exit (1);
    }

    /* nodal variables */
    if (check_serial_var(exoid_out, "nodal", Restart_Info.NVar_Node_Out,
                         Restart_Info.NV_Name_Out) < 0) {
      fprintf(stderr, "%s: error returned while checking nodal variables.\n",
              yo);
      exit (1);
    }

    /* now free up some memory that is no longer needed */

    /* the variable names */
    safe_free((void **) &(Restart_Info.GV_Name));
    safe_free((void **) &(Restart_Info.EV_Name));
    safe_free((void **) &(Restart_Info.NV_Name));

    for (i = 0; i < Restart_Info.NVar_Node_Out; i++) {
      if (Restart_Info.NV_Name_Out[i]) free (Restart_Info.NV_Name_Out[i]);
    }
    if (Restart_Info.NV_Name_Out) free (Restart_Info.NV_Name_Out);

    for (i = 0; i < Restart_Info.NVar_Elem_Out; i++) {
      if (Restart_Info.EV_Name_Out[i]) free (Restart_Info.EV_Name_Out[i]);
    }
    if (Restart_Info.EV_Name_Out) free (Restart_Info.EV_Name_Out);

  } /* End: "if (Proc == 0)" */

  /*********************************************************/
  /* allocate space for all of the variables, and indecies */
  /*********************************************************/
  if ((Restart_Info.NVar_Glob > 0) && (Proc == 0)) {
    global_var =  array_alloc(__FILE__, __LINE__, 1, Restart_Info.NVar_Glob,
                              word_size);
    if (!global_var) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
  }

  if (Restart_Info.NVar_Node_Out > 0) {

    /* allocate the rest of the storage space */
    nvar_gross = array_alloc(__FILE__, __LINE__, 2, Restart_Info.NVar_Node_Out,
                             Num_Nodes_Gross, word_size);
    nvar_sorted = array_alloc(__FILE__, __LINE__, 2, Restart_Info.NVar_Node_Out,
                              Num_Nodes_Net, word_size);
    if ((!nvar_gross) || (!nvar_sorted)) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
  }

  if (Restart_Info.NVar_Elem_Out > 0) {

    /* allocate the rest of the storage space */
    evar_gross = array_alloc(__FILE__, __LINE__, 2, Restart_Info.NVar_Elem_Out,
                             Num_Elem_Gross, word_size);
    evar_sorted = array_alloc(__FILE__, __LINE__, 2, Restart_Info.NVar_Elem_Out,
                              Num_Elem_Net, word_size);
    if ((!evar_gross) || (!evar_sorted)) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }

    /*
     * if there are elemental variables to be written, then read and gather
     * the elemental truth table information based on the original number of 
     * element variables.
     */
    Restart_Info.GElem_TT = (int *) array_alloc(__FILE__, __LINE__, 1,
                                       (Restart_Info.NVar_Elem * Num_Elem_Blk),
                                                sizeof(int));
    Restart_Info.Elem_TT  = (int **) array_alloc(__FILE__, __LINE__, 2,
                                                 Proc_Info[2],
                                       (Restart_Info.NVar_Elem * Num_Elem_Blk),
                                                 sizeof(int));

    Restart_Info.GElem_TT_Out = (int *) array_alloc(__FILE__, __LINE__, 1,
                                (Restart_Info.NVar_Elem_Out * Num_Elem_Blk),
                                sizeof(int));

    if ((!Restart_Info.GElem_TT) || (!Restart_Info.Elem_TT) ||
        (!Restart_Info.GElem_TT_Out)) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }

    cnt = Restart_Info.NVar_Elem * Num_Elem_Blk;
    for (index = 0; index < cnt; index++)
      Restart_Info.GElem_TT[index] = 0;

    cnt = Restart_Info.NVar_Elem_Out * Num_Elem_Blk;
    for (index = 0; index < cnt; index++)
      Restart_Info.GElem_TT_Out[index] = 0;

    /* gather and write the elemental truth table */
    if (Proc == 0)
      printf("\n\t%s: Gathering elemental truth table.\n", yo);

    if (gather_elem_tt(exoid_out, vartab) < 0) {
      fprintf(stderr, "%s: error ocurred while reconstructing truth table.\n",
              yo);
      exit(1);
    }
  } /* End "if (Restart_Info.NVar_Elem > 0)" */

  /* need to know the precision on the time values */
  if (word_size == sizeof(float)) tv_sp = (float *) time_vals;
  else                            tv_dp = (double *) time_vals;

  /*
   * now read, gather and write the variable information from each
   * parallel file
   */
  for (index = 0; index < Restart_Info.Num_Times; index++) {

    /* Read data from the parallel results files, based on the original */
    /* time step value.  Write data to the scalar results FEM file, based */ 
    /* on the number of time steps in the subset. */
    if (!Subset_Flag) {
      time_step = index+1;
    } else {
      time_step = Restart_Info.Time_Ind[index];
    }

    if (Proc == 0)
      printf("\n\t%s: Reading variables for time step %d.\n", yo, time_step);

    /* Read data from the parallel results files, based on the original */
    /* time step value.  */
    if (read_vars(word_size, global_var, nvar_gross, evar_gross,
                  time_step) < 0) {
      fprintf(stderr, "[%d] %s: Error occurred while reading variables.\n",
              Proc, yo);
      exit(1);
    }

    /* sort and remove duplicates in the nodal variable list */
    if (Restart_Info.NVar_Node_Out > 0)
      sort_map_2d_void(word_size, Nodal_Sort_Map, nvar_gross, nvar_sorted,
                       Num_Nodes_Net, Restart_Info.NVar_Node_Out);

    /* sort and remove duplicates in the elemental variable list */
    if (Restart_Info.NVar_Elem_Out > 0)
      sort_map_2d_void(word_size, Elem_Sort_Map, evar_gross, evar_sorted,
                       Num_Elem_Net, Restart_Info.NVar_Elem_Out);

    if (word_size == sizeof(float)) time_ptr = &(tv_sp[time_step-1]);
    else                            time_ptr = &(tv_dp[time_step-1]);

    if (Proc == 0)
      printf("\t%s: Writing variables for time step %d.\n", yo, time_step);

    /* Write data to the scalar results FEM file, based */
    /* on the number of time steps in the subset. */
    if (gather_vars(exoid_out, word_size, time_ptr, global_var, nvar_sorted,
                    evar_sorted, (index+1)) < 0) {
      fprintf(stderr, "[%d] %s: Error occurred while gathering variables.\n",
              Proc, yo);
      exit(1);
    }

  } /* End: "for (index = 0; index < Restart_Info.Num_Times; index++)" */

  if ((Restart_Info.NVar_Glob > 0) && (Proc == 0))
    safe_free((void **) &global_var);

  if (Restart_Info.NVar_Node_Out > 0) {
    safe_free((void **) &nvar_sorted);
    safe_free((void **) &nvar_gross);
  }

  if (Restart_Info.NVar_Elem_Out > 0) {
    safe_free((void **) &evar_sorted);
    safe_free((void **) &evar_gross);
    safe_free((void **) &(Restart_Info.GElem_TT));
    safe_free((void **) &(Restart_Info.Elem_TT));
    safe_free((void **) &(Restart_Info.GElem_TT_Out));
  }
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int check_serial_var(int exoid_out, char *type, int nvar,
                            char *var_names[])
{
  /* Local declarations. */
  static char  yo[] = "check_serial_var";

  int          inum, i;
  char         **snames;
  char         short_type[2];

/***************************** BEGIN EXECUTION ******************************/

  /* grab the first char off the type variable, should be "g", "e", or "n" */
  strncpy(short_type, type, 1);

  /* check if there are already variables defined in the file */
  if (ex_get_var_param(exoid_out, short_type, &inum) < 0) {
    fprintf(stderr, "%s: unable to get %s var param from serial file\n",
            yo, type);
    return -1;
  }

  /*
   * if there are already variable defined in the serial file, make sure
   * that they are the same variables as in the parallel file.
   */
  if (inum > 0) {
    if (inum != nvar) {
      fprintf(stderr, "%s: fatal: there are %d %s variables in the serial"
              " and %d in the parallel file.\n", yo, inum, type, nvar);
      return -1;
    }

    /* allocate space for the variable names */
    snames = (char **) array_alloc(__FILE__, __LINE__, 2, inum,
                                   MAX_STR_LENGTH+1, sizeof(char));

    if (ex_get_var_names(exoid_out, short_type, inum, snames) < 0) {
      fprintf(stderr, "%s: unable to get %s var param from serial file\n",
              yo, type);
      return -1;
    }

    /* now check that the names match in the right order */
    for (i = 0; i < inum; i++) {
      if (strcmp(snames[i], var_names[i]) != 0) {
        fprintf(stderr, "%s: %s variable number %d does not match\n",
                yo, type, (i+1));
        safe_free ((void **) &snames);
        return -1;
      }
    }
    safe_free ((void **) &snames);
  }
  else if (nvar > 0) {
    if (ex_put_var_param(exoid_out, short_type, nvar) < 0) {
      fprintf(stderr, "%s: unable to put %s var param in serial file\n",
              yo, type);
      return -1;
    }

    if (ex_put_var_names(exoid_out, short_type, nvar, var_names) < 0) {
      fprintf(stderr, "%s: unable to put %s var names in serial file\n",
              yo, type);
      return -1;
    }
  }

  return inum;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void put_records(int exoid_out, int num_qa, int num_info)
{
/***************************** BEGIN EXECUTION ******************************/

  /* there must be at least one QA record */
  if(ex_put_qa(exoid_out, num_qa, (char *(*)[]) &(QA_Record[0])) < 0) {
    fprintf(stderr, "[put_records]: ERROR Could not put QA records into file\n");
      exit (1);
  }

  if(num_info > 0) {
    if(ex_put_info(exoid_out, num_info, Info_Record) < 0) {
      fprintf(stderr, "[put_records]: ERROR Could not put Info records into file\n");
      exit (1);
    }
  }

  /* free up memory associated with the Info and QA records */
  if (num_qa > 0)   safe_free((void **) &QA_Record);
  if (num_info > 0) safe_free((void **) &Info_Record);

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int gather_elem_tt(int exoid_out, int vartab)
{
  /* Local Variables */
  char   *yo = "gather_elem_tt";

  char    cTemp[MAX_FNL+1];
  int     pexoid, iproc;
  int     cpu_ws, io_ws;
  int     i, cnt, glob_pindx;
  int     index, offset;
  int     iblk, ivar;
  float   ver;

/**************************** execution begins *******************************/

  cpu_ws = sizeof(float);

  strcpy(cTemp, PIO_Info.Par_Exo_Res_File_Name);

  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {

    /*
     * Need to reset this in case the parallel files are different
     * precision from the mesh file.
     */
    io_ws = 0;

    gen_par_filename(cTemp, Par_Nem_File_Name, Proc_Ids[iproc],
                     Proc_Info[0]);

    /* Stage the writes if specified in the input file */
    glob_pindx = 0;
    pdisk_stage_begin(&PIO_Info, Proc, Proc_Info, Proc_Ids, iproc,
                      &glob_pindx);

    if ((pexoid = ex_open(Par_Nem_File_Name, EX_READ, &cpu_ws, &io_ws,
                          &ver)) == -1) {
      fprintf(stderr,"[%d] %s: Could not open parallel Exodus II file\n",
              Proc, yo);
      return -1;
    }

    /* get the truth table from each file for this processor, based on
       the original number of elemental variables */
    if (ex_get_elem_var_tab(pexoid, Num_Elem_Blk, Restart_Info.NVar_Elem,
                            Restart_Info.Elem_TT[iproc]) < 0) {
      fprintf(stderr, "[%d] %s: error reading elem truth table.\n", Proc, yo);
      return -1;
    }

    /* Close the parallel file */
    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing parallel exoII file\n",
               yo);
      return -1;
    }

    /*
     * If staged writes are enabled then the current processor tells the next
     * processor in the chain to perform it's writes.
     */

    pdisk_stage_end(&PIO_Info, Proc, Proc_Info, Proc_Ids, iproc, glob_pindx);

  }

  /* now put the tables on this processor together, based on
       the original number of elemental variables */
  cnt = Num_Elem_Blk * Restart_Info.NVar_Elem;
  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {
    for (i = 0; i < cnt; i++) {
      /* check if there is already a value for this entry */
      if (Restart_Info.GElem_TT[i] == 0)
        Restart_Info.GElem_TT[i] = Restart_Info.Elem_TT[iproc][i];
    }
  }

  /* copy the global table to a global table subset based on the number
     of elemental variables to be written */
  index = 0;
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {
    for (ivar = 0; ivar < Restart_Info.NVar_Elem_Out; ivar++) {

      offset = (iblk * Restart_Info.NVar_Elem) +
                 (Restart_Info.EVar_Index[ivar] - 1);
      Restart_Info.GElem_TT_Out[index] = Restart_Info.GElem_TT[offset];
      index++;
    }
  }

#ifdef DEBUG
    if (Debug_Flag >= 7) {
      print_sync_start(Proc, Num_Proc, FALSE);
      printf ("\n\n[%d]: Local Elemental Truth Table:\n", Proc);
      for (i = 0; i < Num_Elem_Blk; i++) {
        printf ("\teb %d:", (i + 1));
        for (j = 0; j < Restart_Info.NVar_Elem_Out; j++) {
          printf ("\t %d",
                  Restart_Info.GElem_TT_Out[(i*Restart_Info.NVar_Elem_Out)+j]);
        }
        printf ("\n");
      }
      print_sync_end(Proc, Num_Proc, FALSE);
    }
#endif

  /* now fanin the tables to processor 0 */
  cnt = Num_Elem_Blk * Restart_Info.NVar_Elem_Out;
  fanin_int(Restart_Info.GElem_TT_Out, cnt, 0, Proc, Num_Proc);

  /*
   * now put the truth table in the mesh file, if there are not
   * elemental variables already defined in it
   */
  /* open the scalar file */
  if ((Proc == 0) && (!vartab)) {

    /* copy the global table to a global table subset based on the number 
       of elemental variables to be written */

#ifdef DEBUG
    if (Debug_Flag >= 5) {
      printf ("\n\nGlobal Elemental Truth Table:\n");
      for (i = 0; i < Num_Elem_Blk; i++) {
        printf ("\teb %d:", (i + 1));
        for (j = 0; j < Restart_Info.NVar_Elem_Out; j++) {
          printf ("\t %d",
                  Restart_Info.GElem_TT_Out[(i*Restart_Info.NVar_Elem_Out)+j]);
        }
        printf ("\n");
      }
    }
#endif

    if (ex_put_elem_var_tab(exoid_out, Num_Elem_Blk, Restart_Info.NVar_Elem_Out,
                            Restart_Info.GElem_TT_Out) < 0) {
      fprintf(stderr, "%s: error ocurred while writing truth table.\n",
              yo);
      return -1;
    }
  }

  /* all processor will have to have this */
  brdcst(Proc, Num_Proc, (char *) Restart_Info.GElem_TT_Out, 
         (cnt * sizeof(int)), 0);

  return 0;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int read_vars(int word_size, void *global_var, void *nodal_var,
                     void *elem_var, int time_step)
{
  /* Local Variables */
  char    *yo = "read_vars";
  char     cTemp[MAX_FNL+1];
  int	   iproc;
  int      cnt, glob_pindx, pexoid, ivar, iblk;
  int      cpu_ws, io_ws;
  int      node_offset, elem_offset1, elem_offset2;
  float    ver;

  float  **nptr_sp, **eptr_sp;
  double **nptr_dp, **eptr_dp;
  void    *ptr;

/**************************** execution begins *******************************/
  nptr_sp = 0;
  eptr_sp = 0;
  nptr_dp = 0;
  eptr_dp = 0;
  ptr     = 0;
  
  cpu_ws = word_size;
  io_ws = 0;
  glob_pindx = 0;
  cnt = 0;

  strncpy(cTemp, PIO_Info.Par_Exo_Res_File_Name, MAX_FNL);

  node_offset = 0;
  elem_offset1 = 0;
  elem_offset2 = 0;

  if (word_size == sizeof(float)) {
    nptr_sp = (float **) nodal_var;
    eptr_sp = (float **) elem_var;
  }
  else {
    nptr_dp = (double **) nodal_var;
    eptr_dp = (double **) elem_var;
  }

  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {

    gen_par_filename(cTemp, Par_Nem_File_Name, Proc_Ids[iproc], Proc_Info[0]);

    /* Stage the writes if specified in the input file */
    pdisk_stage_begin(&PIO_Info, Proc, Proc_Info, Proc_Ids, iproc,
                      &glob_pindx);

    if ((pexoid = ex_open(Par_Nem_File_Name, EX_READ, &cpu_ws, &io_ws,
                          &ver)) == -1) {
      fprintf(stderr,"[%d] %s: Could not open parallel Exodus II file\n",
              Proc, yo);
      return -1;
    }

    /**************************** global variables **************************
     *
     * NOTE: only have to get these from proc 0 file, since they should be
     *       the same in all files.
     */
    if (Restart_Info.NVar_Glob > 0) {
      if (Proc == 0 && iproc == 0) {

        if (ex_get_glob_vars(pexoid, time_step, Restart_Info.NVar_Glob,
                             global_var) < 0) {
          fprintf(stderr, "%s: error reading global vars for time step %d.\n",
                  yo, time_step);
          return -1;
        }

      } /* End "if (Proc == 0 && iproc == 0)" */

    } /* End: "if (Restart_Info.NVar_Glob > 0)" */


    /*************************** nodal variables ***************************/
    if (Restart_Info.NVar_Node_Out > 0) {
      /* now loop through and get each variable for this time step */
      cnt = Proc_Num_Nodes[iproc];
      for (ivar = 0; ivar < Restart_Info.NVar_Node_Out; ivar++) {

        /* determine the precision */
        if (word_size == sizeof(float)) ptr = &(nptr_sp[ivar][node_offset]);
        else                             ptr = &(nptr_dp[ivar][node_offset]);

        if (ex_get_nodal_var(pexoid, time_step, 
                             Restart_Info.NVar_Index[ivar],
                             cnt, ptr) < 0) {
          fprintf(stderr, "[%d] %s: error reading nodal variable, %d.\n",
                  Proc, yo, Restart_Info.NVar_Index[ivar]);
          return -1;
        }
      }

      /* adjust the offset for the next processors information */
      node_offset += cnt;
    }


    /************************* elemental variables *************************/
    if (Restart_Info.NVar_Elem_Out > 0) {
      for (ivar = 0; ivar < Restart_Info.NVar_Elem_Out; ivar++) {

        /* need to reset this for each varable */
        elem_offset2 = elem_offset1;

        for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

          /* determine the precision */
          if (word_size == sizeof(float))
            ptr = &(eptr_sp[ivar][elem_offset2]);
          else
            ptr = &(eptr_dp[ivar][elem_offset2]);

          /*
           * check if variable defined in block
           * this will also be 0 if this is a NULL element block
           * on this processor
           */
          cnt = (iblk * Restart_Info.NVar_Elem) + 
                (Restart_Info.EVar_Index[ivar] - 1);
          if (Restart_Info.Elem_TT[iproc][cnt] == 1) {

            if (ex_get_elem_var(pexoid, time_step,
                                Restart_Info.EVar_Index[ivar],
                                EB_Ids[iblk],
                                Proc_Num_Elem_In_Blk[iproc][iblk], ptr) < 0) {
              fprintf(stderr, "[%d] %s: error reading elemental var, %d.\n",
                      Proc, yo, (ivar + 1));
              return -1;
            }

          }

          /* move the offset */
          elem_offset2 += Proc_Num_Elem_In_Blk[iproc][iblk];

        } /* End "for (iblk = 0; iblk < Num_Elem_Blk; iblk++)" */
      } /* End "for (ivar = 0; ivar < Restart_Info.NVar_Elem; ivar++)" */

      /* move the offset */
      elem_offset1 += Proc_Num_Elem[iproc];

    }

    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing proc 0 exoII results"
               " file\n", yo);
      return -1;
    }

    /*
     * If staged writes are enabled then the current processor tells the next
     * processor in the chain to perform it's writes.
     */

    pdisk_stage_end(&PIO_Info, Proc, Proc_Info, Proc_Ids, iproc, glob_pindx);

  } /* End: "for (iproc = 0; iproc < Proc_Info[2]; iproc++)" */

  return 0;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int gather_vars(int exoid_out, int word_size, void *time_val,
                       void *global_var, void *nodal_var, void *elem_var,
                       int time_step)
/*
 * This function gathers the variable to processor 0, and then
 * processor 0 writes them to the mesh file
 */

{
  /* Local Variables */
  char    *yo = "gather_vars";

  int      ivar, cnt, iblk, imsg, offset;
  int      gsize, num_mesgs, *start_pos, start, end;
  void    *gvec_ordered;

  float  **fptr;
  double **dptr;
  void    *ptr;

/**************************** execution begins *******************************/

  fptr = 0;
  dptr = 0;
  
  /* add the time value to the results file */
  if (Proc == 0) {
    /* now add the time value to the serial file */
    if (ex_put_time(exoid_out, time_step, time_val) < 0) {
      fprintf(stderr, "%s: error putting time value in serial file.\n", yo);
      return -1;
    }
  }

  /************************** global variables *******************************/
  if ((Proc == 0) && (Restart_Info.NVar_Glob > 0)) {
    if (ex_put_glob_vars(exoid_out, time_step, Restart_Info.NVar_Glob,
                         global_var) < 0)
    {
      fprintf(stderr, "%s: error writing global vars for time step %d.\n",
              yo, time_step);
      return -1;
    }
  }

  /************************** nodal variables ********************************/
  if (Restart_Info.NVar_Node_Out > 0) {

    if (word_size == sizeof(float)) fptr = (float **) nodal_var;
    else                            dptr = (double **) nodal_var;

    /*
     *    Use Num_Node, the total number of nodes in the mesh, to find out how
     *    many global fann-ins need to be done
     */

    num_mesgs = break_message_up(word_size, Num_Node, MAX_CHUNK_SIZE,
                                 &start_pos);

    /*
     * The first record will always be the largest record, it is assumed.
     * Therefore, we can allocate the ordered array at the top of the loop,
     * and use it throughout.
     */

    gsize        = start_pos[1] - start_pos[0];
    gvec_ordered = array_alloc(__FILE__, __LINE__, 1, gsize, word_size);

#ifdef DEBUG
    if (Debug_Flag >= 2 && Proc == 0) {
      printf("\n\nMessage summary for nodal variables:\n");
      printf("\tTime Index: %d\n", time_step);
      printf("\tNumber of nodal variables: %d\n", Restart_Info.NVar_Node_Out);
      printf("\tNumber of messages per nodal variable: %d\n", num_mesgs);
      printf("\tMax message size per nodal variable: %d\n", gsize);
      printf("\tMin message size per nodal variable: %d\n",
             start_pos[num_mesgs]-start_pos[num_mesgs-1]);
    }
#endif

    for (ivar = 0; ivar < Restart_Info.NVar_Node_Out; ivar++) {
      for (imsg = 0; imsg < num_mesgs; imsg++) {

        gsize = start_pos[imsg+1] - start_pos[imsg];

        if (word_size == sizeof(float)) ptr = fptr[ivar];
        else                            ptr = dptr[ivar];

        /* NOTE: nodal_map is from 1 to n, not 0 to n-1 */
        if (fanin_void_slab(word_size, GNode_Seq, ptr, Num_Nodes_Net,
                            (start_pos[imsg] + 1), start_pos[imsg+1],
                            gvec_ordered) < 0) {
          fprintf(stderr, "[%d] %s: fannin returned an error condition\n",
                  Proc, yo);
          return -1;
        }

        /*
         * Write a slab of nodal variables values to the exoII file on
         * the front end
         */
        if (Proc == 0) {

#ifdef DEBUG
          if (word_size == sizeof(float)) fptr1 = (float *) gvec_ordered;
          else                            dptr1 = (double *) gvec_ordered;

          if (Debug_Flag >= 7) {
            printf ("\n\n[%d]: Global Sorted Nodal Variables:\n", Proc);
            printf ("\tfor time index %d\n", time_step);
            printf("\tvar %d:", (ivar + 1));
            for (cnt = 0; cnt < gsize; cnt++)
              if (word_size == sizeof(float))
                printf ("  %f", fptr1[cnt]);
              else
                printf ("  %lf", dptr1[cnt]);
            printf("\n");
          }
#endif

          if (ex_put_nodal_var_slab(exoid_out, time_step, (ivar + 1),
                                    (start_pos[imsg] + 1), gsize,
                                    gvec_ordered) < 0) {
            fprintf(stderr, "[%d] %s: error writing nodal variable, %d.\n",
                    Proc, yo, (ivar + 1));
            return -1;
          }
        }
      } /* End "for (imsg = 0; imsg < num_mesgs; imsg++) */

    } /* End "for (ivar = 0; ivar < Restart_Info.NVar_Node_Out; ivar++)" */

    safe_free((void **) &gvec_ordered);
    safe_free((void **) &start_pos);

  } /* End "if (Restart_Info.NVar_Node_Out > 0)" */



  /************************ elemental variables ******************************/

  if (Restart_Info.NVar_Elem_Out > 0) {

    offset = 0;
    for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

      if (word_size == sizeof(float)) fptr = (float **) elem_var;
      else                            dptr = (double **) elem_var;

      /*
       *    Use EB_Cnts, the total number of elements in this block for
       *    the global mesh, to find out how many global fan-ins need to
       *    be done
       */

      num_mesgs = break_message_up(word_size, EB_Cnts[iblk], MAX_CHUNK_SIZE,
                                   &start_pos);

      /*
       * The first record will always be the largest record, it is assumed.
       * Therefore, we can allocate the ordered array at the top of the
       * loop, and use it throughout.
       */

      gsize        = start_pos[1] - start_pos[0];
      gvec_ordered = array_alloc(__FILE__, __LINE__, 1, gsize, word_size);

#ifdef DEBUG
      if (Debug_Flag >= 2 && Proc == 0) {
        printf("\n\nMessage summary for elemental variables:\n");
        printf("\tTime Index: %d\n", time_step);
        printf("\tElemental Block Id: %d\n", EB_Ids[iblk]);
        printf("\tNumber of elemental variables: %d\n",
               Restart_Info.NVar_Elem);
        printf("\tNumber of messages per elemental variable: %d\n",
               num_mesgs);
        printf("\tMax message size per elemental variable: %d\n", gsize);
        printf("\tMin message size per elemental variable: %d\n",
               start_pos[num_mesgs]-start_pos[num_mesgs-1]);
      }
#endif

      for (ivar = 0; ivar < Restart_Info.NVar_Elem_Out; ivar++) {

        cnt = iblk * Restart_Info.NVar_Elem_Out + ivar;
        if (Restart_Info.GElem_TT_Out[cnt] == 1) {
          for (imsg = 0; imsg < num_mesgs; imsg++) {

            if (word_size == sizeof(float)) ptr = fptr[ivar];
            else                            ptr = dptr[ivar];

            gsize = start_pos[imsg+1] - start_pos[imsg];

            /* need to compensate for the element blocks */
            /* Note: elem_map is from 1 to n, not 0 to n-1 */
            start = start_pos[imsg] + offset + 1;
            end   = start_pos[imsg+1] + offset;

            if (fanin_void_slab(word_size, GElem_Seq, ptr, Num_Elem_Net,
                                start, end, gvec_ordered) < 0) {
              fprintf(stderr, "[%d] %s: fannin returned an error condition\n",
                      Proc, yo);
              return -1;
            }

#ifdef DEBUG
            if (word_size == sizeof(float)) fptr1 = (float *) gvec_ordered;
            else                            dptr1 = (double *) gvec_ordered;

            if (Debug_Flag >= 7) {
              printf ("\n\n[%d]: Elemental Variables after fanin:\n", Proc);
              printf ("\tTime Step: %d\n", time_step);
              printf ("\tElement Block: %d\n", (iblk + 1));
              printf ("\tVariable Number: %d\n", (ivar + 1));
              printf ("\t");
              for (i = 0; i < gsize; i++) {
                if (word_size == sizeof(float))
                  printf ("  %f", fptr1[i]);
                else
                  printf ("  %lf", dptr1[i]);
              }
              printf ("\n");

            }
#endif

            /*
             * Write a slab of elemental variables values to the exoII file
             * on the front end
             */

            if (Proc == 0) {

              if (ex_put_elem_var_slab(exoid_out, time_step, (ivar + 1),
                                       EB_Ids[iblk], (start_pos[imsg] + 1),
                                       gsize, gvec_ordered) < 0) {
                fprintf(stderr, "[%d] %s: error writing element variable, %d.\n",
                        Proc, yo, (ivar + 1));
                return -1;
              }
            }
          } /* End "for (imsg = 0; imsg < num_mesgs; imsg++)" */

        } /* End "if (Restart_Info.GElem_TT[cnt] == 1)" */
      } /* End "for (ivar = 0; ivar < Restart_Info.NVar_Elem; ivar++)" */

      /* set the offset for the next element block */
      offset += EB_Cnts[iblk];

      safe_free((void **) &gvec_ordered);
      safe_free((void **) &start_pos);
    } /* End "for (iblk = 0; iblk < Num_Elem_Blk; iblk++)" */
  } /* End "if (Restart_Info.NVar_Elem > 0)" */

  return 0;

}

