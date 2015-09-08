#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "exodusII.h"
#include "el_geom_const.h"
#include "ps_pario_const.h"
#include "rf_allo.h"
#include "rf_io_const.h"
#include "rf_mp_const.h"
#include "jo_map_const.h"

extern void check_exodus_error (int, int error, char *);

char GeomTitle[MAX_LINE_LENGTH+1]; /* geometry title for results file */

/************ R O U T I N E S   I N   T H I S   F I L E ***********************
*
*  Name_of_Routine              type                 Called by
*  ---------------------------------------------------------------
*
*  read_init_proc ()                             main:jo_main.c
*
******************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

void read_init_proc(int word_size)

/*
 * This function reads general mesh information from each parallel
 * file so that each processor has the information that it needs.
 */
{
  /* local declarations */
  char  *yo = "read_init_proc";

  char   cTemp[MAX_FNL+1], name[MAX_LINE_LENGTH+1];
  char  *tmpstr;
  int    pexoid, iproc, error, i;
  int    cpu_ws, io_ws, dummy, hold_int1, hold_int2;
  float  ver;

/**************************** execution begins *******************************/

  /* allocate memory */

  /* for nodes and elements */
  Proc_Num_Nodes = (int *) array_alloc(__FILE__, __LINE__, 1, 2 * Proc_Info[2],
                                       sizeof(int));
  if (!Proc_Num_Nodes) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }
  Proc_Num_Elem = Proc_Num_Nodes + Proc_Info[2];

  /* for element block information */
  if (Num_Elem_Blk > 0) {
    /*
     * Proc_Num_Elem_In_Blk has one extra row of space to hold the
     * totals for this block. This will be filled in when the global
     * maps are created.
     */
    Proc_Num_Elem_In_Blk = (int **) array_alloc(__FILE__, __LINE__, 2,
                                                (Proc_Info[2] + 1),
                                                Num_Elem_Blk, sizeof(int));
    if (!Proc_Num_Elem_In_Blk) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
  
    Proc_Nodes_Per_Elem = (int **) array_alloc(__FILE__, __LINE__, 2, 2,
                                               Num_Elem_Blk, sizeof(int));
    if (!Proc_Nodes_Per_Elem) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
    Proc_Num_Attr           = Proc_Nodes_Per_Elem + 1;


    EB_Types     = (char **) array_alloc (__FILE__, __LINE__, 2, Num_Elem_Blk,
                                          MAX_STR_LENGTH + 1, sizeof(char));
    if (!EB_Types) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }

    /* initialize some of the arrays */
    for (i = 0; i < Num_Elem_Blk; i++) {
      Proc_Nodes_Per_Elem[0][i] = 0;
      Proc_Num_Attr[0][i] = 0;
      memset(EB_Types[i], '\0', (MAX_STR_LENGTH + 1));
    }
  }

  if (!Gen_Flag) {

    /* for node sets */
    if (Num_Node_Set > 0) {
      Proc_NS_Count = (int **) array_alloc(__FILE__, __LINE__, 2,
                                           (2 * Proc_Info[2]), Num_Node_Set,
                                           sizeof(int));
      if (!Proc_NS_Count) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
      Proc_NS_DF_Count = Proc_NS_Count + Proc_Info[2];
    }

    /* and for side sets */
    if (Num_Side_Set > 0) {
      Proc_SS_Elem_Count = (int **) array_alloc(__FILE__, __LINE__, 2,
                                                (2 * Proc_Info[2]),
                                                Num_Side_Set, sizeof(int));
      if (!Proc_SS_Elem_Count) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
      Proc_SS_DF_Count = Proc_SS_Elem_Count + Proc_Info[2];
    }
  }

  strcpy(cTemp, PIO_Info.Par_Exo_Res_File_Name);

  /* cpu word size based on mesh file */
  cpu_ws = word_size; 
  io_ws = 0;

  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {

    gen_par_filename(cTemp, Par_Nem_File_Name, Proc_Ids[iproc], Proc_Info[0]);

    if ((pexoid = ex_open(Par_Nem_File_Name, EX_READ, &cpu_ws, &io_ws,
                          &ver)) == -1) {
      fprintf(stderr,"[%d] %s: Could not open parallel Exodus II file\n",
              Proc, yo);
      exit (1);
    }

    /*************************************/
    /* get the element block information */
    /*************************************/
    for (i = 0; i < Num_Elem_Blk; i++) {
      /*
       * Note: for name, number of attributes, and number of
       * nodes per element, only need one array per processor
       * instead of one array per file. This information should
       * be the same for element blocks in different files
       */
      error = ex_get_elem_block(pexoid, EB_Ids[i], name,
                                &Proc_Num_Elem_In_Blk[iproc][i],
                                &hold_int1, &hold_int2);

      check_exodus_error(iproc,error, "ex_get_elem_block");

      /*
       * now check if there is new information for the name,
       * nodes, or attributes arrays.
       */
      if ((strlen(name) > 0) && (strcmp(name, "NULL") != 0)) {
        strcpy(EB_Types[i], name);
        Proc_Nodes_Per_Elem[0][i] = hold_int1;
        Proc_Num_Attr[0][i] = hold_int2;
      }
    }

    if (!Gen_Flag) {

      /********************************/
      /* get the node set information */
      /********************************/
      for (i = 0; i < Num_Node_Set; i++) {
        /* NOTE: Id's will be the same on all processors */
        error = ex_get_node_set_param(pexoid, Proc_NS_Ids[0][i],
                                      &Proc_NS_Count[iproc][i],
                                      &Proc_NS_DF_Count[iproc][i]);

        check_exodus_error(iproc,error, "ex_get_node_set_param");
      }

      /********************************/
      /* get the side set information */
      /********************************/
      for (i = 0; i < Num_Side_Set; i++) {
        /* NOTE: Id's will be the same on all processors */
        error = ex_get_side_set_param(pexoid, Proc_SS_Ids[0][i],
                                      &Proc_SS_Elem_Count[iproc][i],
                                      &Proc_SS_DF_Count[iproc][i]);

        check_exodus_error(iproc,error, "ex_get_side_set_param");
      }

    }

    /*
     * make sure that only the processor 0 file title is saved
     * since that is where the mesh file title is saved
     */
    if (Proc_Ids[iproc] == 0) tmpstr = GeomTitle;
    else                      tmpstr = name;

    /* and get initial information */
    error = ex_get_init(pexoid, tmpstr, &Num_Dim, &(Proc_Num_Nodes[iproc]),
                        &(Proc_Num_Elem[iproc]), &dummy, &dummy, &dummy);

    check_exodus_error(iproc,error, "ex_get_loadbal_param");

    /* Close the parallel file */
    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing parallel exoII file\n",
               yo);
      exit (1);
    }

  } /* End: "for (iproc = 0; iproc < Proc_Info[2]; iproc++)" */

}
