#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include "pe_common.h"
#include "el_geom_const.h"
#include "ps_pario_const.h"
#include "rf_allo.h"
#include "rf_message.h"
#include "rf_mp_const.h"
#include "rf_io_const.h"
#include "rf_util.h"
#include "jo_const.h"
#include "jo_map_const.h"
#include "jo_mesh.h"
#include "jo_util.h"

#include "exodusII.h"


/******************** ROUTINES IN THIS FILE ***********************************
*
*     Name                      Type                  Called By
*   --------                 ---------              ---------------
*
*  put_mesh                     void              main:jo_main.c
*  read_put_coord               int                    put_mesh
******************************************************************************/


static int  read_put_coord(int, int);

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void put_mesh(int exoid_out, int word_size)
{
  /* Local Variables */
  char    *yo = "put_mesh";

  int      start_time = 0, iblk, iflag;
  int     *dist_fact;

  /*------------------- begin execution -------------------------------------*/

  /****************************************/
  /* start off by getting the coordinates */
  /****************************************/
  if(Proc == 0)
    start_time = second();

  if (read_put_coord(exoid_out, word_size) < 0) {
    fprintf(stderr, "%s: error ocurred getting the coordinates.\n",
            yo);
    exit(1);
  }

  if(Proc == 0) {
    printf("\n\nTime to get coordinates: %.2f\n",
           second() - start_time);
  }

  /*************************************/
  /* get the element block information */
  /*************************************/
  if(Proc == 0)
    start_time = second();

  if (put_eb_info(exoid_out) < 0) {
    fprintf(stderr, "%s: error ocurred getting the elem block information.\n",
            yo);
    exit(1);
  }

  if(Proc == 0) {
    printf("\n\nTime to get element block information: %.2f\n",
           second() - start_time);
  }

  /**************************************/
  /* get the element block connectivity */
  /**************************************/
  if(Proc == 0)
    start_time = second();

  if (read_put_connect(exoid_out) < 0) {
    fprintf(stderr, "%s: error ocurred getting the elem block connectivity.\n",
            yo);
    exit(1);
  }

  if(Proc == 0) {
    printf("\n\nTime to get connectivity lists: %.2f\n",
           second() - start_time);
  }

  /************************************/
  /* get the element block attributes */
  /************************************/
  
  /* make sure that there are some attributes to get */
  iflag = 0;
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
    if (Proc_Num_Attr[0][iblk] > 0) {
      iflag = 1;
      break;
    }

  if (iflag) {
    if(Proc == 0)
      start_time = second();

    if (read_put_attrib(exoid_out, word_size) < 0) {
      fprintf(stderr, "%s: error ocurred getting the elem block attributes.\n",
              yo);
      exit(1);
    }

    if(Proc == 0) {
      printf("\n\nTime to get element block attributes: %.2f\n",
             second() - start_time);
    }
  }

  /* free up some more memory */
  safe_free ((void **) &Proc_Nodes_Per_Elem);

  if (Num_Node_Set > 0) {
    /******************************/
    /* put the node set parameter */
    /******************************/

    dist_fact = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Node_Set,
                                    sizeof(int));
    if (!dist_fact) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
    }
 
    if(Proc == 0)
      start_time = second();

    if (put_ns_info(exoid_out, dist_fact) < 0) {
      fprintf(stderr, "%s: error ocurred writing node set parameters.\n", yo);
      exit(1);
    }

    if(Proc == 0) {
      printf("\n\nTime to write node set parameters: %.2f\n",
             second() - start_time);
    }

    /*********************/
    /* put the node sets */
    /*********************/

    if(Proc == 0)
      start_time = second();

    if (read_put_node_sets(exoid_out, word_size, dist_fact) < 0) {
      fprintf(stderr, "%s: error ocurred getting the node sets.\n", yo);
      exit(1);
    }

    if(Proc == 0) {
      printf("\n\nTime to get node sets: %.2f\n", second() - start_time);
    }

    safe_free ((void **) &dist_fact);

  } /* End: "if (Num_Node_Set > 0)" */

  /* should be done with this now */
  safe_free ((void **) &GNodes);

  if (Num_Side_Set > 0) {
    /******************************/
    /* put the side set parameter */
    /******************************/

    dist_fact = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Side_Set,
                                    sizeof(int));
    if (!dist_fact) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
    }

    if(Proc == 0)
      start_time = second();

    if (put_ss_info(exoid_out, dist_fact) < 0) {
      fprintf(stderr, "%s: error ocurred writing side set parameters.\n", yo);
      exit(1);
    }

    if(Proc == 0) {
      printf("\n\nTime to write side set parameters: %.2f\n",
             second() - start_time);
    }

    /*********************/
    /* put the side sets */
    /*********************/

    if(Proc == 0)
      start_time = second();

    if (read_put_side_sets(exoid_out, word_size, dist_fact) < 0) {
      fprintf(stderr, "%s: error ocurred getting the side sets.\n", yo);
      exit(1);
    }

    if(Proc == 0) {
      printf("\n\nTime to get side sets: %.2f\n", second() - start_time);
    }

    safe_free ((void **) &dist_fact);

  } /* End: "if (Num_Side_Set > 0)" */

  /* should be done with this now */
  safe_free ((void **) &GElems);

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int read_put_coord(int exoid_out, int word_size)
{
  /* Local Variables */
  char    *yo = "read_put_coord";

  char     cTemp[MAX_FNL+1];
  int      pexoid, iproc, glob_pindx, offset;
  int      cpu_ws, io_ws, idim, imsg;
  int      gsize, num_mesgs, *start_pos;

  float    ver;
  float   *coor_sp, *coor_sorted_sp;
  double  *coor_dp, *coor_sorted_dp;
  void    *coord, *coor[3] = {NULL, NULL, NULL};
  void    *coord_sorted, *coor_sorted[3] = {NULL, NULL, NULL};
  void    *gvec_ordered[3] = {NULL, NULL, NULL};
  /*------------------- begin execution -------------------------------------*/

  /* allocate memory */
  coord = array_alloc(__FILE__, __LINE__, 1, (Num_Dim * Num_Nodes_Gross),
                      word_size);
  coord_sorted = array_alloc(__FILE__, __LINE__, 1, (Num_Dim * Num_Nodes_Net),
                             word_size);
  if ((!coord) || (!coord_sorted)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  if (Proc == 0)
    for (idim = 0; idim < Num_Dim; idim++) {
      Coord_Name[idim] = (char *) array_alloc(__FILE__, __LINE__, 1,
                                              MAX_STR_LENGTH + 1,
                                              sizeof(char));
      if (!Coord_Name[idim]) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
    }

  if (word_size == sizeof(float)) {
    coor_sp = (float *) coord;
    coor_sorted_sp = (float *) coord_sorted;
  }
  else {
    coor_dp = (double *) coord;
    coor_sorted_dp = (double *) coord_sorted;
  }

  offset = 0;
  cpu_ws = word_size;
  io_ws = 0;

  strcpy(cTemp, PIO_Info.Par_Exo_Res_File_Name);

  /* read the coordinates from the parallel files */
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

    /* determine the precision */
    if (word_size == sizeof(float))
      for (idim = 0; idim < Num_Dim; idim++)
        coor[idim] = coor_sp + (idim * Num_Nodes_Gross) + offset;
    else
      for (idim = 0; idim < Num_Dim; idim++)
        coor[idim] = coor_dp + (idim * Num_Nodes_Gross) + offset;

    check_exodus_error(iproc,ex_get_coord(pexoid, coor[0], coor[1], coor[2]),
                       "ex_get_coord");

    /* if this is the proc 0 file, read the coordinates */
    if (Proc_Ids[iproc] == 0)
      check_exodus_error(iproc,ex_get_coord_names(pexoid, Coord_Name),
                         "ex_get_coord_names");

    offset += Proc_Num_Nodes[iproc];


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

  } /* End "for (iproc = 0; iproc < Proc_Info[2]; iproc++)" */

  /* sort the coordinates */

  if (word_size == sizeof(float))
    for (idim = 0; idim < Num_Dim; idim++) {
      coor[idim] = coor_sp + (idim * Num_Nodes_Gross);
      coor_sorted[idim] = coor_sorted_sp + (idim * Num_Nodes_Net);
    }
  else
    for (idim = 0; idim < Num_Dim; idim++) {
      coor[idim] = coor_dp + (idim * Num_Nodes_Gross);
      coor_sorted[idim] = coor_sorted_dp + (idim * Num_Nodes_Net);
    }

  for (idim = 0; idim < Num_Dim; idim++)
    sort_map_void(word_size, Nodal_Sort_Map, coor[idim], coor_sorted[idim],
                  Num_Nodes_Net);

  /* free up some memory */
  safe_free (&coord);

  /* now fanin the coordinates, and write out the values */

  /*
   * use MAX_CHUNK_SIZE/Num_Dim, because coordinates for each dimension
   * must be fanned in before they can be written out.
   */
  num_mesgs = break_message_up(word_size, Num_Node, MAX_CHUNK_SIZE/Num_Dim,
                               &start_pos);

  /*
   * The first record will always be the largest record, it is assumed.
   * Therefore, we can allocate the ordered array at the top of the loop,
   * and use it throughout.
   */

  gsize        = start_pos[1] - start_pos[0];
  for (idim = 0; idim < Num_Dim; idim++) {
    gvec_ordered[idim] = array_alloc(__FILE__, __LINE__, 1, gsize, word_size);
    if (!gvec_ordered[idim]) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
  }

#ifdef DEBUG
  if (Debug_Flag >= 2 && Proc == 0) {
    printf("\n\nMessage summary for coordinates:\n");
    printf("\tNumber of dimensions: %d\n", Num_Dim);
    printf("\tNumber of messages: %d\n", num_mesgs);
    printf("\tMax message size: %d\n", gsize);
    printf("\tMin message size: %d\n",
           start_pos[num_mesgs]-start_pos[num_mesgs-1]);
  }
#endif

    for (imsg = 0; imsg < num_mesgs; imsg++) {

      /* have to get all of the dimensions before writing it out */
      for (idim = 0; idim < Num_Dim; idim++) {

        gsize = start_pos[imsg+1] - start_pos[imsg];

        if (fanin_void_slab(word_size, GNode_Seq, coor_sorted[idim],
                            Num_Nodes_Net, (start_pos[imsg] + 1),
                            start_pos[imsg+1], gvec_ordered[idim]) < 0) {
          fprintf(stderr, "[%d] %s: error while faning in the coordinates.\n",
                  Proc, yo);
          return -1;
        }

      } /* End: "for (idim = 0; idim < Num_Dim; idim++)" */

      /* Write out the coordinate slab to the results file */
      if (Proc == 0) {

        check_exodus_error(iproc,ex_put_n_coord(exoid_out, (start_pos[imsg] + 1),
                                          gsize, gvec_ordered[0],
                                          gvec_ordered[1], gvec_ordered[2]),
                            "ex_put_n_coord");

        check_exodus_error(iproc,ex_put_coord_names(exoid_out, Coord_Name),
                           "ex_put_coord_names");

      } /* End: "if (Proc == 0)" */

    } /* End: "for (imsg = 0; imsg < num_mesgs; imsg++)" */

    for (idim = 0; idim < Num_Dim; idim++)
      safe_free((void **) &(gvec_ordered[idim]));
    safe_free((void **) &start_pos);
    safe_free((void **) &coord_sorted);

    return 0;

}
