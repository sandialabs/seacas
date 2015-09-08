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

#include "exodusII.h"

/******************** ROUTINES IN THIS FILE ***********************************
*
*     Name                      Type                  Called By
*   --------                 ---------              ---------------
*
*  put_eb_info                  int            jo_mesh:put_mesh
*  read_put_connect             int            jo_mesh:put_mesh
*  conv_connect                 int                    read_put_connect
*  read_put_attrib              int            jo_mesh:put_mesh
******************************************************************************/

static int conv_connect(int, int, int **);

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int put_eb_info(int exoid_out)
{
  /* Local Variables */
  int      iblk;

  /*------------------- begin execution -------------------------------------*/

  /*
   * fan in the list of # nodes per element, and # attributes per
   * element. These were allocated as contiguous blocks of memory
   * in read_init_proc().
   */
  fanin_int(Proc_Nodes_Per_Elem[0], (2 * Num_Elem_Blk), 0, Proc, Num_Proc);

  /* and the element types */
  fanin_str(EB_Types[0], (Num_Elem_Blk * (MAX_STR_LENGTH + 1)), Proc,
            Num_Proc);

  /* write them to the results file */
  if (Proc == 0) {

    for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

#ifdef DEBUG
      if (Debug_Flag >= 7 && Proc == 0) {
        printf("\nPutting block information for element block %d\n", iblk+1);
        printf("Element Block Id: %d\n", EB_Ids[iblk]);
        printf("Element Block Type: %s\n", EB_Types[iblk]);
        printf("Element Block Count: %d\n", EB_Cnts[iblk]);
        printf("Number of Nodes Per Element: %d\n",
               Proc_Nodes_Per_Elem[0][iblk]);
        printf("Number of Attributes Per Element: %d\n\n",
               Proc_Num_Attr[0][iblk]);
      }
#endif

      check_exodus_error(Proc,ex_put_elem_block(exoid_out,
						EB_Ids[iblk],
						EB_Types[iblk],
						EB_Cnts[iblk],
						Proc_Nodes_Per_Elem[0][iblk],
						Proc_Num_Attr[0][iblk]),
                          "ex_put_elem_block");
    }
  }

  /*
   * and broadcast back out the complete lists
   * for the # nodes, and # attributes
   */
  brdcst (Proc, Num_Proc, (char *) Proc_Nodes_Per_Elem[0],
          (2 * Num_Elem_Blk * sizeof(int)), 0);

  return 0;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int read_put_connect(int exoid_out)
{
  /* Local Variables */
  char    *yo = "read_put_connect";

  char     cTemp[MAX_FNL+1];
  int      pexoid, iproc, iblk, i, j;
  int      cpu_ws, io_ws, imsg, num_elem;
  int      gsize, num_mesgs, *start_pos, start, end;
  int     *gvec_ordered;
  int    **connect, **connect_ptr, **conn_sorted;
  int     *offset, offset1;

  float    ver;

  /*------------------- begin execution -------------------------------------*/

  /* allocate memory to hold the local values */
  connect_ptr = (int **) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Gross,
                                     sizeof(int *));
  connect = (int **) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Blk,
                                 sizeof(int *));
  if ((!connect_ptr) || (!connect)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {
    /*
     * count up the number of elements to be read in each block
     * by this processor
     */

    num_elem = 0;
    for (iproc = 0; iproc < Proc_Info[2]; iproc++)
      num_elem += Proc_Num_Elem_In_Blk[iproc][iblk];

    if (num_elem > 0) {
      connect[iblk] = (int *) array_alloc(__FILE__, __LINE__, 1,
                                     (num_elem * Proc_Nodes_Per_Elem[0][iblk]),
                                          sizeof(int));
      if (!connect[iblk]) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
    } else
      connect[iblk] = NULL;

  } /* End "for (iblk = 0; iblk < Num_Elem_Blk; iblk++)" */

  /* need an offset for each element block */
  offset = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Blk,
                               sizeof(int));
  if (!offset) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) offset[iblk] = 0;

  /*
   * setup the connect_ptr array;
   * this will make sorting easier later
   */
  j = 0;
  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {
    for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
      for (i = 0; i < Proc_Num_Elem_In_Blk[iproc][iblk]; i++) {
        connect_ptr[j++] = &connect[iblk][offset[iblk]];
        /* move the offset to the next element */
        offset[iblk] += Proc_Nodes_Per_Elem[0][iblk];
      }
  }

  /**********************************/
  /* read in the connectivity lists */
  /**********************************/

  strcpy(cTemp, PIO_Info.Par_Exo_Res_File_Name);

  /* cpu word size doesn't matter, only retrieving int's */
  cpu_ws = 0;
  io_ws = 0;

  /* reset the element block offsets */
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) offset[iblk] = 0;
  offset1 = 0;

  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {

    num_elem = 0;

    gen_par_filename(cTemp, Par_Nem_File_Name, Proc_Ids[iproc], Proc_Info[0]);

    if ((pexoid = ex_open(Par_Nem_File_Name, EX_READ, &cpu_ws, &io_ws,
                          &ver)) == -1) {
      fprintf(stderr,"[%d] %s: Could not open parallel Exodus II file\n",
              Proc, yo);
      exit (1);
    }

    for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

      /* Make sure that there is something to get */
      if (Proc_Num_Elem_In_Blk[iproc][iblk] > 0) {
        check_exodus_error(iproc,ex_get_elem_conn(pexoid,
                                            EB_Ids[iblk],
                                            &connect[iblk][offset[iblk]]),
                           "ex_get_elem_conn");

      }

      /* move this blocks offset for the next processor */
      offset[iblk] += Proc_Num_Elem_In_Blk[iproc][iblk]
                      * Proc_Nodes_Per_Elem[0][iblk];

      num_elem += Proc_Num_Elem_In_Blk[iproc][iblk];

    }

    if (conv_connect(pexoid, iproc, &connect_ptr[offset1]) < 0) {
      fprintf(stderr, "[%d] %s: error converting connect table node numbers.\n",
              Proc, yo);
      return -1;
    }

    offset1 += num_elem;

    /* Close the parallel file */
    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing parallel exoII file\n",
               yo);
      exit (1);
    }

  } /* End: "for (iproc = 0; iproc < Proc_Info[2]; iproc++)" */

  safe_free ((void **) &offset);

  conn_sorted = (int **) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Net,
                                     sizeof(int *));
  if (!conn_sorted) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  /* sort the connect tables */
  for (i = 0; i < Num_Elem_Net; i++)
    conn_sorted[i] = connect_ptr[Elem_Sort_Map[i]];

  /* done with this array */
  safe_free ((void **) &connect_ptr);

  /**********************************************/
  /* fanin and write out the connectivity lists */
  /**********************************************/

  offset1 = 0;
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

    /*
     * find the message size for this block
     */
    num_mesgs = break_message_up(sizeof(int), EB_Cnts[iblk],
                               (MAX_CHUNK_SIZE / Proc_Nodes_Per_Elem[0][iblk]),
                                 &start_pos);

    /*
     * The first record will always be the largest record, it is assumed.
     * Therefore, we can allocate the ordered array at the top of the
     * loop, and use it throughout.
     */

    gsize        = start_pos[1] - start_pos[0];
    gvec_ordered = (int *) array_alloc(__FILE__, __LINE__, 1,
                                       (gsize * Proc_Nodes_Per_Elem[0][iblk]),
                                       sizeof(int));

#ifdef DEBUG
    if (Debug_Flag >= 2 && Proc == 0) {
      printf("\n\nMessage summary for Connectivity Table:\n");
      printf("\tElemental Block Id: %d\n", EB_Ids[iblk]);
      printf("\tNumber of Nodes per Element: %d\n",
             Proc_Nodes_Per_Elem[0][iblk]);
      printf("\tNumber of messages for connect table: %d\n", num_mesgs);
      printf("\tMax message size for connect table: %d\n", gsize);
      printf("\tMin message size for connect table: %d\n",
             start_pos[num_mesgs]-start_pos[num_mesgs-1]);
    }
#endif

    for (imsg = 0; imsg < num_mesgs; imsg++) {

      gsize = start_pos[imsg+1] - start_pos[imsg];

      /* need to compensate for the element blocks */
      /* Note: elem_map is from 1 to n, not 0 to n-1 */
      start = start_pos[imsg] + offset1 + 1;
      end   = start_pos[imsg+1] + offset1;


      if (fanin_int_2d_slab(GElem_Seq, conn_sorted, Num_Elem_Net,
                            Proc_Nodes_Per_Elem[0][iblk],
                            start, end, gvec_ordered) < 0) {
        fprintf(stderr, "[%d] %s: error while faning in the connect table.\n",
                Proc, yo);
        return -1;
      }

      if (Proc == 0)
        check_exodus_error(iproc,ex_put_n_elem_conn(exoid_out,
                                              EB_Ids[iblk],
                                              (start_pos[imsg] + 1),
                                              gsize, gvec_ordered),
                           "ex_put_n_elem_conn");

    } /* End: "for (imsg = 0; imsg < num_mesgs; imsg++)" */

    /* set the offset for the next element block */
    offset1 += EB_Cnts[iblk];

    safe_free ((void **) &gvec_ordered);
    safe_free ((void **) &start_pos);

  } /* End: "for (iblk = 0; iblk < Num_Elem_Blk; iblk++)" */

  safe_free ((void **) &conn_sorted);

  for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
    if (connect[iblk] != NULL) safe_free ((void **) &connect[iblk]);
  safe_free ((void **) &connect);

  return 0;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int conv_connect(int pexoid, int iproc, int **connect)
{
  /* Local Variables */
  char    *yo = "conv_connect";

  int      iblk, ielem, inode, gnode, pos, offset;
  int     *node_num_map;

  /*------------------- begin execution -------------------------------------*/

  node_num_map = (int *) array_alloc (__FILE__, __LINE__, 1,
                                      Proc_Num_Nodes[iproc], sizeof(int));
  if (!node_num_map) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  /* read this processor's global node map */
  if (ex_get_node_num_map(pexoid, node_num_map) < 0) {
    fprintf(stderr, "[%d] %s: error reading node number map.\n",
            Proc, yo);
    return -1;
  }

  /* For each node in the node_num_map, replace it with it's global id
     so the node_num_map becomes a map from local (on_processor)
     id to global (all_processors) id
  */
  for (inode = 0; inode < Proc_Num_Nodes[iproc]; inode++) {
    gnode = node_num_map[inode];
    pos = bin_search2(gnode, Num_Nodes_Net, GNodes[0]);
    if (pos < 0) {
      fprintf(stderr, "[%d] %s: can't find node, %d, in global map.\n",
	      Proc, yo, gnode);
      return -1;
    } else {
      node_num_map[inode] = GNode_Seq[pos];
    }
  }
    
  offset = 0;
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {
    for (ielem = 0; ielem < Proc_Num_Elem_In_Blk[iproc][iblk]; ielem++) {
      for (inode = 0; inode < Proc_Nodes_Per_Elem[0][iblk]; inode++) {
        /* get the global number from the local node number map */
        connect[offset][inode] = node_num_map[connect[offset][inode] - 1];
      }
      offset++;   /* increment to the next element in the list */
    }
  }

  safe_free ((void **) &node_num_map);

  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int read_put_attrib(int exoid_out, int word_size)
{
  /* Local Variables */
  char    *yo = "read_put_attrib";

  char     cTemp[MAX_FNL+1];
  int      pexoid, iproc, iblk, i, j;
  int      cpu_ws, io_ws, imsg, num_elem;
  int      gsize, num_mesgs, *start_pos, start, end;
  int     *offset, offset1;


  float    ver;
  float  **attrib_sp, **attr_ptr_sp, **attr_srtd_sp;
  double **attrib_dp, **attr_ptr_dp, **attr_srtd_dp;
  void    *hold_void;
  void    *gvec_ordered;


  /*------------------- begin execution -------------------------------------*/

  /* allocate memory to hold the local values */
  if (word_size == sizeof(float)) {
    attr_ptr_sp = (float **) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Gross,
                                         sizeof(float *));
    attrib_sp = (float **) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Blk,
                                       sizeof(float *));
    if ((!attr_ptr_sp) || (!attrib_sp)) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
  }
  else {
    attr_ptr_dp = (double **) array_alloc(__FILE__, __LINE__, 1,
                                          Num_Elem_Gross, sizeof(double *));
    attrib_dp = (double **) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Blk,
                                        sizeof(double *));
    if ((!attr_ptr_dp) || (!attrib_dp)) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
  }

  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

    if (Proc_Num_Attr[0][iblk] > 0) {
      /*
       * count up the number of elements to be read in each block
       * by this processor
       */

      num_elem = 0;
      for (iproc = 0; iproc < Proc_Info[2]; iproc++)
        num_elem += Proc_Num_Elem_In_Blk[iproc][iblk];

      if (num_elem > 0) {
        hold_void = array_alloc (__FILE__, __LINE__, 1,
                                 (num_elem * Proc_Num_Attr[0][iblk]),
                                 word_size);
        if (!hold_void) {
          fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
          exit (1);
        }
      }
      else
        hold_void = NULL;
    }
    else
      hold_void = NULL;

    if (word_size == sizeof(float)) attrib_sp[iblk] = (float *) hold_void;
    else                            attrib_dp[iblk] = (double *) hold_void;

  } /* End "for (iblk = 0; iblk < Num_Elem_Blk; iblk++)" */

  /* need an offset for each element block */
  offset = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Blk,
                               sizeof(int));
  if (!offset) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) offset[iblk] = 0;

  /*
   * setup the attr_ptr_?? array;
   * this will make sorting easier later
   */
  j = 0;
  for (iproc = 0; iproc < Proc_Info[2]; iproc++)
    for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
      for (i = 0; i < Proc_Num_Elem_In_Blk[iproc][iblk]; i++) {
        if (Proc_Num_Attr[0][iblk] > 0)
	  if (word_size == sizeof(float)) hold_void = &attrib_sp[iblk][offset[iblk]];
	  else                            hold_void = &attrib_dp[iblk][offset[iblk]];
          
        else
          hold_void = NULL;

        if (word_size == sizeof(float))
          attr_ptr_sp[j++] = (float *) hold_void;
        else
          attr_ptr_dp[j++] = (double *) hold_void;

        /* move the offset to the next element */
        offset[iblk] += Proc_Num_Attr[0][iblk];
      }


  /********************************/
  /* read in the attribute tables */
  /********************************/

  strcpy(cTemp, PIO_Info.Par_Exo_Res_File_Name);

  cpu_ws = word_size;
  io_ws = 0;

  /* reset the element block offsets */
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) offset[iblk] = 0;

  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {

    num_elem = 0;

    gen_par_filename(cTemp, Par_Nem_File_Name, Proc_Ids[iproc], Proc_Info[0]);

    if ((pexoid = ex_open(Par_Nem_File_Name, EX_READ, &cpu_ws, &io_ws,
                          &ver)) == -1) {
      fprintf(stderr,"[%d] %s: Could not open parallel Exodus II file\n",
              Proc, yo);
      exit (1);
    }

    for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

      /* Make sure that there is something to get */
      if ((Proc_Num_Attr[0][iblk] > 0)
           && (Proc_Num_Elem_In_Blk[iproc][iblk] > 0)) {

        if (word_size == sizeof(float))
          hold_void = &attrib_sp[iblk][offset[iblk]];
        else
          hold_void = &attrib_dp[iblk][offset[iblk]];


        check_exodus_error(iproc,ex_get_elem_attr(pexoid,
                                            EB_Ids[iblk],
                                            hold_void),
                           "ex_get_elem_conn");

      }

      /* move this blocks offset for the next processor */
      offset[iblk] += Proc_Num_Elem_In_Blk[iproc][iblk]
                      * Proc_Num_Attr[0][iblk];

    }

    /* Close the parallel file */
    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing parallel exoII file\n",
               yo);
      exit (1);
    }

  } /* End: "for (iproc = 0; iproc < Proc_Info[2]; iproc++)" */

  safe_free ((void **) &offset);

  if (word_size == sizeof(float)) {
    attr_srtd_sp = (float **) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Net,
                                          sizeof(float *));
    if (!attr_srtd_sp) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
  }
  else {
    attr_srtd_dp = (double **) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Net,
                                           sizeof(double *));
    if (!attr_srtd_dp) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }
  }

  /* sort the connect tables */
  if (word_size == sizeof(float)) {
    for (i = 0; i < Num_Elem_Net; i++)
      attr_srtd_sp[i] = attr_ptr_sp[Elem_Sort_Map[i]];

    /* done with this array */
    safe_free ((void **) &attr_ptr_sp);

  }
  else {
    for (i = 0; i < Num_Elem_Net; i++)
      attr_srtd_dp[i] = attr_ptr_dp[Elem_Sort_Map[i]];

    /* done with this array */
    safe_free ((void **) &attr_ptr_dp);
  }

  /********************************************/
  /* fanin and write out the attribute tables */
  /********************************************/

  offset1 = 0;
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

    if (Proc_Num_Attr[0][iblk] > 0) {
      /*
       * find the message size for this block
       */
      num_mesgs = break_message_up(word_size, EB_Cnts[iblk],
                               (MAX_CHUNK_SIZE / Proc_Num_Attr[0][iblk]),
                                   &start_pos);

      /*
       * The first record will always be the largest record, it is assumed.
       * Therefore, we can allocate the ordered array at the top of the
       * loop, and use it throughout.
       */

      gsize        = start_pos[1] - start_pos[0];
      gvec_ordered = array_alloc(__FILE__, __LINE__, 1,
                                 (gsize * Proc_Num_Attr[0][iblk]),
                                 word_size);

#ifdef DEBUG
      if (Debug_Flag >= 2 && Proc == 0) {
        printf("\n\nMessage summary for Attribute Table:\n");
        printf("\tElemental Block Id: %d\n", EB_Ids[iblk]);
        printf("\tNumber of Attributes per Element: %d\n",
               Proc_Num_Attr[0][iblk]);
        printf("\tNumber of messages for attribute table: %d\n", num_mesgs);
        printf("\tMax message size per elemental variable: %d\n", gsize);
        printf("\tMin message size per elemental variable: %d\n",
               start_pos[num_mesgs]-start_pos[num_mesgs-1]);
      }
#endif

      for (imsg = 0; imsg < num_mesgs; imsg++) {

        gsize = start_pos[imsg+1] - start_pos[imsg];

        /* need to compensate for the element blocks */
        /* Note: elem_map is from 1 to n, not 0 to n-1 */
        start = start_pos[imsg] + offset1 + 1;
        end   = start_pos[imsg+1] + offset1;

        if (word_size == sizeof(float)) hold_void = attr_srtd_sp;
        else                            hold_void = attr_srtd_dp;

        if (fanin_void_2d_slab(word_size, GElem_Seq, hold_void, Num_Elem_Net,
                               Proc_Num_Attr[0][iblk],
                               start, end, gvec_ordered) < 0) {
          fprintf(stderr, "[%d] %s: error while faning in the attributes.\n",
                  Proc, yo);
          return -1;
        }

        if (Proc == 0)
          check_exodus_error(iproc,ex_put_n_elem_attr(exoid_out,
                                                EB_Ids[iblk],
                                                (start_pos[imsg] + 1),
                                                gsize, gvec_ordered),
                             "ex_put_n_elem_attr");

      } /* End: "for (imsg = 0; imsg < num_mesgs; imsg++)" */

      safe_free ((void **) &gvec_ordered);
      safe_free ((void **) &start_pos);

    } /* End: "if (Proc_Num_Attr[0][iblk] > 0)" */

    /* set the offset for the next element block */
    offset1 += EB_Cnts[iblk];

  } /* End: "for (iblk = 0; iblk < Num_Elem_Blk; iblk++)" */

  if (word_size == sizeof(float)) {
    safe_free ((void **) &attr_srtd_sp);

    for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
      if (attrib_sp[iblk] != NULL) safe_free ((void **) &attrib_sp[iblk]);
    safe_free ((void **) &attrib_sp);
  }
  else {
    safe_free ((void **) &attr_srtd_dp);

    for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
      if (attrib_dp[iblk] != NULL) safe_free ((void **) &attrib_dp[iblk]);
    safe_free ((void **) &attrib_dp);
  }

  return 0;
}
