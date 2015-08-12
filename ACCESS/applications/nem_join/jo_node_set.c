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
*  put_ns_info                  int            jo_mesh:put_mesh
*  read_put_node_sets           int            jo_mesh:put_mesh
*  conv_node_sets               int                    read_put_node_sets
******************************************************************************/

static int  conv_node_sets(int, int, int **, int *);

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int put_ns_info(int exoid_out, int *dist_fact)
{
  /* Local Variables */
  int      iset, iproc = 0;

  /*------------------- begin execution -------------------------------------*/

  /*
   * Since global distribution factors are not distributed by nem_spread,
   * need to find out which node sets have distribution factors
   */
  for (iset = 0; iset < Num_Node_Set; iset++) {
    dist_fact[iset] = 0;
    for (iproc = 0; iproc < Proc_Info[2]; iproc++) {
      if (Proc_NS_DF_Count[iproc][iset] > 0) {
        dist_fact[iset] = 1;
        break;
      }
    }
  }

  /* now fanin these values */
  fanin_int(dist_fact, Num_Node_Set, 0, Proc, Num_Proc);

  /* write them to the results file */
  if (Proc == 0) {

    for (iset = 0; iset < Num_Node_Set; iset++) {

      if (dist_fact[iset] > 0) dist_fact[iset] = NS_Cnts[iset];

      check_exodus_error(iproc,ex_put_node_set_param(exoid_out,
                                               Proc_NS_Ids[0][iset],
                                               NS_Cnts[iset],
                                               dist_fact[iset]),
                          "ex_put_node_set_param");

    }

    safe_free ((void **) &NS_Cnts);
  }

  /* Broadcast out the complete distribution factor list */
  brdcst (Proc, Num_Proc, (char *) dist_fact, (Num_Node_Set * sizeof(int)), 0);

  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int read_put_node_sets(int exoid_out, int word_size, int *dist_fact)
{
  /* Local Variables */
  char    *yo = "read_put_node_sets";

  char     cTemp[MAX_FNL+1];
  int      pexoid, iset, iproc;
  int      cpu_ws, io_ws, imsg, num_nodes;
  int      gsize, num_mesgs, *start_pos;
  int      i, j, fill, offset1, err;
  int      df_flag = 0;
  int     *ivec_ordered;
  int    **node_set, *nodes;
  int     *offset;

  float    ver;
  float  **dist_fact_sp=NULL, *df_sp=NULL, *hold_float=NULL;
  double **dist_fact_dp=NULL, *df_dp=NULL, *hold_double=NULL;
  void    *hold_df;
  void    *fvec_ordered;

  /*------------------- begin execution -------------------------------------*/

  /* find out if there are any distribution factors */
  for (iset = 0; iset < Num_Node_Set; iset++)
    if (dist_fact[iset] > 0) {
      df_flag = 1;
      break;
    }

  /* allocate memory */
  node_set = (int **) array_alloc(__FILE__, __LINE__, 1, Num_Node_Set,
                                  sizeof(int *));
  offset = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Node_Set,
                               sizeof(int));
  if ((!node_set) || (!offset)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  if (df_flag) {
    if (word_size == sizeof(float)) {
      dist_fact_sp = (float **) array_alloc(__FILE__, __LINE__, 1,
                                            Num_Node_Set, sizeof(float *));
      if (!dist_fact_sp) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
    }
    else {
      dist_fact_dp = (double **) array_alloc(__FILE__, __LINE__, 1,
                                             Num_Node_Set, sizeof(double *));
      if (!dist_fact_dp) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
    }
  }

  /*
   * now find out how much space is needed
   * for each node set on this processor
   */
  for (iset = 0; iset < Num_Node_Set; iset++) {
    num_nodes = 0;
    for (iproc = 0; iproc < Proc_Info[2]; iproc++)
      num_nodes += Proc_NS_Count[iproc][iset];

    if (num_nodes > 0) {
      node_set[iset] = (int *) array_alloc(__FILE__, __LINE__, 1, num_nodes,
                                           sizeof(int));
      if (!node_set[iset]) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }

      /*
       * if there is are distribution factors for this node set,
       * then allocate the memory for them
       */
      if (dist_fact[iset] > 0) {
        hold_df = array_alloc(__FILE__, __LINE__, 1, num_nodes, word_size);
        if (!hold_df) {
          fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
          exit (1);
        }
      }
      else
        hold_df = NULL;

    }
    else {
      node_set[iset] = NULL;
      hold_df = NULL;
    }

    /* determine the precision */
    if (df_flag) {
      if (word_size == sizeof(float)) dist_fact_sp[iset] = (float *) hold_df;
      else                            dist_fact_dp[iset] = (double *) hold_df;
    }

    offset[iset] = 0;    /* might as well initialize these here */
  }

  /*************************/
  /* read in the node sets */
  /*************************/

  strcpy(cTemp, PIO_Info.Par_Exo_Res_File_Name);

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

    for (iset = 0; iset < Num_Node_Set; iset++)
      /* Make sure that there is something to get */
      if (Proc_NS_Count[iproc][iset] > 0) {
        check_exodus_error(iproc,ex_get_node_set(pexoid,
                                           Proc_NS_Ids[0][iset],
                                           &node_set[iset][offset[iset]]),
                           "ex_get_node_set");

        if (dist_fact[iset]) {

          if (word_size == sizeof(float))
            hold_df = &dist_fact_sp[iset][offset[iset]];
          else
            hold_df = &dist_fact_dp[iset][offset[iset]];

          check_exodus_error(iproc,ex_get_node_set_dist_fact(pexoid,
                                                       Proc_NS_Ids[0][iset],
                                                       hold_df),
                             "ex_get_node_set_dist_fact");

         }
       }


    if (conv_node_sets(pexoid, iproc, node_set, offset) < 0) {
      fprintf(stderr, "[%d] %s: error converting node set node numbers.\n",
              Proc, yo);
      return -1;
    }

    /* now move the offsets */
    for (iset = 0; iset < Num_Node_Set; iset++)
      offset[iset] += Proc_NS_Count[iproc][iset];


    /* Close the parallel file */
    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing parallel exoII file\n",
               yo);
      exit (1);
    }

  } /* End: "for (iproc = 0; iproc < Proc_Info[2]; iproc++)" */


  /* sort each list, and remove duplicates */
  for (iset = 0; iset < Num_Node_Set; iset++) {
    if (dist_fact[iset]) {
      if (word_size == sizeof(float))
        sort_int_float(offset[iset], node_set[iset], dist_fact_sp[iset]);
      else
        sort_int_double(offset[iset], node_set[iset], dist_fact_dp[iset]);
    }
    else
      sort_int(offset[iset], node_set[iset]);

    fill = 0;
    for (i = 0; i < offset[iset]; i++) {
      node_set[iset][fill] = node_set[iset][i];
      if (dist_fact[iset]) {
        if (word_size == sizeof(float))
          dist_fact_sp[iset][fill] = dist_fact_sp[iset][i];
        else
          dist_fact_dp[iset][fill] = dist_fact_dp[iset][i];
      }

      /* check if this is a repeated index */
      if ((i+1) == offset[iset] || node_set[iset][i] != node_set[iset][i+1])
        fill++;
    }

    /* reset the vector length */
    offset[iset] = fill;
  } /* End: "for (iset = 0; iset < Num_Node_Set; iset++)" */

  /************************************/
  /* fanin and write out the node set */
  /************************************/

  /*
   * To do this without much extra work, an array or the entire
   * set of nodes will be fanned in. It will only contain 1's and
   * 0's, with a 1 meaning that this node is in the node set
   *
   * In order to save time, grab the df's at the same time
   */
  nodes = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Nodes_Net,
                              sizeof(int));
  if (!nodes) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  if (df_flag) {
    hold_df = array_alloc(__FILE__, __LINE__, 1, Num_Nodes_Net, word_size);
    if (!hold_df) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }

    if (word_size == sizeof(float)) df_sp = (float *) hold_df;
    else                            df_dp = (double *) hold_df;
  }

  /*
   * find the message size for this block
   */
  if (df_flag)
    /*
     * if there are distribution factors, adjust the message size
     * so as not to overrun any comm buffers
     */
    num_mesgs = break_message_up(word_size, Num_Node, MAX_CHUNK_SIZE/2,
                                 &start_pos);
  else
    num_mesgs = break_message_up(sizeof(int), Num_Node, MAX_CHUNK_SIZE,
                                   &start_pos);

  /*
   * The first record will always be the largest record, it is assumed.
   * Therefore, we can allocate the ordered array at the top of the
   * loop, and use it throughout.
   */

  gsize        = start_pos[1] - start_pos[0];
  ivec_ordered = (int *) array_alloc(__FILE__, __LINE__, 1, gsize,
                                     sizeof(int));
  if (df_flag) {
    fvec_ordered = array_alloc(__FILE__, __LINE__, 1, gsize, word_size);

    if (word_size == sizeof(float)) hold_float = (float *) fvec_ordered;
    else                            hold_double = (double *) fvec_ordered;
  }

#ifdef DEBUG
  if (Debug_Flag >= 2 && Proc == 0) {
    printf("\n\nMessage summary for Node Sets:\n");
    printf("\tNumber of Nodes to be fanned in: %d\n", Num_Node);
    if (df_flag)
      printf("\tNumber of Dist Factors to be fanned in: %d\n", Num_Node);
    printf("\tNumber of messages per node set: %d\n", num_mesgs);
    printf("\tMax message size per node set: %d\n", gsize);
    printf("\tMin message size per node set: %d\n",
           start_pos[num_mesgs]-start_pos[num_mesgs-1]);
  }
#endif


  for (iset = 0; iset < Num_Node_Set; iset++) {
    /* setup the array for this node set */
    j = 0;
    for (i = 0; i < Num_Nodes_Net; i++) {
      if ((node_set[iset] != NULL) && (j < offset[iset]) &&
            (GNode_Seq[i] == node_set[iset][j])) {
        nodes[i] = 1;
        if (dist_fact[iset]) {
          if (word_size == sizeof(float)) df_sp[i] = dist_fact_sp[iset][j];
          else                            df_dp[i] = dist_fact_dp[iset][j];
        }
        j++;
      }
      else {
        nodes[i] = 0;
        if (dist_fact[iset]) {
          if (word_size == sizeof(float)) df_sp[i] = 0.0f;
          else                            df_dp[i] = 0.0;
        }
      }
    }

#ifdef DEBUG
    if (Debug_Flag >= 7 && Proc == 0) {
      printf("\n\nFanning in Node Set and Distribution Factors\n");
      printf("Node Set %d\n", (iset + 1));
      printf("Number of nodes found %d\n", j);
      printf("\tnumber\tnodes");
      if (dist_fact[iset])
        printf("\tdist fact\n");
      else
        printf("\n");
      for (i = 0; i < Num_Nodes_Net; i++) {
        if (dist_fact[iset]) {
          if (word_size == sizeof(float))
            printf("\t%d\t%d\t%f\n", i, nodes[i], df_sp[i]);
          else
            printf("\t%d\t%d\t%lf\n", i, nodes[i], df_dp[i]);
        }
        else
          printf("\t%d\t%d\n", i, nodes[i]);
      }
    }
#endif


    offset1 = 0;
    /* now fanin the arrays */
    for (imsg = 0; imsg < num_mesgs; imsg++) {

      gsize = start_pos[imsg+1] - start_pos[imsg];

      if (dist_fact[iset])
        err = fanin_int_void_slab(word_size, GNode_Seq, nodes, hold_df,
                                  Num_Nodes_Net, (start_pos[imsg]+1),
                                  start_pos[imsg+1], ivec_ordered,
                                  fvec_ordered);
      else
        err = fanin_int_slab(GNode_Seq, nodes, Num_Nodes_Net,
                             (start_pos[imsg]+1), start_pos[imsg+1],
                             ivec_ordered);
      if (err < 0) {
        fprintf(stderr, "[%d] %s: error while faning in node set, %d.\n",
                Proc, yo, (iset + 1));
        return -1;
      }

      /* now get out the node set */
      fill = 0;
      for (i = 0; i < gsize; i++)
        if (ivec_ordered[i] > 0) {
          ivec_ordered[fill] = start_pos[imsg] + i + 1;

          if (dist_fact[iset]) {
            if (word_size == sizeof(float))
              hold_float[fill] = hold_float[i];
            else
              hold_double[fill] = hold_double[i];
          }
          fill++;
        }

      if ((Proc == 0) && (fill > 0)) {
        check_exodus_error(iproc,ex_put_n_node_set(exoid_out, Proc_NS_Ids[0][iset],
                                             (offset1 + 1), fill, ivec_ordered),
                           "ex_put_n_node_set");

        if (dist_fact[iset]) {
          check_exodus_error(iproc,ex_put_n_node_set_df(exoid_out,
                                                  Proc_NS_Ids[0][iset],
                                                  (offset1 + 1), fill,
                                                  fvec_ordered),
                             "ex_put_n_node_set");
        }

        offset1 += fill;

      }

    } /* End: "for (imsg = 0; imsg < num_mesgs; imsg++)" */

  } /* End: "for (iset = 0; iset < Num_Node_Set; iset++)" */

  safe_free ((void **) &nodes);
  safe_free ((void **) &ivec_ordered);
  if (df_flag) {
    safe_free ((void **) &hold_df);
    safe_free ((void **) &fvec_ordered);
  }
  safe_free ((void **) &start_pos);
  safe_free ((void **) &offset);
  for (iset = 0; iset < Num_Node_Set; iset++) {
    safe_free ((void **) &node_set[iset]);
    if (dist_fact[iset]) {
      if (word_size == sizeof(float))
        safe_free ((void **) &dist_fact_sp[iset]);
      else
        safe_free ((void **) &dist_fact_dp[iset]);
    }
    safe_free ((void **) &node_set[iset]);
  }
  safe_free ((void **) &node_set);
  if (df_flag) {
    if (word_size == sizeof(float))
      safe_free ((void **) &dist_fact_sp);
    else
      safe_free ((void **) &dist_fact_dp);
  }
  return 0;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int conv_node_sets(int pexoid, int iproc, int **node_sets, int *offsets)
{
  /* Local Variables */
  char    *yo = "conv_node_sets";

  int      iset, inode, gnode, lnode, pos;
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

  for (iset = 0; iset < Num_Node_Set; iset++) {
    for (inode = 0; inode < Proc_NS_Count[iproc][iset]; inode++) {

      /* get the global number from the local node number map */
      lnode = node_sets[iset][offsets[iset] + inode];
      gnode = node_num_map[lnode - 1];

      /* now find that number in the global list */
      if ((pos = bin_search2(gnode, Num_Nodes_Net, GNodes[0])) < 0) {
        fprintf(stderr, "[%d] %s: can't find node, %d, in global map.\n",
                Proc, yo, gnode);
        return -1;
      }

      /* once that position is found, the global internal number
       * should be the corresponding number in the sequential
       * node number map
       */
       node_sets[iset][offsets[iset] + inode] = GNode_Seq[pos];
    }
  }

  safe_free ((void **) &node_num_map);

  return 0;

}

