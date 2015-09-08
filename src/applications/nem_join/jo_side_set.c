#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "sort_utils.h"

#include "pe_common.h"
#include "el_geom_const.h"
#include "ps_pario_const.h"
#include "rf_allo.h"
#include "rf_comm.h"
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
*  put_ss_info                  int            jo_mesh:put_mesh
*  read_put_side_sets           int            jo_mesh:put_mesh
*  conv_side_sets               int                    read_put_side_sets
******************************************************************************/

static int  conv_side_sets(int, int, int **, int *, int *);

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int put_ss_info(int exoid_out, int *dist_fact)
{
  /* Local Variables */
  char    *yo = "put_ss_info";

  int      iset, iproc=0, df_cnt;

  /*------------------- begin execution -------------------------------------*/

  /*
   * Since global distribution factors are not distributed by nem_spread,
   * need to find out which side sets have distribution factors
   *
   * NOTE: In order to find out the distribution factor counts, find out
   * how many distribution factors there are per element. This assumes
   * that the side sets are have homogeneous element types (so far
   * this is the case in every example that I have). This makes it
   * much easier to get in the distribution factors in with the
   * side sets.
   */
  for (iset = 0; iset < Num_Side_Set; iset++) {
    dist_fact[iset] = 0;
    for (iproc = 0; iproc < Proc_Info[2]; iproc++) {
      if (Proc_SS_DF_Count[iproc][iset] > 0) {
        /* check if my assumption is correct */
        if ((Proc_SS_DF_Count[iproc][iset] %
               Proc_SS_Elem_Count[iproc][iset]) > 0) {
          fprintf(stderr, "[%d] %s: Side set %d not homogeneous\n",
                  Proc, yo, (iset + 1));
          return (-1);
        }
        dist_fact[iset] = Proc_SS_DF_Count[iproc][iset] /
                            Proc_SS_Elem_Count[iproc][iset];
        break;
      }
    }
  }

  /* now fanin these values */
  fanin_int(dist_fact, Num_Side_Set, 0, Proc, Num_Proc);

  /* write them to the results file */
  if (Proc == 0) {

    for (iset = 0; iset < Num_Side_Set; iset++) {

      df_cnt = dist_fact[iset] * SS_Cnts[iset];

      check_exodus_error(iproc,ex_put_side_set_param(exoid_out,
                                               Proc_SS_Ids[0][iset],
                                               SS_Cnts[iset],
                                               df_cnt),
                          "ex_put_side_set_param");

    }

    safe_free ((void **) &SS_Cnts);
  }

  /* Broadcast out the complete distribution factor list */
  brdcst (Proc, Num_Proc, (char *) dist_fact, (Num_Side_Set * sizeof(int)), 0);

  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int read_put_side_sets(int exoid_out, int word_size, int *dist_fact)
{
  /* Local Variables */
  char     *yo = "read_put_side_sets";

  char      cTemp[MAX_FNL+1];
  int       pexoid, iset, iproc;
  int       cpu_ws, io_ws, imsg, num_elem, total_elem;
  int       max_sides, sides_this_elem;
  int       gsize, num_mesgs, *start_pos;
  int       i, j, k, fill, offset1, err;
  int       df_flag = 0, max_df_per_elem = 0;
  int      *ivec_ordered, *ids_ordered;
  int     **side_set, **side_ids, *sides;
  int      *offset;
  int      *loc_index;

  float     ver;
  float  ***dist_fact_sp=NULL, **df_sp=NULL, *hold_float=NULL;
  double ***dist_fact_dp=NULL, **df_dp=NULL, *hold_double=NULL;
  void     *hold_df=NULL;
  void     *fvec_ordered;

  /*------------------- begin execution -------------------------------------*/

  /* find out if there are any distribution factors */
  for (iset = 0; iset < Num_Side_Set; iset++)
    if (dist_fact[iset] > 0) {
      df_flag = 1;
      break;
    }

  /* allocate memory */
  side_set = (int **) array_alloc(__FILE__, __LINE__, 1, (2 * Num_Side_Set),
                                  sizeof(int *));
  offset = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Side_Set,
                               sizeof(int));
  if ((!side_set) || (!offset)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }
  side_ids = side_set + Num_Side_Set;

  if (df_flag) {
    if (word_size == sizeof(float)) {
      dist_fact_sp = (float ***) array_alloc(__FILE__, __LINE__, 1,
                                             Num_Side_Set, sizeof(float **));
      if (!dist_fact_sp) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
    }
    else {
      dist_fact_dp = (double ***) array_alloc(__FILE__, __LINE__, 1,
                                              Num_Side_Set, sizeof(double **));
      if (!dist_fact_dp) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
    }
  }

  /*
   * now find out how much space is needed
   * for each side set on this processor
   */
  total_elem = 0;
  for (iset = 0; iset < Num_Side_Set; iset++) {
    num_elem = 0;
    for (iproc = 0; iproc < Proc_Info[2]; iproc++)
      num_elem += Proc_SS_Elem_Count[iproc][iset];

    if (num_elem > 0) {
      side_set[iset] = (int *) array_alloc(__FILE__, __LINE__, 1,
                                           (2 * num_elem), sizeof(int));
      if (!side_set[iset]) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
      side_ids[iset] = side_set[iset] + num_elem;

      /*
       * if there is are distribution factors for this side set,
       * then allocate the memory for them
       *
       * the distribution count should be the number of nodes per
       * side (held in dist_fact) times the number of elements
       */
      if (dist_fact[iset] > 0) {
        hold_df = array_alloc(__FILE__, __LINE__, 2, num_elem, dist_fact[iset],
                              word_size);
        if (!hold_df) {
          fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
          exit (1);
        }

        if (dist_fact[iset] > max_df_per_elem)
          max_df_per_elem = dist_fact[iset];
      }
      else
        hold_df = NULL;

    }
    else {
      side_set[iset] = NULL;
      hold_df = NULL;
    }

    /* determine the precision */
    if (df_flag) {
      if (word_size == sizeof(float)) dist_fact_sp[iset] = (float **) hold_df;
      else                            dist_fact_dp[iset] = (double **) hold_df;
    }

    total_elem += num_elem;

    offset[iset] = 0;    /* might as well initialize these here */
  }

  /* Do an indexed sort of the GElems list... */
  loc_index = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Net,
                                  sizeof(int));

  /* Initialize index array */
  for (i=0; i < Num_Elem_Net; i++)
    loc_index[i] = i;
  gds_iqsort(GElems[0], loc_index, Num_Elem_Net);
     
  /*************************/
  /* read in the side sets */
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

    for (iset = 0; iset < Num_Side_Set; iset++) {
      /* Make sure that there is something to get */
      if (Proc_SS_Elem_Count[iproc][iset] > 0) {
        check_exodus_error(iproc,ex_get_side_set(pexoid,
                                           Proc_SS_Ids[0][iset],
                                           &side_set[iset][offset[iset]],
                                           &side_ids[iset][offset[iset]]),
                           "ex_get_side_set");

        if (dist_fact[iset]) {

          /*
           * Use the fact that array_alloc() gets a contiguous block
           * of memory to read in the distribution factors
           */
          if (word_size == sizeof(float))
            hold_df = dist_fact_sp[iset][offset[iset]];
          else
            hold_df = dist_fact_dp[iset][offset[iset]];

          check_exodus_error(iproc,ex_get_side_set_dist_fact(pexoid,
                                                       Proc_SS_Ids[0][iset],
                                                       hold_df),
                             "ex_get_side_set_dist_fact");

        }
      }

#ifdef DEBUG
      if (Debug_Flag >= 7 && Proc == 0) {
        printf("\n\nSide Set Values read in from file for proc %d\n",
               Proc_Ids[iproc]);
        printf("Side Set %d\n", (iset + 1));
        printf("\tnumber\tside");
        if (dist_fact[iset])
          printf("\tdist fact\n");
        else
          printf("\n");
        for (i = 0; i < Proc_SS_Elem_Count[iproc][iset]; i++) {
          printf("\t%d\t%d", side_set[iset][offset[iset] + i],
                             side_ids[iset][offset[iset] + i]);
          if (dist_fact[iset]) {
            printf("\t");
            for (j = 0; j < dist_fact[iset]; j++) {
              if (word_size == sizeof(float))
                printf("%f  ", dist_fact_sp[iset][offset[iset] + i][j]);
              else
                printf("%lf  ", dist_fact_dp[iset][offset[iset] + i][j]);
            }
          }
          printf("\n");
        }
      }
#endif
    } /* End: for (iset = 0; iset < Num_Side_Set; iset++)" */


    if (conv_side_sets(pexoid, iproc, side_set, offset, loc_index) < 0) {
      fprintf(stderr, "[%d] %s: error converting side set element numbers.\n",
              Proc, yo);
      return -1;
    }

    /* now move the offsets */
    for (iset = 0; iset < Num_Side_Set; iset++)
      offset[iset] += Proc_SS_Elem_Count[iproc][iset];


    /* Close the parallel file */
    if(ex_close (pexoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing parallel exoII file\n",
               yo);
      exit (1);
    }

  } /* End: "for (iproc = 0; iproc < Proc_Info[2]; iproc++)" */

  safe_free((void **) &loc_index);

  max_sides = 0;
  sides_this_elem = 1;

  /* sort each list, and remove duplicates */
  for (iset = 0; iset < Num_Side_Set; iset++) {
    if (dist_fact[iset]) {
      if (word_size == sizeof(float))
        hold_df = dist_fact_sp[iset];
      else
        hold_df = dist_fact_dp[iset];
      sort_int_int_ptr(offset[iset], side_set[iset], side_ids[iset],
                       (char **) hold_df);
    }
    else
      sort_int_int(offset[iset], side_set[iset], side_ids[iset]);

#ifdef DEBUG
      if (Debug_Flag >= 7 && Proc == 0) {
        printf("\nSide Set %d after sorting on Elements\n", (iset + 1));
        printf("\tnumber\telement\tside");
        if (dist_fact[iset])
          printf("\tdist fact\n");
        else
          printf("\n");
        for (i = 0; i < offset[iset]; i++) {
          printf("\t%d\t%d\t%d", (i + 1), side_set[iset][i],
                 side_ids[iset][i]);
          if (dist_fact[iset]) {
            printf("\t");
            for (j = 0; j < dist_fact[iset]; j++) {
              if (word_size == sizeof(float))
                printf("%f  ", dist_fact_sp[iset][i][j]);
              else
                printf("%lf  ", dist_fact_dp[iset][i][j]);
            }
          }
          printf("\n");
        }
      }
#endif


    /* now order each element's entry in the side set by the sides ids */
    fill = 0;
    i = 1;
    do {
      while (i < offset[iset] && side_set[iset][i] == side_set[iset][fill])
        i++;

      if (dist_fact[iset]) {
        if (word_size == sizeof(float))
          hold_df = dist_fact_sp[iset] + fill;
        else
          hold_df = dist_fact_dp[iset] + fill;
        sort_int_ptr(i-fill, side_ids[iset]+fill, (char **) hold_df);
      }
      else
        sort_int(i-fill, side_ids[iset]+fill);

      fill = i;
      i++;

    } while (fill < offset[iset]);

#ifdef DEBUG
      if (Debug_Flag >= 7 && Proc == 0) {
        printf("\nSide Set %d after sorting on Sides\n", (iset + 1));
        printf("\tnumber\telement\tside");
        if (dist_fact[iset])
          printf("\tdist fact\n");
        else
          printf("\n");
        for (i = 0; i < offset[iset]; i++) {
          printf("\t%d\t%d\t%d", (i + 1), side_set[iset][i],
                 side_ids[iset][i]);
          if (dist_fact[iset]) {
            printf("\t");
            for (j = 0; j < dist_fact[iset]; j++) {
              if (word_size == sizeof(float))
                printf("%f  ", dist_fact_sp[iset][i][j]);
              else
                printf("%lf  ", dist_fact_dp[iset][i][j]);
            }
          }
          printf("\n");
        }
      }
#endif

    /* now remove duplicates */
    fill = 0;
    for (i = 0; i < offset[iset]; i++) {
      side_set[iset][fill] = side_set[iset][i];
      side_ids[iset][fill] = side_ids[iset][i];
      if (dist_fact[iset]) {
        if (word_size == sizeof(float))
          dist_fact_sp[iset][fill] = dist_fact_sp[iset][i];
        else
          dist_fact_dp[iset][fill] = dist_fact_dp[iset][i];
      }

      /* check if this is a repeated index
       * Remember to check side ids when looking for duplicates
       */
      if ((i+1) < offset[iset]) {
        /* check if the element is the same */
        if (side_set[iset][i] == side_set[iset][i+1]) {
          if (side_ids[iset][i] != side_ids[iset][i+1]) {
            sides_this_elem++;
            fill++;
          }
        }
        else {
          fill++;
          /*
           * check to see if this elem has more sides in
           * the side set than any previous ones
           */
          if (sides_this_elem > max_sides) max_sides = sides_this_elem;
          sides_this_elem = 1; /* starting on a new elem */
        }
      }
      else fill++;
    }

    /* reset the vector length */
    offset[iset] = fill;

#ifdef DEBUG
      if (Debug_Flag >= 7 && Proc == 0) {
        printf("\nSide Set %d after removing duplicates\n", (iset + 1));
        printf("\tnumber\telement\tside");
        if (dist_fact[iset])
          printf("\tdist fact\n");
        else
          printf("\n");
        for (i = 0; i < offset[iset]; i++) {
          printf("\t%d\t%d\t%d", (i + 1), side_set[iset][i],
                 side_ids[iset][i]);
          if (dist_fact[iset]) {
            printf("\t");
            for (j = 0; j < dist_fact[iset]; j++) {
              if (word_size == sizeof(float))
                printf("%f  ", dist_fact_sp[iset][i][j]);
              else
                printf("%lf  ", dist_fact_dp[iset][i][j]);
            }
          }
          printf("\n");
        }
      }
#endif

  } /* End: "for (iset = 0; iset < Num_Side_Set; iset++)" */

  /* now all of the procs have to have the same value for this */
  max_sides =  gmax_int(max_sides, Proc, Num_Proc);

  /************************************/
  /* fanin and write out the side set */
  /************************************/

  /*
   * To do this without much extra work, an array for the entire
   * set of elements will be fanned in, with max_sides entries
   * for each element. If an element is in the sides set, then
   * the side id is put in one of the element's positions in
   * the array. Otherwise, the array is initialized to 0.
   *
   * In order to save time, grab the df's at the same time
   */
  sides = (int *) array_alloc(__FILE__, __LINE__, 1,
                              (Num_Elem_Net * max_sides), sizeof(int));
  if (!sides) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  if (df_flag) {
    if (word_size == sizeof(float)) {
      df_sp = (float **) array_alloc(__FILE__, __LINE__, 2,
                                     (Num_Elem_Net * max_sides),
                                     max_df_per_elem, sizeof(float));
      if (!df_sp) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
      hold_df = df_sp;
    }
    else {
      df_dp = (double **) array_alloc(__FILE__, __LINE__, 2,
                                      (Num_Elem_Net * max_sides),
                                      max_df_per_elem, sizeof(double));
      if (!df_dp) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
      hold_df = df_dp;
    }
  }

  for (iset = 0; iset < Num_Side_Set; iset++) {
    /*
     * find the message size for this block
     */
    if (dist_fact[iset])
      /*
       * if there are distribution factors, adjust the message size
       * so as not to overrun any comm buffers
       */
      num_mesgs = break_message_up(word_size, Num_Elem,
                          MAX_CHUNK_SIZE / (max_sides * (dist_fact[iset] + 1)),
                                   &start_pos);
    else
      num_mesgs = break_message_up(sizeof(int), Num_Elem,
                                   MAX_CHUNK_SIZE / max_sides, &start_pos);

    /*
     * The first record will always be the largest record, it is assumed.
     * Therefore, we can allocate the ordered array at the top of the
     * loop, and use it throughout.
     */

    gsize        = start_pos[1] - start_pos[0];
    ivec_ordered = (int *) array_alloc(__FILE__, __LINE__, 1,
                                       (2 * (gsize * max_sides)), sizeof(int));
    ids_ordered = ivec_ordered + (gsize * max_sides);
    if (dist_fact[iset]) {
      fvec_ordered = array_alloc(__FILE__, __LINE__, 1,
                                 (gsize * max_sides * dist_fact[iset]),
                                 word_size);

      if (word_size == sizeof(float)) hold_float = (float *) fvec_ordered;
      else                            hold_double = (double *) fvec_ordered;
    }

#ifdef DEBUG
    if (Debug_Flag >= 2 && Proc == 0) {
      printf("\n\nMessage summary for Side Set %d:\n", (iset + 1));
      printf("\tSide Set Id: %d\n", Proc_SS_Ids[0][iset]);
      printf("\tNumber of Elements to be fanned in: %d\n", Num_Elem);
      printf("\tNumber of Sides per element: %d\n", max_sides);
      if (dist_fact[iset])
        printf("\tNumber of Dist Factors per side: %d\n", dist_fact[iset]);
      printf("\tNumber of messages per side set: %d\n", num_mesgs);
      printf("\tMax message size per side set: %d\n", gsize);
      printf("\tMin message size per side set: %d\n",
             start_pos[num_mesgs]-start_pos[num_mesgs-1]);
    }
#endif


    /* setup the array for this side set */
    fill = 0;
    for (i = 0; i < Num_Elem_Net; i++) {
      for (j = 0; j < max_sides; j++) {
        if ((side_set[iset] != NULL) && (fill < offset[iset]) &&
              (GElem_Seq[i] == side_set[iset][fill])) {
          /* set the value to the side id */
          sides[i * max_sides + j] = side_ids[iset][fill];
	  
	  if (word_size == sizeof(float))
	    for (k = 0; k < dist_fact[iset]; k++) {
              df_sp[i * max_sides + j][k] = dist_fact_sp[iset][fill][k];
	    }
	  else
	    for (k = 0; k < dist_fact[iset]; k++) {
              df_dp[i * max_sides + j][k] = dist_fact_dp[iset][fill][k];
	    }
          fill++;
        }
        else {
          sides[i * max_sides + j] = 0;
	  if (word_size == sizeof(float)) {
	    for (k = 0; k < dist_fact[iset]; k++) {
	      df_sp[i * max_sides + j][k] = 0.0f;
	    }
	  } else {
	    for (k = 0; k < dist_fact[iset]; k++) {
	      df_dp[i * max_sides + j][k] = 0.0;
	    }
	  }
        }
      }
    }

#ifdef DEBUG
    if (Debug_Flag >= 7 && Proc == 0) {
      printf("\n\nFanning in Side Set and Distribution Factors\n");
      printf("Side Set %d\n", (iset + 1));
      printf("Number of elements set %d\n", fill);
      printf("\tnumber\tside");
      if (dist_fact[iset])
        printf("\tdist fact\n");
      else
        printf("\n");
      for (i = 0; i < (Num_Elem_Net * max_sides); i++) {
        printf("\t%d\t%d", ((i / max_sides) + 1), sides[i]);
        if (dist_fact[iset]) {
          printf("\t");
          for (j = 0; j < dist_fact[iset]; j++) {
            if (word_size == sizeof(float))
              printf("%f  ", df_sp[i][j]);
            else
              printf("%lf  ", df_dp[i][j]);
          }
        }
        printf("\n");
      }
    }
#endif


    offset1 = 0;
    /* now fanin the arrays */
    for (imsg = 0; imsg < num_mesgs; imsg++) {

      gsize = start_pos[imsg+1] - start_pos[imsg];

      if (dist_fact[iset])
        err = fanin_iblk_ptr_slab(word_size, GElem_Seq, sides, hold_df,
                                  Num_Elem_Net, max_sides, dist_fact[iset],
                                  (start_pos[imsg]+1), start_pos[imsg+1],
                                  ivec_ordered, fvec_ordered);
      else
        err = fanin_iblk_slab(GElem_Seq, sides, Num_Elem_Net, max_sides,
                              (start_pos[imsg]+1), start_pos[imsg+1],
                              ivec_ordered);
      if (err < 0) {
        fprintf(stderr, "[%d] %s: error while faning in side set, %d.\n",
                Proc, yo, (iset + 1));
        return -1;
      }

#ifdef DEBUG
      if (Debug_Flag >= 7 && Proc == 0) {
        printf("\n\nAfter the Fanin\n");
        printf("Side Set %d\n", (iset + 1));
        printf("Message Number %d\n", (imsg + 1));
        printf("\tnumber\tside");
        if (dist_fact[iset])
          printf("\tdist fact\n");
        else
          printf("\n");
        for (i = 0; i < (gsize * max_sides); i++) {
          printf("\t%d\t%d", ((i / max_sides) + (start_pos[imsg] + 1)),
                 ivec_ordered[i]);
          if (dist_fact[iset]) {
            printf("\t");
            for (j = 0; j < dist_fact[iset]; j++) {
              if (word_size == sizeof(float))
                printf("%f  ", hold_float[(i * dist_fact[iset]) + j]);
              else
                printf("%lf  ", hold_double[(i * dist_fact[iset]) + j]);
            }
          }
          printf("\n");
        }
      }
#endif

      /* now get out the side set */
      fill = 0;
      for (i = 0; i < (gsize * max_sides); i++)
        if (ivec_ordered[i] > 0) {
          ids_ordered[fill] = ivec_ordered[i];
          ivec_ordered[fill] = ((i / max_sides) + (start_pos[imsg] + 1));

          for (j = 0; j < dist_fact[iset]; j++) {
            if (word_size == sizeof(float))
              hold_float[(fill * dist_fact[iset]) + j]
                = hold_float[(i * dist_fact[iset]) + j];
            else
              hold_double[(fill * dist_fact[iset]) + j]
                = hold_double[(i * dist_fact[iset]) + j];
          }
          fill++;
        }

#ifdef DEBUG
      if (Debug_Flag >= 7 && Proc == 0) {
        printf("\n\nAfter retrieving the Side Set\n");
        printf("Side Set %d\n", (iset + 1));
        printf("Message Number %d\n", (imsg + 1));
        printf("\tnumber\telement\tside");
        if (dist_fact[iset])
          printf("\tdist fact\n");
        else
          printf("\n");
        for (i = 0; i < fill; i++) {
          printf("\t%d\t%d\t%d", (offset1 + 1 + i), ivec_ordered[i],
                 ids_ordered[i]);
          if (dist_fact[iset]) {
            printf("\t");
            for (j = 0; j < dist_fact[iset]; j++) {
              if (word_size == sizeof(float))
                printf("%f  ", hold_float[(i * dist_fact[iset]) + j]);
              else
                printf("%lf  ", hold_double[(i * dist_fact[iset]) + j]);
            }
          }
          printf("\n");
        }
      }
#endif

      if ((Proc == 0) && (fill > 0)) {
        check_exodus_error(iproc,ex_put_n_side_set(exoid_out, Proc_SS_Ids[0][iset],
                                             (offset1 + 1), fill,
                                             ivec_ordered, ids_ordered),
                           "ex_put_n_side_set");

        if (dist_fact[iset]) {
          check_exodus_error(iproc,ex_put_n_side_set_df(exoid_out,
                                                  Proc_SS_Ids[0][iset],
                                               (offset1 * dist_fact[iset] + 1),
                                                  (dist_fact[iset] * fill),
                                                  fvec_ordered),
                             "ex_put_n_side_set_df");
        }

        offset1 += fill;

      }

    } /* End: "for (imsg = 0; imsg < num_mesgs; imsg++)" */

    safe_free ((void **) &ivec_ordered);
    safe_free ((void **) &start_pos);
    if (df_flag)
      safe_free ((void **) &fvec_ordered);

  } /* End: "for (iset = 0; iset < Num_Side_Set; iset++)" */

  safe_free ((void **) &sides);
  if (df_flag) {
    if (word_size == sizeof(float)) safe_free ((void **) &df_sp);
    else                            safe_free ((void **) &df_dp);
  }
  safe_free ((void **) &offset);
  for (iset = 0; iset < Num_Side_Set; iset++) {
    safe_free ((void **) &side_set[iset]);
    if (dist_fact[iset]) {
      if (word_size == sizeof(float))
        safe_free ((void **) &dist_fact_sp[iset]);
      else
        safe_free ((void **) &dist_fact_dp[iset]);
    }
    safe_free ((void **) &side_set[iset]);
  }
  safe_free ((void **) &side_set);
  if (df_flag) {
    if (word_size == sizeof(float)) {
      safe_free ((void **) &dist_fact_sp);
    }
    else {
      safe_free ((void **) &dist_fact_dp);
    }
  }

  return 0;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int conv_side_sets(int pexoid, int iproc, int **side_sets, int *offsets,
			  int *loc_index)
{
  /* Local Variables */
  char    *yo = "conv_side_sets";

  int      iset, ielem, gelem, lelem, pos;
  int     *elem_num_map;
  
  /*------------------- begin execution -------------------------------------*/

  elem_num_map = (int *) array_alloc (__FILE__, __LINE__, 1,
                                      Proc_Num_Elem[iproc], sizeof(int));
  if (!elem_num_map) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  /* read this processor's global element map */
  if (ex_get_elem_num_map(pexoid, elem_num_map) < 0) {
    fprintf(stderr, "[%d] %s: error reading element number map.\n",
            Proc, yo);
    return -1;
  }

  for (iset = 0; iset < Num_Side_Set; iset++) {
    for (ielem = 0; ielem < Proc_SS_Elem_Count[iproc][iset]; ielem++) {

      /* get the global number from the local element number map */
      lelem = side_sets[iset][offsets[iset] + ielem];
      gelem = elem_num_map[lelem - 1];

      /*
       * now find that number in the global list
       *
       */

      /* Do an indexed binary search using the 'loc_index' array
	 as the index array which orders GElems */
      pos = gds_ibin_search(gelem, GElems[0], loc_index, Num_Elem_Net);
      
      if (pos < 0) {
        fprintf(stderr, "[%d] %s: can't find element, %d, in global map.\n",
                Proc, yo, gelem);
        return -1;
      } else {
	pos = loc_index[pos];
      }

      /* once that position is found, the global internal number
       * should be the corresponding number in the sequential
       * element number map
       */
       side_sets[iset][offsets[iset] + ielem] = GElem_Seq[pos];
    }
  }

  safe_free((void **) &elem_num_map);

  return 0;

}
