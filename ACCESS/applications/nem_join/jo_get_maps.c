#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "rf_allo.h"

#include "pe_common.h"
#include "el_geom_const.h"
#include "ps_pario_const.h"
#include "rf_comm.h"
#include "rf_io_const.h"
#include "rf_message.h"
#include "rf_mp_const.h"
#include "rf_util.h"
#include "jo_map_const.h"

#include "exodusII.h"

/******************** ROUTINES IN THIS FILE ***********************************
*
*     Name                      Type                  Called By
*   --------                 ---------              ---------------
*
*  get_global_maps             void              main:jo_main.c
*  read_local_maps              int                   get_global_maps
*  check_global_map             int                   get_global_maps
*  sort_global_map              int                   get_global_maps
*  gather_nodal_map             int                   get_global_maps
*  gather_element_map           int                   get_global_maps
*  sort_local_map               int                   various places
*  gen_global_seq_map           int                   get_global_maps
******************************************************************************/

#ifdef USE_MPI
extern void psort_int(int, int, int *, int *, int);
#endif

extern void check_exodus_error (int, int error, char *);

static int  read_local_maps(int *, int *, int *, int *);
static int  check_global_map(int *, int, int, int *);
static int  sort_global_map(int *, int, int *, int *);
static int  gather_nodal_map(int, int *, int, int);
static int  gather_element_map(int, int *, int, int, int *);
static void sort_local_map(int *, int *, int *, int);
static int  gen_global_seq_map(int, int, int *, int *, int, int);

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

void get_global_maps(int exoid_out)
{
  /* Local Variables */
  char    *yo = "get_global_maps";

  int      iproc, seq_node, seq_elem;
  int     *buf, ilen, i;
  int      iblk, offset1, offset2, hold_len;
  int     *blk_cnts;         /* needed to holded block counts of sorted list */

  /*------------------- begin execution -------------------------------------*/

  /*
   * Start by allocating space enough to hold the maps for all
   * of the elements and nodes.
   */
  Num_Nodes_Gross = 0;
  Num_Elem_Gross = 0;
  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {
    Num_Nodes_Gross += Proc_Num_Nodes[iproc];
    Num_Elem_Gross += Proc_Num_Elem[iproc];
  }

  GNodes = (int **) array_alloc(__FILE__, __LINE__, 2, 1, Num_Nodes_Gross,
                                sizeof(int));
  GElems = (int **) array_alloc(__FILE__, __LINE__, 2, 1, Num_Elem_Gross,
                                sizeof(int));
  if ((!GNodes) || (!GElems)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  /*
   * read the elment and node maps from the files that this
   * processor is responsible for, sort and reduce that list
   */
  Num_Nodes_Net = Num_Nodes_Gross;
  Num_Elem_Net = Num_Elem_Gross;
  if (read_local_maps(GNodes[0], &Num_Nodes_Net, GElems[0],
                     &Num_Elem_Net) < 0) {
    fprintf(stderr, "[%d] %s: error returned reading local map.\n", Proc, yo);
    exit (1);
  }

#ifdef DEBUG
    if (Debug_Flag >= 5) {
      print_sync_start(Proc, Num_Proc, FALSE);
      printf ("\n\n[%d]: Num_Nodes_Gross:%d\n", Proc, Num_Nodes_Gross);
      printf ("\n\n[%d]: Num_Nodes_Net:%d\n", Proc, Num_Nodes_Net);
      printf ("\n\n[%d]: Num_Elem_Gross:%d\n", Proc, Num_Elem_Gross);
      printf ("\n\n[%d]: Num_Elem_Net:%d\n", Proc, Num_Elem_Net);
      print_sync_end(Proc, Num_Proc, FALSE);
    }
#endif /* DEBUG */


  /* Check if the global maps are already sequential */

  /************** nodal map **************/
  if (check_global_map(GNodes[0], Num_Nodes_Net, Num_Node, &seq_node) < 0) {
    fprintf(stderr, "[%d] %s: error returned checking global node map.\n",
            Proc, yo);
    exit (1);
  }
  
  /************** elemental map **************/
  if (check_global_map(GElems[0], Num_Elem_Net, Num_Elem, &seq_elem) < 0) {
    fprintf(stderr, "[%d] %s: error returned checking global element map.\n",
            Proc, yo);
    exit (1);
  }

  /*
   * If there is no scalar mesh file, then have to globally sort
   * the maps, and then fan them in.
   */
  if (!Gen_Flag) {

    /*
     * allocate the buffer for the global sorts
     *
     * assume that no processor will ever hold more than
     * double the largest number of values being sorted
     * at any one time.
     */
    ilen = PEX_MAX(Num_Nodes_Net, Num_Elem_Net) * 2;
    hold_len = ilen;
    buf = (int *) array_alloc(__FILE__, __LINE__, 1, ilen, sizeof(int));
    if (!buf) {
      fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
      exit (1);
    }

    /************** nodal map **************/
    if (!seq_node) {

      if (sort_global_map(GNodes[0], Num_Nodes_Net, buf, &ilen) < 0) {
        fprintf(stderr, "[%d] %s: error returned sorting global node map.\n",
                Proc, yo);
        exit (1);
      }

      if (gather_nodal_map(exoid_out, buf, ilen, Num_Node) < 0) {
        fprintf(stderr, "[%d] %s: error returned gatherring global node map.\n",
                Proc, yo);
          exit (1);
      }

      /* reset this, since it may have changed */
      ilen = hold_len;
    }

    /************** elemental map **************/
    if (!seq_elem) {

      /* this has to be done by element block */

      blk_cnts = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Blk,
                                     sizeof(int));
      if (!blk_cnts) {
        fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
        exit (1);
      }
      offset1 = 0;
      offset2 = 0;
      for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {

#ifdef DEBUG
      if (Debug_Flag >= 5) {
        print_sync_start(Proc, Num_Proc, FALSE);
        printf ("\n\n[%d]: Global Element Map to be sorted:\n", Proc);
        printf ("\tElement Block: %d\n", (iblk + 1));
        printf ("\t");
        for (i = 0; i < Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk]; i++)
          printf ("  %d", GElems[0][offset1 + i]);
        printf ("\n");

        print_sync_end(Proc, Num_Proc, FALSE);
      }
#endif /* DEBUG */

        if (sort_global_map(&GElems[0][offset1],
                            Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk],
                            &buf[offset2], &ilen) < 0) {
          fprintf(stderr, "[%d] %s: error returned sorting global element"
                          "  map.\n", Proc, yo);
          exit (1);
        }
        /* hang on to how many are in this block for the sorted list */
        blk_cnts[iblk] = ilen;

        /* move the offset */
        offset1 += Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk];
        offset2 += ilen;
        ilen = hold_len - offset2; /* this is the remaining space in buf */

      }

      /* this should be the total length now */
      ilen = offset2;

      if (gather_element_map(exoid_out, buf, ilen, Num_Elem, blk_cnts) < 0) {
        fprintf(stderr, "[%d] %s: error returned gatherring global element"
                        " map.\n", Proc, yo);
          exit (1);
      }

      safe_free ((void **) &blk_cnts);
    }

    safe_free ((void **) &buf);

  } /* End "if (!Gen_Flag)" */

  /*
   * allocate space to hold the sorted sequential global list;
   * this will allow the later fanin's to be much easier
   */
  GNode_Seq = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Nodes_Net,
                                  sizeof(int));
  GElem_Seq = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Net,
                                  sizeof(int));
  if ((!GNode_Seq) || (!GElem_Seq)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  /*
   * now broadcast out sequential maps for
   * use in the fanin's
   */
  /************** nodal map **************/
  if (!seq_node) {
    if (gen_global_seq_map(exoid_out, Num_Node, GNodes[0], GNode_Seq,
                           Num_Nodes_Net, 0) < 0) {
      fprintf(stderr, "[%d] %s: error returned reading sequential node map.\n",
              Proc, yo);
        exit (1);
    }
  } else {
    for (i = 0; i < Num_Nodes_Net; i++)
      GNode_Seq[i] = GNodes[0][i];
  }

  /************** elemental map **************/
  if (!seq_elem) {
    if (gen_global_seq_map(exoid_out, Num_Elem, GElems[0], GElem_Seq,
                           Num_Elem_Net, 1) < 0) {
      fprintf(stderr, "[%d] %s: error returned reading sequential element"
                      " map.\n", Proc, yo);
        exit (1);
    }
  } else {
    for (i = 0; i < Num_Elem_Net; i++)
      GElem_Seq[i] = GElems[0][i];
  }

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int read_local_maps(int *nmap, int *nmap_len, int *emap, int *emap_len)
{
  /* Local Variables */
  char    *yo = "read_local_maps";

  char     cTemp[MAX_FNL+1];
  int      pexoid, iproc, glob_pindx;
  int      node_offset, elem_offset;
  int      cpu_ws, io_ws;
  int      hold1, hold2;
  int      i, iblk, fill, blk_cnt;
  int     *tmp_sortmap, *tmp_emap, *proc_offset;
  float    ver;

/**************************** execution begins *******************************/

  /* word size does not matter, only retrieving ints here */
  cpu_ws = 0;
  io_ws = 0;

  strcpy(cTemp, PIO_Info.Par_Exo_Res_File_Name);

  node_offset = 0;
  elem_offset = 0;

  /*
   * since element orderring is based on blocks, I have to sort
   * the map for each block seperately. So, I have to have a
   * buffer to move blocks from different processors around.
   */
  tmp_emap = (int *) array_alloc(__FILE__, __LINE__, 1, *emap_len,
                                 sizeof(int));
  if (!tmp_emap) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  for (iproc = 0; iproc < Proc_Info[2]; iproc++) {

    gen_par_filename(cTemp, Par_Nem_File_Name, Proc_Ids[iproc], Proc_Info[0]);

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

    /* read the global element and node maps from each file */
    if (Proc_Num_Nodes[iproc] > 0)
      if (ex_get_node_num_map(pexoid, &(nmap[node_offset])) < 0) {
        fprintf(stderr, "[%d] %s: error reading node number map.\n",
                Proc, yo);
        return -1;
      }

    if (Proc_Num_Elem[iproc] > 0)
      if (ex_get_elem_num_map(pexoid, &(tmp_emap[elem_offset])) < 0) {
        fprintf(stderr, "[%d] %s: error reading element number map.\n",
                Proc, yo);
        return -1;
      }

    /* adjust the offset for the next processors information */
    node_offset += Proc_Num_Nodes[iproc];
    elem_offset += Proc_Num_Elem[iproc];

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

  /*
   * need sort maps to keep track of where information
   * was when it was read in
   */
  Nodal_Sort_Map = (int *) array_alloc(__FILE__, __LINE__, 1, *nmap_len,
                                       sizeof(int));
  Elem_Sort_Map = (int *) array_alloc(__FILE__, __LINE__, 1, *emap_len,
                                      sizeof(int));
  /* need the following to shuffle the elements into blocks */
  tmp_sortmap = (int *) array_alloc(__FILE__, __LINE__, 1, *emap_len,
                                    sizeof(int));
  if ((!Nodal_Sort_Map) || (!Elem_Sort_Map) || (!tmp_sortmap)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  /*
   * fill the sort maps with the current positions
   * of the global maps
   */
  for (i = 0; i < *nmap_len; i++)
    Nodal_Sort_Map[i] = i;
  for (i = 0; i < *emap_len; i++)
    tmp_sortmap[i] = i;

  /* shuffle the element map so that it is divided by element block */

  /* need offsets for each processor */
  proc_offset = (int *) array_alloc(__FILE__, __LINE__, 1, Proc_Info[2],
                                    sizeof(int));
  if (!proc_offset) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }
  proc_offset[0] = 0;
  for (iproc = 1; iproc < Proc_Info[2]; iproc++)
    proc_offset[iproc] = proc_offset[iproc - 1] + Proc_Num_Elem[iproc - 1];
  elem_offset = 0;

  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {
    for (iproc = 0; iproc < Proc_Info[2]; iproc++) {
      for (i = 0; i < Proc_Num_Elem_In_Blk[iproc][iblk]; i++) {
        /* now shuffle these elements to their correct place */
        emap[elem_offset] = tmp_emap[proc_offset[iproc]];
        Elem_Sort_Map[elem_offset++] = tmp_sortmap[proc_offset[iproc]++];
      }
    }
    /* hang on to this */
    Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk] = elem_offset;
  }

  /* and finnish figuring out the number for each block */
  hold1 = Proc_Num_Elem_In_Blk[Proc_Info[2]][0];
  for (iblk = 1; iblk < Num_Elem_Blk; iblk++) {
    hold2 = Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk] - hold1;
    hold1 = Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk];
    Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk] = hold2;
  }

  /* free up some memory */
  safe_free ((void **) &tmp_sortmap);
  safe_free ((void **) &tmp_emap);
  safe_free ((void **) &proc_offset);

#ifdef DEBUG
  if (Debug_Flag >= 10) {
    print_sync_start(Proc, Num_Proc, FALSE);
    printf ("\n\n[%d]: Global Element Map (after shuffle):\n", Proc);
    printf ("\tElem Num\tSort Num\n");
    for (i = 0; i < *emap_len; i++)
      printf ("\t%d\t%d\n", emap[i], Elem_Sort_Map[i]);

    print_sync_end(Proc, Num_Proc, FALSE);
  }
#endif /* DEBUG */

  /* now I have to sort the local lists, and get rid of duplicates */
  sort_local_map(nmap, Nodal_Sort_Map, nmap_len, 1);

  /*
   * have to sort and get rid of duplicates in the
   * element map one block at a time
   */
  fill = 0;
  elem_offset = 0;
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++) {
    sort_local_map(&emap[elem_offset], &Elem_Sort_Map[elem_offset],
                   &Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk], 0);

    /* now get rid of duplicates for this block */
    blk_cnt = 0;
    for (i = 0; i < Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk]; i++) {
      emap[fill] = emap[elem_offset];
      Elem_Sort_Map[fill] = Elem_Sort_Map[elem_offset];

      /* check if this is a repeated entry */
      if ((i+1) == Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk] ||
            emap[elem_offset] != emap[elem_offset + 1]) {
        fill++;
        blk_cnt++;
      }

      elem_offset++;
    }

    Proc_Num_Elem_In_Blk[Proc_Info[2]][iblk] = blk_cnt;
  }

  /* reset the vector length */
  *emap_len = fill;

  return 0;
}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
/*
 * This function makes a simple check to see if the global map is
 * already sequential. If the maximum number in the local maps is
 * the same as the number of elements/nodes total, then the map
 * must be sequential.
 */
static int check_global_map(int *map, int len, int glen, int *seq)
{
  /* Local Variables */
  char    *yo = "check_global_map";

  int      i, max = 0, gmax = 0;

/**************************** execution begins *******************************/

  /* find the max on this processor */
  for (i = 0; i < len; i++) {
    if (map[i] > max) max = map[i];
  }

  /* now find out what the max on all of the processors is */
  gmax = gmax_int(max, Proc, Num_Proc);

  if (gmax < glen) {
    fprintf (stderr, "[%d] %s: fatal: too many elements/nodes\n", Proc, yo);
    return -1;
  }
  else if (gmax == glen)
    *seq = 1;
  else
    *seq = 0;

  return 0;
}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int sort_global_map(int *map, int len, int *smap, int *smap_len)
{
  /* Local Variables */
  int      i;

/**************************** execution begins *******************************/
  /* now pack the local values into the buffers */
  for (i = 0; i < len; i++)
    smap[i] = map[i];

  *smap_len = len;

  if (Num_Proc > 1) {
    sort_local_map(smap, NULL, smap_len, 1);
  }

#ifdef DEBUG
  if (Debug_Flag >= 5) {
    print_sync_start(Proc, Num_Proc, FALSE);
    printf ("\n\n[%d]: Sorted Global Map:\n", Proc);
    printf ("\t");
    for (i = 0; i < *smap_len; i++)
      printf ("  %d", smap[i]);
    printf ("\n");

    print_sync_end(Proc, Num_Proc, FALSE);
  }
#endif /* DEBUG */

  return 0;
}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int gather_nodal_map(int exoid_out, int *map, int len, int glen)
{
  /* Local Variables */
  char    *yo = "gather_nodal_map";

  int      i, imsg;
  int     *index, idx_len, start;
  int      gsize, num_mesgs, *start_pos, *gvec_ordered;


/**************************** execution begins *******************************/


  idx_len = PEX_MAX(len, Num_Proc);

  index = (int *) array_alloc(__FILE__, __LINE__, 1, idx_len, sizeof(int));
  if (!index) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  for (i = 0; i < idx_len; i++) index[i] = 0;

  /* fill in this processors position */
  index[Proc] = len;

  /* fanin the lengths to processor 0 */
  fanin_int(index, Num_Proc, 0, Proc, Num_Proc);

  if (Proc == 0)
    /* now add them to make it an entry */
    for (i = 1; i < Num_Proc; i++)
      index[i] += index[i - 1];

  /* and broadcast it out */
  brdcst (Proc, Num_Proc, (char *) index, (Num_Proc * sizeof(int)), 0);

  if (Proc == 0) start = 1;
  else           start = index[Proc - 1] + 1;

  /* now fill what is essentially the global index */
  for (i = 0; i < len; i++) index[i] = start + i;

  /* break up the message size */
  num_mesgs = break_message_up(sizeof(int), glen, MAX_CHUNK_SIZE, &start_pos);

  /*
   * The first record will always be the largest record, it is assumed.
   * Therefore, we can allocate the ordered array at the top of the loop,
   * and use it throughout.
   */
  gsize        = start_pos[1] - start_pos[0];
  gvec_ordered = (int *) array_alloc(__FILE__, __LINE__, 1, gsize,
                                     sizeof(int));

#ifdef DEBUG
  if (Debug_Flag >= 2 && Proc == 0) {
    printf("\n\nMessage summary for Global Node Map:\n");
    printf("\tNumber of messages per map: %d\n", num_mesgs);
    printf("\tMax message size per map: %d\n", gsize);
    printf("\tMin message size per map: %d\n",
           start_pos[num_mesgs]-start_pos[num_mesgs-1]);
  }
#endif

  for (imsg = 0; imsg < num_mesgs; imsg++) {

    gsize = start_pos[imsg+1] - start_pos[imsg];

    /* NOTE: map is from 1 to n, not 0 to n-1 */
    if (fanin_int_slab(index, map, len, (start_pos[imsg] + 1),
                       start_pos[imsg+1], gvec_ordered) < 0) {
      fprintf(stderr, "[%d] %s: fannin returned an error condition\n",
              Proc, yo);
      return -1;
    }

    /*
     * Write a slab of the global map to the exoII file on the front end
     */
    if (Proc == 0)
      if (ex_put_n_node_num_map(exoid_out, (start_pos[imsg] + 1), gsize,
                                gvec_ordered) < 0) {
        fprintf(stderr, "%s: error writing element number map.\n", yo);
        return -1;
      }

  } /* End "for (imsg = 0; imsg < num_mesgs; imsg++)" */

  safe_free ((void **) &gvec_ordered);
  safe_free ((void **) &start_pos);
  safe_free ((void **) &index);

  return 0;

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int gather_element_map(int exoid_out, int *map, int len, int glen,
                              int *blk_cnts)
{
  /* Local Variables */
  char    *yo = "gather_element_map";

  int      j, i, iblk, imsg;
  int     *index, idx_len;
  int     *start;
  int      gsize, num_mesgs, *start_pos, *gvec_ordered;


/**************************** execution begins *******************************/

  idx_len = PEX_MAX(len, (Num_Proc * Num_Elem_Blk));

  index = (int *) array_alloc(__FILE__, __LINE__, 1, idx_len, sizeof(int));
  start = (int *) array_alloc(__FILE__, __LINE__, 1, Num_Elem_Blk,
                              sizeof(int));
  if ((!index) || (!start)) {
    fprintf(stderr, "%s: fatal: insufficient memory\n", yo);
    exit (1);
  }

  for (i = 0; i < idx_len; i++) index[i] = 0;

  /*
   * fill in this processors position
   * for each element block
   */
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
    index[Proc + (iblk * Num_Proc)] = blk_cnts[iblk];

  /* fanin the lengths to processor 0 */
  fanin_int(index, (Num_Proc * Num_Elem_Blk), 0, Proc, Num_Proc);

  if (Proc == 0)
    /* now add them to make it an entry */
    for (i = 1; i < (Num_Proc * Num_Elem_Blk); i++)
      index[i] += index[i - 1];

  /* and broadcast it out */
  brdcst (Proc, Num_Proc, (char *) index,
          (Num_Proc * Num_Elem_Blk * sizeof(int)), 0);

  for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
    if (Proc == 0 && iblk == 0)
      start[iblk] = 1;
    else
      start[iblk] = index[Proc + (iblk * Num_Proc) - 1] + 1;

  /* now fill what is essentially the global index */
  j = 0;
  for (iblk = 0; iblk < Num_Elem_Blk; iblk++)
    for (i = 0; i < blk_cnts[iblk]; i++)
      index[j++] = start[iblk] + i;

  safe_free ((void **) &start);

  /* break up the message size */
  num_mesgs = break_message_up(sizeof(int), glen, MAX_CHUNK_SIZE, &start_pos);

  /*
   * The first record will always be the largest record, it is assumed.
   * Therefore, we can allocate the ordered array at the top of the loop,
   * and use it throughout.
   */
  gsize        = start_pos[1] - start_pos[0];
  gvec_ordered = (int *) array_alloc(__FILE__, __LINE__, 1, gsize,
                                     sizeof(int));

#ifdef DEBUG
  if (Debug_Flag >= 2 && Proc == 0) {
    printf("\n\nMessage summary for Global Element Map:\n");
    printf("\tNumber of messages per map: %d\n", num_mesgs);
    printf("\tMax message size per map: %d\n", gsize);
    printf("\tMin message size per map: %d\n",
           start_pos[num_mesgs]-start_pos[num_mesgs-1]);
  }
#endif

  for (imsg = 0; imsg < num_mesgs; imsg++) {

    gsize = start_pos[imsg+1] - start_pos[imsg];

    /* NOTE: map is from 1 to n, not 0 to n-1 */
    if (fanin_int_slab(index, map, len, (start_pos[imsg] + 1),
                       start_pos[imsg+1], gvec_ordered) < 0) {
      fprintf(stderr, "[%d] %s: fannin returned an error condition\n",
              Proc, yo);
      return -1;
    }

    /*
     * Write a slab of the global map to the exoII file on the front end
     */
    if (Proc == 0)
      if (ex_put_n_elem_num_map(exoid_out, (start_pos[imsg] + 1), gsize,
                                gvec_ordered) < 0) {
        fprintf(stderr, "%s: error writing element number map.\n", yo);
        return -1;
      }

  } /* End "for (imsg = 0; imsg < num_mesgs; imsg++)" */

  safe_free ((void **) &gvec_ordered);
  safe_free ((void **) &start_pos);
  safe_free ((void **) &index);

  return 0;

}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static void sort_local_map(int *global_map, int *sort_map, int *len, int dup)
{
  /* Local Variables */
  int      i, fill;

/**************************** execution begins *******************************/

  /*
   * if the sort_map is NULL, then only sort and get rid of
   * duplicates in the global_map
   */

  /* first sort the map and sort lists (if necessary) */
  if (sort_map != NULL)
    sort_int_int(*len, global_map, sort_map);
  else
    sort_int(*len, global_map);

  /*
   * if "dup" is set, then get rid of any duplicates due
   * to border or external nodes/elements
   */
  if (dup) {
    fill = 0;
    for (i = 0; i < *len; i++) {
      global_map[fill] = global_map[i];
      if (sort_map != NULL) sort_map[fill] = sort_map[i];

      /* check if this is a repeated index */
      if ((i+1) == *len || global_map[i] != global_map[i+1]) fill++;
    }

    /* reset the vector length */
    *len = fill;
  }
}


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
static int gen_global_seq_map(int exoid, int glen, int *loc_map, int *seq_map,
                              int len, int map_type)
{
  /* Local Variables */
  int      i, j;
  int      num_mesgs, *mesg_start, max_ids_per_mesg;
  int      istart_id, iend_id, num_ids_in_mesg;
  int     *global_ids, *seq_ids;
  int     *inter, num_inter, prob_type;


/**************************** execution begins *******************************/

  /*
   * Find out how many pieces to break the read of the global number map up
   * into. Note that we're sending both the values AND their sequential IDs,
   * thus the MAX_CHUNK_SIZE/2.
   */

  num_mesgs = break_message_up(sizeof(int), glen, MAX_CHUNK_SIZE/2,
                               &mesg_start);

  max_ids_per_mesg = mesg_start[1] - mesg_start[0];

#ifdef DEBUG
  if (Debug_Flag >= 2 && Proc == 0) {
    if (map_type)
      printf("\n\nMessage summary for Global Sequential Element IDs:\n");
    else
      printf("\n\nMessage summary for Global Sequential Node IDs:\n");
    printf("\tNumber of messages needed for Global Sequential IDs: %d\n",
           num_mesgs);
    printf("\tMax message size needed for Global Sequential IDs: %d\n",
           max_ids_per_mesg);
    printf("\tMin message size needed for Global Sequential IDs: %d\n",
           mesg_start[num_mesgs]-mesg_start[num_mesgs-1]);
  }
#endif


  global_ids = (int *) array_alloc(__FILE__, __LINE__, 1, 3 * max_ids_per_mesg,
                                   sizeof(int));
  if (!global_ids) {
    fprintf(stderr, "Insufficient memory\n");
    exit(1);
  }
  seq_ids = global_ids + max_ids_per_mesg;
  inter   = seq_ids + max_ids_per_mesg;

  /* Loop over each message */
  for (i = 0; i < num_mesgs; i++) {

    istart_id       = mesg_start[i];
    iend_id         = mesg_start[i+1];
    num_ids_in_mesg = iend_id - istart_id;

    /*
     * Read in the specified global ids and calculate their associated
     * sequential global numbers.
     */

    if (Proc == 0) {
      if (map_type)
        check_exodus_error(Proc,ex_get_n_elem_num_map(exoid,
						      istart_id+1,
						      num_ids_in_mesg,
						      global_ids),
                           "ex_get_n_elem_num_map");
      
      else
        check_exodus_error(Proc,ex_get_n_node_num_map(exoid,
						      istart_id+1,
						      num_ids_in_mesg,
						      global_ids),
                           "ex_get_n_node_num_map");
      
      for (j = 0; j < num_ids_in_mesg; j++)
        seq_ids[j] = istart_id + j + 1;
    }

    /* Broadcast the information to the other processors */
    brdcst_maxlen(Proc, Num_Proc, (char *)global_ids,
                  2 * num_ids_in_mesg * sizeof(int), 0);

    sync(Proc, Num_Proc);

    /*
     * find the intersection between the global ids, and the
     * list of global ids on this processor
     *
     * For the elemental case, the global ids are sorted by
     * element block, and might not be monotonically increasing.
     */
    if (map_type) prob_type = 0;
    else          prob_type = 2;
    num_inter = find_inter_pos(inter, num_ids_in_mesg, global_ids, len,
                               loc_map, prob_type);

    if (num_inter > 0)
      for(j = 0; j < num_ids_in_mesg; j++)
        if(inter[j] >= 0)
          seq_map[inter[j]] = seq_ids[j];

  } /* End "for (i = 0; i < num_mesgs; i++)" */

#ifdef DEBUG
  if (Debug_Flag >= 5) {
    char     type[10];
    print_sync_start(Proc, Num_Proc, FALSE);
    if (map_type) strcpy(type, "Element");
    else          strcpy(type, "Node");
    printf ("\n\n[%d]: Global & Sequential %s Maps:\n", Proc, type);
    printf ("\tGlobal\tSequential\n");
    for (i = 0; i < len; i++)
      printf ("\t%d\t%d\n", loc_map[i], seq_map[i]);

    print_sync_end(Proc, Num_Proc, FALSE);
  }
#endif /* DEBUG */

  safe_free ((void **) &global_ids);
  safe_free ((void **) &mesg_start);

  return 0;

}
