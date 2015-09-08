#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "exodusII.h"

#include "jo_const.h"
#include "el_geom_const.h"
#include "rf_io_const.h"

extern char GeomTitle[]; /* geometry title from proc 0 file (jo_read_init.c) */

/************ R O U T I N E S   I N   T H I S   F I L E ***********************
*
*  Name_of_Routine              type                 Called by
*  ---------------------------------------------------------------
*
*  setup_scalar_file ()                        main:jo_main.c
*
******************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int setup_scalar_file(int *res_exoid, int word_size, int num_qa, int num_info)
{
/* Local declarations. */
  static char  yo[] = "setup_scalar_file";

  int          mesh_exoid, inum;
  int          cpu_ws, io_ws;
  float        ver;

/***************************** BEGIN EXECUTION ******************************/

  /*
   * the word size used in the program and for the scalar results
   * file are based on the word size used in the parallel results
   * file
   */
  cpu_ws = word_size;
  io_ws = word_size;

  /* create a new results file */
  *res_exoid = ex_create (Exo_Res_File, EX_CLOBBER, &cpu_ws, &io_ws);
  if (*res_exoid == -1) {
    fprintf (stderr, "%s: ERROR creating the scalar results file, %s\n", yo,
             Exo_Res_File);
    return -1;
  }

  /*
   * put the info and qa records in the file here.
   * If put before anything else, the 'nc_enddef' is much smaller.
   */
  put_records(*res_exoid, num_qa, num_info);

  if (Gen_Flag) {
    /*
     * open up the input mesh file to make some checks before
     * copying it to the results file
     */
    mesh_exoid = ex_open (ExoFile, EX_READ, &cpu_ws, &io_ws, &ver);
    if (mesh_exoid == -1) {
      fprintf (stderr, "%s: ERROR openning the scalar mesh file, %s\n", yo,
               ExoFile);
      return -1;
    }

    /* Get the number of time indices contained in the file */
    if(ex_inquire(mesh_exoid, EX_INQ_TIME, &(inum), NULL, NULL) < 0) {
      fprintf(stderr, "%s: Error getting number of time indices from"
                      " file, %s\n", yo, ExoFile);
      return -1;
    }

    if (ex_copy(mesh_exoid, *res_exoid) < 0) {
      fprintf (stderr, "%s: ERROR copying mesh file, %s, to results file, %s\n",
               yo, ExoFile, Exo_Res_File);
      return -1;
    }

    /* close the mesh file */
    if(ex_close (mesh_exoid) == -1) {
      fprintf (stderr, "%s: ERROR: Error in closing scalar mesh file\n", yo);
      return -1;
    }
  }
  else {

    /* put the initialization parameters in the new file */
    if(ex_put_init(*res_exoid, GeomTitle, Num_Dim, Num_Node, Num_Elem,
                   Num_Elem_Blk, Num_Node_Set, Num_Side_Set) < 0) {
      fprintf(stderr, "%s: Error putting initialization parameters in"
                      " results file, %s\n", yo, Exo_Res_File);
      return -1;
    }

    /*
     * and make sure to update the file so other functions can
     * access the initial information
     */
    if(ex_update(*res_exoid) < 0) {
      fprintf(stderr, "%s: Error updating results file, %s\n",
              yo, Exo_Res_File);
      return -1;
    }

  }

  return 0;
}
