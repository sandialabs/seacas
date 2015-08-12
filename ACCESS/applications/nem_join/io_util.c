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
#include "pe_str_util_const.h"

#define TLIST_CNT 20

void scan_range (const char *range, int *start, int *end, int *inc);

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
/* void parse_time_step_list (char *buffer)
 *
 *   This function parses time step information from the input file. 
 *   Syntax for the input file:
 *   time step list = < time step information >
 *
 *   Format of the <time step information>:
 *   MUST begin with "{" and end with "}"
 *   May be specified as a range of steps with optional increment or
 *   as a list of steps.
 *
 *   Range increment is optional; defaults to 1.
 *   Syntax for a range is "<start step> - <end step>, <increment>" 
 *
 *   List is a space or comma-separated set of time steps.
 *   Syntax for a list is "<step n> <step m> ..." OR
 *                        "<step n>,<step m> ..."
 *   
 *   Range and list strings MUST be separated by "/", except for the initial
 *   and ending "/", which are not needed.
 *
 *   Time steps will be numbered consecutively in the scalar results FEM file,
 *   i.e., 1 to total number of steps specified. 
 *
 *   Examples:
 *   {1 - 100 / 101 - 299, 5 / 300 - 399}  
 *      All steps from 1 to 100, 
 *      then every 5th step, i.e. 101, 106, 111,... 291, 296, 
 *      then all steps from 300 to 399.
 *
 *   {1, 10, 15, 25, 30 / 50 - 500, 10}
 *      Steps as listed 1, 10, 15, 25, 30,
 *      then every 10th step, i.e., 50, 60, 70, ...500.
 *
 *   {1 - 300, 5}
 *      Every 5th step, i.e. 1, 6, 11, ...296.
 *
 *   {5 15 20}
 *      Steps as listed 5, 15, 20.
 *
 *   Input
 *   -----
 *   buffer - time step information
 */


void
parse_time_step_list (char *buffer) {

/* local declarations */
  char *substr_buf;
  char *cptr, *cptr2;
  char *bufferp = buffer;
  int	start_step, end_step, increment;
  int   tlist_alloc;

  /****** Time Step List ******/
  /* Use the existing Restart_Info data structure; not used for restart */  
  /* figure out how many time steps there are in this list */

  /* check to see if the user wants all of the time steps */
  if (token_compare(buffer, "all")) {
    Restart_Info.Num_Times = -1; /* -1 designates read all times */
  }
  /* check to see if the user wants the last time step only */
  else if (token_compare(buffer, "last")) {
    Restart_Info.Num_Times = -2; /* -2 designates read last time step */

    /* Allocate the time step list */
    Restart_Info.Time_Ind  = (int *) array_alloc(__FILE__, __LINE__, 1,
                                                       1, sizeof(int));

    if(!(Restart_Info.Time_Ind)) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }
  }
  /* check to see if the user wants to specifically turn this off */
  else if (token_compare(buffer, "off")) {
    Restart_Info.Num_Times = 0;
  }
  else {
  /* Allocate the time step list */
    Restart_Info.Time_Ind  = (int *) array_alloc(__FILE__, __LINE__, 1,
                                                       TLIST_CNT, sizeof(int));
    if(!(Restart_Info.Time_Ind)) {
      fprintf(stderr, "Insufficient memory\n");
      exit(1);
    }
    tlist_alloc = TLIST_CNT;
    Restart_Info.Num_Times = 0;

    /* Parse the list of time steps */
    strip_string (buffer, "{");
    substr_buf = (char *) calloc (strlen(buffer)+1, sizeof(char));

    /* bufferp initially points to the beginning of the time step string */
    while (bufferp && (strcmp(bufferp, "") != 0) &&
                      (strcmp(bufferp, "}") != 0)) {

      /* Parse into substrings */
      if (!(cptr = strchr(bufferp, '/'))) cptr = strchr(bufferp, '}');

      if (cptr) {

        /* clear the substring buffer and copy in the next substring */
        memset(substr_buf, 0, strlen(substr_buf));
        strncpy(substr_buf, bufferp, (cptr - bufferp));

        /* check for a range of time steps */
        if (strchr (substr_buf, '-') != NULL) {
          scan_range (substr_buf, &start_step, &end_step, &increment);

          /* store the time steps specified by the range */
          while (start_step <= end_step) {
            Restart_Info.Time_Ind[Restart_Info.Num_Times] = start_step;
            start_step+= increment;
            (Restart_Info.Num_Times)++;

            if (Restart_Info.Num_Times >= tlist_alloc) {
              tlist_alloc += TLIST_CNT;
              Restart_Info.Time_Ind  =
                (int *) realloc(Restart_Info.Time_Ind, tlist_alloc*sizeof(int));
              if(!(Restart_Info.Time_Ind)) {
                fprintf(stderr, "Insufficient memory\n");
                exit(1);
              }
            }
          }

        } else {

          /* store the time steps specified as a list */
          cptr2 = strtok (substr_buf, ", \t;");

          while (cptr2) {
            sscanf(cptr2, "%d", &Restart_Info.Time_Ind[Restart_Info.Num_Times]);
            (Restart_Info.Num_Times)++;

            if (Restart_Info.Num_Times >= tlist_alloc) {
              tlist_alloc += TLIST_CNT;
              Restart_Info.Time_Ind  =
                (int *) realloc(Restart_Info.Time_Ind, tlist_alloc*sizeof(int));
              if(!(Restart_Info.Time_Ind)) {
                fprintf(stderr, "Insufficient memory\n");
                exit(1);
              }
            }
            /* get the next time step in the list, or NULL at the end of list */
            cptr2 = strtok (NULL, ", \t;");
          }
        }
      }
      /* move pointer to beginning of next substring, or end of string */
      bufferp = cptr+1;
    }
  }
  /* reset string to zero to indicate completion of time step parsing */
  buffer = "\0";
}


void
scan_range (const char *range, int *start, int *end, int *inc) {

  char *rangep;

  *start = *end = 0;
  *inc = 1;

  rangep = (char *) calloc (strlen(range)+1, sizeof(char));
  strcpy (rangep, range);

  /* read starting time step */
  rangep = strtok (rangep, "-");
  if (rangep) sscanf(rangep, "%d", start);

  rangep = strtok (NULL, ", \t;");
  /* read ending time step */
  if (rangep) sscanf(rangep, "%d", end);

  rangep = strtok (NULL, ", \t;");
  /* read optional increment, 1 by default */
  if (rangep) sscanf(rangep, "%d", inc);

}
