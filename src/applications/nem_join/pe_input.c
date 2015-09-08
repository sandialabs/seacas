/*--------------------------------------------------------------------------*/
/* Purpose: Determine file types for command files and read in the parallel */
/*          ExodusII command file.                                          */
/*--------------------------------------------------------------------------*/
/* Author(s):  Gary L. Hennigan (1421)                                      */
/*             Matthew M. St.John (9221)                                    */
/*--------------------------------------------------------------------------*/
/* Supported Environment(s):  nCube                                         */
/*                            Intel Paragon                                 */
/*--------------------------------------------------------------------------*/
/* Revision History:                                                        */
/*    08 December 1993:       Date of creation.                             */
/*    18 June 1997:           Changed to new file format, for nem_join      */
/*--------------------------------------------------------------------------*/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "exodusII.h"

#include "rf_comm.h"
#include "rf_allo.h"

#include "rf_io_const.h"
#include "el_geom_const.h"
#include "ps_pario_const.h"
#include "pe_str_util_const.h"
#include "rf_mp_const.h"

#define TLIST_CNT 20

extern void parse_time_step_list (char *buffer);


/*****************************************************************************/
/*****************************************************************************/
int read_pexoII_info(char *filename)

/*
 *          This function reads the ASCII parallel-exodus command file.
 *
 *   Input
 *   -----
 *   filename - The name of the command file.
 *
 *   Elemental Variable List
 *      Specify 'all' if the user wants to join all elemental variables or
 *      a list of specific elemental variables to be joined, case insensitive.
 *      Default is 'all'.
 *
 *      Syntax:
 *      Elemental Variable List = <Varn, Varm, ...> 
 *      Elem Var List = <Varn, Varm, ...> 
 *      Elem Var List = all
 *      Elemental Variable List = all
 *      Examples:
 *      elemental variable list = density, pressure
 *      elem var list = pressure
 *      elem var list = all
 *
 *   Nodal Variable List
 *      Specify 'all' if the user wants to join all nodal variables or
 *      a list of specific nodal variables to be joined, case insensitive.
 *      Default is 'all'.
 *
 *      Syntax:
 *      Nodal Variable List = <Varn, Varm, ...> 
 *      Nodal Var List = <Varn, Varm, ...> 
 *      Nodal Var List = all
 *      Nodal Variable List = all
 *      Examples:
 *      nodal variable list = DISPLX, DISPLY, DISPLZ
 *      nodal var list = displz
 *      nodal var list = all
 *
 *   Time Step List
 *      Specify 'all' if the user wants to join all time steps; a range or
 *      list to join a subset of specific time steps.
 *      Default is 'all'.
 *      Time steps will be numbered consecutively in the scalar results FEM 
 *      file, i.e., 1 to total number of steps specified. 
 *
 *      MUST begin with "{" and end with "}"
 *      May be specified as a range of steps with optional increment or
 *      as a list of steps.
 *         Range increment is optional; defaults to 1.
 *         Syntax for a range is "<start step> - <end step>, <increment>"
 *
 *         List is a space or comma-separated set of time steps.
 *      Range and list strings MUST be separated by "/", except for the initial
 *      and ending "/", which are not needed.
 *
 *      Syntax:
 *      Time Step List = all
 *      Time Step = all
 *      Time Step List = {<rangen> / <listm> }
 *      Time Step = {<rangen> / <listm> }
 *      <range> is defined as "<start step> - <end step>, <increment>" 
 *         <increment> is optional; defaults to 1.
 *      <list> is defined as "<step n> <step m> ..." OR
 *                        "<step n>,<step m> ..."
 *   
 *      Examples:
 *      time step list = {1 - 100 / 101 - 299, 5 / 300 - 399}  
 *         All steps from 1 to 100, 
 *         then every 5th step, i.e. 101, 106, 111,... 291, 296, 
 *         then all steps from 300 to 399.
 *
 *      time step = {1, 10, 15, 25, 30 / 50 - 500, 10}
 *         Steps as listed 1, 10, 15, 25, 30,
 *         then every 10th step, i.e., 50, 60, 70, ...500.
 *
 *      time step list = {1 - 300, 5}
 *         Every 5th step, i.e. 1, 6, 11, ...296.
 *
 *      time step = {5 15 20}
 *         Steps as listed 5, 15, 20.
 *
 */
{
/* local declarations */
  static char *yo = "read_pexoII_info";

  FILE *file_cmd;
  char  inp_line[MAX_INPUT_STR_LN + 1];
  char  inp_copy[MAX_INPUT_STR_LN + 1];
  char *cptr, *cptr2;
  int   i, icnt, tlist_alloc;
  int   start_index;

/***************************** BEGIN EXECUTION ******************************/

  /* Open the file */
  if((file_cmd=fopen(filename, "r")) == NULL)
    return -1;

  /* Begin parsing the input file */
  while(fgets(inp_line, MAX_INPUT_STR_LN, file_cmd)) {
    /* skip any line that is a comment */
    if((inp_line[0] != '#') && (inp_line[0] != '\n')) {

      strcpy(inp_copy, inp_line);
      clean_string(inp_line, " \t\r");
      cptr = strtok(inp_line, "\t=");
      /****** The input ExodusII file name ******/
      if (token_compare(cptr, "input fem file")) {
        if(strlen(ExoFile) == 0)
        {
          cptr = strtok(NULL, "\t=");
          strip_string(cptr, " \t\n\r");
          strcpy(ExoFile, cptr);
        }
      }
      /****** The input NemesisI load balance file name ******/
      else if (token_compare(cptr, "lb file")) {
        if(strlen(Exo_LB_File) == 0)
        {
          cptr = strtok(NULL, "\t=");
          strip_string(cptr, " \t\n\r");
          strcpy(Exo_LB_File, cptr);
        }
      }
      /****** The scalar results ExodusII file name ******/
      else if (token_compare(cptr, "scalar results fem file")) {
        if(strlen(Exo_Res_File) == 0)
        {
          cptr = strtok(NULL, "\t=");
          strip_string(cptr, " \t\n\r");
          strcpy(Exo_Res_File, cptr);
        }
      }
      /****** The parallel results ExodusII file name ******/
      else if (token_compare(cptr, "parallel results file base name")) {
        if(strlen(Par_Nem_File_Name) == 0)
        {
          cptr = strtok(NULL, "\t=");
          strip_string(cptr, " \t\n\r");
          strcpy(Par_Nem_File_Name, cptr);
        }
      }
      /****** The Number of Processors ******/
      else if (token_compare(cptr, "number of processors")) {
        if (Proc_Info[0] < 0) {
          cptr = strtok(NULL, "\t=");
          strip_string(cptr, " \t\n\r");
          if(sscanf(cptr, "%d", &(Proc_Info[0])) != 1) {
            fprintf(stderr, "%s: ERROR, can\'t interp int for number of"
                            " Processors.\n", yo);
            exit(1);
          }
        }
      }
      /****** Is There a Scalar Mesh File to Use ******/
      else if (token_compare(cptr, "use scalar mesh file")) {
        if (Gen_Flag < 0) {
          cptr = strtok(NULL, "\t=");
          strip_string(cptr, " \t\n\r");
          if (Gen_Flag < 0) {
            if (token_compare(cptr, "yes"))
              Gen_Flag = 1;
            else
              Gen_Flag = 0;
          }
        }
      }
      /****** The Debug reporting level ******/
      else if (token_compare(cptr, "debug")) {
        if (Debug_Flag < 0) {
          cptr = strtok(NULL, "\t=");
          strip_string(cptr, " \t\n\r");
          if(sscanf(cptr, "%d", &Debug_Flag) != 1) {
            fprintf(stderr, "%s: ERROR, can\'t interp int for Debug_Flag\n",
                    yo);
            exit(1);
          }
        }
      }

      /****** Time Step List ******/
      else if ((token_compare(cptr, "time step list")) ||
               (token_compare(cptr, "time step"))) {
        cptr = strtok(NULL, "\t=");
        strip_string(cptr, " \t\n\r");
        parse_time_step_list (cptr);

      } /* End "if (token_compare(cptr, "time step list"))" */

      /****** Nodal Variable List ******/
      /* Use the existing Restart_Info data structure; not used for restart */  
      else if (token_compare(cptr, "nodal var list") ||
               token_compare(cptr, "nodal variable list")) {
        cptr = strtok(NULL, "\t=");
        strip_string(cptr, " \t\n\r");

        /* check to see if the user wants to read all nodal variables */
        if (strcmp(cptr, "all") == 0) {
          /* -1 designates read all nodal vars */
          Restart_Info.NVar_Node_Out = -1; 
        }
        else {

          /* Allocate the list of nodal variables to be joined */
          tlist_alloc = TLIST_CNT;

          Restart_Info.NV_Name_Out = (char **) 
            malloc(tlist_alloc * (sizeof (char *)));

            for (i = 0; i < tlist_alloc; i++) {
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
          Restart_Info.NVar_Node_Out = 0;

          /* Parse the list of nodal variable names */

          cptr2 = strtok(cptr, ", \t;");

          while(cptr2) {
            
            strncpy (Restart_Info.NV_Name_Out[Restart_Info.NVar_Node_Out], 
                     cptr2, (string_length(cptr2)+1));
            if (strncmp (Restart_Info.NV_Name_Out[Restart_Info.NVar_Node_Out],
                         "\0", 1) != 0) {
              (Restart_Info.NVar_Node_Out)++;
            }

            if (Restart_Info.NVar_Node_Out >= tlist_alloc) {
              start_index = tlist_alloc;
              tlist_alloc += TLIST_CNT;

              Restart_Info.NV_Name_Out = (char **) 
                realloc(Restart_Info.NV_Name_Out,
                        tlist_alloc * (sizeof (char *)));

              for (i = start_index; i < tlist_alloc; i++) {
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
            }
            cptr2 = strtok(NULL, ", \t;");
          }
        }
      } /* End "if (token_compare(cptr, "nodal variable list"))" */

      /****** Elemental Variable List ******/
      /* Use the existing Restart_Info data structure; not used for restart */  
      else if (token_compare(cptr, "elem var list") ||
               token_compare(cptr, "elemental variable list")) {
        cptr = strtok(NULL, "\t=");
        strip_string(cptr, " \t\n\r");

        /* check to see if the user wants to read all elemental variables */
        if (strcmp(cptr, "all") == 0) {
          /* -1 designates read all elemental vars */
          Restart_Info.NVar_Elem_Out = -1; 
        }
        else {

          /* Allocate the list of elemental variables to be joined */
          tlist_alloc = TLIST_CNT;

          Restart_Info.EV_Name_Out = (char **) 
            malloc(tlist_alloc * (sizeof (char *)));

            for (i = 0; i < tlist_alloc; i++) {
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
          Restart_Info.NVar_Elem_Out = 0;

          /* Parse the list of elemental variable names */

          cptr2 = strtok(cptr, ", \t;");

          while(cptr2) {
            
            strncpy (Restart_Info.EV_Name_Out[Restart_Info.NVar_Elem_Out], 
                     cptr2, (string_length(cptr2)+1));
            if (strncmp (Restart_Info.EV_Name_Out[Restart_Info.NVar_Elem_Out],
                         "\0", 1) != 0) {
              (Restart_Info.NVar_Elem_Out)++;
            }

            if (Restart_Info.NVar_Elem_Out >= tlist_alloc) {
              start_index = tlist_alloc;
              tlist_alloc += TLIST_CNT;

              Restart_Info.EV_Name_Out = (char **) 
                realloc(Restart_Info.EV_Name_Out,
                        tlist_alloc * (sizeof (char *)));

              for (i = start_index; i < tlist_alloc; i++) {
                Restart_Info.EV_Name_Out[i] = (char *) 
                  malloc((MAX_STR_LENGTH+1) * (sizeof (char)));
                if (!Restart_Info.EV_Name_Out[i]) {
                  fprintf(stderr, "Insufficient memory\n");
                  exit(1);
                }
              }
              if(!(Restart_Info.EV_Name_Out)) {
                fprintf(stderr, "Insufficient memory\n");
                exit(1);
              }
            }
            cptr2 = strtok(NULL, ", \t;");
          }
        }

      } /* End "if (token_compare(cptr, "elemental variable list"))" */

      /****** Reserved Space for Variables ******/
      else if (token_compare(cptr, "reserve space")) {

        cptr = strchr(cptr, '\0');
        cptr++;
        strip_string(cptr," \t\n=\r");
        cptr = strtok(cptr, ",");

        while (cptr != NULL) {
          strip_string(cptr, " \t\n\r");
          string_to_lower(cptr, '=');
          if (strstr(cptr, "nodal")) {
            cptr2 = strchr(cptr, '=');
            if (cptr2 == NULL) {
              fprintf(stderr, "Error: integer value must be specified for"
                              " reserve space.\n");
              return 0;
            }
            cptr2++;
            icnt = sscanf(cptr2, "%d", &Num_Nod_Var);
            if ((icnt <= 0) || (Num_Nod_Var < 0)) {
              fprintf(stderr, "Error: Invalid value for nodal variable\n");
              return 0;
            }
          }
          else if (strstr(cptr, "elemental")) {
            cptr2 = strchr(cptr, '=');
            if (cptr2 == NULL) {
              fprintf(stderr, "Error: integer value must be specified for"
                              " reserve space.\n");
              return 0;
            }
            cptr2++;
            icnt = sscanf(cptr2, "%d", &Num_Elem_Var);
            if ((icnt <= 0) || (Num_Elem_Var < 0)) {
              fprintf(stderr, "Error: Invalid value for elemental variable\n");
              return 0;
            }
          }
          else if (strstr(cptr, "global")) {
            cptr2 = strchr(cptr, '=');
            if (cptr2 == NULL) {
              fprintf(stderr, "Error: integer value must be specified for"
                              " reserve space.\n");
              return 0;
            }
            cptr2++;
            icnt = sscanf(cptr2, "%d", &Num_Glob_Var);
            if ((icnt <= 0) || (Num_Glob_Var < 0)) {
              fprintf(stderr, "Error: Invalid value for global variable\n");
              return 0;
            }
          }

          cptr = strtok(NULL, ",");

        } /* End "while (cptr != NULL)" */
      } /* End "else if (token_compare(cptr, "reserve space"))" */
      /****** Parallel Disk Information ******/
      else if (token_compare(cptr, "parallel disk info")) {

        cptr = strchr(cptr, '\0');
        cptr++;
        strip_string(cptr," \t\n=\r");
        cptr = strtok(cptr, ",");
        strip_string(cptr, " \t\n\r");
        string_to_lower(cptr, '=');

        /* the first sub-option must be "number" */
        if (!strstr(cptr, "number")) {
          fprintf(stderr, "Error: First sup-option for disk info must be "
                  "\"number\"\n");
          return 0;
        }
        else {
          cptr2 = strchr(cptr, '=');
          if (cptr2 == NULL) {
            fprintf(stderr, "Error: integer value must be specified for"
                            " reserve space.\n");
            return 0;
          }
          cptr2++;
          icnt = sscanf(cptr2, "%d", &(PIO_Info.Num_Dsk_Ctrlrs));
          if ((icnt <= 0) || (PIO_Info.Num_Dsk_Ctrlrs <= 0)) {
            fprintf(stderr, "Error: Invalid value for # of raid controllers\n");
            return 0;
          }
        }

        cptr = strtok(NULL, ",");
        while (cptr != NULL) {
          strip_string(cptr, " \t\n\r");
          string_to_lower(cptr, '=');
          if (strstr(cptr, "list")) {
            /*
             * So, "number" references the length of the list, and
             * I need to do some shuffling to make the new form
             * work with the old code.
             */
            PIO_Info.Dsk_List_Cnt = PIO_Info.Num_Dsk_Ctrlrs;
            PIO_Info.Num_Dsk_Ctrlrs = 0;

            /* "{" defines the beginning of the list */
            cptr = strchr(cptr, '{');
            if (cptr == NULL) {
              fprintf(stderr, "Error: disk list must be specified\n");
              return 0;
            }
            cptr++;

            /* allocate memory for to hold the values */
            PIO_Info.Dsk_List = (int *) array_alloc(__FILE__, __LINE__, 1,
                                                    PIO_Info.Dsk_List_Cnt,
                                                    sizeof(int));
            for (i = 0; i < (PIO_Info.Dsk_List_Cnt - 1); i++) {
              sscanf(cptr, "%d", &(PIO_Info.Dsk_List[i]));
              cptr = strtok(NULL, ", \t;");
            }
            /* last one is a special case */
            sscanf(cptr, "%d}", &(PIO_Info.Dsk_List[i]));
          }
          else if (strstr(cptr, "offset")) {
            cptr2 = strchr(cptr, '=');
            if (cptr2 == NULL) {
              fprintf(stderr, "Error: value must be specified with the "
                              "\"offset\" option.\n");
              return 0;
            }
            cptr2++;
            icnt = sscanf(cptr2, "%d", &(PIO_Info.PDsk_Add_Fact));
            if ((icnt <= 0) || (PIO_Info.PDsk_Add_Fact < 0)) {
              fprintf(stderr, "Error: Invalid value for offset\n");
              return 0;
            }
          }
          else if (strstr(cptr, "zeros")) {
            PIO_Info.Zeros = 1;
          }
          else if (strstr(cptr, "stage_off")) {
            strcpy(PIO_Info.Staged_Writes, "no");
          }
          else if (strstr(cptr, "stage_on")) {
            strcpy(PIO_Info.Staged_Writes, "yes");
          }

          cptr = strtok(NULL, ",");
        }
      } /* End "else if (token_compare(cptr, "parallel disk info"))" */
      else if (token_compare(cptr, "parallel file location")) {
        cptr = strchr(cptr, '\0');
        cptr++;
        strip_string(cptr," \t\n=\r");
        cptr = strtok(cptr, ",");

        while (cptr != NULL) {
          strip_string(cptr, " \t\n\r");
          string_to_lower(cptr, '=');
          if (strstr(cptr, "root")) {
            cptr2 = strchr(cptr, '=');
            if(cptr2 == NULL)
            {
              fprintf(stderr, "fatal: must specify a path with \"root\"");
              return 0;
            }
            cptr2++;
            if(strlen(cptr2) == 0)
            {
              fprintf(stderr, "fatal: invalid path name specified with"
                              " \"root\"");
              return 0;
            }
            strcpy(PIO_Info.Par_Dsk_Root, cptr2);
          }
          if (strstr(cptr, "subdir")) {
            cptr2 = strchr(cptr, '=');
            if(cptr2 == NULL)
            {
              fprintf(stderr, "fatal: must specify a path with \"subdir\"");
              return 0;
            }
            cptr2++;
            if(strlen(cptr2) == 0)
            {
              fprintf(stderr, "fatal: invalid path name specified with"
                              " \"subdir\"");
              return 0;
            }
            strcpy(PIO_Info.Par_Dsk_SubDirec, cptr2);
            if (PIO_Info.Par_Dsk_SubDirec[strlen(PIO_Info.Par_Dsk_SubDirec)-1]
                != '/')
              strcat(PIO_Info.Par_Dsk_SubDirec, "/");
          }

          cptr = strtok(NULL, ",");
        }
      }

    } /* End "if(inp_line[0] != '#')" */
  } /* End "while(fgets(inp_line, MAX_INPUT_STR_LN, file_cmd))" */


  /* Close the command file */
  printf("\n\n");
  fclose(file_cmd);

  return 0;
}

/*****************************************************************************/
/*****************************************************************************/
void brdcst_command_info(void)
{
  int byte_cnt;

   if(Proc == 0)
   {
      strcpy(PIO_Info.Scalar_LB_File_Name, Exo_LB_File);
      strcpy(PIO_Info.Scalar_Exo_File_Name, ExoFile);
      strcpy(PIO_Info.Par_Exo_Res_File_Name, Par_Nem_File_Name);
   }

   brdcst(Proc, Num_Proc, (char *) &PIO_Info, sizeof(PIO_Info), 0);

   if(PIO_Info.Dsk_List_Cnt > 0) {
     byte_cnt = (PIO_Info.Dsk_List_Cnt) * sizeof(int);

     if(Proc != 0)
       PIO_Info.Dsk_List = (int *) array_alloc(__FILE__, __LINE__, 1,
                                               PIO_Info.Dsk_List_Cnt,
                                               sizeof(int));

     brdcst(Proc, Num_Proc, (char *) PIO_Info.Dsk_List, byte_cnt, 0);
   }

   return;
}
