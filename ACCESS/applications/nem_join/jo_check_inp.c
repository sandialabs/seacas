#include <string.h>
#include <stdio.h>

#include "rf_io_const.h"
#include "el_geom_const.h"
#include "ps_pario_const.h"


char Proc0File[MAX_FNL+1]; /* filename for processor 0 */

int check_inp()
{
  char  *yo = "check_inp";

  int    ctrlID, i, iTemp;
  int    iMaxDigit=0;
  char   cTemp[MAX_FNL+1], *cptr;
  FILE  *res_fd;

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*                 Check the input and output files                          */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

  /*
   * Check if the user has decided whether or not to use an
   * existing genesis file. Default is not to use one.
   */
  if (Gen_Flag < 0) Gen_Flag = 0;

  if (Gen_Flag) {
    /* make sure that the input FEM file was specified */
    if (strlen(ExoFile) <= 0) {
      fprintf(stderr, "%s: fatal - must specify input FEM file.\n", yo);
      return 0;
    }

    /* check input FEM file exists and is readable */
    if (!(res_fd = fopen(ExoFile, "r"))) {
      fprintf(stderr, "%s: fatal - unable to open input FEM file %s.\n",
              yo, ExoFile);
      return 0;
    }
    fclose(res_fd);
  }

  /* check for the parallel Nemesis file for proc 0 */
  if (strlen(Par_Nem_File_Name) <= 0) {
    fprintf(stderr, "%s: fatal - must specify parallel results file base"
                    "  name.\n", yo);
    return 0;
  }

  /* check if the file to put the results in was specified */
  if (strlen(Exo_Res_File) <= 0) {
    /* if not, then build a name from the input FEM filename */
    strcpy(Exo_Res_File, ExoFile);
    cptr = strrchr(Exo_Res_File, '.');
    *cptr = '\0';
    strcat(Exo_Res_File, "-out.e");
  }

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*                 Check the parallel IO specifications                      */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

  /* make sure that the user specified the number of processors */
  if (Proc_Info[0] < 0) {
    fprintf(stderr, "%s: fatal - must specify the number of processors.\n",
            yo);
    return 0;
  }

  /* check that there is a list of disks, or a number of raids */
  if ((PIO_Info.Dsk_List_Cnt <= 0) && (PIO_Info.Num_Dsk_Ctrlrs <= 0)) {
    fprintf(stderr, "%s: fatal - must specify a number of raids, or a disk"
                    " list.\n", yo);
    return 0;
  }

  /* default is not to have preceeding 0's in the disk names */
  if (PIO_Info.Zeros < 0) PIO_Info.Zeros = 0;

  /* most systems that we deal with start their files systems with 1 not 0 */
  if (PIO_Info.PDsk_Add_Fact < 0) PIO_Info.PDsk_Add_Fact = 1;

  /* default for nem_join is not to stage the reads */
  if (strlen(PIO_Info.Staged_Writes) <= 0)
    strcpy(PIO_Info.Staged_Writes, "no");

  if (strlen(PIO_Info.Par_Dsk_Root) <= 0) {
    fprintf(stderr, "%s: Error - Root directory for parallel files must"
                    " be specified.\n", yo);
    return 0;
  }

  if (strlen(PIO_Info.Par_Dsk_SubDirec) <= 0) {
    fprintf(stderr, "%s: Error - Subdirectory for parallel files must"
                    " be specified.\n", yo);
    return 0;
  }

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*                         Check the parallel file                           */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

  /* need to build the parallel file name here */

  iTemp = Proc_Info[0];
  do
  {
    iTemp /= 10;
    iMaxDigit++;
  }
  while(iTemp >= 1);

  /*
   * Append the number of processors in this run to the scalar file name
   * along with a '.' (period).
   */
  Proc0File[0] = 0x00;
  strcpy(Proc0File, Par_Nem_File_Name);
  strcat(Proc0File, ".");
  sprintf(cTemp, "%d", Proc_Info[0]);
  strcat(Proc0File, cTemp);
  strcat(Proc0File, ".");

  /*
   * Append the proper number of zeros to the filename.
   */
  for(i=0; i < iMaxDigit; i++)
    strcat(Proc0File, "0");

  /*
   * Generate the name of the directory on which the parallel disk
   * array resides. This also directs which processor writes to what
   * disk.
   */
  strcpy(cTemp, Proc0File);

  if (PIO_Info.Dsk_List_Cnt <= 0)
    ctrlID = PIO_Info.PDsk_Add_Fact; /* this should be the first one */
  else
    ctrlID = PIO_Info.Dsk_List[0]; /* this should be the first one */
  if(PIO_Info.Zeros) {
    if(ctrlID <= 9) {
      sprintf(Proc0File, "%s%d%d/%s%s", PIO_Info.Par_Dsk_Root,0,
              ctrlID, PIO_Info.Par_Dsk_SubDirec, cTemp);
    }
    else {
      sprintf(Proc0File, "%s%d/%s%s", PIO_Info.Par_Dsk_Root,
              ctrlID, PIO_Info.Par_Dsk_SubDirec, cTemp);
    }
  }
  else {
    sprintf(Proc0File, "%s%d/%s%s", PIO_Info.Par_Dsk_Root, ctrlID,
            PIO_Info.Par_Dsk_SubDirec, cTemp);
  }

  if (!(res_fd = fopen(Proc0File, "r"))) {
    fprintf(stderr, "%s: fatal - unable to open results file for"
                    " processor 0, %s.\n", yo, Proc0File);
    return 0;
  }
  fclose(res_fd);

  return 1;
}
