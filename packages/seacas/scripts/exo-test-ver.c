/* $Id: exo-test-ver.c,v 1.1 2008/06/19 14:57:43 gdsjaar Exp $ */
/*****************************************************************************
*
* test EXODUS file type:  Marilyn K. Smith, SNL, Janaury 1994.
*
* based on code developed by: 
*
* author - Sandia National Laboratories
*          Larry A. Schoof 
*          V. R. Yarberry
*          G. D. Sjaardema
*
*          
* environment - UNIX
*
* entry conditions - 
*   input parameters:
*      filename
*
* exit conditions - status value 
*      3             for a netCDF file, not EXODUSII, assumed unknown
*      2	     for a file that is not netCDF, assumed EXODUSI   
*      -major        for a netCDF file, EXODUSII, return negative of
*                                       major version number of file
*                                       to avoid failure/success values.
*                         
* description:
* exo-test-ver will distinguish between EXODUS and EXODUSII file types.
* exo-test-ver will return examine the version number and return the negative
* of the major version number to avoid the status value of "success."
*
* BUGS:
* 1. Assumes that all netcdf files are EXODUSII files.
* 2. Assumes that all EXODUSII files are based on netcdf.
*
* revision history - 
*
*
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "netcdf.h"
#include "exodusII.h"

#ifdef vms
#define EXIT_SUCCESS 1
#define EXIT_FAILURE 0
#else
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif
#endif

#define NOT_NETCDF 2
#define NOT_EXODUSII 3

char *progname;
char *filename;

int main(int argc, char *argv[])
{
   int c1, c2, c3, c4;
   FILE *fid;

   int j,k;

   int exoid; 
   int CPU_word_size,IO_word_size;
   float version;
   int file_size;
   int netcdf_based = 0;
   int hdf5_based = 0;
   int nc_format = 0;
   
   char cversion[9];

   CPU_word_size = 0;			/* float or double */
   IO_word_size  = 0;			/* use what is stored in file */

/* Determine if filename was given */
    progname = argv[0];

    if (argc <= 1) exit(EXIT_FAILURE);

/* examine file */

    filename = argv[1];          /* filename path */

    fid = fopen(filename,"r");
    if (fid == NULL) {
      (void) fprintf(stderr,"         Could not open %s\n",
		     filename);
      exit(EXIT_FAILURE);
    }
    c1 = getc(fid);
    c2 = getc(fid);
    c3 = getc(fid);
    c4 = getc(fid);
    fclose(fid);
    if (c1 == 'C' && c2 == 'D' && c3 == 'F') {
      netcdf_based = 1;
    }
    else if (c2 == 'H' && c3 == 'D' && c4 == 'F') {
      hdf5_based = 1;
    } else {
      (void) fprintf(stderr,"         %s is not a netCDF file\n",
		     filename);
      exit(NOT_NETCDF);
    }  
    exoid = ex_open ( filename,
                     EX_READ, 		/* access mode = READ */
                     &CPU_word_size,	/* CPU word size */
                     &IO_word_size,	/* IO word size */
                     &version);		/* ExodusII library version */

   if (exoid < 0)
	{ (void) fprintf(stderr,"         %s is not an EXODUSII file\n",
		         filename);
          exit(NOT_EXODUSII);
        }
   
   file_size = ex_large_model(exoid);
   
   fprintf (stderr,"\n\t%s is an EXODUSII file, version %4.2f\n",
            filename,version);
   if (file_size == 0)
     fprintf(stderr, "\t\tFile size attribute is 'normal model'\n");
   else
     fprintf(stderr, "\t\tFile size attribute is 'large model'\n");
     
   /* Determine netcdf file version... */
   nc_inq_format(exoid, &nc_format);
   if (nc_format == NC_FORMAT_CLASSIC) {
     fprintf(stderr, "\t\tNetCDF Variant is 'classic'\n");
   }
   else if (nc_format == NC_FORMAT_64BIT) {
     fprintf(stderr, "\t\tNetCDF Variant is '64-bit offset'\n");
   }
   else if (nc_format == NC_FORMAT_NETCDF4) {
     fprintf(stderr, "\t\tNetCDF Variant is 'netCDF-4'\n");
   }
   else if (nc_format == NC_FORMAT_NETCDF4_CLASSIC) {
     fprintf(stderr, "\t\tNetCDF Variant is 'netCDF-4 classic'\n");
   }
   else {
     fprintf(stderr, "\t\tNetCDF Variant is 'unknown'\n");
   }

   fprintf(stderr, "\n");
   
   if (ex_close (exoid) == -1)
	printf ("ex_close failed");

   version = version + 0.00005;
   sprintf(cversion,"%4.2f",version);

   k = strlen(cversion);
   for (j=0; j<k; j++)
	if (cversion[j] == '.') break;

   if (j == k) 
	{ (void) fprintf(stderr,"         %s is not an EXODUSII file\n",
	                        filename); 
          exit(NOT_EXODUSII);
        }

   exit(-2);
}
