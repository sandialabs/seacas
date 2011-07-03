/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: mpistring.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2009/06/09 18:37:57 $
 *    Revision: 1.7 $
 ****************************************************************************/



/* Wrapper for MPI routines that take string arguments, to avoid the
   difference between how Fortran compilers pass strings */
#include <mpi.h>
#include <malloc.h>

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

void my_get_processor_name_(int *int_name, int *namelen, int *ierr)
{
   int i, fnamelen;
   char *name;
   fnamelen = *namelen;
   name = (char *)malloc(MPI_MAX_PROCESSOR_NAME*sizeof(char));
   *ierr = MPI_Get_processor_name(name, namelen);
   if (*namelen > fnamelen) *namelen=fnamelen;
   for (i=0; i<*namelen; i++) int_name[i] = (int)name[i];
}

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif
