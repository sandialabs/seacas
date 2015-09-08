#ifndef _PE_COMMON_H
#define _PE_COMMON_H

/*****************************************************************************/
/* This file contains defines that are common to both nem_spread, and        */
/* nem_join. Most of them were taken from rf_salsa.h which is unique         */
/* to nem_spread.                                                            */
/*****************************************************************************/

/*
 * Default value of the chunk size (in bytes) for use in I/O and message
 * passing
 */

#ifndef MAX_CHUNK_SIZE
#ifdef ncube
#define MAX_CHUNK_SIZE   300000
#else
#define MAX_CHUNK_SIZE 10485760

/* Small message size for debugging purposes */
/*#define MAX_CHUNK_SIZE 16384*/
#endif
#endif

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

/* Redefine function "exit" for OSF and Puma so that */
/* the process is killed on all nodes when a single  */
/* node exits.  See routines osf_exit and smos_exit  */
/* in md_wrap_intel_c.c and md_wrap_smos_c.c,        */
/* respectively.                                     */
#ifdef PARA
#define exit(a) osf_exit(a, __FILE__, __LINE__)
#endif
#ifdef SMOS
#define exit(a) smos_exit(a, __FILE__, __LINE__)
#endif

# define PEX_MAX(x,y) (( x > y ) ? x : y)     /* max function */
# define PEX_MIN(x,y) (( x < y ) ? x : y)     /* min function */

#endif /* _PE_COMMON_H */
