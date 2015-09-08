#ifndef _JO_CONST_H
#define _JO_CONST_H

/*****************************************************************************/
/*                           USERS SECTION                                   */
/*       User may want to change settings in this section                    */
/*****************************************************************************/

/* Default value of the chunk size (in bytes) for use in I/O and message
   passing
*/

#define UTIL_NAME "nem_join"
#define VER_STR   "3.16"

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

/*****************************************************************************/
/*              PROTOTYPES FOR COMMON MP FUNCTIONS                           */
/*****************************************************************************/


extern int md_write(char *buffer, int nbytes, int dest, int type, int *flag);

extern int md_read(char *buffer, int nbytes, int *source, int *type, int *flag);
extern void whoami (int *Proc, int *proc, int *host, int *Dim);

extern void check_exodus_error (int, int, char *);

/*****************************************************************************/
/*     EXTERN STATEMENTS FOR GLOBAL FUNCTIONS CALLED DIRECTLY BY nem_join.c  */
/*****************************************************************************/

extern void   get_parallel_info   (int *, int *, int *);
extern int    check_inp           (void);
extern void   brdcst_command_info (void);
extern void   brdcst_init_global  (int *, void **, int *, int *);
extern void   read_init_proc      (int);
extern int    setup_scalar_file   (int *, int, int, int);
extern void   get_global_maps     (int);
extern void   put_mesh            (int, int);
extern void   put_records         (int, int, int);
extern void   put_results         (int, int, void *);
extern double second              (void);

/*****************************************************************************/

#endif /* #ifndef _JO_CONST_H */
