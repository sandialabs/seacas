#include <stdio.h>	/* For maximum filename length */

/*********** rf_io_const.h -- constants for external IO purposes**************/

#define MAX_INPUT_STR_LN 4096   /* maximum string length for read_string()  */

#define	MAX_NNV		40	/* maximum number of nodal variables */
#define	MAX_NEV		 5	/* maximum number of element variables */
#define	MAX_NHV		 5	/* maximum number of history variables */
#define	MAX_NGV		 5	/* maximum number of global variables */

/* Restart structure */
struct Restart_Description {

  int       Num_Times;	/* The number of time indices to join */
  int      *Time_Ind;	/* Time indicies to read, need to keep track of all */
  float     Time_sp;	/* Time to read, only need one at a time */
  double    Time_dp;

  int       NVar_Glob;	/* Number of global variables read */
  int       NVar_Elem;	/* Number of elemental variables read */
  int       NVar_Node;	/* Number of nodal variables read */

  int       NVar_Node_Out;      /* Number of nodal variables to write */
  int       NVar_Elem_Out;      /* Number of elemental variables to write */

  int     **Elem_TT;	/* Elemental variable truth table */
  int      *GElem_TT;	/* Global Elemental variable truth table */

  int     **Elem_TT_Out;    /* Elemental variable truth table */
  int      *GElem_TT_Out;   /* Global Elemental variable truth table */

  int      *NVar_Index;         /* list of nodal variables to be joined */
  int      *EVar_Index;         /* list of elemental variables to be joined */

  /*
   * The following variables olny hold the values for one time step
   * at a time. Each time step will be read in, distributed, and
   * then written out before and subsequent time steps are written.
   *
   * To be able to support single or double precision exodus files,
   * need to have both float and double pointers here.
   */
  float    *Glob_Vals_sp; /* Global variable values, only one per variable *
                          * and processor                                 */
  double   *Glob_Vals_dp;
  float  ***Elem_Vals_sp; /* Element variable values for each processor */
  double ***Elem_Vals_dp;
  float  ***Node_Vals_sp; /* Nodal variable values for each processor */
  double ***Node_Vals_dp;

  char    **NV_Name;	/* Names of the nodal variables */
  char    **EV_Name;	/* Names of the elemental variables */
  char    **GV_Name;	/* Names of the global variables */

  char    **NV_Name_Out;        /* Names of nodal variables to be joined */
  char    **EV_Name_Out;        /* Names of elemental variables to be joined */

};

typedef struct Restart_Description  RESTART;
typedef struct Restart_Description *RESTART_PTR;

/*****************************************************************************/
/*	EXTERN STATEMENTS for GLOBALS USED IN I/O ROUTINES		     */
/*****************************************************************************/

/**Extern statements for parameters in rf_io.h */

extern
char ExoFile[];		  /* Exodus II File containing problem definition.   */
                          /* This name is the root name.                     */

extern
char Exo_LB_File[];
                          /* Exodus II file containing the mesh load-balance */
                          /* information                                     */
extern
char Exo_Res_File[];
                          /* Exodus II file containing the mesh result       */
                          /* information                                     */
extern
int  Debug_Flag;	/* Flag to specify debug info is to be printed out.
			   The value of this flag determines the level of
			   diagnostic info which is printed to stdout
			   Debug_Flag == 0 	No output
					 .
					 .
					 9	maximum output             */
extern
int Gen_Flag;		/* Flag used by nem_join to determine if the user
			   wants to use an existing genesis file rather
			   than generating one from the parallel files */

extern
int Subset_Flag;        /* Flag used by nem_join to determine if the user
                           wants to read a subset of time indices from the
                           parallel ExodusII files containing results   */

extern
int Num_Nod_Var;		/* The number of nodal variables to reserve */
				/* space in the output file for. */
extern
int Num_Elem_Var;		/* The number of elemental variables to */
				/* reserve space in the output file for. */

extern
int Num_Glob_Var;		/* The number of global variables to reserve */
				/* space in the output file for. */

extern RESTART Restart_Info;	/* The restart information structure */

/*****************************************************************************/
/*	PROTOTYPES FOR GLOBAL FUNCTIONS THAT READ THE INPUT FILE             */
/*****************************************************************************/

 extern int   read_string (FILE *, char *, int );
 extern int   look_for    (FILE *, char *, char [], int);
 extern void  strip       (char * );

/*****************************************************************************/
