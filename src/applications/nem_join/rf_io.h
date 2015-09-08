/*
*
*
*
*	Include file for I/O global varibales used in FEM
*	problem specification
*
*/

char ExoFile[MAX_FNL+1];    /* Exodus II File containing problem definition. */
                            /* This name is the root name.                   */
char Exo_LB_File[MAX_FNL+1];/* Exodus II file containing the mesh
                             * load-balanceinformation                       */
char Exo_Res_File[MAX_FNL+1]; /* Exodus II file containing the mesh results  */
                            /* information                                   */
int  Debug_Flag = 1;	    /* Flag to specify debug info is to be printed out.
			       The value of this flag determines the level of
			       diagnostic info which is printed to stdout
			       Debug_Flag == 0 	No debug output
			                     .
					     .
					     9	maximum output               */
int Gen_Flag = 1;           /* Flag used by nem_join to determine if the user
                               wants to use an existing genesis file rather
                               than generating one from the parallel files */

int Subset_Flag = 0;        /* Flag used by nem_join to determine if the user
                               wants to read a subset of time indices from the
                               parallel ExodusII files containing results   */

int Num_Nod_Var  = 0;		/* The number of nodal variables to reserve */
				/* space in the output file for. */
int Num_Elem_Var = 0;		/* The number of elemental variables to */
				/* reserve space in the output file for. */
int Num_Glob_Var = 0;		/* The number of global variables to reserve */
				/* space in the output file for. */

RESTART Restart_Info;		/* Restart information structure */
