/*
 *    Declaration of
 *	Global variables defined for purpose of parallel implementation
 *
 *
 */

/*
 *      Parameters describing the characteristics of the Parallel Machine
 */

extern int
    Num_Proc,       /* Total number of processors	  */
    Proc,           /* Processor number               */
    Dim;            /* Dimension of logical hypercube */

extern void print_heap_info(int, char*);
/************** END of rf_mp.h ******************************************/
