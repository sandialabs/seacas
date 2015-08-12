#ifndef _RF_UTIL_H
#define _RF_UTIL_H

/*****************************************************************************/
/*     EXTERN STATEMENTS FOR GLOBAL FUNCTIONS IN rf_util.c                   */
/*****************************************************************************/


extern int   ilog2i (unsigned int n);

extern void  sortN_int_int    (int nval, int ra[], int narr, ...);
extern void  sortN_int_float  (int nval, int ra[], int narr, ...);
extern void  sortN_int_floatlist  (int nval, int ra[], int narr, float *rb[]);
extern void  sortN_int_double (int nval, int ra[], int narr, ...);
extern void  sortN_int_doublelist (int nval, int ra[], int narr, double *rb[]);

extern void sort_int_int(int count, int ra[], int rb[]);
extern void sort_int_float(int count, int ra[], float rb[]);
extern void sort_int_double(int count, int ra[], double rb[]);
extern void sort_int_ptr(int nval, int ra[], char *rb[]);
extern void sort_int_int_ptr(int nval, int ra[], int rb[], char *rc[]);
extern void sort_int (int n, int ra[]);

extern int   find_max (int list_length, int list[]);
extern int   find_min (int list_length, int list[]);
extern int   find_inter (int inter_ptr[], int set1[], int set2[],
                         int length1, int length2, int prob_type);
extern int   find_inter_pos (int intersect[], int length1, int set1[],
                             int length2, int set2[], int prob_type);
extern void  init_vec_value (double *u, double value, int n);
extern void  dcopy1 (int N, double *dx, double *dy);
extern void  exchange_pointers (void **pointer1, void **pointer2);
extern void  get_remap_index (int n,int arrin[], int indx[]);
extern void  remap_int_array    (int N, int index[], int array[]);
extern void  remap_double_array (int N, int index[], double array[]);
extern void  print_global_vec (int total_nodes, double sol_vec[], int gnodes[],
                               int var_no, int k, int proc, int num_procs);
extern int   in_list_mono   (int ivalue, int *ibegin, int iend, int ivector[]);
extern void  iindexx        (unsigned int n, int arr[], unsigned int indx[]);
extern void  sort3 (int n, int ra[], int rb[], int m);
extern int   find_range     (int start_value, int end_value, int List[],
                             int num, int *start_pos, int *end_pos);
extern int   bin_search     (int List[],  int num, int value);
extern int   bin_search2    (int value,   int num, int List[]);
extern int   bin_search_min (int List[],  int num, int value);
extern void  print_line     (char *charstr, int ntimes);
extern int   break_message_up (unsigned int, int, int, int **);
extern double srandom1      (int *seed);
/*****************************************************************************/

#endif /* #ifndef _RF_UTIL_H */
