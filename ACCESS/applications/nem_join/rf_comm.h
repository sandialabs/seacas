#ifndef _RF_COMM_H
#define _RF_COMM_H

/*****************************************************************************/
/*     EXTERN STATEMENTS FOR GLOBAL FUNCTIONS IN rf_comm.c                   */
/*****************************************************************************/

extern int    nwrite_big (char *, int, int, int, int *);
extern int    nread_big  (char *, int, int *, int *, int *);
extern void   brdcst_maxlen (const int, const int, char *, int, int);
extern void   brdcst (int, int, char *, int,  int);
extern void   sync (int, int);
extern void   print_sync_start (int, int, int);
extern void   print_sync_end   (int, int, int);
extern int    gsum_int    (int,    int, int);
extern double gsum_double (double, int, int);
extern double gsum_max    (double, int, int);
extern double gsum_min    (double, int, int);
extern int    gmax_int    (int,    int, int);
extern double gavg_double (double, int, int);
extern int    gmin_int    (int,    int, int);
extern double gmax_double (double, int, int);
extern double gmin_double (double, int, int);

/*****************************************************************************/

#endif /* #ifndef _RF_COMM_H */
