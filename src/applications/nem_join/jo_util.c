#include <stdlib.h>

/************ R O U T I N E S   I N   T H I S   F I L E  **********************

       NAME                             TYPE
----------------------------------------------------------------------
	sort_map_void			void
	sort_map_2d_void		void
	sort_map_ptr			void

******************************************************************************/

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void sort_map_void(int word_size, int *sort_map, void *arr,
                   void *sorted_arr, int slen)
{
  /* Local Variables */
  register int      i;
  float   *fptr1, *fptr2;
  double  *dptr1, *dptr2;

/**************************** execution begins *******************************/

  /* first get the right precision and sort function */

  if (word_size == sizeof(float)) {
    fptr1 = (float *) arr;
    fptr2 = (float *) sorted_arr;
  }
  else {
    dptr1 = (double *) arr;
    dptr2 = (double *) sorted_arr;
  }

  /*
   * sort the array using the sort map determined when the global
   * number map was formed.
   */
  if (word_size == sizeof(float))
    for (i = 0; i < slen; i++)
      fptr2[i] = fptr1[sort_map[i]];
  else
    for (i = 0; i < slen; i++)
      dptr2[i] = dptr1[sort_map[i]];

}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void sort_map_2d_void(int word_size, int *sort_map, void *arr,
                      void *sorted_arr, int slen, int narr)
{
  /* Local Variables */
  register int      i, j;
  float  **fptr1, **fptr2;
  double **dptr1, **dptr2;

/**************************** execution begins *******************************/

  /* first get the right precision and sort function */

  if (word_size == sizeof(float)) {
    fptr1 = (float **) arr;
    fptr2 = (float **) sorted_arr;
  }
  else {
    dptr1 = (double **) arr;
    dptr2 = (double **) sorted_arr;
  }

  /*
   * sort the arrays using the sort map determined when the global
   * number map was formed.
   */
  for (i = 0; i < slen; i++) {
    if (word_size == sizeof(float))
      for (j = 0; j < narr; j++)
        fptr2[j][i] = fptr1[j][sort_map[i]];
    else
      for (j = 0; j < narr; j++)
        dptr2[j][i] = dptr1[j][sort_map[i]];
  }
}
