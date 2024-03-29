/*
 * @HEADER
 *
 * ***********************************************************************
 *
 *  Zoltan Toolkit for Load-balancing, Partitioning, Ordering and Coloring
 *                  Copyright 2012 Sandia Corporation
 *
 * Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 * the U.S. Government retains certain rights in this software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the Corporation nor the names of the
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA CORPORATION OR THE
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Questions? Contact Karen Devine	kddevin@sandia.gov
 *                    Erik Boman	egboman@sandia.gov
 *
 * ***********************************************************************
 *
 * @HEADER
 */


#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "params_const.h"
#include "zoltan_types.h"
#include "zoltan_util.h"
#include "zz_const.h"

/*****************************************************************************/
/***   Function prototypes:                                                ***/
/*****************************************************************************/

static void Zoltan_Print_Assigned_Param_Vals(PARAM_VARS * );

/*****************************************************************************/
/***   Local macros:                                                       ***/
/*****************************************************************************/
#define SET_PARAM_VAL(datatype,value) { \
  for (i=lo; i<hi; i++) \
    ((datatype *) param_ptr->ptr)[i] = value; \
  }
/*****************************************************************************/
/***   Function definitions:                                               ***/
/*****************************************************************************/

int      Zoltan_Assign_Param_Vals(
PARAM_LIST * change_list,	/* list of parameter values being changed */
PARAM_VARS * params,		/* structure describing parameters        */
int debug_level,                /* level for output of debugging info     */
int proc,                       /* processor # (controls debug printing)  */
int print_proc                  /* processor that should perform printing */
)
{
    char     *yo = "Zoltan_Assign_Param_Vals";
    char      msg[256];
    char     *name;		/* name of parameter being reset */
    char     *val;		/* new value for parameter       */
    int       index;		/* index of parameter entry      */
    int       lo, hi;		/* lower/upper bounds on index   */
    int       found;		/* is name found?                */
    int       ierr;		/* error code                    */
    int       i;		/* loop variable                 */
    PARAM_VARS *param_ptr;      /* pointer to current param      */

    ierr = ZOLTAN_OK;

    while (change_list != NULL) {
        param_ptr = params;
	name = change_list->name;
	val = change_list->new_val;
	index = change_list->index;

	found = 0;
	while (param_ptr->name != NULL) {
	    if (!strcmp(param_ptr->name, name)) {
		found = 1;
		break;
	    }
	    param_ptr++;
	}

	if (found) {		/* name found */

          /* Check that param_ptr->ptr isn't NULL */
          if (param_ptr->ptr == NULL) {
             ierr = ZOLTAN_WARN;
             if (debug_level > 0 && proc == print_proc) {
                sprintf(msg, "Parameter %s is not bound "
                       "to any variable.  Parameter ignored.\n",
                        param_ptr->name);
                ZOLTAN_PRINT_WARN(proc, yo, msg);
             }
          }

          /* Check that index is in valid range */
          if ((index > param_ptr->length) || (index < -1)) {
             ierr = ZOLTAN_WARN;
             if (debug_level > 0 && proc == print_proc) {
                sprintf(msg, "Invalid index %d for parameter %s. "
                       "Parameter entry ignored.\n",
                        index, param_ptr->name);
                ZOLTAN_PRINT_WARN(proc, yo, msg);
             }
          }

          if (ierr == ZOLTAN_OK) { /* OK so far. */

            if (index == -1){  /* Set all entries in a param vector. */
              lo = 0;
              hi = param_ptr->length;
              if (hi == 0) hi = 1; /* Special case for scalar parameters. */
            }
            else {  /* Set just one entry in the param vector. */
              lo = index;
              hi = lo+1;
            }

	    /* Figure out what type it is and read value. */
	    if (!strcmp(param_ptr->type, "INT") ||
                !strcmp(param_ptr->type, "INTEGER")) {
		/* First special case if True or False */
		if (*val == 'T')
		    SET_PARAM_VAL(int, 1)
		else if (*val == 'F')
		    SET_PARAM_VAL(int, 0)
		else {
		    SET_PARAM_VAL(int, atoi(val))
		}
	    }

	    else if ((!strcmp(param_ptr->type, "FLOAT")) ||
	             (!strcmp(param_ptr->type, "REAL"))) {
		SET_PARAM_VAL(float, atof(val))
	    }

	    else if (!strcmp(param_ptr->type, "DOUBLE")) {
		SET_PARAM_VAL(double, atof(val))
	    }

	    else if (!strcmp(param_ptr->type, "LONG")) {
		/* First special case if True or False */
		if (*val == 'T')
		    SET_PARAM_VAL(long, 1)
		else if (*val == 'F')
		    SET_PARAM_VAL(long, 0)
		else {
		    SET_PARAM_VAL(long, atol(val))
		}
	    }

	    else if (!strcmp(param_ptr->type, "CHAR")) {
		SET_PARAM_VAL(char, (*val))
	    }

	    else if (!strcmp(param_ptr->type, "STRING")) {
                /* String parameters are assumed to be scalar. */
		strncpy((char *) param_ptr->ptr, val, MAX_PARAM_STRING_LEN);
	    }
	}
      }

      change_list = change_list->next;
    }

    if (debug_level > 0 && proc == print_proc)
        Zoltan_Print_Assigned_Param_Vals(params);

    return ierr;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

static void Zoltan_Print_Assigned_Param_Vals(
PARAM_VARS * params 		/* structure describing parameters */
)
{
/* Prints the parameter values in PARAM_VARS *param.     */
PARAM_VARS *param_ptr = params;       /* pointer to current param */
int i;

    while (param_ptr->name != NULL) {
      if (param_ptr->ptr != NULL) {
        if (!strcmp(param_ptr->type, "INT") ||
            !strcmp(param_ptr->type, "INTEGER")) {

          if (param_ptr->length < 1)
            printf("ZOLTAN Parameter %s = %d\n",
                    param_ptr->name, *((int *) param_ptr->ptr));
          else
            for (i=0; i<param_ptr->length; i++)
              printf("ZOLTAN Parameter %s = %d\n",
                      param_ptr->name, ((int *) param_ptr->ptr)[i]);

        }
        else if (!strcmp(param_ptr->type, "FLOAT") ||
                 !strcmp(param_ptr->type, "REAL")) {
          if (param_ptr->length < 1)
            printf("ZOLTAN Parameter %s = %f\n",
                    param_ptr->name, *((float *) param_ptr->ptr));
          else
            for (i=0; i<param_ptr->length; i++)
              printf("ZOLTAN Parameter %s = %f\n",
                      param_ptr->name, ((float *) param_ptr->ptr)[i]);

        }
        else if (!strcmp(param_ptr->type, "DOUBLE")) {
          if (param_ptr->length < 1)
            printf("ZOLTAN Parameter %s = %f\n",
                    param_ptr->name, *((double *) param_ptr->ptr));
          else
            for (i=0; i<param_ptr->length; i++)
              printf("ZOLTAN Parameter %s = %f\n",
                      param_ptr->name, ((double *) param_ptr->ptr)[i]);

        }
        else if (!strcmp(param_ptr->type, "LONG")) {
          if (param_ptr->length < 1)
            printf("ZOLTAN Parameter %s = %ld\n",
                    param_ptr->name, *((long *) param_ptr->ptr));
          else
            for (i=0; i<param_ptr->length; i++)
              printf("ZOLTAN Parameter %s = %ld\n",
                      param_ptr->name, ((long *) param_ptr->ptr)[i]);

        }
        else if (!strcmp(param_ptr->type, "STRING")) {
            printf("ZOLTAN Parameter %s = %s\n",
                    param_ptr->name, (char *) param_ptr->ptr);
        }
        else if (!strcmp(param_ptr->type, "CHAR")) {
            printf("ZOLTAN Parameter %s = %c\n",
                    param_ptr->name, *((char *) param_ptr->ptr));
        }
      }
      param_ptr++;
    }
}

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif
