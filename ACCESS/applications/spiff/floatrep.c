/*                        Copyright (c) 1988 Bellcore
**                            All Rights Reserved
**       Permission is granted to copy or use this program, EXCEPT that it
**       may not be sold for profit, the copyright notice must be reproduced
**       on copies, and credit should be given to Bellcore where it is due.
**       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/


#ifndef lint
static char rcsid[] =
    "$Header: /usr/sharelan/cvs/SEACAS/prepost/spiff/floatrep.c,v 1.2 2004/10/29 14:00:40 gdsjaar Exp $";
#endif

#include "misc.h"
#include "floatrep.h"

R_float R_makefloat(void)
{
    R_float retval;

    retval = Z_ALLOC(1, struct R_flstr);
    retval->mantissa = Z_ALLOC(R_MANMAX, char);
    return (retval);
}

int R_getexp(R_float ptr)
{
    return (ptr->exponent);
}
