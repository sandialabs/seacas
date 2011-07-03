/*                        Copyright (c) 1988 Bellcore
**                            All Rights Reserved
**       Permission is granted to copy or use this program, EXCEPT that it
**       may not be sold for profit, the copyright notice must be reproduced
**       on copies, and credit should be given to Bellcore where it is due.
**       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/


#ifndef lint
static char rcsid[] =
    "$Header: /usr/sharelan/cvs/SEACAS/prepost/spiff/visual.c,v 1.2 2004/10/29 14:00:41 gdsjaar Exp $";
#endif

#include "misc.h"
/*
**	dummy code for systems that don't have
**	the mgr window manager installed
*/
int V_visual(int d)
{
    Z_fatal("visual mode is not available on this machine\n");
    return (-d);		/* boiler plate */
}

void V_cleanup(void)
{
}
