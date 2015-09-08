/*                        Copyright (c) 1988 Bellcore
**                            All Rights Reserved
**       Permission is granted to copy or use this program, EXCEPT that it
**       may not be sold for profit, the copyright notice must be reproduced
**       on copies, and credit should be given to Bellcore where it is due.
**       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

#ifndef Z_INCLUDED

#define	Z_LINELEN	1024
#define	Z_WORDLEN	  20

extern char Z_err_buf[];

/*
**	helpful macros
*/
#define Z_ABS(x)	(( (x) < (0) )? (-(x)):(x))
#define Z_MIN(x,y)	(( (x) < (y) )? (x):(y))
#define Z_MAX(x,y)	(( (x) > (y) )? (x):(y))

#define Z_ALLOC(n,type)	((type*) _Z_myalloc((n) * sizeof (type)))
extern int *_Z_myalloc(int k);

/*
**	lines needed to shut up lint
*/
extern void Z_complain(char *str);
extern void Z_fatal(char *str);
extern void Z_exceed(int d);
extern void Z_setquiet(void);
#ifndef NOCHATTER
extern void Z_chatter(char *str);
#endif

#define Z_INCLUDED
#endif
