/*                        Copyright (c) 1988 Bellcore
**                            All Rights Reserved
**       Permission is granted to copy or use this program, EXCEPT that it
**       may not be sold for profit, the copyright notice must be reproduced
**       on copies, and credit should be given to Bellcore where it is due.
**       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

#ifndef W_INCLUDED

#include <stdio.h>

#define _W_COMWORD	16
#define _W_COMMAX	20
#define _W_BOLMAX	20
#define _W_LITMAX	20

/*
**	these three data structures used to be much
**		different.  eventually, the differences
**		have disappeared as the code has evolved.
**		obviously, they should now be collapsed.
**		someday . . .
*/
typedef struct {
    char begin[_W_COMWORD];
    char end[_W_COMWORD];
    char escape[_W_COMWORD];
} _W_bolstruct, *W_bol;

typedef struct {
    char begin[_W_COMWORD];
    char end[_W_COMWORD];
    char escape[_W_COMWORD];
    int nestbit;
} _W_comstruct, *W_com;

typedef struct {
    char begin[_W_COMWORD];
    char end[_W_COMWORD];
    char escape[_W_COMWORD];
} _W_litstruct, *W_lit;

#define W_bolbegin(ptr)		(ptr->begin)
#define W_bolend(ptr)		(ptr->end)
#define W_bolescape(ptr)	(ptr->escape)

#define W_litbegin(ptr)		(ptr->begin)
#define W_litend(ptr)		(ptr->end)
#define W_litescape(ptr)	(ptr->escape)

#define W_combegin(ptr)		(ptr->begin)
#define W_comend(ptr)		(ptr->end)
#define W_comescape(ptr)	(ptr->escape)

extern char _W_bolchar;
extern char _W_eolchar;

#define W_setbolchar(x)		(_W_bolchar = x)
#define W_seteolchar(x)		(_W_eolchar = x)

extern W_bol W_isbol(char *str);
extern W_lit W_islit(char *str);
extern W_com W_iscom(char *str);

extern int W_is_bol(W_bol ptr);
extern int W_is_lit(W_lit ptr);
extern int W_is_com(W_com ptr);

extern _W_bolstruct _W_bols[];
extern _W_litstruct _W_lits[];
extern _W_comstruct _W_coms[];

extern void W_clearcoms(void);
extern void W_clearlits(void);
extern void W_addcom(char *str, int nestflag);
extern void W_addlit(char *str);

#define W_BOLNULL		((W_bol)0)
#define W_COMNULL		((W_com)0)
#define W_LITNULL		((W_lit)0)

#define W_INCLUDED
#endif
