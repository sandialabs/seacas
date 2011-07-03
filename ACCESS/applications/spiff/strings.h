/*                        Copyright (c) 1988 Bellcore
**                            All Rights Reserved
**       Permission is granted to copy or use this program, EXCEPT that it
**       may not be sold for profit, the copyright notice must be reproduced
**       on copies, and credit should be given to Bellcore where it is due.
**       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

#ifndef S_INCLUDED
extern void S_wordcpy(char *to, char *from);
extern void S_skipword(char **theptr);
extern void S_skipspace(char **theptr);
extern void S_nextword(char **theptr);
extern int S_wordcmp(char *s1, char *s2);
extern void S_trimzeros(char *str);
extern void S_savestr(char **to, char *from);
extern void S_savenstr(char **to, char *from, int cnt);
extern void S_allocstr(char **to, int size);
#define S_INCLUDED
#endif
