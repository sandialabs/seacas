/*
 * msub - Read file(s) and perform substitutions using values of
 * 	variables defined in makefile.
 *
 * Syntax: msub [ -e ] [ -f makefile ] [ +Rstr -Rstr] [ file ... ]
 *
 *
 * -e		Environment variables override makefile variables.
 * -f makefile	Specify alternate makefile.  Multiple -f options may
 * 		be given.
 * +Rstr		Specify variable reference initiator string.
 * -Rstr		Specify variable reference terminator string.
 *
 * +R and -R options must be given in pairs, although multiple pairs may
 * be specified.
 *
 * 27 Mar 1990	Paul DuBois	dubois@primate.wisc.edu
 *
 * 27 Mar 1990 V1.0
 * - Created.
 * 16 Apr 1993 V1.1
 * - Rewritten.
 * 25 Oct 1993 V1.11
 * - Added support for -e and var=value on command line.
 * 09 Mar 1996 V1.12
 * - Coercion of arguments to Malloc() and read() to avoid coredump
 * on NetBSD systems.
 * 30 Mar 1997 V1.13
 * - Under WinNT, the read() on the Makefile returns less than the requested
 * number of bytes if \r\n -> \n mapping is done, so make the failure test
 * less stringent.
 */

# ifdef WIN32
# include "X11\Xw32defs.h"
#endif

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define Malloc(n)	_Malloc((int) (n))

# define	bufSiz	2048

# define	FromMakefile(vp)	(vp->source == precMakefile)


typedef	struct Var	Var;

struct Var
{
	char	*var;
	char	*value;
	int	expanded;
	int	source;
	Var	*next;
};


static void	ReadMake ();
static int	CheckAssignment ();
static Var	*AddVar ();
static Var	*FindVar ();
static Var	*FindVarUsingEnv ();
static int	FindVarRef ();
static int	FindEndVarRef ();
static void	Expand ();
static void	Substitute ();
static char	*_Malloc();
static char	*NewString();
static void	Panic ();


static char	*usage = "Usage: msub [ +Rstr -Rstr] [ -f makefile ] file";
static char	*makefile = (char *) NULL;

static Var	*vvList = (Var *) NULL;
static int	nVars = 0;

static char	*mfRefInit[2] = { "${", "$(" };
static char	*mfRefTerm[2] = { "}", ")" };

static char	*usrRefInit[10];
static char	*usrRefTerm[10];
static int	usrRefSeqCnt = 0;

static char	**refInit;
static char	**refTerm;
static int	refSeqCnt;

static int	refInitIdx;
static int	varIdx;
static int	refTermIdx;
static int	restIdx;

/*
 * Assignment source precedences.  Command line assignments always override
 * assignments from Makefile or environment variable values.  Normally
 * assignments with Makefiles override environment variables, but if -e is
 * specified on the command line, their precedences are reversed.
 */

static int	precCmdLine = 3;
static int	precMakefile = 2;
static int	precEnvVar = 1;


int
main (argc, argv)
int	argc;
char	*argv[];
{
FILE	*f;
Var	*vp;
char	*p, *q;
int	pass;
int	needExpand;
int	toggle = 0;

	--argc;
	++argv;

	while (argc > 0)
	{
		/* see if it's a var=value argument */
		if (CheckAssignment (argv[0], precCmdLine))
		{
			--argc;
			++argv;
			continue;
		}
		/* if not, and it's not a flag, stop processing arguments */
		if (argv[0][0] != '-' && argv[0][0] != '+')
			break;
		if (strcmp (argv[0], "-e") == 0)
		{
		int	tmp;

			/* reverse precedence of makefile/env. assignments */
			tmp = precMakefile;
			precMakefile = precEnvVar;
			precEnvVar = tmp;
		}
		else if (strcmp (argv[0], "-f") == 0)
		{
			if (argc < 2)
				Panic (usage);
			if ((f = fopen (makefile = argv[1], "r")) == (FILE *) NULL)
				Panic ("cannot open makefile argument");
			ReadMake (f);
			(void) fclose (f);
			--argc;
			++argv;
		}
		else if (strncmp (argv[0], "+R", 2) == 0)
		{
			if (toggle == 1)
				Panic ("missing -R argument after +R");
			toggle = 1;
			if (strlen (argv[0]) == 2)
				Panic ("empty reference initiator specified");
			usrRefInit[usrRefSeqCnt] = &argv[0][2];
		}
		else if (strncmp (argv[0], "-R", 2) == 0)
		{
			if (toggle == 0)
				Panic ("missing +R argument before -R");
			toggle = 0;
			if (strlen (argv[0]) == 2)
				Panic ("empty reference terminator specified");
			usrRefTerm[usrRefSeqCnt] = &argv[0][2];
			++usrRefSeqCnt;
		}
		else
			Panic (usage);	/* bad flag */
		--argc;
		++argv;
	}

	if (toggle == 1)
		Panic ("missing -R argument after +R");
	if (usrRefSeqCnt == 0)	/* no reference sequences specified */
	{
		refInit = mfRefInit;
		refTerm = mfRefTerm;
		refSeqCnt = 2;
	}
	else
	{
		refInit = usrRefInit;
		refTerm = usrRefTerm;
		refSeqCnt = usrRefSeqCnt;
	}

	if (makefile == (char *) NULL)	/* no -f options were given */
	{
		if ((f = fopen ("makefile", "r")) == (FILE *) NULL)
		{
			if ((f = fopen ("Makefile", "r")) == (FILE *) NULL)
				Panic ("cannot open makefile or Makefile");
		}
		ReadMake (f);
		(void) fclose (f);
	}

	/* Replace instances of '$$' with a single ^A */
	/* (only for assignments from Makefile) */

	for (vp = vvList; vp != (Var *) NULL; vp = vp->next)
	{
		if (!FromMakefile (vp))
			continue;
		for (p = q = vp->value; *p != '\0'; p++, q++)
		{
			*q = *p;
			if (*p == '$' && *(p+1) == '$')
			{
				*q = '\01';
				p++;
			}
		}
		*q = '\0';
	}

	/* determine which variable values contain embedded references */

	for (vp = vvList; vp != (Var *) NULL; vp = vp->next)
	{
		if (!FromMakefile (vp))
			vp->expanded = 1;
		else
			vp->expanded = !FindVarRef (vp->value, 2, mfRefInit,
								mfRefTerm);
	}


	/* expand values to eliminate embedded references */

	for (pass = 0; pass < nVars; pass++)
	{
		needExpand = 0;
		for (vp = vvList; vp != (Var *) NULL; vp = vp->next)
		{
			if (!vp->expanded)
				Expand (vp);
			if (!vp->expanded)
				needExpand = 1;
		}
		if (needExpand == 0)	/* loop while values need expanding */
			break;
	}

	/* sanity check: shouldn't have to make more than nVars passes */
	if (pass >= nVars)
		Panic ("Too many expansion passes.  Something's wrong!");

	/* Replace instances of ^A with a '$' */
	/* (only for assignments from Makefile) */

	for (vp = vvList; vp != (Var *) NULL; vp = vp->next)
	{
		if (!FromMakefile (vp))
			continue;
		for (p = vp->value; *p != '\0'; p++)
		{
			if (*p == '\01')
				*p = '$';
		}
	}

	/* read source file(s) and perform substitutions */
	if (argc == 0)
		Substitute (stdin);
	else while (argc > 0)
	{
		if ((f = fopen (*argv, "r")) == (FILE *) NULL)
		{
			(void) fprintf (stderr, "cannot open \"%s\".\n", *argv);
			Panic ("Quitting.");
		}
		Substitute (f);
		(void) fclose (f);
		--argc;
		++argv;
	}

	return (0);
}


/*
 * Read a makefile, combine continuation lines (by joining lines with a space
 * and stripping leading whitespace on next line), null-terminate each line,
 * find variable assignments
 */

static void
ReadMake (f)
FILE	*f;
{
struct stat	st;
int	fd;
char	*p, *s;
char	*sNext;
char	*mfBuf;		/* makefile buffer */
int	i;

	fd = fileno (f);
	if (fstat (fd, &st) != 0)
		Panic ("cannot stat Makefile");
	mfBuf = Malloc (st.st_size + 1);
	i = read (fd, mfBuf, (int) st.st_size);
#ifdef WIN32
	/* just test for reading > 0 bytes if file size is > 0 */
	if (i <= 0 && st.st_size > 0)
		Panic ("cannot read Makefile");
#else
	if (i != st.st_size)
		Panic ("cannot read Makefile");
#endif
	mfBuf[st.st_size] = '\0';	/* make sure it's null-terminated */

	p = s = mfBuf;
	while (s <= mfBuf + st.st_size)
	{
		if (*s == '\\' && *(s+1) == '\n')
		{
			*p++ = ' ';
			s += 2;		/* skip continuation characters */
			while (isspace (*s))
				++s;
		}
		else
		{
			if (*s == '\n')	/* convert linefeeds to nulls */
				*s = '\0';
			*p++ = *s++;
		}
	}
	*p = '\0';

	s = mfBuf;
	while (s <= p)
	{
		/* need sNext because CheckAssignment() modifies current line */
		sNext = s + strlen (s) + 1;	/* find next line */
		(void) CheckAssignment (s, precMakefile);
		s = sNext;
	}

	free (mfBuf);		/* done with Makefile; jettison it */
}


/*
 * Determine whether the line contains a variable assignment, i.e.,
 * an identifier, an equals sign, and optionally a value following.
 *
 * If the value contains a comment afterward, the comment becomes
 * part of the value.  This is a bug, but it's too much trouble to
 * do the parsing necessary to find such things.  Just don't use them.
 *
 * This is called to process assignments from command line arguments and
 * from the makefile.
 *
 * Return non-zero if assignment was found, zero otherwise.
 */

static int
CheckAssignment (s, source)
char	*s;
int	source;
{
char	name[bufSiz], *np;
int	len;

	while (isspace (*s))		/* skip whitespace */
		++s;
	if (!isalpha (*s) && *s != '_')		/* no variable */
		return (0);
	np = name;
	while (isalnum (*s) || *s == '_')
		*np++ = *s++;
	*np = '\0';
	while (isspace (*s))		/* skip whitespace */
		++s;
	if (*s != '=')			/* no '=' */
		return (0);
	++s;				/* skip '=' */
	while (isspace (*s))		/* skip whitespace */
		++s;
	/* take rest of line as definition after trimming trailing whitespace */
	len = strlen (s);
	while (len > 0 && isspace (s[len-1]))
		s[--len] = '\0';
	(void) AddVar (name, s, source);
	return (1);
}


static Var *
FindVar (var)
char	*var;
{
Var	*vp;

	for (vp = vvList; vp != (Var *) NULL; vp = vp->next)
	{
		if (strcmp (vp->var, var) == 0)
			return (vp);
	}
	return ((Var *) NULL);
}


/*
 * Find a variable and return pointer to Var structure.  Never returns NULL.
 * If the variable isn't found, create an instance (either with an empty
 * value or the value from the environment variable of the same name if one
 * exists).  If the variable is found and is from the makefile, override it
 * from the environment if environment variabless have higher precedence (i.e.,
 * if -e was specified on the command line).
 */

static Var *
FindVarUsingEnv (var)
char	*var;
{
Var	*vp;
char	*val;

	if ((vp = FindVar (var)) == (Var *) NULL)
	{
		if ((val = getenv (var)) == (char *) NULL)
			val = "";
		vp = AddVar (var, val, precEnvVar);
	}
	else if (FromMakefile (vp) && precMakefile < precEnvVar)
	{
		if ((val = getenv (var)) != (char *) NULL)
			vp = AddVar (var, val, precEnvVar);
	}
	return (vp);
}

static Var *
AddVar (var, value, source)
char	*var, *value;
int	source;
{
Var	*vp;

	if ((vp = FindVar (var)) == (Var *) NULL)
	{
		vp = (Var *) Malloc (sizeof (Var));
		vp->var = NewString (var);
		vp->value = NewString (value);
		vp->source = source;
		vp->expanded = 0;
		vp->next = vvList;
		vvList = vp;
		++nVars;
	}
	else if (vp->source <= source)	/* exists; replace previous value if */
	{				/* new source has higher priority */
		free (vp->value);
		vp->value = NewString (value);
		vp->source = source;
	}
	return (vp);
}


/*
 * Find a variable reference in a string.  Return zero if no reference
 * found.  Otherwise return non-zero and set four globals:
 *
 * refInitIdx	index within s of beginning of reference initiator
 * varIdx	index within s of beginning of variable
 * refTermIdx	index within s of beginning of reference terminator
 * restIdx	index within s of rest of string following reference
 */

static int
FindVarRef (s, nRefSeqs, initRef, termRef)
char	*s;
int	nRefSeqs;
char	*initRef[];
char	*termRef[];
{
char	*p;
int	len, i;

	p = s;
	while (*p != '\0')
	{
		/* look for reference initiation sequence */
		for (i = 0; i < nRefSeqs; i++)
		{
			len = strlen (initRef[i]);
			if (strncmp (p, initRef[i], len) == 0
				&& FindEndVarRef (s, p-s+len, termRef[i]))
			{
				refInitIdx = p - s;
				varIdx = p - s + len;
				return (1);
			}
		}
		++p;
	}
	return (0);
}


static int
FindEndVarRef (s, idx, term)
char	*s;
int	idx;
char	*term;
{
char	*p = s + idx;	/* point to first char past reference initiator */
int	len = strlen (term);

	if (!isalpha (*p) && *p != '_')	/* no variable name present */
		return (0);
	while (isalnum (*p) || *p == '_')
	{
		if (strncmp (p+1, term, len) == 0)
		{
			refTermIdx = p + 1 - s;
			restIdx = refTermIdx + len;
			return (1);
		}
		++p;
	}
	return (0);
}


/*
 * Look for variable references in a variable value and expand them
 * when possible.  If a variable is referenced that has not itself
 * been fully expanded, defer expansion until another pass, at which
 * time the referenced variable might then be expanded.  This
 * prevents infinite expansions on circular references.
 */

static void
Expand (vp)
Var	*vp;
{
Var	*vp2;
char	buf[bufSiz * 4], *p;

	while (FindVarRef (vp->value, 2, mfRefInit, mfRefTerm))
	{
		(void) strncpy (buf, vp->value+varIdx, refTermIdx-varIdx);
		buf[refTermIdx-varIdx] = '\0';
		if ((vp2 = FindVarUsingEnv (buf)) != (Var *) NULL)
		{
			if (!vp2->expanded)
				return;		/* can't substitute yet */
			p = vp2->value;
		}
		else
			p = "";
		/* substitute value for reference, replace value */
		(void) strncpy (buf, vp->value, refInitIdx);
		(void) strcpy (buf+refInitIdx, p);
		(void) strcpy (buf+refInitIdx+strlen(p), vp->value+restIdx);
		free (vp->value);
		vp->value = NewString (buf);
	}
	vp->expanded = 1;	/* no more embedded references */
}


/*
 * Read through file, substituting variable values for variable
 * references.  If a variable reference is found that is for an
 * unknown variable, substitute the empty string.
 *
 * This really should check write() return values...
 */

static void
Substitute (f)
FILE	*f;
{
Var	*vp;
char	buf[bufSiz * 4], name[bufSiz], *p;

	while (fgets (buf, (int) sizeof (buf), f) != (char *) NULL)
	{
		p = buf;
		while (FindVarRef (p, refSeqCnt, refInit, refTerm))
		{
			(void) write (1, p, (int) refInitIdx);
			(void) strncpy (name, p+varIdx, refTermIdx-varIdx);
			name[refTermIdx-varIdx] = '\0';
			if ((vp = FindVarUsingEnv (name)) != (Var *) NULL)
				(void) write (1, vp->value, (int) strlen (vp->value));
			p += restIdx;
		}
		(void) write (1, p, (int) strlen (p));
	}
}


/*
 * Allocate space.
 */

static char *
_Malloc (size)
int	size;
{
char	*p;

	if ((p = (char *) malloc ((size_t) size)) == (char *) NULL)
		Panic ("Malloc: out of space");
	return (p);
}


/*
 * Allocate space for a string, copy the string into it, and return
 * a pointer to the copy.
 */

static char *
NewString (s)
char	*s;
{
	return (strcpy (Malloc (strlen (s) + 1), s));
}


static void
Panic (s)
char	*s;
{
	(void) fprintf (stderr, "msub: %s\n", s);
	exit (1);
}
