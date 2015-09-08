#ifndef HAVE_PROTOTYPES
#   if defined(__STDC__) || defined(__GNUC__) || defined(__cplusplus) || defined(c_plusplus)
#       define	HAVE_PROTOTYPES
#   endif
#endif

#undef PROTO
#ifdef HAVE_PROTOTYPES
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif


/* function declarations for dynamic array allocation */

extern double 		*array_alloc (
	char *file,
	int lineno,
	int numdim,
	...
);

extern void		safe_free  (
	void *ptr
);
