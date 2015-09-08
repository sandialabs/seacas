char    *Coord_Name[3] = {NULL, NULL, NULL};    /* The name(s) of the */
                                                /* coordinate axes.   */

double   PIO_Time_Array[26];			/* Vector for timings */

struct Parallel_IO PIO_Info;

char    Par_Nem_File_Name[MAX_FNL];  /* The par nemesis file name. */

/*
 * The following variables are used when a single processor is to write
 * info for a processor other than itself.
 */
int     Proc_For=-1;
int     Raid_For=-1;
int     Num_Proc_For=-1;
int     Proc_Info[4];
int    *Proc_Ids;

/* Function prototypes */

int which_file(char *, char *, char **, char **);
int read_pexoII_info(char *);
void gen_par_filenam(char *, char *);
