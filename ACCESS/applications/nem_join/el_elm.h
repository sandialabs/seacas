/*
 * Define element types.
 * The idea here is that the last digit of each element type will indicate the
 * order of interpolation for that type, i.e.,
 *
 *     order of element = last digit + 1
 */

/* 1-d elements */

#define BAR2                0
#define BAR3                1

/* 2-d elements */

#define QUAD4              10
#define QUAD8              21
#define QUAD9              31
#define TRI3               40
#define TRI6               51

/* 3-d elements */

#define HEX8              100
#define HEX20             201
#define HEX27             301
#define TET4              400
#define TET10             501
#define TET8              511
#define SHELL4            505
#define SHELL8            510
#define SPHERE            515
#define WEDGE6            520
#define WEDGE15           521
#define WEDGE16           525
#define HEXSHELL          550
#define TSHELL3           530
#define TSHELL6           535
#define PYRAMID5          560
#define PYRAMID13         561

/* define element data "request for information" types */

#define NNODES            1
#define NQUAD             2
#define NDIM              3
#define NQUAD_SURF        4
#define NINTERP           5
#define NN_SIDE           6
#define CHILD             7

/* define shape function information types */

#define PSI               0
#define DPSI_S            1
#define DPSI_T            2
#define DPSI_U            3

/* define maximum quantities */

#define MAX_NODES_PER_SIDE    9
#define MAX_NP_ELEM      27
#define MAX_SUR_ELEM_2D  18  /* Maximum number of elements            */
                             /* surrounding (containing )a given node */
#define MAX_SUR_ELEM_3D  60  /* Maximum number of elements            */
                             /* surrounding (containing )a given node */
#define MAX_UNK_ELEM     270 /* Maximum nunber of unknowns defined at */
                             /* global nodes at an element            */
                             /* (kluge that will be replaced by a     */
                             /*  better structure later)              */
#define MAX_QUAD_PTS     27  /* Maximum number of quad points in any  */
                             /* points                                */
#define MAX_PDIM         3   /* Maximum physical problem dimension    */



/******************************* PROTOTYPES FOR el_elm_info.c ****************/

extern int elem_info(
		     int info,
		     int ielem_type,
                     int supp
		     );

extern int imax_quad(
		     int n,
		     int elem_type[]
		     );

extern void find_stu(
		     int iquad,
		     int ielem_type,
		     double *s, double *t, double *u
		     );

extern void find_surf_stu(
			  int iquad,
			  int ielem_type,
			  int id_side,
			  double *s, double *t, double *u
			  );

extern double Gq_weight(
			int iquad,
			int ielem_type
			);

extern double Gq_surf_weight(
			     int iquad,
			     int ielem_type
			     );

extern int in_list(
		   int ivalue,
		   int ibegin,
		   int iend,
		   int ivector[]
		   );

extern int get_type(
		    char string[],
		    int nodes
		    );

/******************************* PROTOTYPES FOR rf_shape.c *******************/

extern double shape(
                    double s, double t, double u,
                    int Ielem_type,
                    int Iquant,
                    int Ipnode
                    );

/******************************** other PROTOTYPES ***************************/

extern double length_scale(void);
extern double dist(double x1, double y1, double z1, double x2, double y2,
                   double z2);

/*****************************end of el_elm.h ********************************/

