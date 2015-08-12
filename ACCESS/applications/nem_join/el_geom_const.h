/*-----------------------------------------------------------------------------
*
*  Include file containing element geometry variable definitions
*
*
*----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/*      STRUCTURES FOR COMMUNICATION MAPS                                    */
/*---------------------------------------------------------------------------*/
struct e_comm_map
{
  int   map_id;
  int   elem_cnt;
  int  *elem_ids;
  int  *side_ids;
  int  *proc_ids;
};

struct n_comm_map
{
  int   map_id;
  int   node_cnt;
  int  *node_ids;
  int  *proc_ids;
};

typedef struct e_comm_map ELEM_COMM_MAP;
typedef struct n_comm_map NODE_COMM_MAP;

/*---------------------------------------------------------------------------*/
/*	PROTOTYPES FOR FUNCTIONS OPERATING ON THE GEOMETRY                   */
/*---------------------------------------------------------------------------*/
extern int ss_to_node_list(
			   int elem_type,
			   int elem_id,
			   int side_id,
			   int side_node_list[],
                           int local_proc
			   );

/*---------------------------------------------------------------------------*/
/*	GLOBAL QUANTITITES THAT ARE THE SAME ON ALL PROCESSORS		     */
/*---------------------------------------------------------------------------*/

extern int   Num_Dim;         /* Number of physical dimensions in the problem*/
extern int   Num_Node;        /* Total number of nodes in the entire mesh    *
		               * - this is a global quantity                 */
extern int   Num_Elem;        /* Total number of elements in the entire mesh *
		               * - this is a global quantity                 */
extern int   Num_Elem_Blk;    /* Total number of element blocks              */
extern int   Num_Node_Set;    /* Total number of node sets defined in the    *
			       * mesh exoII file			     */
extern int   Num_Side_Set;    /* Total number of side sets defined in the    *
			       * mesh exoII file			     */
extern int   Max_NP_Elem;     /* Maximum number of nodes in any element      *
		               *  - this is a global quantity                */
extern int   Num_QA_Recs;     /* Number of QA records in original file       */
extern int   Num_Info_Recs;   /* Number of Info records in original file     */

/*---------------------------------------------------------------------------*/
/*    VARIABLES THAT DEAL WITH SPECIFICATION OF LOAD BALANCE PROPERTIES      */
/*		THAT ARE THE DIFFERENT ON EACH PROCESSOR		     */
/*---------------------------------------------------------------------------*/

extern int *Num_Internal_Nodes;
			      /* Number of internal nodes on the current proc*/
extern int *Num_Border_Nodes; /* Number of border nodes on the current proc  */
extern int *Num_External_Nodes;
			      /* Number of external nodes on the current proc*/
extern int *Num_Internal_Elems;
			      /* Number of Elements on the local processor.  */

extern int *Num_Border_Elems;
                              /* Number of Elements on the local processor *
                               * and shared by other processors. */

extern int *Num_N_Comm_Maps;   /* Number of nodal communication maps */

extern int *Num_E_Comm_Maps;   /* Number of elemental communication maps */

extern ELEM_COMM_MAP **E_Comm_Map;  /* Elemental communication map structure */

extern NODE_COMM_MAP **N_Comm_Map;  /* Nodal communication map structure */

extern int  **GNodes;         /* Data structure which contains the internal, *
			       * border, and external nodes on each processor*
			       * They are structured in that order, and      *
			       * monotonically within each category          *
			       *  Type: int vector of length                 *
 			       *       (Num_Internal_Nodes + Num_Border_Nodes*
			                Num_External_Nodes) 		     */

extern int  **GElems;         /* Data structure which contains the internal  *
			       * elements on each processor.  It is a map    *
			       * from the local element number to the global *
			       * element number.  			     *
			       *  Type: int vector of length                 *
 			       *        Num_Internal_Elems                   */

extern int **Elem_Map;	      /* Map for Nemesis output */

extern int  **GElem_Blks;     /* Data structure which contains the mapping   *
                               * from the local element block number to the  *
	                       * global element block number                 *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Elem_Blk                    */

extern int   **GNode_Sets;    /* Data structure which contains the mapping   *
                               * from the local node set number to the       *
	                       * global node set number                      *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Node_Sets                   */

extern int   **GSide_Sets;    /* Data structure which contains the mapping   *
                               * from the local side set number to the       *
	                       * global side set number                      *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Side_Sets                   */

/*---------------------------------------------------------------------------*/
/*    VARIABLES THAT DEAL WITH SPECIFICATION OF NODAL PROPERTIES   	     */
/*		THAT ARE THE DIFFERENT ON EACH PROCESSOR		     */
/*---------------------------------------------------------------------------*/

extern float ***Coor_sp;
extern double ***Coor_dp;     /* 2d dynamically allocated array containing   *
                               * the physical space coordinates for each     *
			       * node, defined on the local processor.       *
			       *  Type: double/float vector of length        *
			       *        Num_Dim   by			     *
 			       *       (Num_Internal_Nodes + Num_Border_Nodes*
			       *        Num_External_Nodes) 		     */

/*---------------------------------------------------------------------------*/
/*    VARIABLES THAT DEAL WITH SPECIFICATION OF ELEMENT PROPERTIES   	     */
/*		THAT ARE THE DIFFERENT ON EACH PROCESSOR		     */
/*---------------------------------------------------------------------------*/

extern int **Proc_Connect_Ptr;/* Vector of pointers to the start of each     *
			       * element in Proc_Elem_Connect                *
			       *  Type: int vector of length                 *
 			       *        Num_Internal_Elems                   */

extern int  **Elem_Type;      /* Vector which contains the element code for  *
 			       * each defined on the current processor       *
			       *  Type: int vector of length                 *
 			       *        Num_Internal_Elems                   */

extern int **Proc_Elem_Connect;/* Connectivity lists for the elements        *
                                * required by the current processor          *
                                *  Type: int vector of variable length       */

extern double **Proc_Elem_Attr_dp;
extern float  **Proc_Elem_Attr_sp; /* Attribute list for the elements        *
                                    * required by the current processor      *
                                    *  Type: float vector of variable length */


/*---------------------------------------------------------------------------*/
/*    VARIABLES THAT DEAL WITH SPECIFICATION OF ELEMENT BLOCK PROPERTIES     */
/*		THAT ARE THE DIFFERENT ON EACH PROCESSOR		     */
/*---------------------------------------------------------------------------*/

extern int *Proc_Num_Elem_Blk;/* Number of element blocks on this processor  */

extern int **Proc_Num_Elem_In_Blk;
                              /* Number of elements in the processor's       *
			       * element blocks                              *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Elem_Blk                    */

extern int **Proc_Elem_Blk_Ids;
			      /* Element block id's for the processor's      *
			       * element blocks                              *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Elem_Blk                    */

extern int **Proc_Elem_Blk_Types;
			      /* Element block types for the processor's     *
			       * element blocks                              *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Elem_Blk                    */

extern int **Proc_Nodes_Per_Elem;
			      /* Number of nodes per element for each        *
			       * block on the current processor              *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Elem_Blk                    */

extern int **Proc_Num_Attr;   /* Number of attributes for each block on the  *
                               * current processor                           *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Elem_Blk                    */

extern int  *Elem_Blk_2_Matls;
                              /* Mapping of element block NUMBERS to material*
			       * IDs  	                	             *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Elem_Blk                    */



/*---------------------------------------------------------------------------*/
/*	VARIABLES THAT DEAL WITH SPECIFICATION OF NODE SETS		     */
/*		THAT ARE THE DIFFERENT ON EACH PROCESSOR		     */
/*---------------------------------------------------------------------------*/

extern int *Proc_Num_Node_Sets; /* Number of node sets on the current proc   */

extern int *Proc_NS_List_Length;
			      /* Total length of all the node set lists      */

extern int **Proc_NS_Ids;     /* Node sets ids for the node sets in a given  *
			       * processor                                   *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Node_Sets                   */

extern int **Proc_NS_Count;   /* Number of nodes in each node set defined on *
			       * the current processor.                      *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Node_Sets                   */

extern int **Proc_NS_DF_Count;
                              /* Number of distribution factors associated   *
                               * with a node set.                            *
                               *  Type: int vector of length                 *
                               *        Proc_Num_Node_Sets                   */

extern int **Proc_NS_Pointers; /* Vector of pointers into Proc_NS_List for   *
                                * each node set defined on the current proc  *
                                *  Type: int vector of length                *
                                *        Proc_Num_Node_Sets                  */

extern int **Proc_NS_List;    /* Node sets list record for the nodes sets in *
			       * a given processor                           *
			       *  Type: int vector of length                 *
 			       *        Proc_NS_List_Length                  */

extern float  **Proc_NS_Dist_Fact_sp;
extern double **Proc_NS_Dist_Fact_dp;
			      /* Node sets distribution factors for the node *
			       * sets on a given processor                   *
			       *  Type: float vector of length               *
 			       *        Proc_NS_List_Length                  */



/*---------------------------------------------------------------------------*/
/*	VARIABLES THAT DEAL WITH SPECIFICATION OF SIDE SETS		     */
/*		THAT ARE THE DIFFERENT ON EACH PROCESSOR		     */
/*---------------------------------------------------------------------------*/

extern int *Proc_Num_Side_Sets; /* Number of side sets on the current proc  */

extern int *Proc_SS_Elem_List_Length;
		              /* Total length of all the side set node lists */
extern int *Proc_SS_Side_List_Length;
			      /* Total length of all the side set element    *
			       * lists                                       */

extern int **Proc_SS_Ids;     /* Side sets ids for side sets in a given      *
			       * processor                                   *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Side_Sets                   */
extern int **Proc_SS_Elem_Count;
			      /* Side sets count record for elements (see the*
			       * EXODUS manual) for the side sets in a given *
			       * processor                                   *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Side_Sets                   */

extern int **Proc_SS_DF_Pointers;
                              /* Pointer into the distribution factors for   *
                               * the side sets.                              *
                               *  Type: int array of dimensions              *
                               *    Num_Proc * Proc_Num_Side_Sets            */

extern float  **Proc_SS_Dist_Fact_sp;
extern double **Proc_SS_Dist_Fact_dp;
                              /* Pointer for storage of the distribution     *
                               * factors.                                    */

extern int **Proc_SS_DF_Count;
                              /* Count of the number of distribution         *
                               * factors in this side set.                   *
                               *  Type: int vector of length                 *
                               *        Proc_Num_Side_Sets                   */

extern int **Proc_SS_Side_Count;
			      /* Side sets count record for nodes (see the   *
			       * EXODUS manual) for the side sets in a given *
			       * processor                                   *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Side_Sets                   */

extern int **Proc_SS_Elem_Pointers;
			      /* Side sets pointer record for elements (see  *
			       * the EXODUS manual) for the side sets in a   *
			       * given processor                             *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Side_Sets                   */

extern int **Proc_SS_Side_Pointers;
			      /* Side sets pointer record for nodes (see     *
			       * the EXODUS manual) for the side sets in a   *
			       * given processor                             *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Side_Sets                   */

extern int **Proc_SS_Elem_List;
			      /* Side sets element list record for the side  *
			       * sets in a given processor                   *
			       *  Type: int vector of length                 *
 			       *        Proc_Num_Side_Sets                   */

extern int **Proc_SS_Side_List;
                              /* Side sets node list record for the side sets*
			       * in a given processor                        *
			       *  Type: int vector of length                 *
 			       *        Proc_SS_Node_List_Length             */

/*---------------------------------------------------------------------------*/
/*		VARIABLES THAT DEAL WITH GENERAL INFORMATION THAT IS         */
/*			THE SAME ON EVERY PROCESSOR                          */
/*---------------------------------------------------------------------------*/

extern char **QA_Record;	/* The QA Records from the original file     */

extern char **Info_Record;	/* The Information Records from the original *
                                 * file                                      */

/*
 * Structure to hold information from element quality checks for element blocks
 */

struct Elem_Quality_struct
{
  int elem_blk_id;
  int distortion_max_id;
  int distortion_min_id;
  int stretch_max_id;
  int stretch_min_id;
  int aspect_ratio_max_id;
  int aspect_ratio_min_id;

  double distortion_max;
  double distortion_min;
  double distortion_mean;
  double stretch_max;
  double stretch_min;
  double stretch_mean;
  double aspect_ratio_max;
  double aspect_ratio_min;
  double aspect_ratio_mean;
};

/*---------------------------------------------------------------------------*/
/*			END OF el_geom_const.h				     */
/*---------------------------------------------------------------------------*/
