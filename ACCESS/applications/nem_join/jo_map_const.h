/*---------------------------------------------------------------------------*/
/*    VARIABLES THAT DEAL WITH THE NODAL AND ELEMENTAL MAPS                  */
/*              THAT ARE THE DIFFERENT ON EACH PROCESSOR                     */
/*---------------------------------------------------------------------------*/

extern int  *Proc_Num_Nodes;   /* Number of Nodes in each file on local proc */
extern int  *Proc_Num_Elem;    /* Number of Elem in each file on local proc  */

extern int   Num_Nodes_Gross;  /* Total number of nodes in all files on proc */
extern int   Num_Elem_Gross;   /* Total number of elem in all files on proc  */

extern int   Num_Nodes_Net;    /* Number of unique nodes on processor        */
extern int   Num_Elem_Net;     /* Number of unique elements on processor     */

/*****************************************************
* The following two arrays provide maps from
* where information is read in from the parallel 
* files, to where it is in the arrays corresponding
* to the sorted global element and node maps (which do
* not have any duplicate values).
******************************************************/
extern int  *Nodal_Sort_Map;
extern int  *Elem_Sort_Map;

extern int  *GNode_Seq;         /* sequential global nodal map               */
extern int  *GElem_Seq;         /* sequential global elemental map           */

/*---------------------------------------------------------------------------*/
/*              VARIABLES THAT DEAL WITH ELEMENT BLOCK PROPERTIES            */
/*---------------------------------------------------------------------------*/

extern int   *EB_Cnts;         /* Global Element Block Counts                */
extern int   *EB_Ids;          /* Global Element Block Ids                   */

extern char **EB_Types;        /* Element Block types for each element block */

/*---------------------------------------------------------------------------*/
/*              VARIABLES THAT DEAL WITH NODE SET PROPERTIES                 */
/*---------------------------------------------------------------------------*/

extern int   *NS_Cnts;         /* Global Node Set Counts                     */

/*---------------------------------------------------------------------------*/
/*              VARIABLES THAT DEAL WITH SIDE SET PROPERTIES                 */
/*---------------------------------------------------------------------------*/

extern int   *SS_Cnts;         /* Global Side Set Counts                     */


