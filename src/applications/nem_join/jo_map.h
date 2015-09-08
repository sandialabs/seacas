#ifndef NULL
#include <stdio.h>
#endif

/*---------------------------------------------------------------------------*/
/*    VARIABLES THAT DEAL WITH THE NODAL AND ELEMENTAL MAPS                  */
/*              THAT ARE THE DIFFERENT ON EACH PROCESSOR                     */
/*---------------------------------------------------------------------------*/

int  *Proc_Num_Nodes = NULL;   /* Number of Nodes in each file on local proc */
int  *Proc_Num_Elem = NULL;    /* Number of Elem in each file on local proc  */

int   Num_Nodes_Gross = 0;     /* Total number of nodes in all files on proc */
int   Num_Elem_Gross = 0;      /* Total number of elem in all files on proc  */

int   Num_Nodes_Net = 0;       /* Number of unique nodes on processor        */
int   Num_Elem_Net = 0;        /* Number of unique elements on processor     */

/*****************************************************
* The following two arrays provide maps from
* where information is read in from the parallel
* files, to where it is in the arrays corresponding
* to the sorted global element and node maps (which do
* not have any duplicate values).
******************************************************/
int   *Nodal_Sort_Map = NULL;
int   *Elem_Sort_Map = NULL;

int   *GNode_Seq = NULL;       /* sequential global nodal map                */
int   *GElem_Seq = NULL;       /* sequential global elemental map            */

/*---------------------------------------------------------------------------*/
/*              VARIABLES THAT DEAL WITH ELEMENT BLOCK PROPERTIES            */
/*---------------------------------------------------------------------------*/

int   *EB_Cnts = NULL;         /* Global Element Block Counts                */
int   *EB_Ids = NULL;          /* Global Element Block Ids                   */

char **EB_Types = NULL;        /* Element Block types for each element block */

/*---------------------------------------------------------------------------*/
/*              VARIABLES THAT DEAL WITH NODE SET PROPERTIES                 */
/*---------------------------------------------------------------------------*/

int   *NS_Cnts = NULL;         /* Global Node Set Counts                     */

/*---------------------------------------------------------------------------*/
/*              VARIABLES THAT DEAL WITH SIDE SET PROPERTIES                 */
/*---------------------------------------------------------------------------*/

int   *SS_Cnts = NULL;         /* Global Side Set Counts                     */

