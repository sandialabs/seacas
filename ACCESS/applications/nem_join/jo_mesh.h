#ifndef _JO_MESH_H
#define _JO_MESH_H

/*****************************************************************************/
/*        PROTOTYPES FOR ELEMENT BLOCK FUNCTIONS (jo_elem_blk.c)             */
/*****************************************************************************/

extern int  put_eb_info(int);
extern int  read_put_connect(int);
extern int  read_put_attrib(int, int);
extern int  put_ns_info(int, int *);
extern int  read_put_node_sets(int, int, int *);
extern int  put_ss_info(int, int *);
extern int  read_put_side_sets(int, int, int *);


#endif /* #ifndef _JO_MESH_H */
