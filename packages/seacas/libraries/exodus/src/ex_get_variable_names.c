/*
 * Copyright(C) 1999-2020 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"     // for ex_err, etc
#include "exodusII_int.h" // for EX_WARN, etc

/*!
\ingroup ResultsData

The function ex_get_variable_names() reads the names of the results
variables from the database. Memory must be allocated for the name
array before this function is invoked.

\return In case of an error, ex_get_variable_names() returns a
negative number; a warning will return a positive number.  Possible
causes of errors include:
  -  data file not properly opened with call to ex_create() or ex_open()
  -  invalid variable type specified.
  -  a warning value is returned if no variables of the specified
     type are stored in the file.

\param[in]  exoid      exodus file ID returned from a previous call to
ex_create() or ex_open().
\param[in]  obj_type   Variable indicating the type of variable which is
described. Use one
                       of the options in the table below.
\param[in]  num_vars   The number of  var_type variables that will be read
                       from the database.
\param[out] var_names  Returned array of pointers to  num_vars variable names.

| ex_entity_type|  description              |
|---------------|---------------------------|
| #EX_GLOBAL     |  Global entity type       |
| #EX_NODAL      |  Nodal entity type        |
| #EX_NODE_SET   |  Node Set entity type     |
| #EX_EDGE_BLOCK |  Edge Block entity type   |
| #EX_EDGE_SET   |  Edge Set entity type     |
| #EX_FACE_BLOCK |  Face Block entity type   |
| #EX_FACE_SET   |  Face Set entity type     |
| #EX_ELEM_BLOCK |  Element Block entity type|
| #EX_ELEM_SET   |  Element Set entity type  |
| #EX_SIDE_SET   |  Side Set entity type     |

As an example, the following code segment will read the names of the
nodal variables stored in the data file:

~~~{.c}
int error, exoid, num_nod_vars;
char *var_names[10];

\comment{read nodal variables parameters and names}
error = ex_get_variable_param(exoid, EX_NODAL, &num_nod_vars);
int name_len = ex_inquire_int(exoid, EX_INT_MAX_READ_NAME_LENGTH);
for (i=0; i < num_nod_vars; i++) {
   var_names[i] = (char *) calloc ((name_len+1), sizeof(char));
}
error = ex_get_variable_names(exoid, EX_NODAL, num_nod_vars, var_names);
~~~

*/

int ex_get_variable_names(int exoid, ex_entity_type obj_type, int num_vars, char *var_names[])
{
  int         varid, status;
  char        errmsg[MAX_ERR_LENGTH];
  const char *vvarname;

  EX_FUNC_ENTER();
  if (exi_check_valid_file_id(exoid, __func__) == EX_FATAL) {
    EX_FUNC_LEAVE(EX_FATAL);
  }

  switch (obj_type) {
  case EX_NODAL: vvarname = VAR_NAME_NOD_VAR; break;
  case EX_ASSEMBLY: vvarname = VAR_NAME_ASSEMBLY_VAR; break;
  case EX_BLOB: vvarname = VAR_NAME_BLOB_VAR; break;
  case EX_EDGE_BLOCK: vvarname = VAR_NAME_EDG_VAR; break;
  case EX_FACE_BLOCK: vvarname = VAR_NAME_FAC_VAR; break;
  case EX_ELEM_BLOCK: vvarname = VAR_NAME_ELE_VAR; break;
  case EX_NODE_SET: vvarname = VAR_NAME_NSET_VAR; break;
  case EX_EDGE_SET: vvarname = VAR_NAME_ESET_VAR; break;
  case EX_FACE_SET: vvarname = VAR_NAME_FSET_VAR; break;
  case EX_SIDE_SET: vvarname = VAR_NAME_SSET_VAR; break;
  case EX_ELEM_SET: vvarname = VAR_NAME_ELSET_VAR; break;
  case EX_GLOBAL: vvarname = VAR_NAME_GLO_VAR; break;
  default:
    snprintf(errmsg, MAX_ERR_LENGTH, "Warning: invalid variable type %d requested from file id %d",
             obj_type, exoid);
    ex_err_fn(exoid, __func__, errmsg, EX_BADPARAM);
    EX_FUNC_LEAVE(EX_WARN);
  }

  /* inquire previously defined variables  */
  if ((status = nc_inq_varid(exoid, vvarname, &varid)) != EX_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH, "Warning: no %s variables names stored in file id %d",
             ex_name_of_object(obj_type), exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    EX_FUNC_LEAVE(EX_WARN);
  }

  /* read the variable names */
  status = exi_get_names(exoid, varid, num_vars, var_names, obj_type, __func__);
  if (status != EX_NOERR) {
    EX_FUNC_LEAVE(EX_FATAL);
  }
  EX_FUNC_LEAVE(EX_NOERR);
}
