/*
 * Copyright(C) 1999-2024 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"     // for ex_err, etc
#include "exodusII_int.h" // for EX_FATAL, etc
#include <assert.h>
#include <stdbool.h>

/*! Get number of attributes defined on the specified entity
   type/entity id (EX_ASSEMBLY, 100).

   Filters out "internal" or "special" attributes defined by the
   NetCDF library or used by the exodus library internally.
*/

static void exi_field_initialize(ex_field *field)
{
  field->name[0]      = '\0';
  field->nesting      = 0;
  field->type_name[0] = '\0';
  for (int i = 0; i < EX_MAX_FIELD_NESTING; i++) {
    field->type[i]                = EX_FIELD_TYPE_INVALID;
    field->cardinality[i]         = 0;
    field->component_separator[i] = '_';
  }
  field->suffices[0] = '\0';
}

static const char *exi_get_metadata_attribute(char *name, char *prefix, int pre_len)
{
  /*
   * Each field or basis attribute metadata attribute consists of 2 or more attributes.
   * Return the string corresponding to {type} in an attribute of the form "Field@{name}@{type}"
   * or "Basis@{name}@{type}".
   */

  if (strncmp(name, prefix, pre_len) == 0) {
    /* Return the suffix (if any) following the last "@" */
    char *suffix = strrchr(name, '@');
    if (suffix != NULL) {
      suffix++;
      return suffix;
    }
  }
  return NULL;
}

static const char *exi_get_attribute_metadata_name(char *attrib)
{
  /*
   * PRECONDITION: `attrib` is a basis or field metadata attribute of the form
   * "Basis@{name}@{type}" or "Field@{name}@{type}" (NOTE: 'basis' and 'field' are same length)
   *
   * Returns the `{name}` portion in `name`
   */
  static char name[EX_MAX_NAME + 1];
  memset(name, '\0', EX_MAX_NAME + 1);
  for (int i = 0; attrib[i + 6] != '@'; i++) {
    name[i] = attrib[i + 6];
  }
  return name;
}

static int exi_get_attribute_count(int exoid, ex_entity_type obj_type, ex_entity_id id, int *varid)
{
  int att_count = 0;
  int status;

  if (obj_type == EX_GLOBAL) {
    *varid = NC_GLOBAL;

    if ((status = nc_inq(exoid, NULL, NULL, &att_count, NULL)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get GLOBAL attribute count");
      ex_err_fn(exoid, __func__, errmsg, status);
      return EX_FATAL;
    }
  }
  else {
    *varid = exi_get_varid(exoid, obj_type, id);
    if (*varid <= 0) {
      /* Error message handled in exi_get_varid */
      return 0;
    }

    if ((status = nc_inq_var(exoid, *varid, NULL, NULL, NULL, NULL, &att_count)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to get attribute count on %s with id %" PRId64,
               ex_name_of_object(obj_type), id);
      ex_err_fn(exoid, __func__, errmsg, status);
      return EX_FATAL;
    }
  }
  return att_count;
}

int ex_get_field_metadata_count(int exoid, ex_entity_type obj_type, ex_entity_id id)
{
  EX_FUNC_ENTER();

  int varid;
  int att_count = exi_get_attribute_count(exoid, obj_type, id, &varid);
  if (att_count < 0) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: Negative attribute count (%d) on %s with id %" PRId64,
             att_count, ex_name_of_object(obj_type), id);
    ex_err_fn(exoid, __func__, errmsg, EX_INTERNAL);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  /* Get names of each attribute and see if it is a 'Field metadata' name */
  int count = 0;
  for (int i = 0; i < att_count; i++) {
    char name[EX_MAX_NAME + 1];
    int  status;
    if ((status = nc_inq_attname(exoid, varid, i, name)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to get attribute named %s on %s with id %" PRId64, name,
               ex_name_of_object(obj_type), id);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }
    const char *type = exi_get_metadata_attribute(name, "Field@", 6);
    if (type != NULL && strcmp("type", type) == 0) {
      count++;
    }
  }
  EX_FUNC_LEAVE(count);
}

/*! Get the values for the specified attribute. */
int ex_get_field_metadata(int exoid, ex_field *field)
{
  EX_FUNC_ENTER();

  int varid;
  int att_count = exi_get_attribute_count(exoid, field->entity_type, field->entity_id, &varid);
  if (att_count < 0) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: Negative attribute count (%d) on %s with id %" PRId64,
             att_count, ex_name_of_object(field->entity_type), field->entity_id);
    ex_err_fn(exoid, __func__, errmsg, EX_INTERNAL);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  /* Iterate through each Field metadata field and populate `field` */
  int count = -1;
  for (int i = 0; i < att_count; i++) {
    char attr_name[EX_MAX_NAME + 1];
    int  status;
    if ((status = nc_inq_attname(exoid, varid, i, attr_name)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to get attribute named %s on %s with id %" PRId64, attr_name,
               ex_name_of_object(field->entity_type), field->entity_id);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }

    const char *fld_type = exi_get_metadata_attribute(attr_name, "Field@", 6);
    if (fld_type != NULL) {
      /* Get the field name.  We know that the `name` is of the form "Field@{name}@{item}" */
      const char *fld_name = exi_get_attribute_metadata_name(attr_name);

      /* If this is the first time we have seen this `fld_name`, then increment count and store the
       * name */
      if (count < 0 || strcmp(field[count].name, fld_name) != 0) {
        count++;
        exi_field_initialize(&field[count]);
        strcpy(field[count].name, fld_name);
      }

      nc_type type;      /* integer, double, character, ... */
      size_t  val_count; /* how many `type` values */
      if ((status = nc_inq_att(exoid, varid, attr_name, &type, &val_count)) != NC_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: failed to get parameters for attribute named %s on %s with id %" PRId64,
                 attr_name, ex_name_of_object(field->entity_type), field->entity_id);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }

      if (strcmp(fld_type, "type") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, (int *)field[count].type);
        if (field[count].nesting == 0) {
          field[count].nesting = val_count;
        }
        if (field[count].type[0] == EX_FIELD_TYPE_USER_DEFINED && field[count].nesting != 1) {
          char errmsg[MAX_ERR_LENGTH];
          snprintf(errmsg, MAX_ERR_LENGTH,
                   "ERROR: Invalid nesting for field %s on %s with id %" PRId64
                   ". Must be 1 for user-defined field type.\n",
                   field[count].name, ex_name_of_object(field->entity_type), field->entity_id);
          ex_err_fn(exoid, __func__, errmsg, status);
          EX_FUNC_LEAVE(EX_FATAL);
        }
      }
      else if (strcmp(fld_type, "separator") == 0) {
        status = nc_get_att_text(exoid, varid, attr_name, field[count].component_separator);
        if (field[count].nesting == 0) {
          field[count].nesting = val_count;
        }
      }
      else if (strcmp(fld_type, "cardinality") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, field[count].cardinality);
        if (field[count].nesting == 0) {
          field[count].nesting = val_count;
        }
      }
      else if (strcmp(fld_type, "type_name") == 0) {
        status = nc_get_att_text(exoid, varid, attr_name, field[count].type_name);
      }
      else if (strcmp(fld_type, "suffices") == 0) {
        status = nc_get_att_text(exoid, varid, attr_name, field[count].suffices);
      }
      else {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(
            errmsg, MAX_ERR_LENGTH,
            "ERROR: Invalid field metadata attribute type %s on field %s on %s with id %" PRId64,
            fld_type, fld_name, ex_name_of_object(field->entity_type), field->entity_id);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }
      if (status != NC_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: failed to read field metadata attribute type %s on field %s on %s with id "
                 "%" PRId64,
                 fld_type, fld_name, ex_name_of_object(field->entity_type), field->entity_id);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }
    }
  }
  EX_FUNC_LEAVE(EX_NOERR);
}

int ex_get_basis_metadata_count(int exoid)
{
  EX_FUNC_ENTER();

  int varid;
  int att_count = exi_get_attribute_count(exoid, EX_GLOBAL, 0, &varid);
  if (att_count < 0) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: Negative attribute count (%d)", att_count);
    ex_err_fn(exoid, __func__, errmsg, EX_INTERNAL);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  /* Get names of each attribute and see if it is a 'Basis metadata' name */
  int count = 0;
  for (int i = 0; i < att_count; i++) {
    char name[EX_MAX_NAME + 1];
    int  status;
    if ((status = nc_inq_attname(exoid, varid, i, name)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get attribute named %s", name);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }
    const char *type = exi_get_metadata_attribute(name, "Basis@", 6);
    if (type != NULL && strcmp("cardinality", type) == 0) {
      count++;
    }
  }
  EX_FUNC_LEAVE(count);
}

int ex_get_basis_metadata(int exoid, ex_basis *basis)
{
  /*
   * -- If this function is called an there is no basis metadata on the
   *    entity, it will return EX_NOTFOUND; otherwise it will populate
   *    (portions of) `basis` and return EX_NOERR.
   *
   * -- name and cardinality will be populated if empty at time of call.
   * -- other fields will be populated if they are non-null.
   * -- so, can call first time with empty `basis' with 'NULL' on all pointer members.
   *    - `name` and `cardinality` will be populated
   *    - Then malloc/calloc the pointer members that you want populated
   *    - then call again.  `name` will not be populated if non-empty.
   *    - `cardinality` will be checked and must be >= to value on database.
   *      - assumed to be the size of the pointer member allocated space.
   *      - if > value on database, will be set to value on database.
   *      - if < value on database, error unless 0.
   *    - pointer members will be populated if non-NULL.
   */

  EX_FUNC_ENTER();

  int varid;
  int att_count = exi_get_attribute_count(exoid, EX_GLOBAL, 0, &varid);
  if (att_count < 0) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: Negative attribute count (%d)", att_count);
    ex_err_fn(exoid, __func__, errmsg, EX_INTERNAL);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  /* Iterate through each Basis metadata attribute and populate `basis` */
  int count = -1;
  for (int att = 0; att < att_count; att++) {
    char attr_name[EX_MAX_NAME + 1];
    int  status;
    if ((status = nc_inq_attname(exoid, varid, att, attr_name)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get attribute named %s", attr_name);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }

    if (strncmp("Basis@", attr_name, 6) != 0) {
      continue;
    }

    const char *basis_type = exi_get_metadata_attribute(attr_name, "Basis@", 6);
    if (basis_type != NULL) {
      /* Get the basis name.  We know that the `name` is of the form "Basis@{name}@{item}" */
      const char *basis_name = exi_get_attribute_metadata_name(attr_name);

      /* If this is the first time we have seen this `basis_name`, then increment count and store
       * the name */
      if (count < 0 || strcmp(basis[count].name, basis_name) != 0) {
        count++;
        strcpy(basis[count].name, basis_name);
      }

      if (strcmp(basis_type, "cardinality") == 0) {
        int cardinality = 0;
        status          = nc_get_att_int(exoid, varid, attr_name, &cardinality);
        if (basis[count].cardinality > 0 && basis[count].cardinality != cardinality) {
          char errmsg[MAX_ERR_LENGTH];
          snprintf(
              errmsg, MAX_ERR_LENGTH,
              "ERROR: Basis cardinality on the database is %d, but the value passed in the basis "
              "struct is %d.\n\tThis indicates that"
              " not enough memory has been allocated to store the other basis fields.",
              cardinality, basis[count].cardinality);
          ex_err_fn(exoid, __func__, errmsg, EX_BADPARAM);
          EX_FUNC_LEAVE(EX_FATAL);
        }
        basis[count].cardinality = cardinality;
      }

      /* Now, for each non-NULL parameter of `basis`, query the data... */
      else if (basis[count].subc_dim != NULL && strcmp(basis_type, "subc_dim") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, basis[count].subc_dim);
      }
      else if (status == NC_NOERR && basis[count].subc_ordinal != NULL &&
               strcmp(basis_type, "subc_ordinal") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, basis[count].subc_ordinal);
      }
      else if (status == NC_NOERR && basis[count].subc_dof_ordinal != NULL &&
               strcmp(basis_type, "subc_dof_ordinal") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, basis[count].subc_dof_ordinal);
      }
      else if (status == NC_NOERR && basis[count].subc_num_dof != NULL &&
               strcmp(basis_type, "subc_num_dof") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, basis[count].subc_num_dof);
      }
      else if (status == NC_NOERR && basis[count].xi != NULL && strcmp(basis_type, "xi") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, basis[count].xi);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < basis[count].cardinality; i++) {
            basis[count].eta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      else if (status == NC_NOERR && basis[count].eta != NULL && strcmp(basis_type, "eta") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, basis[count].eta);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < basis[count].cardinality; i++) {
            basis[count].eta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      else if (status == NC_NOERR && basis[count].zeta != NULL && strcmp(basis_type, "zeta") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, basis[count].zeta);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < basis[count].cardinality; i++) {
            basis[count].zeta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      // NOTE: Do not put an else since will fall through if the
      // arrays are NULL even though basis_type is valid.

      if (status != NC_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to read field Basis %s metadata",
                 basis[count].name);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }
    }
  }
  EX_FUNC_LEAVE(EX_NOERR);
}
