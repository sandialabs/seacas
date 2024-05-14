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

static const char *exi_get_metadata_attribute(char *name, const char *prefix, int pre_len)
{
  /*
   * Each field or basis attribute metadata attribute consists of 2 or more attributes.
   * Return the string corresponding to {type} in an attribute of the form "Field@{name}@{type}"
   * or "Basis@{name}@{type}" or "Quad@{name}@{type}".
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

static const char *exi_get_attribute_metadata_name(char *attrib, int offset)
{
  /*
   * PRECONDITION: `attrib` is a basis or field metadata attribute of the form
   * "Basis@{name}@{type}" or "Field@{name}@{type}" `offset` is the length
   * of `Basis@` or `Field@` or `Quad@`
   *
   * Returns the `{name}` portion in `name`
   */
  static char name[EX_MAX_NAME + 1];
  memset(name, '\0', EX_MAX_NAME + 1);
  for (int i = 0; attrib[i + offset] != '@'; i++) {
    name[i] = attrib[i + offset];
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
  int count = 0;
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
      const char *fld_name = exi_get_attribute_metadata_name(attr_name, 6);

      /* If this is the first time we have seen this `fld_name`, then increment count and
       * store the name */
      int found = -1;
      int which = 0;
      for (int ii = 0; ii < count; ii++) {
        if (strcmp(field[ii].name, fld_name) == 0) {
          found = ii;
          which = ii;
          break;
        }
      }

      if (found == -1) {
        which = count;
        strcpy(field[count].name, fld_name);
        count++;
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
        status = nc_get_att_int(exoid, varid, attr_name, (int *)field[which].type);
        if (field[which].nesting == 0) {
          field[which].nesting = val_count;
        }
      }
      else if (strcmp(fld_type, "separator") == 0) {
        status = nc_get_att_text(exoid, varid, attr_name, field[which].component_separator);
      }
      else if (strcmp(fld_type, "cardinality") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, field[which].cardinality);
        if (field[which].nesting == 0) {
          field[which].nesting = val_count;
        }
      }
      else if (strcmp(fld_type, "type_name") == 0) {
        status = nc_get_att_text(exoid, varid, attr_name, field[which].type_name);
      }
      else if (strcmp(fld_type, "suffices") == 0) {
        status = nc_get_att_text(exoid, varid, attr_name, field[which].suffices);
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

int exi_get_metadata_count(int exoid, const char *which)
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

  /* Get names of each attribute and see if it is a `which` metadata name */
  size_t att_len = strlen(which);
  int    count   = 0;
  for (int i = 0; i < att_count; i++) {
    char name[EX_MAX_NAME + 1];
    int  status;
    if ((status = nc_inq_attname(exoid, varid, i, name)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get attribute named %s", name);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }
    const char *type = exi_get_metadata_attribute(name, which, att_len);
    if (type != NULL && strcmp("cardinality", type) == 0) {
      count++;
    }
  }
  EX_FUNC_LEAVE(count);
}

int ex_get_basis_metadata_count(int exoid) { return exi_get_metadata_count(exoid, "Basis@"); }

int ex_get_quadrature_metadata_count(int exoid) { return exi_get_metadata_count(exoid, "Quad@"); }

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
  int count = 0;
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
      const char *basis_name = exi_get_attribute_metadata_name(attr_name, 6);

      /* If this is the first time we have seen this `quadrature_name`, then increment count and
       * store the name */
      int found = -1;
      int which = 0;
      for (int i = 0; i < count; i++) {
        if (strcmp(basis[i].name, basis_name) == 0) {
          found = i;
          which = i;
          break;
        }
      }

      if (found == -1) {
        which = count;
        strcpy(basis[count].name, basis_name);
        count++;
      }

      if (strcmp(basis_type, "cardinality") == 0) {
        int cardinality = 0;
        status          = nc_get_att_int(exoid, varid, attr_name, &cardinality);
        if (basis[which].cardinality > 0 && basis[which].cardinality != cardinality) {
          char errmsg[MAX_ERR_LENGTH];
          snprintf(
              errmsg, MAX_ERR_LENGTH,
              "ERROR: Basis cardinality on the database is %d, but the value passed in the basis "
              "struct is %d.\n\tThis indicates that"
              " not enough memory has been allocated to store the other basis fields.",
              cardinality, basis[which].cardinality);
          ex_err_fn(exoid, __func__, errmsg, EX_BADPARAM);
          EX_FUNC_LEAVE(EX_FATAL);
        }
        basis[which].cardinality = cardinality;
      }

      /* Now, for each non-NULL parameter of `basis`, query the data... */
      else if (basis[which].subc_dim != NULL && strcmp(basis_type, "subc_dim") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, basis[which].subc_dim);
      }
      else if (status == NC_NOERR && basis[which].subc_ordinal != NULL &&
               strcmp(basis_type, "subc_ordinal") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, basis[which].subc_ordinal);
      }
      else if (status == NC_NOERR && basis[which].subc_dof_ordinal != NULL &&
               strcmp(basis_type, "subc_dof_ordinal") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, basis[which].subc_dof_ordinal);
      }
      else if (status == NC_NOERR && basis[which].subc_num_dof != NULL &&
               strcmp(basis_type, "subc_num_dof") == 0) {
        status = nc_get_att_int(exoid, varid, attr_name, basis[which].subc_num_dof);
      }
      else if (status == NC_NOERR && basis[which].xi != NULL && strcmp(basis_type, "xi") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, basis[which].xi);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < basis[which].cardinality; i++) {
            basis[which].eta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      else if (status == NC_NOERR && basis[which].eta != NULL && strcmp(basis_type, "eta") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, basis[which].eta);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < basis[which].cardinality; i++) {
            basis[which].eta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      else if (status == NC_NOERR && basis[which].zeta != NULL && strcmp(basis_type, "zeta") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, basis[which].zeta);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < basis[which].cardinality; i++) {
            basis[which].zeta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      // NOTE: Do not put an else since will fall through if the
      // arrays are NULL even though basis_type is valid.

      if (status != NC_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to read field Basis %s metadata",
                 basis[which].name);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }
    }
  }
  EX_FUNC_LEAVE(EX_NOERR);
}

int ex_get_quadrature_metadata(int exoid, ex_quadrature *quadrature)
{
  /*
   * -- If this function is called an there is no quadrature metadata on the
   *    entity, it will return EX_NOTFOUND; otherwise it will populate
   *    (portions of) `quadrature` and return EX_NOERR.
   *
   * -- name and cardinality will be populated if empty at time of call.
   * -- other fields will be populated if they are non-null.
   * -- so, can call first time with empty `quadrature' with 'NULL' on all pointer members.
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

  /* Iterate through each Quadrature metadata attribute and populate `quadrature` */
  int count = 0;
  for (int att = 0; att < att_count; att++) {
    char attr_name[EX_MAX_NAME + 1];
    int  status;
    if ((status = nc_inq_attname(exoid, varid, att, attr_name)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get attribute named %s", attr_name);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }

    if (strncmp("Quad@", attr_name, 5) != 0) {
      continue;
    }

    const char *quadrature_type = exi_get_metadata_attribute(attr_name, "Quad@", 5);
    if (quadrature_type != NULL) {
      /* Get the quadrature name.  We know that the `name` is of the form "Quad@{name}@{item}" */
      const char *quadrature_name = exi_get_attribute_metadata_name(attr_name, 5);

      /* If this is the first time we have seen this `quadrature_name`, then increment count and
       * store the name */
      int found = -1;
      int which = 0;
      for (int i = 0; i < count; i++) {
        if (strcmp(quadrature[i].name, quadrature_name) == 0) {
          found = i;
          which = i;
          break;
        }
      }

      if (found == -1) {
        which = count;
        strcpy(quadrature[count].name, quadrature_name);
        count++;
      }

      if (strcmp(quadrature_type, "cardinality") == 0) {
        int cardinality = 0;
        status          = nc_get_att_int(exoid, varid, attr_name, &cardinality);
        if (quadrature[which].cardinality > 0 && quadrature[which].cardinality != cardinality) {
          char errmsg[MAX_ERR_LENGTH];
          snprintf(
              errmsg, MAX_ERR_LENGTH,
              "ERROR: Quadrature cardinality for %s on the database is %d, but the value passed in "
              "the quadrature "
              "struct is %d.\n\tThis indicates that"
              " not enough memory has been allocated to store the other quadrature fields.",
              quadrature[which].name, cardinality, quadrature[which].cardinality);
          ex_err_fn(exoid, __func__, errmsg, EX_BADPARAM);
          EX_FUNC_LEAVE(EX_FATAL);
        }
        quadrature[which].cardinality = cardinality;
      }

      /* Now, for each non-NULL parameter of `quadrature`, query the data... */
      else if (status == NC_NOERR && quadrature[which].xi != NULL &&
               strcmp(quadrature_type, "xi") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, quadrature[which].xi);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < quadrature[which].cardinality; i++) {
            quadrature[which].eta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      else if (status == NC_NOERR && quadrature[which].eta != NULL &&
               strcmp(quadrature_type, "eta") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, quadrature[which].eta);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < quadrature[which].cardinality; i++) {
            quadrature[which].eta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      else if (status == NC_NOERR && quadrature[which].zeta != NULL &&
               strcmp(quadrature_type, "zeta") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, quadrature[which].zeta);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < quadrature[which].cardinality; i++) {
            quadrature[which].zeta[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      else if (status == NC_NOERR && quadrature[which].weight != NULL &&
               strcmp(quadrature_type, "weight") == 0) {
        status = nc_get_att_double(exoid, varid, attr_name, quadrature[which].weight);
        if (status == NC_ENOTATT) {
          for (int i = 0; i < quadrature[which].cardinality; i++) {
            quadrature[which].weight[i] = 0.0;
          }
          status = NC_NOERR;
        }
      }
      // NOTE: Do not put an else since will fall through if the
      // arrays are NULL even though quadrature_type is valid.

      if (status != NC_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to read field Quadrature %s metadata",
                 quadrature[which].name);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }
    }
  }
  EX_FUNC_LEAVE(EX_NOERR);
}
