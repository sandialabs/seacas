/*
 * Copyright(C) 1999-2022 National Technology & Engineering Solutions
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

static void ex__field_initialize(ex_field *field)
{
  field->name[0] = '\0';
  field->nesting = 0;
  for (int i = 0; i < EX_MAX_FIELD_NESTING; i++) {
    field->type[i]                = EX_FIELD_TYPE_INVALID;
    field->cardinality[i]         = 0;
    field->component_separator[i] = '_';
  }
  field->component_separator[EX_MAX_FIELD_NESTING] = '\0';
}

static bool ex__is_field_metadata_attribute(char *name)
{
  /*
   * Each field attribute metadata attribute consists of 2 or more attributes.
   * A mandatory attribute is of the form "Field@{name}@type".  We can therefore
   * check whether the name ends with "@type" and increment count if found...
   */

  if (strncmp(name, "Field@", 6) == 0) {
    /* Return true if the passed in string ends with "@type" */
    char *suffix = strrchr(name, '@');
    if (suffix != NULL) {
      return (strcmp(suffix, "@type") == 0);
    }
  }
  return (false);
}

static const char *ex__get_field_metadata_name(char *attrib)
{
  /*
   * PRECONDITION: `attrib` is a field metadata attribute of the form
   * "Field@{name}@{type}"
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

static const char *ex__get_field_metadata_type(char *attrib)
{
  /*
   * PRECONDITION: `attrib` is a field metadata attribute of the form
   * "Field@{name}@{type}"
   *
   * Returns pointer to `{type}` portion
   */
  char *begin = strrchr(attrib, '@');
  assert(begin != NULL);
  return ++begin;
}

static int ex__get_attribute_count(int exoid, ex_entity_type obj_type, ex_entity_id id, int *varid)
{
  int att_count = 0;
  int status;

  if (obj_type == EX_GLOBAL) {
    *varid = NC_GLOBAL;

    if ((status = nc_inq(exoid, NULL, NULL, &att_count, NULL)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get GLOBAL attribute count in file id %d",
               exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      return EX_FATAL;
    }
  }
  else {
    *varid = ex__get_varid(exoid, obj_type, id);
    if (*varid <= 0) {
      /* Error message handled in ex__get_varid */
      return 0;
    }

    if ((status = nc_inq_var(exoid, *varid, NULL, NULL, NULL, NULL, &att_count)) != NC_NOERR) {
      char errmsg[MAX_ERR_LENGTH];
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to get attribute count on %s with id %" PRId64 " in file id %d",
               ex_name_of_object(obj_type), id, exoid);
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
  int att_count = ex__get_attribute_count(exoid, obj_type, id, &varid);
  if (att_count < 0) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: Negative attribute count (%d) on %s with id %" PRId64 " in file id %d",
             att_count, ex_name_of_object(obj_type), id, exoid);
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
               "ERROR: failed to get attribute named %s on %s with id %" PRId64 " in file id %d",
               name, ex_name_of_object(obj_type), id, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }
    if (ex__is_field_metadata_attribute(name)) {
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
  int att_count = ex__get_attribute_count(exoid, field->entity_type, field->entity_id, &varid);
  if (att_count < 0) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: Negative attribute count (%d) on %s with id %" PRId64 " in file id %d",
             att_count, ex_name_of_object(field->entity_type), field->entity_id, exoid);
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
               "ERROR: failed to get attribute named %s on %s with id %" PRId64 " in file id %d",
               attr_name, ex_name_of_object(field->entity_type), field->entity_id, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }

    if (ex__is_field_metadata_attribute(attr_name)) {
      /* Get the field name.  We know that the `name` is of the form "Field@{name}@{item}" */
      const char *fld_name = ex__get_field_metadata_name(attr_name);

      /* If this is the first time we have seen this `fld_name`, then increment count and store the
       * name */
      if (count < 0 || strcmp(field[count].name, fld_name) != 0) {
        count++;
        ex__field_initialize(&field[count]);
        strcpy(field[count].name, fld_name);
      }

      nc_type type;      /* integer, double, character, ... */
      size_t  val_count; /* how many `type` values */
      if ((status = nc_inq_att(exoid, varid, attr_name, &type, &val_count)) != NC_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: failed to get parameters for attribute named %s on %s with id %" PRId64
                 " in file id %d",
                 attr_name, ex_name_of_object(field->entity_type), field->entity_id, exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }

      const char *fld_type = ex__get_field_metadata_type(attr_name);

      if (strcmp(fld_type, "type") == 0) {
        status = nc_get_att(exoid, varid, attr_name, field[count].type);
        if (field[count].nesting == 0) {
          field[count].nesting = val_count;
        }
      }
      else if (strcmp(fld_type, "separator") == 0) {
        status = nc_get_att(exoid, varid, attr_name, field[count].component_separator);
        if (field[count].nesting == 0) {
          field[count].nesting = val_count;
        }
      }
      else if (strcmp(fld_type, "cardinality") == 0) {
        status = nc_get_att(exoid, varid, attr_name, field[count].cardinality);
        if (field[count].nesting == 0) {
          field[count].nesting = val_count;
        }
      }
      else {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(
            errmsg, MAX_ERR_LENGTH,
            "ERROR: Invalid field metadata attribute type %s on field %s on %s with id %" PRId64
            " in file id %d",
            fld_type, fld_name, ex_name_of_object(field->entity_type), field->entity_id, exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }
      if (status != NC_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: failed to read field metadata attribute type %s on field %s on %s with id "
                 "%" PRId64 " in file id %d",
                 fld_type, fld_name, ex_name_of_object(field->entity_type), field->entity_id,
                 exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }
    }
  }
  EX_FUNC_LEAVE(EX_NOERR);
}

int ex__get_basis_name(int exoid, ex_entity_type entity_type, ex_entity_id entity_id, char *name)
{
  int varid;
  if (entity_type == EX_GLOBAL) {
    varid = NC_GLOBAL;
  }
  else {
    varid = ex__get_varid(exoid, entity_type, entity_id);
    if (varid <= 0) {
      /* Error message handled in ex__get_varid */
      EX_FUNC_LEAVE(varid);
    }
  }

  int status;
  if ((status = nc_get_att(exoid, varid, "Basis@name", name)) != NC_NOERR) {
    char errmsg[MAX_ERR_LENGTH];
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to read basis name on %s with id %" PRId64 " in file id %d",
             ex_name_of_object(entity_type), entity_id, exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    return EX_FATAL;
  }
  return EX_NOERR;
}

int ex_get_basis_metadata(int exoid, ex_entity_type entity_type, ex_entity_id entity_id,
                          ex_basis *basis)
{
  /*
   * -- There is at most 1 basis definition on an entity.
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

  int status;
  if ((status = ex__get_basis_name(exoid, entity_type, entity_id, basis->name)) != EX_NOERR) {
    /* Error message printed in `ex__get_basis_name` */
    return EX_NOTFOUND;
  }
  fprintf(stderr, "Basis is named '%s'\n", basis->name);
  return EX_NOERR;
}
