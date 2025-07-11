/*
 * Copyright(C) 2022, 2023, 2024, 2025 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"     // for ex_err, etc
#include "exodusII_int.h" // for EX_FATAL, etc

static int exi_print_type_error(int status, const char *name, const char *type,
                                const char *attribute, int exoid, const char *func)
{
  char errmsg[MAX_ERR_LENGTH];
  snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to store '%s' for %s '%s' in file id %d",
           attribute, type, name, exoid);
  ex_err_fn(exoid, func, errmsg, status);
  return EX_FATAL;
}

static int exi_print_attribute_error(int status, const char *name, const char *attribute,
                                     ex_entity_type entity_type, ex_entity_id entity_id, int exoid,
                                     const char *func)
{
  char errmsg[MAX_ERR_LENGTH];
  snprintf(errmsg, MAX_ERR_LENGTH,
           "ERROR: failed to store field metadata '%s' for field '%s' on %s with id %" PRId64
           " in file id %d",
           attribute, name, ex_name_of_object(entity_type), entity_id, exoid);
  ex_err_fn(exoid, func, errmsg, status);
  return EX_FATAL;
}

static int exi_print_attribute_overflow_error(const char *name, const char *attribute,
                                              ex_entity_type entity_type, ex_entity_id entity_id,
                                              int exoid, const char *func)
{
  char errmsg[MAX_ERR_LENGTH];
  snprintf(errmsg, MAX_ERR_LENGTH,
           "ERROR: field name '%s' is too long to generate a valid attribute name for the field "
           "'%s' attribute on %s with id %" PRId64 " in file id %d",
           name, attribute, ex_name_of_object(entity_type), entity_id, exoid);
  ex_err_fn(exoid, func, errmsg, EX_LONGFIELDNAME);
  return EX_FATAL;
}

int ex_put_multi_field_metadata(int exoid, const ex_field field[], const int count)
{
  exi_persist_redef(exoid, __func__);
  for (int i = 0; i < count; i++) {
    if (field[i].type[0] != EX_SCALAR) {
      int status = ex_put_field_metadata(exoid, field[i]);
      if (status != EX_NOERR) {
        char errmsg[MAX_ERR_LENGTH];
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: failed to store field metadata for field '%s' on %s with id %" PRId64
                 " in file id %d",
                 field[i].name, ex_name_of_object(field[i].entity_type), field[i].entity_id, exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
        exi_persist_leavedef(exoid, __func__);
        return EX_FATAL;
      }
    }
  }
  exi_persist_leavedef(exoid, __func__);
  return EX_NOERR;
}

int ex_put_field_metadata(int exoid, const ex_field field)
{
  /*
   * The attribute name is `Field@{name}@type`
   *
   * Field `nesting` is calculated as size of `type` field
   * The `type` field is a sequence of integers which are the values of the `ex_field_type` enum.
   * NOTE: For backward compatibility, we can only add new entries to this enum at the end.
   *
   * If size of `component_separator` == 0 then the default '_' separator used by all component
   * levels If size of `component_separator` == 1 then that separator used by all component levels
   * Else the size must equal nesting and it specifies a potentially different separator for each
   * level.
   */
#if 0
  fprintf(stderr,
          "ex_put_field_metadata: Field '%s' of type '%s' with separator '%s' on block %lld\n",
          field.name, ex_field_type_enum_to_string(field.type[0]), field.component_separator,
          field.entity_id);
#endif

  exi_persist_redef(exoid, __func__);
  int                status         = 0;
  int                ret            = 0;
  static const char *field_template = "Field@%s@%s";
  char               attribute_name[EX_MAX_NAME + 1];

  ret = snprintf(attribute_name, EX_MAX_NAME + 1, field_template, field.name, "type");
  if (ret < 0 || ret > EX_MAX_NAME + 1) {
    return exi_print_attribute_overflow_error(field.name, "type", field.entity_type,
                                              field.entity_id, exoid, __func__);
  }
  if ((status = ex_put_integer_attribute(exoid, field.entity_type, field.entity_id, attribute_name,
                                         field.nesting, field.type)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_attribute_error(status, field.name, "type", field.entity_type, field.entity_id,
                                     exoid, __func__);
  }

  /* Do not write if empty... */
  if (field.type_name[0] != '\0') {
    ret = snprintf(attribute_name, EX_MAX_NAME + 1, field_template, field.name, "type_name");
    if (ret < 0 || ret > EX_MAX_NAME + 1) {
      return exi_print_attribute_overflow_error(field.name, "type_name", field.entity_type,
                                                field.entity_id, exoid, __func__);
    }
    if ((status = ex_put_text_attribute(exoid, field.entity_type, field.entity_id, attribute_name,
                                        field.type_name)) != EX_NOERR) {
      exi_persist_leavedef(exoid, __func__);
      return exi_print_attribute_error(status, field.name, "type_name", field.entity_type,
                                       field.entity_id, exoid, __func__);
    }
  }

  /* Default component_separator is '_'.  Avoid writing if that is what it is... */
  if (field.component_separator[0] != '_' || field.nesting > 1) {
    ret = snprintf(attribute_name, EX_MAX_NAME + 1, field_template, field.name, "separator");
    if (ret < 0 || ret > EX_MAX_NAME + 1) {
      return exi_print_attribute_overflow_error(field.name, "separator", field.entity_type,
                                                field.entity_id, exoid, __func__);
    }
    if ((status = ex_put_text_attribute(exoid, field.entity_type, field.entity_id, attribute_name,
                                        field.component_separator)) != EX_NOERR) {
      exi_persist_leavedef(exoid, __func__);
      return exi_print_attribute_error(status, field.name, "separator", field.entity_type,
                                       field.entity_id, exoid, __func__);
    }
  }

  bool needs_cardinality = false;
  for (int i = 0; i < field.nesting; i++) {
    if (field.type[i] == EX_FIELD_TYPE_USER_DEFINED || field.type[i] == EX_FIELD_TYPE_SEQUENCE) {
      needs_cardinality = true;
      break;
    }
  }
  if (needs_cardinality) {
    ret = snprintf(attribute_name, EX_MAX_NAME + 1, field_template, field.name, "cardinality");
    if (ret < 0 || ret > EX_MAX_NAME + 1) {
      return exi_print_attribute_overflow_error(field.name, "cardinality", field.entity_type,
                                                field.entity_id, exoid, __func__);
    }
    if ((status = ex_put_integer_attribute(exoid, field.entity_type, field.entity_id,
                                           attribute_name, field.nesting, field.cardinality)) !=
        EX_NOERR) {
      exi_persist_leavedef(exoid, __func__);
      return exi_print_attribute_error(status, field.name, "cardinality", field.entity_type,
                                       field.entity_id, exoid, __func__);
    }
  }

  exi_persist_leavedef(exoid, __func__);
  return EX_NOERR;
}

int exi_put_type_attribute(int exoid, const char *att_root, const char *name, const char *type,
                           ex_type value_type, int cardinality, const void *entry)
{
  int status = EX_NOERR;
  if (entry != NULL) {
    static const char *template = "%s@%s@%s";
    char attribute_name[EX_MAX_NAME + 1];
    snprintf(attribute_name, EX_MAX_NAME + 1, template, att_root, name, type);
    if (value_type == EX_INTEGER) {
      status = ex_put_integer_attribute(exoid, EX_GLOBAL, 0, attribute_name, cardinality, entry);
    }
    else if (value_type == EX_DOUBLE) {
      status = ex_put_double_attribute(exoid, EX_GLOBAL, 0, attribute_name, cardinality, entry);
    }
  }
  return status;
}

int exi_put_basis_attribute(int exoid, const char *basis_name, const char *type, ex_type value_type,
                            int cardinality, const void *basis_entry)
{
  return exi_put_type_attribute(exoid, "Basis", basis_name, type, value_type, cardinality,
                                basis_entry);
}

int exi_put_quad_attribute(int exoid, const char *quad_name, const char *type, ex_type value_type,
                           int cardinality, const void *quad_entry)
{
  return exi_put_type_attribute(exoid, "Quad", quad_name, type, value_type, cardinality,
                                quad_entry);
}

int ex_put_basis(int exoid, const ex_basis basis)
{
  /*
   * typedef struct ex_basis {
   * char    name[EX_MAX_NAME + 1];
   * int     cardinality; -- number of `basis` points == dimension of non-null subc_*, xi, eta, zeta
   * int    *subc_dim;
   * int    *subc_ordinal;
   * int    *subc_dof_ordinal;
   * int    *subc_num_dof;
   * double *xi;
   * double *eta;
   * double *zeta;
   *  } ex_basis;
   */

  exi_persist_redef(exoid, __func__);
  int status;
  if ((status = exi_put_basis_attribute(exoid, basis.name, "cardinality", EX_INTEGER, 1,
                                        &basis.cardinality)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, basis.name, "basis", "cardinality", exoid, __func__);
  }

  if ((status = exi_put_basis_attribute(exoid, basis.name, "subc_dim", EX_INTEGER,
                                        basis.cardinality, basis.subc_dim)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, basis.name, "basis", "subc_dim", exoid, __func__);
  }

  if ((status = exi_put_basis_attribute(exoid, basis.name, "subc_ordinal", EX_INTEGER,
                                        basis.cardinality, basis.subc_ordinal)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, basis.name, "basis", "subc_ordinal", exoid, __func__);
  }

  if ((status = exi_put_basis_attribute(exoid, basis.name, "subc_dof_ordinal", EX_INTEGER,
                                        basis.cardinality, basis.subc_dof_ordinal)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, basis.name, "basis", "subc_dof_ordinal", exoid, __func__);
  }

  if ((status = exi_put_basis_attribute(exoid, basis.name, "subc_num_dof", EX_INTEGER,
                                        basis.cardinality, basis.subc_num_dof)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, basis.name, "basis", "subc_num_dof", exoid, __func__);
  }

  if ((status = exi_put_basis_attribute(exoid, basis.name, "xi", EX_DOUBLE, basis.cardinality,
                                        basis.xi)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, basis.name, "basis", "xi", exoid, __func__);
  }

  if ((status = exi_put_basis_attribute(exoid, basis.name, "eta", EX_DOUBLE, basis.cardinality,
                                        basis.eta)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, basis.name, "basis", "eta", exoid, __func__);
  }

  if ((status = exi_put_basis_attribute(exoid, basis.name, "zeta", EX_DOUBLE, basis.cardinality,
                                        basis.zeta)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, basis.name, "basis", "zeta", exoid, __func__);
  }
  exi_persist_leavedef(exoid, __func__);
  return EX_NOERR;
}

int ex_put_quadrature(int exoid, const ex_quadrature quad)
{
  /*
   * typedef struct ex_quad {
   * char    name[EX_MAX_NAME + 1];
   * int     cardinality; -- number of `quad` points == dimension of non-null subc_*, xi, eta, zeta
   * int    *subc_dim;
   * int    *subc_ordinal;
   * int    *subc_dof_ordinal;
   * int    *subc_num_dof;
   * double *xi;
   * double *eta;
   * double *zeta;
   *  } ex_quad;
   */

  exi_persist_redef(exoid, __func__);
  int status;
  if ((status = exi_put_quad_attribute(exoid, quad.name, "cardinality", EX_INTEGER, 1,
                                       &quad.cardinality)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, quad.name, "quad", "cardinality", exoid, __func__);
  }

  if ((status = exi_put_quad_attribute(exoid, quad.name, "xi", EX_DOUBLE, quad.cardinality,
                                       quad.xi)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, quad.name, "quad", "xi", exoid, __func__);
  }

  if ((status = exi_put_quad_attribute(exoid, quad.name, "eta", EX_DOUBLE, quad.cardinality,
                                       quad.eta)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, quad.name, "quad", "eta", exoid, __func__);
  }

  if ((status = exi_put_quad_attribute(exoid, quad.name, "zeta", EX_DOUBLE, quad.cardinality,
                                       quad.zeta)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, quad.name, "quad", "zeta", exoid, __func__);
  }

  if ((status = exi_put_quad_attribute(exoid, quad.name, "weight", EX_DOUBLE, quad.cardinality,
                                       quad.weight)) != EX_NOERR) {
    exi_persist_leavedef(exoid, __func__);
    return exi_print_type_error(status, quad.name, "quad", "weight", exoid, __func__);
  }
  exi_persist_leavedef(exoid, __func__);
  return EX_NOERR;
}

int ex_put_field_suffices(int exoid, const ex_field field, const char *suffices)
{
  /*
   * For a user-defined field metadata type, output the `cardinality`-count suffices.
   * The suffices are in a single comma-separated string.
   * This call is only valid if the field metadata type is user-defined.
   *
   * Example:  cardinality = 4, type is EX_USER_DEFINED, name is "Species"
   * Suffices = "h2o,gas,ch4,methane"
   * The fields would be "Species_h2o", "Species_gas", "Species_ch4", "Species_methane"
   *
   * Accesses field.entity_type, field.entity_id, field.name, field.type, field.cardinality
   * The attribute name is `Field@{name}@suffices`
   */
  int  status;
  int  ret;
  char errmsg[MAX_ERR_LENGTH];

  char               attribute_name[EX_MAX_NAME + 1];
  static const char *field_template = "Field@%s@%s";

  if (field.type[0] != EX_FIELD_TYPE_USER_DEFINED) {
    snprintf(
        errmsg, MAX_ERR_LENGTH,
        "ERROR: Field '%s' is not of type EX_FIELD_TYPE_USER_DEFINED; cannot specify suffices.",
        field.name);
    ex_err_fn(exoid, __func__, errmsg, EX_BADPARAM);
    return EX_FATAL;
  }

  /* Count the commas in the comma-separated list of suffices.  Must be one less than the field
   * cardinality... */
  int comma_count = 0;
  for (size_t i = 0; i < strlen(suffices); i++) {
    if (suffices[i] == ',') {
      comma_count++;
    }
  }
  if (comma_count + 1 != field.cardinality[0]) {
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: Field '%s' cardinality is %d but there were %d suffices defined.  These must "
             "be equal.",
             field.name, field.cardinality[0], comma_count + 1);
    ex_err_fn(exoid, __func__, errmsg, EX_BADPARAM);
    return EX_FATAL;
  }

  ret = snprintf(attribute_name, EX_MAX_NAME + 1, field_template, field.name, "suffices");
  if (ret < 0 || ret > EX_MAX_NAME + 1) {
    return exi_print_attribute_overflow_error(field.name, "suffices", field.entity_type,
                                              field.entity_id, exoid, __func__);
  }
  if ((status = ex_put_text_attribute(exoid, field.entity_type, field.entity_id, attribute_name,
                                      suffices)) != EX_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to store field suffices for field '%s' on %s with id %" PRId64
             " in file id %d",
             field.name, ex_name_of_object(field.entity_type), field.entity_id, exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    return EX_FATAL;
  }
  return EX_NOERR;
}
