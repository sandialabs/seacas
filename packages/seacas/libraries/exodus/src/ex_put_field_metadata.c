/*
 * Copyright(C) 2022 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"     // for ex_err, etc
#include "exodusII_int.h" // for EX_FATAL, etc

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
  char       attribute_name[NC_MAX_NAME + 1];
  static int count = 0;
  count++;
  fprintf(stderr, "Field '%s' of type '%s' with separator '%s' on block %lld\n", field.name,
          ex_field_type_enum_to_string(field.type[0]), field.component_separator, field.entity_id);

  static char *field_template = "Field@%s@%s";
  sprintf(attribute_name, field_template, field.name, "type");
  ex_put_integer_attribute(exoid, field.entity_type, field.entity_id, attribute_name, field.nesting,
                           field.type);

  sprintf(attribute_name, field_template, field.name, "separator");
  ex_put_text_attribute(exoid, field.entity_type, field.entity_id, attribute_name,
                        field.component_separator);

  bool needs_cardinality = false;
  for (int i = 0; i < field.nesting; i++) {
    if (field.type[i] == EX_FIELD_TYPE_USER_DEFINED || field.type[i] == EX_FIELD_TYPE_SEQUENCE) {
      needs_cardinality = true;
      break;
    }
  }
  if (needs_cardinality) {
    sprintf(attribute_name, field_template, field.name, "cardinality");
    ex_put_integer_attribute(exoid, field.entity_type, field.entity_id, attribute_name,
                             field.nesting, field.cardinality);
  }

  return EX_NOERR;
}

int ex_put_basis_metadata(int exoid, const ex_field field) { return EX_FATAL; }

int ex_put_quadrature_metadata(int exoid, const ex_field field) { return EX_FATAL; }

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
  char         attribute_name[NC_MAX_NAME + 1];
  static char *field_template = "Field@%s@%s";

  if (field.type[0] != EX_FIELD_TYPE_USER_DEFINED) {
    return EX_FATAL;
  }
  sprintf(attribute_name, field_template, field.name, "suffices");
  ex_put_text_attribute(exoid, field.entity_type, field.entity_id, attribute_name, suffices);

  return EX_NOERR;
}
