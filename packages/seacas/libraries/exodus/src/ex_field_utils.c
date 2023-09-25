/*
 * Copyright(C) 1999-2023 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"     // for ex_err, etc
#include "exodusII_int.h" // for EX_FATAL, etc
#include <assert.h>
#include <math.h>

#define SIZE(X) sizeof(X) / sizeof(X[0])

static int number_width(const size_t number)
{
  if (number == 0) {
    return 1;
  }
  int width = (int)floor(log10(number)) + 1;
  return width;
}

static void verify_valid_component(int component, size_t cardinality, size_t suffix_size)
{
  assert(cardinality == suffix_size);
  assert(component - 1 < suffix_size);
}

const char *ex_component_field_namefix(ex_field *field, int component[EX_MAX_FIELD_NESTING])
{
  // Return the name of the field corresponding to the specified 1-based component(s)
  static char field_name[EX_MAX_NAME];
  const char *suffices[EX_MAX_FIELD_NESTING] = {NULL};
  for (int i = 0; i < field->nesting; i++) {
    suffices[i] = ex_field_component_suffix(field, i, component[i]);
  }

  switch (field->nesting) {
  case 1:
    sprintf(field_name, "%s%c%s", field->name, field->component_separator[0], suffices[0]);
    break;
  case 2:
    sprintf(field_name, "%s%c%s%c%s", field->name, field->component_separator[0], suffices[0],
            field->component_separator[1], suffices[1]);
    break;
  case 3:
    sprintf(field_name, "%s%c%s%c%s%c%s", field->name, field->component_separator[0], suffices[0],
            field->component_separator[1], suffices[1], field->component_separator[2], suffices[2]);
    break;
  case 4:
    sprintf(field_name, "%s%c%s%c%s%c%s%c%s", field->name, field->component_separator[0],
            suffices[0], field->component_separator[1], suffices[1], field->component_separator[2],
            suffices[2], field->component_separator[3], suffices[3]);
    break;
  default: sprintf(field_name, "invalid");
  }
  return field_name;
}

const char *ex_field_component_suffix(ex_field *field, int nest_level, int component)
{
#define X "x"
#define Y "y"
#define Z "z"
#define Q "q"
#define S "s"

#define XX "xx"
#define YY "yy"
#define ZZ "zz"
#define XY "xy"
#define YZ "yz"
#define ZX "zx"
#define YX "yx"
#define ZY "zy"
#define XZ "xz"

  switch (field->type[nest_level]) {
  case EX_VECTOR_1D: {
    static const char *suffix[] = {X};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_VECTOR_2D: {
    static const char *suffix[] = {X, Y};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_VECTOR_3D: {
    static const char *suffix[] = {X, Y, Z};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_QUATERNION_2D: {
    static const char *suffix[] = {S, Q};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_QUATERNION_3D: {
    static const char *suffix[] = {X, Y, Z, Q};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_FULL_TENSOR_36: {
    static const char *suffix[] = {XX, YY, ZZ, XY, YZ, ZX, YX, ZY, XZ};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_FULL_TENSOR_32: {
    static const char *suffix[] = {XX, YY, ZZ, XY, YX};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_FULL_TENSOR_22: {
    static const char *suffix[] = {XX, YY, XY, YX};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_FULL_TENSOR_16: {
    static const char *suffix[] = {XX, XY, YZ, ZX, YX, ZY, XZ};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_FULL_TENSOR_12: {
    static const char *suffix[] = {XX, XY, YX};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_SYM_TENSOR_33: {
    static const char *suffix[] = {XX, YY, ZZ, XY, YZ, ZX};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_SYM_TENSOR_31: {
    static const char *suffix[] = {XX, YY, ZZ, XY};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_SYM_TENSOR_21: {
    static const char *suffix[] = {XX, YY, XY};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_SYM_TENSOR_13: {
    static const char *suffix[] = {XX, XY, YZ, ZX};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_SYM_TENSOR_11: {
    static const char *suffix[] = {XX, XY};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_SYM_TENSOR_10: {
    static const char *suffix[] = {XX};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_ASYM_TENSOR_03: {
    static const char *suffix[] = {XY, YZ, ZX};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_ASYM_TENSOR_02: {
    static const char *suffix[] = {XY, YZ};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_ASYM_TENSOR_01: {
    static const char *suffix[] = {XY};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_MATRIX_2X2: {
    static const char *suffix[] = {"11", "12", "21", "22"};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_MATRIX_3X3: {
    static const char *suffix[] = {"11", "12", "13", "21", "22", "23", "31", "32", "33"};
    verify_valid_component(component, ex_field_cardinality(field->type[nest_level]), SIZE(suffix));
    return suffix[component - 1];
  }
  case EX_FIELD_TYPE_USER_DEFINED: {
    if (field->suffices[nest_level] != NULL) {
      static char user_suffix[32];
      // `user_suffices` is a comma-separated string.  Assume component is valid.
      char *string = strdup(&field->suffices[nest_level]);
      char *tofree = string;
      char *token  = strsep(&string, ",");
      for (int i = 0; i < component - 1; i++) {
        token = strsep(&string, ",");
      }
      if (token != NULL) {
        ex_copy_string(user_suffix, token, 256);
        free(tofree);
        return user_suffix;
      }
      free(tofree);
    }
    return "invalid";
  }
  case EX_BASIS: {
    // Suffices are just 1...#components.
    static char user_suffix[32];
    static char format[8];
    int         width = number_width(field->cardinality[nest_level]);
    sprintf(format, "%c%d%dd", '%', 0, width);
    sprintf(user_suffix, format, component);
    return user_suffix;
  }
  case EX_FIELD_TYPE_SEQUENCE: {
    // Suffices are just 0...#components-1.
    static char user_suffix[32];
    static char format[8];
    int         width = number_width(field->cardinality[nest_level]);
    sprintf(format, "%c%d%dd", '%', 0, width);
    sprintf(user_suffix, format, component - 1);
    return user_suffix;
  }
  case EX_QUADRATURE:
  case EX_SCALAR:
  case EX_FIELD_TYPE_INVALID:
  default: return "invalid";
  }
}

int ex_field_cardinality(const ex_field_type field_type)
{
  switch (field_type) {
  case EX_FIELD_TYPE_USER_DEFINED: return -1;
  case EX_FIELD_TYPE_SEQUENCE: return -1;
  case EX_QUADRATURE: return -1;
  case EX_BASIS: return -1;
  case EX_SCALAR: return 1;
  case EX_VECTOR_1D: return 1;
  case EX_VECTOR_2D: return 2;
  case EX_VECTOR_3D: return 3;
  case EX_QUATERNION_2D: return 2;
  case EX_QUATERNION_3D: return 4;
  case EX_FULL_TENSOR_36: return 9;
  case EX_FULL_TENSOR_32: return 5;
  case EX_FULL_TENSOR_22: return 4;
  case EX_FULL_TENSOR_16: return 7;
  case EX_FULL_TENSOR_12: return 3;
  case EX_SYM_TENSOR_33: return 6;
  case EX_SYM_TENSOR_31: return 4;
  case EX_SYM_TENSOR_21: return 3;
  case EX_SYM_TENSOR_13: return 4;
  case EX_SYM_TENSOR_11: return 2;
  case EX_SYM_TENSOR_10: return 1;
  case EX_ASYM_TENSOR_03: return 3;
  case EX_ASYM_TENSOR_02: return 2;
  case EX_ASYM_TENSOR_01: return 1;
  case EX_MATRIX_2X2: return 4;
  case EX_MATRIX_3X3: return 9;
  case EX_FIELD_TYPE_INVALID: return -1;
  }
  return -1;
}

/** The reverse of `field_type_enum_to_string` Used to convert the
 *  string into a `field_type` enum.  The string is parsed from the
 *  database as a user-readable attribute.  For example, EX_VECTOR_2D
 *  is on the database instead of a raw number 2
 */
ex_field_type ex_field_string_to_field_type(const char *field_name)
{
  if (strcmp(field_name, "EX_FIELD_TYPE_USER_DEFINED") == 0) {
    return EX_FIELD_TYPE_USER_DEFINED;
  }
  if (strcmp(field_name, "EX_FIELD_TYPE_SEQUENCE") == 0) {
    return EX_FIELD_TYPE_SEQUENCE;
  }
  if (strcmp(field_name, "EX_BASIS") == 0) {
    return EX_BASIS;
  }
  if (strcmp(field_name, "EX_QUADRATURE") == 0) {
    return EX_QUADRATURE;
  }
  if (strcmp(field_name, "EX_SCALAR") == 0) {
    return EX_SCALAR;
  }
  if (strcmp(field_name, "EX_VECTOR_1D") == 0) {
    return EX_VECTOR_1D;
  }
  if (strcmp(field_name, "EX_VECTOR_2D") == 0) {
    return EX_VECTOR_2D;
  }
  if (strcmp(field_name, "EX_VECTOR_3D") == 0) {
    return EX_VECTOR_3D;
  }
  if (strcmp(field_name, "EX_QUATERNION_2D") == 0) {
    return EX_QUATERNION_2D;
  }
  if (strcmp(field_name, "EX_QUATERNION_3D") == 0) {
    return EX_QUATERNION_3D;
  }
  if (strcmp(field_name, "EX_FULL_TENSOR_36") == 0) {
    return EX_FULL_TENSOR_36;
  }
  if (strcmp(field_name, "EX_FULL_TENSOR_32") == 0) {
    return EX_FULL_TENSOR_32;
  }
  if (strcmp(field_name, "EX_FULL_TENSOR_22") == 0) {
    return EX_FULL_TENSOR_22;
  }
  if (strcmp(field_name, "EX_FULL_TENSOR_16") == 0) {
    return EX_FULL_TENSOR_16;
  }
  if (strcmp(field_name, "EX_FULL_TENSOR_12") == 0) {
    return EX_FULL_TENSOR_12;
  }
  if (strcmp(field_name, "EX_SYMMETRIC_TENSOR_33") == 0) {
    return EX_SYM_TENSOR_33;
  }
  if (strcmp(field_name, "EX_SYMMETRIC_TENSOR_31") == 0) {
    return EX_SYM_TENSOR_31;
  }
  if (strcmp(field_name, "EX_SYMMETRIC_TENSOR_21") == 0) {
    return EX_SYM_TENSOR_21;
  }
  if (strcmp(field_name, "EX_SYMMETRIC_TENSOR_13") == 0) {
    return EX_SYM_TENSOR_13;
  }
  if (strcmp(field_name, "EX_SYMMETRIC_TENSOR_11") == 0) {
    return EX_SYM_TENSOR_11;
  }
  if (strcmp(field_name, "EX_SYMMETRIC_TENSOR_10") == 0) {
    return EX_SYM_TENSOR_10;
  }
  if (strcmp(field_name, "EX_ASYMMETRIC_TENSOR_03") == 0) {
    return EX_ASYM_TENSOR_03;
  }
  if (strcmp(field_name, "EX_ASYMMETRIC_TENSOR_02") == 0) {
    return EX_ASYM_TENSOR_02;
  }
  if (strcmp(field_name, "EX_ASYMMETRIC_TENSOR_01") == 0) {
    return EX_ASYM_TENSOR_01;
  }
  if (strcmp(field_name, "EX_MATRIX_2X2") == 0) {
    return EX_MATRIX_2X2;
  }
  if (strcmp(field_name, "EX_MATRIX_3X3") == 0) {
    return EX_MATRIX_3X3;
  }
  if (strcmp(field_name, "EX_FIELD_TYPE_INVALID") == 0) {
    return EX_FIELD_TYPE_INVALID;
  }
  return EX_FIELD_TYPE_INVALID;
}

/** Used to convert a `field_type` enum to a string so it can be
 *  written to the database as a user-readable attribute.  For
 *  example, EX_VECTOR_2D would appear instead of a raw number 2
 */
const char *ex_field_type_enum_to_string(const ex_field_type field_type)
{
  switch (field_type) {
  case EX_FIELD_TYPE_USER_DEFINED: return "EX_FIELD_TYPE_USER_DEFINED";
  case EX_FIELD_TYPE_SEQUENCE: return "EX_FIELD_TYPE_SEQUENCE";
  case EX_BASIS: return "EX_BASIS";
  case EX_QUADRATURE: return "EX_QUADRATURE";
  case EX_SCALAR: return "EX_SCALAR";
  case EX_VECTOR_1D: return "EX_VECTOR_1D";
  case EX_VECTOR_2D: return "EX_VECTOR_2D";
  case EX_VECTOR_3D: return "EX_VECTOR_3D";
  case EX_QUATERNION_2D: return "EX_QUATERNION_2D";
  case EX_QUATERNION_3D: return "EX_QUATERNION_3D";
  case EX_FULL_TENSOR_36: return "EX_FULL_TENSOR_36";
  case EX_FULL_TENSOR_32: return "EX_FULL_TENSOR_32";
  case EX_FULL_TENSOR_22: return "EX_FULL_TENSOR_22";
  case EX_FULL_TENSOR_16: return "EX_FULL_TENSOR_16";
  case EX_FULL_TENSOR_12: return "EX_FULL_TENSOR_12";
  case EX_SYM_TENSOR_33: return "EX_SYMMETRIC_TENSOR_33";
  case EX_SYM_TENSOR_31: return "EX_SYMMETRIC_TENSOR_31";
  case EX_SYM_TENSOR_21: return "EX_SYMMETRIC_TENSOR_21";
  case EX_SYM_TENSOR_13: return "EX_SYMMETRIC_TENSOR_13";
  case EX_SYM_TENSOR_11: return "EX_SYMMETRIC_TENSOR_11";
  case EX_SYM_TENSOR_10: return "EX_SYMMETRIC_TENSOR_10";
  case EX_ASYM_TENSOR_03: return "EX_ASYMMETRIC_TENSOR_03";
  case EX_ASYM_TENSOR_02: return "EX_ASYMMETRIC_TENSOR_02";
  case EX_ASYM_TENSOR_01: return "EX_ASYMMETRIC_TENSOR_01";
  case EX_MATRIX_2X2: return "EX_MATRIX_2X2";
  case EX_MATRIX_3X3: return "EX_MATRIX_3X3";
  case EX_FIELD_TYPE_INVALID: return "EX_FIELD_TYPE_INVALID";
  }
  return "EX_FIELD_TYPE_INVALID";
}

const char *ex_field_name(const ex_field_type field_type)
{
  switch (field_type) {
  case EX_FIELD_TYPE_USER_DEFINED: return "user defined";
  case EX_FIELD_TYPE_SEQUENCE: return "sequence";
  case EX_BASIS: return "basis";
  case EX_QUADRATURE: return "quadrature";
  case EX_SCALAR: return "scalar";
  case EX_VECTOR_1D: return "vector 1D";
  case EX_VECTOR_2D: return "vector 2D";
  case EX_VECTOR_3D: return "vector 3D";
  case EX_QUATERNION_2D: return "quaternion 2D";
  case EX_QUATERNION_3D: return "quaternion 3D";
  case EX_FULL_TENSOR_36: return "full tensor 36";
  case EX_FULL_TENSOR_32: return "full tensor 32";
  case EX_FULL_TENSOR_22: return "full tensor 22";
  case EX_FULL_TENSOR_16: return "full tensor 16";
  case EX_FULL_TENSOR_12: return "full tensor 12";
  case EX_SYM_TENSOR_33: return "symmetric tensor 33";
  case EX_SYM_TENSOR_31: return "symmetric tensor 31";
  case EX_SYM_TENSOR_21: return "symmetric tensor 21";
  case EX_SYM_TENSOR_13: return "symmetric tensor 13";
  case EX_SYM_TENSOR_11: return "symmetric tensor 11";
  case EX_SYM_TENSOR_10: return "symmetric tensor 10";
  case EX_ASYM_TENSOR_03: return "asymmetric tensor 03";
  case EX_ASYM_TENSOR_02: return "asymmetric tensor 02";
  case EX_ASYM_TENSOR_01: return "asymmetric tensor 01";
  case EX_MATRIX_2X2: return "matrix 2x2";
  case EX_MATRIX_3X3: return "matrix 3x3";
  case EX_FIELD_TYPE_INVALID: return "invalid";
  }
  return "invalid";
}
