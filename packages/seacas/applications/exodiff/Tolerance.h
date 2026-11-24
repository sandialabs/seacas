// Copyright(C) 1999-2020, 2022, 2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#pragma once

#include "map.h"
#include <cmath>
#include <limits>

// See http://realtimecollisiondetection.net/blog/?p=89 for a
// description of the COMBINED tolerance.  Basically:
// The absolute tolerance test fails when x and y become large, and
// the relative tolerance test fails when they become small. It is
// therefore desired to combine these two tests together in a single
// test. Over the years at GDC, as well as in my book, I've
// suggested the following combined tolerance test:
//
// if (Abs(x - y) <= EPSILON * Max(1.0f, Abs(x), Abs(y)) ...

enum class ToleranceMode {
  RELATIVE_    = 0,
  ABSOLUTE_    = 1,
  COMBINED_    = 2,
  IGNORE_      = 3,
  EIGEN_REL_   = 4,
  EIGEN_ABS_   = 5,
  EIGEN_COM_   = 6,
  ULPS_FLOAT_  = 7,
  ULPS_DOUBLE_ = 8
};

class Tolerance
{
public:
  Tolerance() = default;

  Tolerance(ToleranceMode tol_type, double tol_value, double tol_floor)
      : type(tol_type), value(tol_value), floor(tol_floor)
  {
  }

  // Default copy constructor and operator= should work for this simple class...

  bool Diff(double v1, double v2) const;

  double Delta(double v1, double v2) const;

  const char *typestr() const;
  const char *abrstr() const;

  ToleranceMode type{ToleranceMode::RELATIVE_};
  double        value{0.0};
  double        floor{0.0};

  // If true, use the older definition of the floor tolerance which was
  // |a-b| < floor.  The new definition is |a| < floor && |b| < floor
  static bool use_old_floor;

  // Mirrors `-ignore_nans` (set from ED_SystemInterface at startup, same
  // pattern as `use_old_floor`).  When true, NaN-containing comparisons skip
  // the non-finite screen and fall through to the original tolerance
  // arithmetic, i.e. the documented Invalid_Values semantics (NaNs are not
  // checked).  Inf mismatches are always reported regardless of this flag.
  static bool ignore_nans;

private:
  double UlpsDiffFloat(double A, double B) const;
  double UlpsDiffDouble(double A, double B) const;
};

inline double Tolerance::Delta(double v1, double v2) const
{
  if (type == ToleranceMode::IGNORE_) {
    return 0.0;
  }

  // Non-finite screen (see Tolerance::Diff): report an infinite delta for
  // any non-finite mismatch so it is always flagged as a difference.  The
  // NaN leg honors `-ignore_nans`: when set, NaN-only operands fall through
  // to the pre-screen arithmetic below (Invalid_Values semantics).
  bool ignored_nan =
      ignore_nans && (std::isnan(v1) || std::isnan(v2)) && !std::isinf(v1) && !std::isinf(v2);
  if ((!std::isfinite(v1) || !std::isfinite(v2)) && !ignored_nan) {
    bool same = (type == ToleranceMode::EIGEN_REL_ || type == ToleranceMode::EIGEN_ABS_ ||
                 type == ToleranceMode::EIGEN_COM_)
                    ? (std::fabs(v1) == std::fabs(v2))
                    : (v1 == v2);
    return same ? 0.0 : std::numeric_limits<double>::infinity();
  }

  double fabv1 = std::fabs(v1);
  double fabv2 = std::fabs(v2);
  bool   diff  = false;
  if (!use_old_floor) {
    if (fabv1 >= floor || fabv2 >= floor) {
      diff = true;
    }
  }
  else {
    if (std::fabs(v1 - v2) >= floor) {
      diff = true;
    }
  }

  if (diff) {
    if (type == ToleranceMode::RELATIVE_) {
      if (v1 == 0.0 && v2 == 0.0) {
        return 0.0;
      }
      double max = fabv1 < fabv2 ? fabv2 : fabv1;
      return std::fabs(v1 - v2) / max;
    }
    if (type == ToleranceMode::ABSOLUTE_) {
      return std::fabs(v1 - v2);
    }
    if (type == ToleranceMode::COMBINED_) {
      double max = fabv1 < fabv2 ? fabv2 : fabv1;
      if (max > 1.0) {
        return std::fabs(v1 - v2) / max;
      }
      return std::fabs(v1 - v2);
    }
    if (type == ToleranceMode::ULPS_FLOAT_) {
      return UlpsDiffFloat(v1, v2);
    }
    if (type == ToleranceMode::ULPS_DOUBLE_) {
      return UlpsDiffDouble(v1, v2);
    }
    if (type == ToleranceMode::EIGEN_REL_) {
      if (v1 == 0.0 && v2 == 0.0) {
        return 0.0;
      }
      double max = fabv1 < fabv2 ? fabv2 : fabv1;
      return std::fabs(fabv1 - fabv2) / max;
    }
    if (type == ToleranceMode::EIGEN_ABS_) {
      return std::fabs(fabv1 - fabv2);
    }
    if (type == ToleranceMode::EIGEN_COM_) {
      double max = fabv1 < fabv2 ? fabv2 : fabv1;
      if (max > 1.0) {
        return std::fabs(fabv1 - fabv2) / max;
      }
      return std::fabs(fabv1 - fabv2);
    }
  }
  return 0.0;
}
