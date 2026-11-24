// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "../Tolerance.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <limits>

namespace {
  int expect(bool condition, const char *message)
  {
    if (!condition) {
      std::cerr << message << '\n';
      return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
  }

  int expect_delta_inf(const Tolerance &tol, double lhs, double rhs, const char *message)
  {
    return expect(std::isinf(tol.Delta(lhs, rhs)), message);
  }
}

int main()
{
  const double inf = std::numeric_limits<double>::infinity();
  const double nan = std::numeric_limits<double>::quiet_NaN();

  Tolerance::use_old_floor = false;
  Tolerance::ignore_nans   = false;

  Tolerance relative{ToleranceMode::RELATIVE_, 1.0e-6, 0.0};
  Tolerance absolute{ToleranceMode::ABSOLUTE_, 1.0e-6, 0.0};
  Tolerance eigen_relative{ToleranceMode::EIGEN_REL_, 1.0e-6, 0.0};

  if (int rc = expect(!relative.Diff(inf, inf), "same-signed infinities should compare equal")) {
    return rc;
  }
  if (int rc = expect(relative.Delta(inf, inf) == 0.0,
                      "same-signed infinities should have zero delta")) {
    return rc;
  }
  if (int rc = expect(relative.Diff(inf, 1.0), "infinity and finite value should differ")) {
    return rc;
  }
  if (int rc = expect_delta_inf(relative, inf, 1.0,
                                "infinity and finite value should report infinite delta")) {
    return rc;
  }
  if (int rc = expect(relative.Diff(inf, -inf), "opposite infinities should differ")) {
    return rc;
  }
  if (int rc = expect_delta_inf(relative, inf, -inf,
                                "opposite infinities should report infinite delta")) {
    return rc;
  }

  if (int rc = expect(!eigen_relative.Diff(inf, -inf),
                      "eigen relative mode compares absolute infinite magnitudes")) {
    return rc;
  }
  if (int rc = expect(eigen_relative.Delta(inf, -inf) == 0.0,
                      "eigen relative mode reports zero delta for opposite infinite signs")) {
    return rc;
  }

  if (int rc = expect(absolute.Diff(nan, nan), "NaNs should differ by default")) {
    return rc;
  }
  if (int rc = expect_delta_inf(absolute, nan, nan, "NaNs should produce infinite delta by default")) {
    return rc;
  }

  Tolerance::ignore_nans = true;
  if (int rc = expect(!absolute.Diff(nan, nan), "ignore_nans should preserve legacy NaN equality")) {
    return rc;
  }
  if (int rc = expect(!std::isinf(absolute.Delta(nan, nan)),
                      "ignore_nans should not force NaN deltas to infinity")) {
    return rc;
  }
  if (int rc = expect(relative.Diff(inf, 1.0), "ignore_nans must not hide infinity mismatches")) {
    return rc;
  }

  return EXIT_SUCCESS;
}
