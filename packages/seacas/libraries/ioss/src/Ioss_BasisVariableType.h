/*
 * Copyright(C) 2024 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */
#pragma once

#include "ioss_export.h"

#include "Ioss_CodeTypes.h"
#include <string>

#include "Ioss_VariableType.h"

namespace Ioss {
  struct Basis
  {
    int    subc_dim;
    int    subc_ordinal;
    int    subc_dof_ordinal;
    int    subc_num_dof;
    double xi;
    double eta;
    double zeta;
  };

  class IOSS_EXPORT BasisVariableType : public VariableType
  {
  public:
    //  'which' is 1-based
    IOSS_NODISCARD std::string label(int which, const char /* suffix_sep */) const override
    {
      assert(which > 0 && which <= component_count());
      if (component_count() == 1) {
        return "";
      }
      return VariableType::numeric_label(which - 1, component_count(), name());
    }

    BasisVariableType(const std::string &my_name, int number_components, bool delete_me)
        : Ioss::VariableType(my_name, number_components, delete_me)
    {
      m_basis_.resize(number_components);
    }

    BasisVariableType(const BasisVariableType &) = delete;

    IOSS_NODISCARD std::vector<Ioss::Basis> get_basis() { return m_basis_; }
    IOSS_NODISCARD Ioss::Basis get_basis(int which)
    {
      assert(which > 0 && which <= component_count());
      return m_basis_[which - 1];
    }

    void add_basis(int which, const Ioss::Basis &basis)
    {
      assert(which > 0 && which <= component_count());
      m_basis_[which - 1] = basis;
    }
    void add_basis(const std::vector<Ioss::Basis> &basis) { m_basis_ = basis; }

  private:
    std::vector<Ioss::Basis> m_basis_{};
  };
} // namespace Ioss

#if 0
typedef struct ex_basis
{
  /*
   * subc_dim: dimension of the subcell associated with the specified DoF ordinal 
   *      -- 0 node, 1 edge, 2 face, 3 volume [Range: 0..3]
   * subc_ordinal: ordinal of the subcell relative to its parent cell
   *      -- 0..n for each ordinal with the same subc dim [Range: <= DoF ordinal] 
   * subc_dof_ordinal: ordinal of the DoF relative to the subcell 
   * subc_num_dof: cardinality of the DoF set associated with this subcell. 
   * xi, eta, mu (ξ, η, ζ): Parametric coordinate location of the DoF 
   *      -- (Only first ndim values are valid)
   */
  char    name[EX_MAX_NAME + 1];
  int     cardinality; /* number of `basis` points == dimension of non-null subc_*, xi, eta, mu */
  int    *subc_dim;
  int    *subc_ordinal;
  int    *subc_dof_ordinal;
  int    *subc_num_dof;
  double *xi;
  double *eta;
  double *zeta;
} ex_basis;
#endif
