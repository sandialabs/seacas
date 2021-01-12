// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#ifndef ZE_UnitCell_H
#define ZE_UnitCell_H

#include <map>
#include <string>
#include <vector>

#include "Ioss_Region.h"

class UnitCell
{
public:
  UnitCell(std::shared_ptr<Ioss::Region> region);
  UnitCell(const UnitCell &) = delete;

  std::vector<int> categorize_nodes(bool neighbor_i, bool neighbor_j) const;

  std::shared_ptr<Ioss::Region> m_region{nullptr};
  std::vector<int64_t>          min_I_face{};
  std::vector<int64_t>          max_I_face{};
  std::vector<int64_t>          min_J_face{};
  std::vector<int64_t>          max_J_face{};

  std::pair<double, double> minmax_x{};
  std::pair<double, double> minmax_y{};

  size_t cell_II{};
  size_t cell_JJ{};
  size_t cell_KK{};
};

using UnitCellMap = std::map<std::string, std::shared_ptr<UnitCell>>;
#endif
