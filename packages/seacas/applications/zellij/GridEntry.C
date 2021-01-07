// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "GridEntry.h"
#include <Ioss_NodeBlock.h>
#include <algorithm>

std::pair<double, double> GridEntry::get_coordinate_range(enum Axis axis) const
{
  auto                nb = m_region->get_node_blocks()[0];
  std::vector<double> coord;
  if (axis == Axis::X) {
    nb->get_field_data("mesh_model_coordinates_x", coord);
  }
  else if (axis == Axis::Y) {
    nb->get_field_data("mesh_model_coordinates_y", coord);
  }
  else if (axis == Axis::Z) {
    nb->get_field_data("mesh_model_coordinates_z", coord);
  }
  const auto min_max_it = std::minmax_element(coord.begin(), coord.end());
  return std::make_pair(*min_max_it.first, *min_max_it.second);
}
