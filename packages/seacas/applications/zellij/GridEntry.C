// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "GridEntry.h"
#include <Ioss_NodeBlock.h>
#include <algorithm>

void GridEntry::initialize(size_t i, size_t j, std::shared_ptr<UnitCell> unit_cell)
{
  m_i        = i;
  m_j        = j;
  m_unitCell = unit_cell;
}

std::pair<double, double> GridEntry::get_coordinate_range(enum Axis axis) const
{
  if (axis == Axis::X) {
    return m_unitCell->minmax_x;
  }
  if (axis == Axis::Y) {
    return m_unitCell->minmax_y;
  }
  return std::make_pair(0.0, 0.0);
}

// Number of nodes that will be added to global node count when this cell is added to
// grid -- accounts for coincident nodes if cell has neighbor(s)
size_t GridEntry::added_node_count() const
{
  // If no neighbors (to -I, -J), then all nodes would be added...
  auto count = m_unitCell->m_region->get_property("node_count").get_int();

  if (has_neighbor_i()) {
    count -= (m_unitCell->cell_JJ * m_unitCell->cell_KK);
  }

  if (has_neighbor_j()) {
    count -= (m_unitCell->cell_II * m_unitCell->cell_KK);
  }

  if (has_neighbor_i() && has_neighbor_j()) {
    count += m_unitCell->cell_KK;
  }
  return count;
}
