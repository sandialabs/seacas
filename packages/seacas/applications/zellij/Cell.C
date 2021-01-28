// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "Cell.h"
#include <Ioss_NodeBlock.h>
#include <algorithm>

extern bool equivalence_nodes;

void Cell::initialize(size_t i, size_t j, std::shared_ptr<UnitCell> unit_cell)
{
  m_i        = i;
  m_j        = j;
  m_unitCell = unit_cell;
  m_ranks[0] = 0;
  if (m_i > 0) {
    m_ranks[4] = 0;
  }
  if (m_j > 0) {
    m_ranks[2] = 0;
  }
}

std::pair<double, double> Cell::get_coordinate_range(enum Axis axis) const
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
size_t Cell::added_node_count(enum Mode mode) const
{
  // If no neighbors (to -I, -J), then all nodes would be added...
  auto count = m_unitCell->m_region->get_property("node_count").get_int();

  if (equivalence_nodes) {
    if (mode == Mode::GLOBAL) {
      if (has_neighbor_i()) {
        count -= (m_unitCell->cell_JJ * m_unitCell->cell_KK);
      }

      if (has_neighbor_j()) {
        count -= (m_unitCell->cell_II * m_unitCell->cell_KK);
      }

      if (has_neighbor_i() && has_neighbor_j()) {
        count += m_unitCell->cell_KK;
      }
    }
    else if (mode == Mode::PROCESSOR) {
      if (has_neighbor_i() && !processor_boundary(Loc::L)) {
        count -= (m_unitCell->cell_JJ * m_unitCell->cell_KK);
      }

      if (has_neighbor_j() && !processor_boundary(Loc::B)) {
        count -= (m_unitCell->cell_II * m_unitCell->cell_KK);
      }

      if (has_neighbor_i() && has_neighbor_j() && !processor_boundary(Loc::L) &&
          !processor_boundary(Loc::B)) {
        count += m_unitCell->cell_KK;
      }

      // Now the "corner case" ;-) If there is a processor boundary below, but the cell to the BL is
      // on the same rank as this cell, then we have already counted the IJ-line nodes, so need to
      // subtract that count...
      if (processor_boundary(Loc::B) && rank(Loc::BL) == rank(Loc::C)) {
        count -= m_unitCell->cell_KK;
      }
      // Now the other "corner case"
      if (processor_boundary(Loc::B) && rank(Loc::BR) == rank(Loc::C)) {
        count -= m_unitCell->cell_KK;
      }
    }
  }
  return count;
}

std::vector<int> Cell::categorize_nodes(enum Mode mode) const
{
  auto nodes = m_unitCell->categorize_nodes(has_neighbor_i(), has_neighbor_j());
  if (mode == Mode::PROCESSOR) {
    // If there is a processor boundary to the left, then need to change categorization of
    // all nodes on the left to '0'
    if (processor_boundary(Loc::L)) {
      const auto &min_I_face = m_unitCell->min_I_face;
      for (auto node : min_I_face) {
        nodes[node] -= 1;
      }
    }
    if (processor_boundary(Loc::B)) {
      const auto &min_J_face = m_unitCell->min_J_face;
      for (auto node : min_J_face) {
        nodes[node] -= 2;
      }
    }
  }
  return nodes;
}
