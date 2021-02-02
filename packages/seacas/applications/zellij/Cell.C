// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <numeric>

#include "Cell.h"

#include <Ioss_NodeBlock.h>
#include <Ioss_SmartAssert.h>
#include <algorithm>
#include <fmt/format.h>

extern unsigned int debug_level;
extern bool         equivalence_nodes;

void Cell::initialize(size_t i, size_t j, std::shared_ptr<UnitCell> unit_cell)
{
  m_i        = i;
  m_j        = j;
  m_unitCell = unit_cell;
  m_ranks[0] = 0;
  m_ranks[4] = m_i > 0 ? 0 : -1;
  m_ranks[2] = m_j > 0 ? 0 : -1;
  m_ranks[1] = (m_i > 0 && m_j > 0) ? 0 : -1;
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
      if (processor_boundary(Loc::B) && processor_boundary(Loc::L) &&
          rank(Loc::BL) == rank(Loc::C)) {
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

    // Now the "corner case" ;-) If there is a processor boundary below, but the cell to the BL is
    // on the same rank as this cell, then we have already counted the IJ-line nodes, so need to
    // categorize those nodes as already accounted for in a previous map...
    if (processor_boundary(Loc::B) && processor_boundary(Loc::L) && rank(Loc::BL) == rank(Loc::C)) {
      // Want KK() nodes -- First KK of min_i and of min_j.  But since they match, can "unzero"
      // min_i[0..KK)
      for (size_t i = 0; i < m_unitCell->cell_KK; i++) {
        nodes[m_unitCell->min_I_face[i]] = -1;
      }
    }
    // Now the other "corner case"
    if (processor_boundary(Loc::B) && rank(Loc::BR) == rank(Loc::C)) {
      // Want KK() nodes -- First KK of max_i and Last KK of min_j.  But since they match, can
      // "unzero" max_i[0..KK)
      for (size_t i = 0; i < m_unitCell->cell_KK; i++) {
        nodes[m_unitCell->max_I_face[i]] = -1;
      }
    }
  }
  return nodes;
}

template std::vector<int64_t> Cell::generate_node_map(Mode, int64_t) const;
template std::vector<int>     Cell::generate_node_map(Mode, int) const;

template <typename INT> std::vector<INT> Cell::generate_node_map(Mode mode, INT /*dummy*/) const
{
  // Size is node_count + 1 to handle the 1-based connectivity values.
  size_t           cell_node_count = m_unitCell->m_region->get_property("node_count").get_int();
  std::vector<INT> map(cell_node_count + 1);

  INT offset = mode == Mode::PROCESSOR ? m_localNodeIdOffset : m_globalNodeIdOffset;

  if (!equivalence_nodes || !(has_neighbor_i() || has_neighbor_j())) {
    std::iota(map.begin(), map.end(), offset);
  }
  else if (has_neighbor_i() || has_neighbor_j()) {
    // At least one neighboring cell and the nodes are being equivalenced
    // Generate map for the "non-neighbored" nodes (not contiguous with a neighbor cell)
    auto categorized_nodes = categorize_nodes(mode);
    SMART_ASSERT(categorized_nodes.size() == cell_node_count)
    (categorized_nodes.size())(cell_node_count);
    offset++; // To deal with 1-based node numbers.
    for (size_t n = 0; n < cell_node_count; n++) {
      if (categorized_nodes[n] == 0) {
        map[n + 1] = offset++;
      }
    }
  }

  if (equivalence_nodes && has_neighbor_i() &&
      (mode == Mode::GLOBAL || (mode == Mode::PROCESSOR && rank(Loc::C) == rank(Loc::L)))) {
    // Get the neighbor cell...
    // iterate my unit cell's min_I_face() nodes to get index into map
    // At this index, set value to this cells min_I_nodes() node
    // which was created by the neighbor when he was processed...
    SMART_ASSERT(min_I_nodes.size() == m_unitCell->min_I_face.size())
    (m_i)(m_j)(min_I_nodes.size())(m_unitCell->min_I_face.size());

    for (size_t i = 0; i < m_unitCell->min_I_face.size(); i++) {
      auto idx = m_unitCell->min_I_face[i] + 1;
      auto val = min_I_nodes[i];
      map[idx] = (INT)val;
    }
  }

  if (equivalence_nodes && has_neighbor_j() &&
      (mode == Mode::GLOBAL || (mode == Mode::PROCESSOR && rank(Loc::C) == rank(Loc::B)))) {
    SMART_ASSERT(min_J_nodes.size() == m_unitCell->min_J_face.size())
    (m_i)(m_j)(min_J_nodes.size())(m_unitCell->min_J_face.size());

    for (size_t i = 0; i < m_unitCell->min_J_face.size(); i++) {
      auto idx = m_unitCell->min_J_face[i] + 1;
      auto val = min_J_nodes[i];
      map[idx] = (INT)val;
    }
  }

  if (mode == Mode::PROCESSOR) {
    // Now the "corner case" ;-) If there is a processor boundary below, but the cell to the BL is
    // on the same rank as this cell, then we have already counted the IJ-line nodes, so need to
    // categorize those nodes as already accounted for in a previous map...
    if (processor_boundary(Loc::B) && processor_boundary(Loc::L) && rank(Loc::BL) == rank(Loc::C)) {
      // Want KK() nodes -- First KK of min_i and of min_j.  But since they match, can "unzero"
      // min_i[0..KK)
      auto KK = m_unitCell->cell_KK;
      for (size_t i = 0; i < KK; i++) {
        auto idx = m_unitCell->min_J_face[i] + 1;
        auto val = min_J_nodes[i];
        map[idx] = (INT)val;
        if (debug_level & 32) {
          fmt::print("In corner case BL: {} {}\n", idx, val);
        }
      }
    }
    // Now the other "corner case"
    if (processor_boundary(Loc::B) && rank(Loc::BR) == rank(Loc::C)) {
      // Want KK() nodes -- First KK of max_i and Last KK of min_j.  But since they match, can
      // "unzero" max_i[0..KK)
      auto KK       = m_unitCell->cell_KK;
      auto j_offset = min_J_nodes.size() - KK;
      for (size_t i = 0; i < KK; i++) {
        auto idx = m_unitCell->min_J_face[j_offset + i] + 1;
        auto val = min_J_nodes[j_offset + i];
        map[idx] = (INT)val;
        if (debug_level & 32) {
          fmt::print("In corner case BR: {} {} {}\n", idx, val, j_offset + i);
        }
      }
    }
  }
  // Can now clean out the `min_I_nodes` and `min_J_nodes` lists since the data will no longer be
  // needed.
  Ioss::Utils::clear(min_I_nodes);
  Ioss::Utils::clear(min_J_nodes);

  return map;
}

template void Cell::populate_neighbor(Loc location, const std::vector<int64_t> &map,
                                      const Cell &neighbor) const;
template void Cell::populate_neighbor(Loc location, const std::vector<int> &map,
                                      const Cell &neighbor) const;

template <typename INT>
void Cell::populate_neighbor(Loc location, const std::vector<INT> &map, const Cell &neighbor) const
{
  switch (location) {
  case Loc::L:
    neighbor.min_I_nodes.resize(m_unitCell->max_I_face.size());
    for (size_t i = 0; i < m_unitCell->max_I_face.size(); i++) {
      auto idx                = m_unitCell->max_I_face[i] + 1;
      auto val                = map[idx];
      neighbor.min_I_nodes[i] = val;
    }
    if (debug_level & 8) {
      fmt::print("\nCell {} {}\n", neighbor.m_i, neighbor.m_j);
      fmt::print("min_I_nodes: {}\n", fmt::join(neighbor.min_I_nodes, " "));
    }
    break;

  case Loc::B:
    neighbor.min_J_nodes.resize(m_unitCell->max_J_face.size());
    for (size_t i = 0; i < m_unitCell->max_J_face.size(); i++) {
      auto idx                = m_unitCell->max_J_face[i] + 1;
      auto val                = map[idx];
      neighbor.min_J_nodes[i] = val;
    }
    if (debug_level & 8) {
      fmt::print("min_J_nodes: {}\n", fmt::join(neighbor.min_J_nodes, " "));
    }
    break;

  case Loc::BR: {
    neighbor.min_J_nodes.resize(m_unitCell->max_J_face.size());
    auto KK       = m_unitCell->cell_KK;
    auto j_offset = neighbor.min_J_nodes.size() - KK;
    for (size_t i = 0; i < KK; i++) {
      auto idx                           = m_unitCell->max_J_face[i] + 1;
      auto val                           = map[idx];
      neighbor.min_J_nodes[j_offset + i] = val;
      if (debug_level & 32) {
        fmt::print("BR: {} {} {}\n", idx, val, j_offset + i);
      }
    }
  } break;

  case Loc::BL: {
    neighbor.min_J_nodes.resize(m_unitCell->max_J_face.size());
    auto KK       = m_unitCell->cell_KK;
    auto j_offset = neighbor.min_J_nodes.size() - KK;
    for (size_t i = 0; i < KK; i++) {
      auto idx                = m_unitCell->max_J_face[j_offset + i] + 1;
      auto val                = map[idx];
      neighbor.min_J_nodes[i] = val;
      if (debug_level & 32) {
        fmt::print("BL: {} {} {}\n", idx, val, j_offset + i);
      }
    }
  } break;

  default:
    fmt::print(stderr, "\nINTERNAL ERROR: Unhandled direction in populate_neighbor(): {}\n",
               location);
    exit(EXIT_FAILURE);
  }
}
