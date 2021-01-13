// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#ifndef ZE_GridEntry_H
#define ZE_GridEntry_H
//
// `GridEntry`:
//  -- for each location:
//     -- i,j location (could be calculated, but easier if entry knows it...)
//     -- reference to unit_cell region
//     -- global node offset for node ids (over all ranks if parallel)
//     -- local node offset for node ids (for this rank only if parallel)
//     -- global element block offsets for element ids for each element block (over all ranks if
//     parallel)
//     -- local element block offsets for element ids for each element block (for this rank only
//     if parallel)
//     -- offX, offY -- coordinate offsets for nodes (can be calculated?)
//     -- rank (for parallel -- which rank does this entry exist on)
//

#include <memory>
#include <utility>

#include "UnitCell.h"

// Each entry in grid will have the following information:
enum class Axis { X, Y, Z };

class GridEntry
{
public:
  std::pair<double, double> get_coordinate_range(enum Axis) const;
  void                      initialize(size_t i, size_t j, std::shared_ptr<UnitCell> region);

  bool has_neighbor_i() const { return m_i > 0; }
  bool has_neighbor_j() const { return m_j > 0; }

  // Number of nodes that will be added to global node count when this cell is added to
  // grid -- accounts for coincident nodes if cell has neighbor(s)
  size_t added_node_count() const;

  std::vector<int> categorize_nodes() const
  {
    return m_unitCell->categorize_nodes(has_neighbor_i(), has_neighbor_j());
  }

  mutable std::vector<int64_t> max_I_nodes;
  mutable std::vector<int64_t> max_J_nodes;

  std::shared_ptr<UnitCell>     m_unitCell;
  size_t                        m_i{0};
  size_t                        m_j{0};
  int64_t                       m_globalNodeIdOffset{0};
  int64_t                       m_localNodeIdOffset{0};
  std::map<std::string, size_t> m_globalElementIdOffset;
  std::map<std::string, size_t> m_localElementIdOffset;
  double                        m_offX{0.0};
  double                        m_offY{0.0};
  int                           m_rank{0};
  bool                          m_consistent{false};
};

#endif
