// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "Grid.h"
#include <Ioss_ElementBlock.h>
#include <fmt/format.h>

void Grid::initialize(size_t i, size_t j, std::shared_ptr<Ioss::Region> region)
{
  auto &cell    = get_cell(i, j);
  cell.m_i      = i;
  cell.m_j      = j;
  cell.m_region = region;
}

void Grid::finalize()
{
  // All unit_cells have same X, Y (and Z) extent.  Only need X and Y
  auto x_range = get_cell(0, 0).get_coordinate_range(Axis::X);
  auto y_range = get_cell(0, 0).get_coordinate_range(Axis::Y);

  // Assume all unit cells have same element block count (TODO: verify assumption)
  size_t element_block_count =
      get_cell(0, 0).m_region->get_property("element_block_count").get_int();
  std::vector<size_t> element_block_elem_count(element_block_count);
  size_t              global_node_count = 0;
  size_t              global_elem_count = 0;

  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto cell                 = get_cell(i, j);
      cell.m_globalNodeIdOffset = global_node_count;
      cell.m_localNodeIdOffset  = global_node_count;
      global_node_count += cell.m_region->get_property("node_count").get_int();

      cell.m_globalElementIdOffset.resize(element_block_count);
      cell.m_localElementIdOffset.resize(element_block_count);
      const auto &element_blocks = cell.m_region->get_element_blocks();
      assert(element_blocks.size() == element_block_count);

      for (size_t blk = 0; blk < element_block_count; blk++) {
        cell.m_globalElementIdOffset[blk] += element_block_elem_count[blk];
        cell.m_localElementIdOffset[blk] += element_block_elem_count[blk];

        const auto *block = element_blocks[blk];
        element_block_elem_count[blk] += block->entity_count();
        global_elem_count += block->entity_count();
      }

      cell.m_offX = (x_range.second - x_range.first) * i;
      cell.m_offY = (y_range.second - y_range.first) * j;

      cell.m_consistent = true;
    }
  }
  fmt::print("Total Node Count = {:n}, Total Element Count = {:n}\n", global_node_count,
             global_elem_count);
}
