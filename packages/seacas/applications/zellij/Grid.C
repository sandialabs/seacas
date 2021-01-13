// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <numeric>

#include "Grid.h"
#include <Ioss_ElementBlock.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_Region.h>
#include <Ioss_SmartAssert.h>
#include <exodusII.h>
#include <fmt/format.h>

namespace {
  std::vector<int64_t> generate_node_map(Grid &grid, const GridEntry &cell);
}

void Grid::initialize(size_t i, size_t j, std::shared_ptr<UnitCell> unit_cell)
{
  auto &cell      = get_cell(i, j);
  cell.m_i        = i;
  cell.m_j        = j;
  cell.m_unitCell = unit_cell;
  SMART_ASSERT(unit_cell->m_region != nullptr)(i)(j);
}

void Grid::finalize()
{
  // All unit_cells have same X, Y (and Z) extent.  Only need X and Y
  auto x_range = get_cell(0, 0).get_coordinate_range(Axis::X);
  auto y_range = get_cell(0, 0).get_coordinate_range(Axis::Y);

  // Not all unit cells have the same element blocks and the output
  // grid will contain the union of the element blocks on each unit
  // cell...
  //
  // While finalizing the cells, we will create this union for use in
  // the output region.  Would be quicker to do iteration on unit_cell
  // map which is smaller, but for now do it here and see if becomes
  // bottleneck.
  std::map<std::string, Ioss::ElementBlock *> output_element_blocks;
  std::map<std::string, size_t>               element_block_elem_count;
  size_t                                      global_node_count = 0;
  size_t                                      global_elem_count = 0;

  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto &cell                = get_cell(i, j);
      cell.m_globalNodeIdOffset = global_node_count;
      cell.m_localNodeIdOffset  = global_node_count;
      SMART_ASSERT(cell.m_unitCell->m_region != nullptr)(i)(j);
      auto new_nodes = cell.added_node_count();
#if defined(ZELLIJ_DEBUG)
      fmt::print("i, j, node_offset, added_nodes: {} {} {} {}\n", i, j, global_node_count,
                 new_nodes);
#endif
      global_node_count += new_nodes;

      const auto &element_blocks = cell.m_unitCell->m_region->get_element_blocks();
      for (const auto *block : element_blocks) {
        auto &blk                         = block->name();
        cell.m_globalElementIdOffset[blk] = element_block_elem_count[blk];
        cell.m_localElementIdOffset[blk]  = element_block_elem_count[blk];

        element_block_elem_count[blk] += block->entity_count();
        global_elem_count += block->entity_count();

#if defined(ZELLIJ_DEBUG)
        fmt::print("i, j, blk, offset, block_count, global_count: {} {} {} {} {} {}\n", i, j, blk,
                   cell.m_globalElementIdOffset[blk], element_block_elem_count[blk],
                   global_elem_count);
#endif

        // Create output element block if does not exist yet...
        if (output_element_blocks.find(blk) == output_element_blocks.end()) {
          output_element_blocks[blk] = new Ioss::ElementBlock(*block);
        }
      }

      cell.m_offX = (x_range.second - x_range.first) * i;
      cell.m_offY = (y_range.second - y_range.first) * j;

      cell.m_consistent = true;
    }
  }

  // Define the output database...
  // Define a node block...
  std::string block_name        = "nodeblock_1";
  int         spatial_dimension = 3;
  auto        block = new Ioss::NodeBlock(m_region->get_database(), block_name, global_node_count,
                                   spatial_dimension);
  block->property_add(Ioss::Property("id", 1));

  m_region->add(block);

  for (auto &blk : output_element_blocks) {
    auto *block = blk.second;
    block->property_update("entity_count", element_block_elem_count[block->name()]);
    m_region->add(block);
  }
  m_region->end_mode(Ioss::STATE_DEFINE_MODEL);

  m_region->output_summary(std::cerr);
}

void Grid::output_model()
{
  output_nodal_coordinates();
  output_block_connectivity();
}

void Grid::output_nodal_coordinates()
{
  // IOSS does not support partial field output at this time, so need to use raw exodus calls for
  // now...
  int exoid = m_region->get_database()->get_file_pointer();

  std::vector<double> coord_x;
  std::vector<double> coord_y;
  std::vector<double> coord_z;

  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto  cell = get_cell(i, j);
      auto *nb   = cell.m_unitCell->m_region->get_node_blocks()[0];
      nb->get_field_data("mesh_model_coordinates_x", coord_x);
      nb->get_field_data("mesh_model_coordinates_y", coord_y);
      nb->get_field_data("mesh_model_coordinates_z", coord_z);

      if (cell.m_offX != 0.0) {
        std::for_each(coord_x.begin(), coord_x.end(), [&cell](double &d) { d += cell.m_offX; });
      }
      if (cell.m_offY != 0.0) {
        std::for_each(coord_y.begin(), coord_y.end(), [&cell](double &d) { d += cell.m_offY; });
      }

      // Filter coordinates down to only "new nodes"...
      if (cell.has_neighbor_i() || cell.has_neighbor_j()) {
        auto   categorized_nodes = cell.categorize_nodes();
        size_t nn                = 0;
        for (size_t n = 0; n < categorized_nodes.size(); n++) {
          if (categorized_nodes[n] == 0) {
            coord_x[nn] = coord_x[n];
            coord_y[nn] = coord_y[n];
            coord_z[nn] = coord_z[n];
            nn++;
          }
        }
      }

      auto start = cell.m_globalNodeIdOffset + 1;
      auto count = cell.added_node_count();
      ex_put_partial_coord(exoid, start, count, coord_x.data(), coord_y.data(), coord_z.data());
    }
  }
}

void Grid::output_block_connectivity()
{
  // IOSS does not support partial field output at this time, so need to use raw exodus calls for
  // now...
  int exoid = m_region->get_database()->get_file_pointer();

  // TODO: Correctly renumber based on shared nodes between neighboring grid entries...

  std::vector<int64_t> connect;
  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto &cell     = get_cell(i, j);
      auto  node_map = generate_node_map(*this, cell);
      auto  blocks   = cell.m_unitCell->m_region->get_element_blocks();
      for (const auto *block : blocks) {
        block->get_field_data("connectivity_raw", connect);
        for (size_t i = 0; i < connect.size(); i++) {
          connect[i] = node_map[connect[i]];
        }
        auto start = cell.m_globalElementIdOffset[block->name()] + 1;
        auto count = block->entity_count();
        auto id    = block->get_property("id").get_int();
#if defined(ZELLIJ_DEBUG)
        fmt::print(stderr, "i, j, blk, id, start, count: {} {} {} {} {} {}\n", i, j, block->name(),
                   id, start, count);
#endif
        ex_put_partial_conn(exoid, EX_ELEM_BLOCK, id, start, count, connect.data(), nullptr,
                            nullptr);
      }
    }
  }
}

namespace {
  std::vector<int64_t> generate_node_map(Grid &grid, const GridEntry &cell)
  {
    // Generate a "map" from nodes in the input connectivity to the
    // output connectivity in global nodes.  If no neighbors, then
    // this would just be adding `cell.m_globalNodeIdOffset` to each
    // connectivity entry.

    // Size is node_count + 1 to handle the 1-based connectivity values.
    size_t cell_node_count = cell.m_unitCell->m_region->get_property("node_count").get_int();
    std::vector<int64_t> map(cell_node_count + 1);
    if (!(cell.has_neighbor_i() || cell.has_neighbor_j())) {
      std::iota(map.begin(), map.end(), cell.m_globalNodeIdOffset);
    }
    else {
      // At least one neighboring cell.
      // Generate map for the "non-neighbored" nodes (not contiguous with a neighbor cell)
      auto categorized_nodes = cell.categorize_nodes();
      SMART_ASSERT(categorized_nodes.size() == cell_node_count)
      (categorized_nodes.size())(cell_node_count);
      size_t offset = cell.m_globalNodeIdOffset + 1;
      for (size_t n = 0; n < cell_node_count; n++) {
        if (categorized_nodes[n] == 0) {
          map[n + 1] = offset++;
        }
      }
    }

    if (cell.has_neighbor_i()) {
      const auto &neighbor = grid.get_cell(cell.m_i - 1, cell.m_j);
      // Get the neighbor cell...
      // iterate my unit cell's min_I_face() nodes to get index into map
      // At this index, set value to neighbor cells max_I_nodes() node
      for (size_t i = 0; i < cell.m_unitCell->min_I_face.size(); i++) {
        auto idx = cell.m_unitCell->min_I_face[i] + 1;
        auto val = neighbor.max_I_nodes[i];
        map[idx] = val;
      }

      // Can now clean out the neighbors max_I_nodes list since this
      // is the only cell that can use the data...
      Ioss::Utils::clear(neighbor.max_I_nodes);
    }

    if (cell.has_neighbor_j()) {
      const auto &neighbor = grid.get_cell(cell.m_i, cell.m_j - 1);
      for (size_t i = 0; i < cell.m_unitCell->min_J_face.size(); i++) {
        auto idx = cell.m_unitCell->min_J_face[i] + 1;
        auto val = neighbor.max_J_nodes[i];
        map[idx] = val;
      }
      Ioss::Utils::clear(neighbor.max_J_nodes);
    }

    // Now that we have the node map for this cell, we need to save the mappings for the max_I and
    // max_J faces and max_I-max_J edge for use by later neighbors...
    cell.max_I_nodes.resize(cell.m_unitCell->max_I_face.size());
    for (size_t i = 0; i < cell.m_unitCell->max_I_face.size(); i++) {
      auto idx            = cell.m_unitCell->max_I_face[i] + 1;
      auto val            = map[idx];
      cell.max_I_nodes[i] = val;
    }
#if defined(ZELLIJ_DEBUG)
    fmt::print("\nCell {} {}\n", cell.m_i, cell.m_j);
    fmt::print("max_I_nodes: {}\n", fmt::join(cell.max_I_nodes, " "));
#endif
    cell.max_J_nodes.resize(cell.m_unitCell->max_J_face.size());
    for (size_t i = 0; i < cell.m_unitCell->max_J_face.size(); i++) {
      auto idx            = cell.m_unitCell->max_J_face[i] + 1;
      auto val            = map[idx];
      cell.max_J_nodes[i] = val;
    }
#if defined(ZELLIJ_DEBUG)
    fmt::print("max_J_nodes: {}\n", fmt::join(cell.max_J_nodes, " "));
#endif
    // Iterate the connectivity and set connect[i] = map[connect[i]]
    grid.m_region->get_database()->progress(
        fmt::format("Generate Node Map Cell({}, {})", cell.m_i, cell.m_j));
#if defined(ZELLIJ_DEBUG)
    fmt::print("           MAP: {}\n", fmt::join(map, " "));
#endif
    return map;
  }
} // namespace
