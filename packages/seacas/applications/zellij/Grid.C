// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "Grid.h"
#include <Ioss_ElementBlock.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_Region.h>
#include <Ioss_SmartAssert.h>
#include <exodusII.h>
#include <fmt/format.h>

void Grid::initialize(size_t i, size_t j, std::shared_ptr<Ioss::Region> region)
{
  auto &cell    = get_cell(i, j);
  cell.m_i      = i;
  cell.m_j      = j;
  cell.m_region = region;
  SMART_ASSERT(region != nullptr)(i)(j);
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
      auto &cell                = get_cell(i, j);
      cell.m_globalNodeIdOffset = global_node_count;
      cell.m_localNodeIdOffset  = global_node_count;
      SMART_ASSERT(cell.m_region != nullptr)(i)(j);
      global_node_count += cell.m_region->get_property("node_count").get_int();

      cell.m_globalElementIdOffset.resize(element_block_count);
      cell.m_localElementIdOffset.resize(element_block_count);
      const auto &element_blocks = cell.m_region->get_element_blocks();
      assert(element_blocks.size() == element_block_count);

      for (size_t blk = 0; blk < element_block_count; blk++) {
        cell.m_globalElementIdOffset[blk] = element_block_elem_count[blk];
        cell.m_localElementIdOffset[blk]  = element_block_elem_count[blk];

        const auto *block = element_blocks[blk];
        element_block_elem_count[blk] += block->entity_count();
        global_elem_count += block->entity_count();
#if 0
	fmt::print("i, j, blk, offset, block_count, global_count: {} {} {} {} {} {}\n",
		   i, j, blk, cell.m_globalElementIdOffset[blk], element_block_elem_count[blk], global_elem_count);
#endif
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

  // Define Element Blocks... (Copy from cell(0,0))
  const auto &element_blocks = get_cell(0, 0).m_region->get_element_blocks();
  for (size_t blk = 0; blk < element_block_count; blk++) {
    const auto *block  = element_blocks[blk];
    auto        oblock = new Ioss::ElementBlock(*block);
    oblock->property_update("entity_count", element_block_elem_count[blk]);
    m_region->add(oblock);
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
      auto *nb   = cell.m_region->get_node_blocks()[0];
      nb->get_field_data("mesh_model_coordinates_x", coord_x);
      nb->get_field_data("mesh_model_coordinates_y", coord_y);
      nb->get_field_data("mesh_model_coordinates_z", coord_z);

      if (cell.m_offX != 0.0) {
        std::for_each(coord_x.begin(), coord_x.end(), [&cell](double &d) { d += cell.m_offX; });
      }
      if (cell.m_offY != 0.0) {
        std::for_each(coord_y.begin(), coord_y.end(), [&cell](double &d) { d += cell.m_offY; });
      }

      auto start = cell.m_globalNodeIdOffset + 1;
      auto count = nb->entity_count();
      ex_put_partial_coord(exoid, start, count, coord_x.data(), coord_y.data(), coord_z.data());
    }
  }
}

void Grid::output_block_connectivity()
{
  // IOSS does not support partial field output at this time, so need to use raw exodus calls for
  // now...
  int exoid = m_region->get_database()->get_file_pointer();

  std::vector<int64_t> connect;
  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto cell   = get_cell(i, j);
      auto blocks = cell.m_region->get_element_blocks();
      for (size_t blk = 0; blk < blocks.size(); blk++) {
        auto *block = blocks[blk];
        block->get_field_data("connectivity_raw", connect);
        std::for_each(connect.begin(), connect.end(),
                      [&cell](int64_t &node) { node += cell.m_globalNodeIdOffset; });

        auto start = cell.m_globalElementIdOffset[blk] + 1;
        auto count = block->entity_count();
        auto id    = block->get_property("id").get_int();
#if 0
        fmt::print(stderr, "i, j, blk, id, start, count: {} {} {} {} {} {}\n", 
		i, j, blk, id, start, count);
#endif
        ex_put_partial_conn(exoid, EX_ELEM_BLOCK, id, start, count, connect.data(), nullptr,
                            nullptr);
      }
    }
  }
}
