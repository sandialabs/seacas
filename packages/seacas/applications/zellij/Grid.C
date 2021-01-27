// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <numeric>

#include "Decompose.h"
#include "Grid.h"
#include "ZE_SystemInterface.h"
#include "ZE_Version.h"
#include <Ioss_ElementBlock.h>
#include <Ioss_IOFactory.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_Region.h>
#include <Ioss_SmartAssert.h>

#include <exodusII.h>
#include <fmt/format.h>

extern unsigned int debug_level;
bool                equivalence_nodes = true;

namespace {
  template <typename INT>
  std::vector<INT>      generate_node_map(Grid &grid, const Cell &cell, INT /*dummy*/);
  Ioss::PropertyManager parse_properties(SystemInterface &interFace, int int_size);

  void set_coordinate_offsets(Grid &grid);
  void handle_nodes(Grid &grid);
  void handle_elements(Grid &grid);
} // namespace

Grid::Grid(SystemInterface &interFace, size_t extent_i, size_t extent_j)
    : m_gridI(extent_i), m_gridJ(extent_j), m_parallelSize(interFace.ranks())
{
  m_grid.resize(m_gridI * m_gridJ);
  create_output_regions(interFace);
  equivalence_nodes = interFace.equivalence_nodes();
}

void Grid::initialize(size_t i, size_t j, std::shared_ptr<UnitCell> unit_cell)
{
  auto &cell      = get_cell(i, j);
  cell.m_i        = i;
  cell.m_j        = j;
  cell.m_unitCell = unit_cell;
  SMART_ASSERT(unit_cell->m_region != nullptr)(i)(j);
}

void Grid::create_output_regions(SystemInterface &interFace)
{
  m_outputRegions.reserve(interFace.ranks());

  // Define the output database(s)...
  int                   int_size   = interFace.ints32bit() ? 4 : 8;
  Ioss::PropertyManager properties = parse_properties(interFace, int_size);
  if (parallel_size() == 1) {
    properties.add(Ioss::Property("OMIT_EXODUS_NUM_MAPS", 1));
  }
  if (debug_level & 2) {
    properties.add(Ioss::Property("ENABLE_TRACING", 1));
  }

  for (int i = 0; i < interFace.ranks(); i++) {
    std::string outfile = Ioss::Utils::decode_filename(interFace.outputName_, i, interFace.ranks());
    Ioss::DatabaseIO *dbo = Ioss::IOFactory::create("exodus", outfile, Ioss::WRITE_RESTART,
                                                    (MPI_Comm)MPI_COMM_WORLD, properties);
    if (dbo == nullptr || !dbo->ok(true)) {
      std::exit(EXIT_FAILURE);
    }
    m_outputRegions.emplace_back(new Ioss::Region(dbo, "zellij_output_region"));
    output_region(i)->begin_mode(Ioss::STATE_DEFINE_MODEL);
    output_region(i)->property_add(Ioss::Property("code_name", qainfo[0]));
    output_region(i)->property_add(Ioss::Property("code_version", qainfo[2]));
  }
}

void Grid::decompose(size_t ranks, const std::string &method)
{
  decompose_grid(*this, ranks, method);

  // Now iterate the cells and tell each cell whether it is on a processor
  // boundary with its "left" or "lower" neighboring cell.
  for (size_t j = 0; j < JJ(); j++) {
    auto left = get_cell(0, j);
    for (size_t i = 1; i < II(); i++) {
      auto &cell = get_cell(i, j);
      cell.set_rankI(left.rank());
      left = cell;
    }
  }

  for (size_t i = 0; i < II(); i++) {
    auto below = get_cell(i, 0);
    for (size_t j = 1; j < JJ(); j++) {
      auto &cell = get_cell(i, j);
      cell.set_rankJ(below.rank());
      below = cell;
    }
  }

  if (debug_level & 32) {
    for (size_t j = 0; j < JJ(); j++) {
      for (size_t i = 0; i < II(); i++) {
        const auto &cell  = get_cell(i, j);
        auto        left  = cell.rank() != cell.rankI() ? '<' : ' ';
        auto        below = cell.rank() != cell.rankJ() ? '^' : ' ';
        fmt::print(" {}{}{}", left, cell.rank(), below);
      }
      fmt::print("\n");
    }
  }
}

void Grid::finalize()
{
  if (debug_level & 2) {
    util().progress(__func__);
  }

  set_coordinate_offsets(*this);
  handle_nodes(*this);
  handle_elements(*this);

  for (int i = 0; i < m_parallelSize; i++) {
    output_region(i)->end_mode(Ioss::STATE_DEFINE_MODEL);
    output_region(i)->output_summary(std::cerr);
  }
}

template void Grid::output_model(int64_t);
template void Grid::output_model(int);

template <typename INT> void Grid::output_model(INT /*dummy*/)
{
  for (int rank = 0; rank < parallel_size(); rank++) {
    output_nodal_coordinates(rank);
    output_block_connectivity(rank, INT(0));
    if (parallel_size() > 1) {
      output_node_map(rank, INT(0));
      output_element_map(rank, INT(0));
    }
  }
}

void Grid::output_nodal_coordinates(int rank)
{
  // IOSS does not support partial field output at this time, so need to use raw exodus calls for
  // now...
  std::vector<double> coord_x;
  std::vector<double> coord_y;
  std::vector<double> coord_z;

  if (debug_level & 2) {
    util().progress(__func__);
  }

  // This ordering results in `parallel_size() * II() * JJ()` iterations instead of
  // `II() * JJ()`, but should be faster overall since it outputs to the same file
  // for all of the inner 2 for loops instead of potentially switching files every
  // iteration.  This will also work better if need to limit number of open files
  // or if want to limit to a specific range of ranks...
  int exoid = output_region(rank)->get_database()->get_file_pointer();
  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      const auto &cell = get_cell(i, j);
      if (cell.rank() != rank) {
        continue;
      }

      auto *nb = cell.m_unitCell->m_region->get_node_blocks()[0];
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
      if (equivalence_nodes && (cell.has_neighbor_i() || cell.has_neighbor_j())) {
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

      auto start = cell.m_localNodeIdOffset + 1;
      auto count = cell.added_node_count();
      ex_put_partial_coord(exoid, start, count, coord_x.data(), coord_y.data(), coord_z.data());
    }
  }
  if (debug_level & 2) {
    util().progress("\tEnd");
  }
}

template <typename INT> void Grid::output_block_connectivity(int rank, INT /*dummy*/)
{
  // This ordering results in `parallel_size() * II() * JJ()` iterations instead of
  // `II() * JJ()`, but should be faster overall since it outputs to the same file
  // for all of the inner 2 for loops instead of potentially switching files every
  // iteration.  This will also work better if need to limit number of open files
  // or if want to limit to a specific range of ranks...
  std::vector<INT> connect;
  int              exoid = output_region(rank)->get_database()->get_file_pointer();
  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto &cell = get_cell(i, j);
      if (cell.rank() != rank) {
        continue;
      }

      auto node_map = generate_node_map(*this, cell, INT(0));
      auto blocks   = cell.m_unitCell->m_region->get_element_blocks();
      for (const auto *block : blocks) {
        block->get_field_data("connectivity_raw", connect);
        for (size_t k = 0; k < connect.size(); k++) {
          connect[k] = node_map[connect[k]];
        }
        auto start = cell.m_localElementIdOffset[block->name()] + 1;
        auto count = block->entity_count();
        auto id    = block->get_property("id").get_int();
        if (debug_level & 8) {
          fmt::print(stderr, "i, j, blk, id, start, count: {} {} {} {} {} {}\n", i, j,
                     block->name(), id, start, count);
        }
        ex_put_partial_conn(exoid, EX_ELEM_BLOCK, id, start, count, connect.data(), nullptr,
                            nullptr);
      }
      if (debug_level & 2) {
        util().progress(fmt::format("Generated Node Map / Output Connectivity for Cell({}, {})",
                                    cell.m_i, cell.m_j));
      }
    }
  }
}

template <typename INT> void Grid::output_node_map(int rank, INT /*dummy*/)
{
  // This ordering results in `parallel_size() * II() * JJ()` iterations instead of
  // `II() * JJ()`, but should be faster overall since it outputs to the same file
  // for all of the inner 2 for loops instead of potentially switching files every
  // iteration.  This will also work better if need to limit number of open files
  // or if want to limit to a specific range of ranks...
  int exoid = output_region(rank)->get_database()->get_file_pointer();
  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto &cell = get_cell(i, j);
      if (cell.rank() != rank) {
        continue;
      }

      auto start = cell.m_localNodeIdOffset + 1;
      auto count = cell.added_node_count();

      auto             gid = cell.m_globalNodeIdOffset + 1;
      std::vector<INT> map(count);
      std::iota(map.begin(), map.end(), gid);

      ex_put_partial_id_map(exoid, EX_NODE_MAP, start, count, map.data());

      if (debug_level & 2) {
        util().progress(fmt::format("Generated Node Map for Rank {}, Cell({}, {}): start {}, count "
                                    "{}, global_id_start {}\n",
                                    rank, cell.m_i, cell.m_j, start, count, gid));
      }
    }
  }
}

template <typename INT> void Grid::output_element_map(int rank, INT /*dummy*/)
{
  // This ordering results in `parallel_size() * II() * JJ()` iterations instead of
  // `II() * JJ()`, but should be faster overall since it outputs to the same file
  // for all of the inner 2 for loops instead of potentially switching files every
  // iteration.  This will also work better if need to limit number of open files
  // or if want to limit to a specific range of ranks...
  int exoid = output_region(rank)->get_database()->get_file_pointer();

  auto output_blocks = output_region(rank)->get_element_blocks();

  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto &cell = get_cell(i, j);
      if (cell.rank() != rank) {
        continue;
      }

      // This is the element block offset for the "single output file"
      // for the block being output For example, if the total mesh has
      // 3 blocks, with 100, 200, 100 elements, then the element block
      // offset would be 0, 100, 300 (Ioss::ElementBlock::get_offset())
      size_t global_id_offset = 0;

      for (const auto *output_element_block : output_blocks) {
        auto *block = cell.m_unitCell->m_region->get_element_block(output_element_block->name());
        if (block != nullptr) {

          auto             gid = cell.m_globalElementIdOffset[block->name()] + 1 + global_id_offset;
          auto             element_count = block->entity_count();
          std::vector<INT> map(element_count);

          std::iota(map.begin(), map.end(), gid);

          auto output_block_offset = output_element_block->get_offset();

          // This cells element block ids start this far into the portion of the map for this
          // element block
          auto local_offset = cell.m_localElementIdOffset[block->name()];

          auto start = output_block_offset + local_offset + 1;
          ex_put_partial_id_map(exoid, EX_ELEM_MAP, start, element_count, map.data());

          fmt::print("Rank {}: Cell({}, {}), Block {}, start {}, element_count {}, gid {}\n", rank,
                     i, j, block->name(), start, element_count, gid);
        }
        // If we were outputting a single file, then this element
        // block in that file would have this many elements.

        auto global_block_element_count =
            output_element_block->get_property("global_entity_count").get_int();
        global_id_offset += global_block_element_count;
      }
    }
  }
}

namespace {
  void handle_elements(Grid &grid)
  {
    // Not all unit cells have the same element blocks and the output
    // grid will contain the union of the element blocks on each unit
    // cell...
    //
    // While finalizing the cells, we will create this union for use in
    // the output region.  Would be quicker to do iteration on unit_cell
    // map which is smaller, but for now do it here and see if becomes
    // bottleneck.

    std::map<std::string, std::unique_ptr<Ioss::ElementBlock>> output_element_blocks;
    std::vector<std::map<std::string, size_t>> element_block_elem_count(grid.parallel_size());
    std::map<std::string, size_t>              global_element_block_elem_count;

    for (size_t j = 0; j < grid.JJ(); j++) {
      for (size_t i = 0; i < grid.II(); i++) {
        auto &cell = grid.get_cell(i, j);
        auto  rank = cell.rank();

        const auto &element_blocks = cell.m_unitCell->m_region->get_element_blocks();
        for (const auto *block : element_blocks) {
          auto &blk                         = block->name();
          cell.m_globalElementIdOffset[blk] = global_element_block_elem_count[blk];
          cell.m_localElementIdOffset[blk]  = element_block_elem_count[rank][blk];

          element_block_elem_count[rank][blk] += block->entity_count();
          global_element_block_elem_count[blk] += block->entity_count();

          if (debug_level & 8) {
            fmt::print("rank, i, j, blk, loffset, goffset, block_count: {}: {} {} {} {} {} {}\n",
                       rank, i, j, blk, cell.m_localElementIdOffset[blk],
                       cell.m_globalElementIdOffset[blk], element_block_elem_count[rank][blk]);
          }

          // Create output element block if does not exist yet...
          if (output_element_blocks.find(blk) == output_element_blocks.end()) {
            output_element_blocks.emplace(blk, new Ioss::ElementBlock(*block));
          }
        }
      }
    }

    // Calculate values needed to set the "global_entity_count" property on the output element
    // blocks.
    std::map<const std::string, int64_t> global_block_element_count;
    for (auto &blk : output_element_blocks) {
      for (int rank = 0; rank < grid.parallel_size(); rank++) {
        auto *block = blk.second.get();
        global_block_element_count[block->name()] += element_block_elem_count[rank][block->name()];
      }
    }

    // Define the element blocks in the output database...
    for (int rank = 0; rank < grid.parallel_size(); rank++) {
      for (auto &blk : output_element_blocks) {
        auto *block = new Ioss::ElementBlock(*blk.second.get());
        block->property_update("entity_count", element_block_elem_count[rank][block->name()]);
        block->property_update("global_entity_count", global_block_element_count[block->name()]);
        grid.output_region(rank)->add(block);
        if (debug_level & 8) {
          fmt::print("rank, blk, element_count: {}: {} {}\n", rank, block->name(),
                     block->entity_count());
        }
      }
    }
  }

  void handle_nodes(Grid &grid)
  {
    size_t              global_node_count = 0;
    std::vector<size_t> local_node_count(grid.parallel_size());

    for (size_t j = 0; j < grid.JJ(); j++) {
      for (size_t i = 0; i < grid.II(); i++) {
        auto &cell                = grid.get_cell(i, j);
        auto  rank                = cell.rank();
        cell.m_globalNodeIdOffset = global_node_count;
        cell.m_localNodeIdOffset  = local_node_count[rank];
        SMART_ASSERT(cell.m_unitCell->m_region != nullptr)(i)(j);

        auto new_nodes = cell.added_node_count();
        if (debug_level & 8) {
          fmt::print("rank: i, j, node_offset, added_nodes: {}: {} {} {} {}\n", rank, i, j,
                     local_node_count[rank], new_nodes);
        }
        local_node_count[rank] += new_nodes;
        global_node_count += new_nodes;
      }
    }
    // Define the output database node block...
    for (int i = 0; i < grid.parallel_size(); i++) {
      std::string block_name        = "nodeblock_1";
      int         spatial_dimension = 3;
      auto        block = new Ioss::NodeBlock(grid.output_region(i)->get_database(), block_name,
                                       local_node_count[i], spatial_dimension);
      block->property_add(Ioss::Property("id", 1));
      grid.output_region(i)->add(block);
    }
  }

  void set_coordinate_offsets(Grid &grid)
  {
    // All unit_cells have same X, Y (and Z) extent.  Only need X and Y
    auto x_range = grid.get_cell(0, 0).get_coordinate_range(Axis::X);
    auto y_range = grid.get_cell(0, 0).get_coordinate_range(Axis::Y);

    for (size_t j = 0; j < grid.JJ(); j++) {
      for (size_t i = 0; i < grid.II(); i++) {
        auto &cell  = grid.get_cell(i, j);
        cell.m_offX = (x_range.second - x_range.first) * (double)i;
        cell.m_offY = (y_range.second - y_range.first) * (double)j;

        cell.m_consistent = true;
        if (debug_level & 2) {
          grid.util().progress(fmt::format("\tCell({}, {})", i, j));
        }
      }
    }
  }

  template <typename INT>
  std::vector<INT> generate_node_map(Grid &grid, const Cell &cell, INT /*dummy*/)
  {
    // Generate a "map" from nodes in the input connectivity to the
    // output connectivity in global nodes.  If no neighbors, then
    // this would just be adding `cell.m_globalNodeIdOffset` to each
    // connectivity entry.

    // Size is node_count + 1 to handle the 1-based connectivity values.
    size_t cell_node_count = cell.m_unitCell->m_region->get_property("node_count").get_int();
    std::vector<INT> map(cell_node_count + 1);
    if (!equivalence_nodes || !(cell.has_neighbor_i() || cell.has_neighbor_j())) {
      std::iota(map.begin(), map.end(), cell.m_localNodeIdOffset);
    }
    else {
      // At least one neighboring cell.
      // Generate map for the "non-neighbored" nodes (not contiguous with a neighbor cell)
      auto categorized_nodes = cell.categorize_nodes();
      SMART_ASSERT(categorized_nodes.size() == cell_node_count)
      (categorized_nodes.size())(cell_node_count);
      INT offset = (INT)cell.m_globalNodeIdOffset + 1;
      for (size_t n = 0; n < cell_node_count; n++) {
        if (categorized_nodes[n] == 0) {
          map[n + 1] = offset++;
        }
      }
    }

    if (equivalence_nodes && cell.has_neighbor_i()) {
      const auto &neighbor = grid.get_cell(cell.m_i - 1, cell.m_j);
      // Get the neighbor cell...
      // iterate my unit cell's min_I_face() nodes to get index into map
      // At this index, set value to neighbor cells max_I_nodes() node
      for (size_t i = 0; i < cell.m_unitCell->min_I_face.size(); i++) {
        auto idx = cell.m_unitCell->min_I_face[i] + 1;
        auto val = neighbor.max_I_nodes[i];
        map[idx] = (INT)val;
      }

      // Can now clean out the neighbors max_I_nodes list since this
      // is the only cell that can use the data...
      Ioss::Utils::clear(neighbor.max_I_nodes);
    }

    if (equivalence_nodes && cell.has_neighbor_j()) {
      const auto &neighbor = grid.get_cell(cell.m_i, cell.m_j - 1);
      for (size_t i = 0; i < cell.m_unitCell->min_J_face.size(); i++) {
        auto idx = cell.m_unitCell->min_J_face[i] + 1;
        auto val = neighbor.max_J_nodes[i];
        map[idx] = (INT)val;
      }
      Ioss::Utils::clear(neighbor.max_J_nodes);
    }

    // Now that we have the node map for this cell, we need to save the mappings for the max_I and
    // max_J faces and max_I-max_J edge for use by later neighbors...
    // Check whether cell has neighbors on max_I or max_J faces...
    if (equivalence_nodes && (cell.m_i + 1 < grid.II())) {
      cell.max_I_nodes.resize(cell.m_unitCell->max_I_face.size());
      for (size_t i = 0; i < cell.m_unitCell->max_I_face.size(); i++) {
        auto idx            = cell.m_unitCell->max_I_face[i] + 1;
        auto val            = map[idx];
        cell.max_I_nodes[i] = val;
      }
      if (debug_level & 8) {
        fmt::print("\nCell {} {}\n", cell.m_i, cell.m_j);
        fmt::print("max_I_nodes: {}\n", fmt::join(cell.max_I_nodes, " "));
      }
    }

    if (equivalence_nodes && (cell.m_j + 1 < grid.JJ())) {
      cell.max_J_nodes.resize(cell.m_unitCell->max_J_face.size());
      for (size_t i = 0; i < cell.m_unitCell->max_J_face.size(); i++) {
        auto idx            = cell.m_unitCell->max_J_face[i] + 1;
        auto val            = map[idx];
        cell.max_J_nodes[i] = val;
      }
      if (debug_level & 8) {
        fmt::print("max_J_nodes: {}\n", fmt::join(cell.max_J_nodes, " "));
      }
    }
    if (debug_level & 8) {
      fmt::print("           MAP: {}\n", fmt::join(map, " "));
    }
    return map;
  }

  Ioss::PropertyManager parse_properties(SystemInterface &interFace, int int_size)
  {
    Ioss::PropertyManager properties;
    if (int_size == 8) {
      properties.add(Ioss::Property("INTEGER_SIZE_DB", 8));
      properties.add(Ioss::Property("INTEGER_SIZE_API", 8));
    }

    if (interFace.use_netcdf4()) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf4"));
    }

    if (interFace.use_netcdf5()) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf5"));
    }

    if (interFace.compression_level() > 0 || interFace.szip()) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf4"));
      properties.add(Ioss::Property("COMPRESSION_LEVEL", interFace.compression_level()));
      properties.add(Ioss::Property("COMPRESSION_SHUFFLE", true));
      if (interFace.szip()) {
        properties.add(Ioss::Property("COMPRESSION_METHOD", "szip"));
      }
      else if (interFace.zlib()) {
        properties.add(Ioss::Property("COMPRESSION_METHOD", "zlib"));
      }
    }
    return properties;
  }

} // namespace
