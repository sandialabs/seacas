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
  std::vector<INT>      generate_node_map(Grid &grid, const Cell &cell, Mode mode, INT /*dummy*/);
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

  categorize_processor_boundaries();
}

void Grid::categorize_processor_boundaries()
{
  // Now iterate the cells and tell each cell the rank of all neighboring cells...
  // boundary with its "left" or "lower" neighboring cell.

  for (size_t j = 0; j < JJ(); j++) {
    for (size_t i = 0; i < II(); i++) {
      auto &cell = get_cell(i, j);
      if (i > 0) {
        auto &left = get_cell(i - 1, j);
        cell.set_rank(Loc::L, left.rank(Loc::C));
        if (j > 0) {
          auto &BL = get_cell(i - 1, j - 1);
          cell.set_rank(Loc::BL, BL.rank(Loc::C));
        }
        if (j < JJ() - 1) {
          auto &TL = get_cell(i - 1, j + 1);
          cell.set_rank(Loc::TL, TL.rank(Loc::C));
        }
      }
      if (i < II() - 1) {
        auto &right = get_cell(i + 1, j);
        cell.set_rank(Loc::R, right.rank(Loc::C));
        if (j > 0) {
          auto &BR = get_cell(i + 1, j - 1);
          cell.set_rank(Loc::BR, BR.rank(Loc::C));
        }
        if (j < JJ() - 1) {
          auto &TR = get_cell(i + 1, j + 1);
          cell.set_rank(Loc::TR, TR.rank(Loc::C));
        }
      }
      if (j > 0) {
        auto &B = get_cell(i, j - 1);
        cell.set_rank(Loc::B, B.rank(Loc::C));
      }
      if (j < JJ() - 1) {
        auto &T = get_cell(i, j + 1);
        cell.set_rank(Loc::T, T.rank(Loc::C));
      }
    }
  }

  if (debug_level & 32) {
    for (size_t j = 0; j < JJ(); j++) {
      for (size_t i = 0; i < II(); i++) {
        const auto &cell  = get_cell(i, j);
        auto        left  = cell.processor_boundary(Loc::L) ? '<' : ' ';
        auto        below = cell.processor_boundary(Loc::B) ? '^' : ' ';
        fmt::print(" {}{}{}", left, cell.rank(Loc::C), below);
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
      if (cell.rank(Loc::C) != rank) {
        continue;
      }

      auto *nb = cell.m_unitCell->m_region->get_node_blocks()[0];
      nb->get_field_data("mesh_model_coordinates_x", coord_x);
      nb->get_field_data("mesh_model_coordinates_y", coord_y);
      nb->get_field_data("mesh_model_coordinates_z", coord_z);

      // Apply coordinate offsets to all nodes...
      if (cell.m_offX != 0.0) {
        std::for_each(coord_x.begin(), coord_x.end(), [&cell](double &d) { d += cell.m_offX; });
      }
      if (cell.m_offY != 0.0) {
        std::for_each(coord_y.begin(), coord_y.end(), [&cell](double &d) { d += cell.m_offY; });
      }

      // Filter coordinates down to only "new nodes"...
      if (equivalence_nodes && (cell.has_neighbor_i() || cell.has_neighbor_j())) {
        auto   mode              = parallel_size() > 1 ? Mode::PROCESSOR : Mode::GLOBAL;
        auto   categorized_nodes = cell.categorize_nodes(mode);
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
      auto count = cell.added_node_count(Mode::PROCESSOR);
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
      if (cell.rank(Loc::C) != rank) {
        continue;
      }

      auto node_map = generate_node_map(*this, cell, Mode::PROCESSOR, INT(0));
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
      if (cell.rank(Loc::C) != rank) {
        continue;
      }

      auto start = cell.m_localNodeIdOffset + 1;
      auto count = cell.added_node_count(Mode::PROCESSOR);

      if (parallel_size() == 1) {
        auto             gid = cell.m_globalNodeIdOffset + 1;
        std::vector<INT> map(count);
        std::iota(map.begin(), map.end(), gid);
        ex_put_partial_id_map(exoid, EX_NODE_MAP, start, count, map.data());
      }
      else {
        auto map = generate_node_map(*this, cell, Mode::GLOBAL, INT(0));
        ex_put_partial_id_map(exoid, EX_NODE_MAP, start, count, map.data());
      }

      if (debug_level & 2) {
        util().progress(fmt::format("Generated Node Map for Rank {}, Cell({}, {}): start {}, count "
                                    "{}\n",
                                    rank, cell.m_i, cell.m_j, start, count));
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
      if (cell.rank(Loc::C) != rank) {
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
        auto  rank = cell.rank(Loc::C);

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
        auto  rank                = cell.rank(Loc::C);
        cell.m_globalNodeIdOffset = global_node_count;
        cell.m_localNodeIdOffset  = local_node_count[rank];
        SMART_ASSERT(cell.m_unitCell->m_region != nullptr)(i)(j);

        auto new_global_nodes    = cell.added_node_count(Mode::GLOBAL);
        auto new_processor_nodes = cell.added_node_count(Mode::PROCESSOR);
        if (debug_level & 8) {
          fmt::print("rank: i, j, node_offset, added_nodes: {}: {} {} {} {} {}\n", rank, i, j,
                     local_node_count[rank], new_global_nodes, new_processor_nodes);
        }
        local_node_count[rank] += new_processor_nodes;
        global_node_count += new_global_nodes;
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
  std::vector<INT> generate_node_map(Grid &grid, const Cell &cell, Mode mode, INT /*dummy*/)
  {
    // Generate a "map" from nodes in the input connectivity to the
    // output connectivity in global nodes.  If no neighbors, then
    // this would just be adding `cell.m_globalNodeIdOffset` to each
    // connectivity entry.

    std::vector<INT> map = cell.generate_global_node_map(mode, INT(0));
    if (debug_level & 8) {
      fmt::print("           MAP: {}\n", fmt::join(map, " "));
    }

    // Now that we have the node map for this cell, we need to save
    // the mappings for the max_I and max_J faces and max_I-max_J edge
    // for use by later neighbors...  Check whether cell has neighbors
    // on max_I or max_J faces...
    if (equivalence_nodes && (cell.m_i + 1 < grid.II())) {
      const auto &neighbor = grid.get_cell(cell.m_i + 1, cell.m_j);
      cell.populate_neighbor_min_i(map, neighbor);
    }

    if (equivalence_nodes && (cell.m_j + 1 < grid.JJ())) {
      const auto &neighbor = grid.get_cell(cell.m_i, cell.m_j + 1);
      cell.populate_neighbor_min_j(map, neighbor);
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
