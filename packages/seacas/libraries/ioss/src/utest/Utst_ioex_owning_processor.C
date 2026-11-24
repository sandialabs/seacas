// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// Ioex owning_processor and commset entity_processor reads must validate
// file-supplied communication-map entity ids before indexing node buffers or
// local-to-global maps.

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>

#include "Ionit_Initializer.h"
#include "Ioss_CommSet.h"
#include "Ioss_DBUsage.h"
#include "Ioss_NodeBlock.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"
#include "Ioss_PropertyManager.h"
#include "Ioss_Region.h"
#include "Ioss_ScopeGuard.h"
#include "exodus/Ioex_DatabaseIO.h"

#include <exodusII.h>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

namespace {
  const char *filename = "ioex-owning-processor-cmap.exo";

  int parallel_rank()
  {
    Ioss::ParallelUtils util(Ioss::ParallelUtils::comm_world());
    return util.parallel_rank();
  }

  int parallel_size()
  {
    Ioss::ParallelUtils util(Ioss::ParallelUtils::comm_world());
    return util.parallel_size();
  }

  void barrier()
  {
    Ioss::ParallelUtils util(Ioss::ParallelUtils::comm_world());
    util.barrier();
  }

  std::string rank_filename()
  {
    return Ioss::Utils::decode_filename(filename, parallel_rank(), parallel_size());
  }

  void create_parallel_file(int comm_node_id, int comm_elem_id = 1)
  {
    int cpu_word_size = 8;
    int io_word_size  = 8;
    int exoid = ex_create(rank_filename().c_str(), EX_CLOBBER, &cpu_word_size, &io_word_size);
    REQUIRE(exoid >= 0);

    REQUIRE(ex_put_init(exoid, "owning processor cmap", 1, 2, 1, 1, 0, 0) == EX_NOERR);
    REQUIRE(ex_put_init_info(exoid, parallel_size(), 1, "p") == EX_NOERR);
    REQUIRE(ex_put_init_global(exoid, parallel_size() * 2, parallel_size(), 1, 0, 0) == EX_NOERR);
    REQUIRE(ex_put_loadbal_param(exoid, 1, 1, 0, 0, 1, 1, 1, parallel_rank()) == EX_NOERR);

    int node_cmap_id    = 1;
    int node_cmap_count = 1;
    int elem_cmap_id    = 1;
    int elem_cmap_count = 1;
    REQUIRE(ex_put_cmap_params(exoid, &node_cmap_id, &node_cmap_count, &elem_cmap_id,
                               &elem_cmap_count, parallel_rank()) == EX_NOERR);

    int internal_nodes[1] = {1};
    int border_nodes[1]   = {2};
    REQUIRE(ex_put_processor_node_maps(exoid, internal_nodes, border_nodes, nullptr,
                                       parallel_rank()) == EX_NOERR);

    int internal_elems[1] = {0};
    int border_elems[1]   = {1};
    REQUIRE(ex_put_processor_elem_maps(exoid, internal_elems, border_elems, parallel_rank()) ==
            EX_NOERR);

    REQUIRE(ex_put_block(exoid, EX_ELEM_BLOCK, 7, "BAR2", 1, 2, 0, 0, 0) == EX_NOERR);
    int elem_conn[2] = {1, 2};
    REQUIRE(ex_put_conn(exoid, EX_ELEM_BLOCK, 7, elem_conn, nullptr, nullptr) == EX_NOERR);

    int node_map[2] = {parallel_rank() * 2 + 1, parallel_rank() * 2 + 2};
    REQUIRE(ex_put_id_map(exoid, EX_NODE_MAP, node_map) == EX_NOERR);

    int elem_map[1] = {parallel_rank() + 1};
    REQUIRE(ex_put_id_map(exoid, EX_ELEM_MAP, elem_map) == EX_NOERR);

    int comm_nodes[1] = {comm_node_id};
    int comm_procs[1] = {1 - parallel_rank()};
    REQUIRE(ex_put_node_cmap(exoid, node_cmap_id, comm_nodes, comm_procs, parallel_rank()) ==
            EX_NOERR);

    int comm_elems[1] = {comm_elem_id};
    int comm_sides[1] = {1};
    REQUIRE(ex_put_elem_cmap(exoid, elem_cmap_id, comm_elems, comm_sides, comm_procs,
                             parallel_rank()) == EX_NOERR);
    REQUIRE(ex_close(exoid) == EX_NOERR);
  }

  std::unique_ptr<Ioss::Region> open_region()
  {
    Ioss::PropertyManager properties;
    properties.add(Ioss::Property("INTEGER_SIZE_DB", 4));
    properties.add(Ioss::Property("INTEGER_SIZE_API", 4));

    auto *db_io  = new Ioex::DatabaseIO(nullptr, filename, Ioss::READ_MODEL,
                                        Ioss::ParallelUtils::comm_world(), properties);
    auto  region = std::make_unique<Ioss::Region>(db_io);
    REQUIRE(db_io->ok());
    return region;
  }

  Ioss::NodeBlock *node_block(Ioss::Region &region)
  {
    const auto &blocks = region.get_node_blocks();
    REQUIRE(blocks.size() == 1);
    return blocks[0];
  }

  Ioss::CommSet *node_commset(Ioss::Region &region)
  {
    Ioss::CommSet *commset = region.get_commset("commset_node");
    REQUIRE(commset != nullptr);
    return commset;
  }

  Ioss::CommSet *side_commset(Ioss::Region &region)
  {
    Ioss::CommSet *commset = region.get_commset("commset_side");
    REQUIRE(commset != nullptr);
    return commset;
  }
} // namespace

int main(int argc, char *argv[])
{
#ifdef SEACAS_HAVE_MPI
  MPI_Init(&argc, &argv);
  ON_BLOCK_EXIT(MPI_Finalize);
#endif

  Catch::Session session;
  int            return_code = session.applyCommandLine(argc, argv);
  if (return_code == 0) {
    return_code = session.run();
  }

  return return_code;
}

TEST_CASE("Ioex validates communication-map node ids before owning_processor indexing")
{
  Ioss::Init::Initializer init_db;
  REQUIRE(parallel_size() == 2);

  SECTION("valid communication-map node ids read owning processors and mapped entity processors")
  {
    create_parallel_file(2);
    barrier();
    auto region = open_region();

    std::vector<int> owning_processors;
    REQUIRE_NOTHROW(node_block(*region)->get_field_data("owning_processor", owning_processors));
    REQUIRE(owning_processors.size() == 2);
    CHECK(owning_processors[0] == parallel_rank());
    CHECK(owning_processors[1] == 0);

    std::vector<int> entity_processors;
    REQUIRE_NOTHROW(node_commset(*region)->get_field_data("entity_processor", entity_processors));
    REQUIRE(entity_processors.size() == 2);
    CHECK(entity_processors[0] == parallel_rank() * 2 + 2);
    CHECK(entity_processors[1] == 1 - parallel_rank());
  }

  SECTION("zero communication-map node id is rejected before owning_processor indexing")
  {
    create_parallel_file(0);
    barrier();
    auto region = open_region();

    std::vector<int> owning_processors;
    CHECK_THROWS_AS(node_block(*region)->get_field_data("owning_processor", owning_processors),
                    std::runtime_error);
  }

  SECTION("high communication-map node id is rejected before owning_processor indexing")
  {
    create_parallel_file(3);
    barrier();
    auto region = open_region();

    std::vector<int> owning_processors;
    CHECK_THROWS_AS(node_block(*region)->get_field_data("owning_processor", owning_processors),
                    std::runtime_error);
  }

  SECTION("high communication-map node id is rejected before node-map indexing")
  {
    create_parallel_file(3);
    barrier();
    auto region = open_region();

    std::vector<int> entity_processors;
    CHECK_THROWS_AS(node_commset(*region)->get_field_data("entity_processor", entity_processors),
                    std::runtime_error);
  }

  SECTION("high communication-map element id is rejected before element-map indexing")
  {
    create_parallel_file(2, 2);
    barrier();
    auto region = open_region();

    std::vector<int> entity_processors;
    CHECK_THROWS_AS(side_commset(*region)->get_field_data("entity_processor", entity_processors),
                    std::runtime_error);
  }
}
