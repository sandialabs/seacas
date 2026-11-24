// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// IOSS block-adjacency calculation must validate raw connectivity node ids
// before using them as node-array indices.

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>

#include "Ionit_Initializer.h"
#include "Ioss_DBUsage.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"
#include "Ioss_PropertyManager.h"
#include "Ioss_Region.h"
#include "Ioss_ScopeGuard.h"
#include "exodus/Ioex_DatabaseIO.h"

#include <algorithm>
#include <exodusII.h>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

namespace {
  const char *filename = "ioex-block-adjacency-connectivity.exo";

  void create_two_block_file(const std::vector<int> &block1_connectivity,
                             const std::vector<int> &block2_connectivity)
  {
    int cpu_word_size = 8;
    int io_word_size  = 8;
    int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
    REQUIRE(exoid >= 0);

    REQUIRE(ex_put_init(exoid, "block adjacency connectivity", 2, 3, 2, 2, 0, 0) == EX_NOERR);

    double x[3] = {0.0, 1.0, 2.0};
    double y[3] = {0.0, 0.0, 0.0};
    REQUIRE(ex_put_coord(exoid, x, y, nullptr) == EX_NOERR);

    REQUIRE(ex_put_block(exoid, EX_ELEM_BLOCK, 7, "BAR2", 1, 2, 0, 0, 0) == EX_NOERR);
    REQUIRE(ex_put_conn(exoid, EX_ELEM_BLOCK, 7, block1_connectivity.data(), nullptr, nullptr) ==
            EX_NOERR);

    REQUIRE(ex_put_block(exoid, EX_ELEM_BLOCK, 8, "BAR2", 1, 2, 0, 0, 0) == EX_NOERR);
    REQUIRE(ex_put_conn(exoid, EX_ELEM_BLOCK, 8, block2_connectivity.data(), nullptr, nullptr) ==
            EX_NOERR);

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

  bool contains_name(const Ioss::NameList &names, const std::string &name)
  {
    return std::find(names.begin(), names.end(), name) != names.end();
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

TEST_CASE("Ioex block adjacency validates connectivity node ids")
{
  Ioss::Init::Initializer init_db;

  SECTION("valid shared-node connectivity reports adjacent blocks")
  {
    create_two_block_file({1, 2}, {2, 3});
    auto region = open_region();

    const auto &blocks = region->get_element_blocks();
    REQUIRE(blocks.size() == 2);

    Ioss::NameList first_adjacencies;
    Ioss::NameList second_adjacencies;
    REQUIRE_NOTHROW(first_adjacencies = blocks[0]->get_block_adjacencies());
    REQUIRE_NOTHROW(second_adjacencies = blocks[1]->get_block_adjacencies());

    CHECK(contains_name(first_adjacencies, blocks[1]->name()));
    CHECK(contains_name(second_adjacencies, blocks[0]->name()));
  }

  SECTION("zero connectivity id is rejected before indexing")
  {
    create_two_block_file({1, 0}, {2, 3});
    auto region = open_region();

    const auto &blocks = region->get_element_blocks();
    REQUIRE(blocks.size() == 2);
    CHECK_THROWS_AS(blocks[0]->get_block_adjacencies(), std::runtime_error);
  }

  SECTION("high connectivity id is rejected before indexing")
  {
    create_two_block_file({1, 4}, {2, 3});
    auto region = open_region();

    const auto &blocks = region->get_element_blocks();
    REQUIRE(blocks.size() == 2);
    CHECK_THROWS_AS(blocks[0]->get_block_adjacencies(), std::runtime_error);
  }
}
