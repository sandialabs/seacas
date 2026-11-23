// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// Ioex connectivity reads must reject files whose stored per-entry
// connectivity count does not match the IOSS topology-derived buffer size.

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>

#include "Ionit_Initializer.h"
#include "Ioss_ConcreteVariableType.h"
#include "Ioss_DBUsage.h"
#include "Ioss_ElementBlock.h"
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
const char *filename = "ioex-connectivity-count.exo";

void create_element_block_file(const char *topology, int nodes_per_element)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  REQUIRE(exoid >= 0);

  REQUIRE(ex_put_init(exoid, "connectivity count", 3, nodes_per_element, 1, 1, 0, 0) == EX_NOERR);
  REQUIRE(ex_put_block(exoid, EX_ELEM_BLOCK, 7, topology, 1, nodes_per_element, 0, 0, 0) ==
          EX_NOERR);

  std::vector<int> connect(nodes_per_element);
  for (int i = 0; i < nodes_per_element; i++) {
    connect[i] = i + 1;
  }
  REQUIRE(ex_put_conn(exoid, EX_ELEM_BLOCK, 7, connect.data(), nullptr, nullptr) == EX_NOERR);
  REQUIRE(ex_close(exoid) == EX_NOERR);
}

std::unique_ptr<Ioss::Region> open_region()
{
  Ioss::PropertyManager properties;
  properties.add(Ioss::Property("INTEGER_SIZE_DB", 4));
  properties.add(Ioss::Property("INTEGER_SIZE_API", 4));

  auto *db_io = new Ioex::DatabaseIO(nullptr, filename, Ioss::READ_MODEL,
                                     Ioss::ParallelUtils::comm_world(), properties);
  auto  region = std::make_unique<Ioss::Region>(db_io);
  REQUIRE(db_io->ok());
  return region;
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

TEST_CASE("Ioex validates element connectivity count against topology buffer size")
{
  Ioss::Init::Initializer     init_db;
  Ioss::StorageInitializer    initialize_storage;

  SECTION("matching topology and connectivity counts read successfully")
  {
    create_element_block_file("BAR2", 2);
    auto region = open_region();

    const auto &blocks = region->get_element_blocks();
    REQUIRE(blocks.size() == 1);

    std::vector<int> connectivity;
    REQUIRE_NOTHROW(blocks[0]->get_field_data("connectivity_raw", connectivity));
    REQUIRE(connectivity.size() == 2);
    CHECK(connectivity[0] == 1);
    CHECK(connectivity[1] == 2);
  }

  SECTION("stored connectivity count larger than topology count throws before reading")
  {
    create_element_block_file("BAR2", 3);
    auto region = open_region();

    const auto &blocks = region->get_element_blocks();
    REQUIRE(blocks.size() == 1);
    CHECK(blocks[0]->topology()->number_nodes() == 2);

    std::vector<int> connectivity;
    CHECK_THROWS_AS(blocks[0]->get_field_data("connectivity_raw", connectivity), std::runtime_error);
  }
}
