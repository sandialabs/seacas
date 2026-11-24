// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// IOSS element-block bounding-box calculation must validate raw connectivity
// node ids before using them as node-array indices.

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>

#include "Ionit_Initializer.h"
#include "Ioss_BoundingBox.h"
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
  const char *filename = "ioex-bounding-box-connectivity.exo";

  void create_file_with_connectivity(const std::vector<int> &connectivity)
  {
    int cpu_word_size = 8;
    int io_word_size  = 8;
    int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
    REQUIRE(exoid >= 0);

    REQUIRE(ex_put_init(exoid, "bounding box connectivity", 3, 4, 1, 1, 0, 0) == EX_NOERR);

    double x[4] = {0.0, 1.0, 1.0, 0.0};
    double y[4] = {0.0, 0.0, 1.0, 1.0};
    double z[4] = {0.0, 0.0, 0.0, 0.0};
    REQUIRE(ex_put_coord(exoid, x, y, z) == EX_NOERR);

    REQUIRE(ex_put_block(exoid, EX_ELEM_BLOCK, 7, "QUAD4", 1, 4, 0, 0, 0) == EX_NOERR);
    REQUIRE(ex_put_conn(exoid, EX_ELEM_BLOCK, 7, connectivity.data(), nullptr, nullptr) ==
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

TEST_CASE("Ioex element-block bounding boxes validate connectivity node ids")
{
  Ioss::Init::Initializer init_db;

  SECTION("valid connectivity produces the expected bounding box")
  {
    create_file_with_connectivity({1, 2, 3, 4});
    auto region = open_region();

    const auto &blocks = region->get_element_blocks();
    REQUIRE(blocks.size() == 1);

    Ioss::AxisAlignedBoundingBox bbox;
    REQUIRE_NOTHROW(bbox = blocks[0]->get_bounding_box());
    CHECK(bbox.xmin == 0.0);
    CHECK(bbox.xmax == 1.0);
    CHECK(bbox.ymin == 0.0);
    CHECK(bbox.ymax == 1.0);
  }

  SECTION("zero connectivity id is rejected before indexing")
  {
    create_file_with_connectivity({1, 2, 0, 4});
    auto region = open_region();

    const auto &blocks = region->get_element_blocks();
    REQUIRE(blocks.size() == 1);
    CHECK_THROWS_AS(blocks[0]->get_bounding_box(), std::runtime_error);
  }

  SECTION("high connectivity id is rejected before indexing")
  {
    create_file_with_connectivity({1, 2, 5, 4});
    auto region = open_region();

    const auto &blocks = region->get_element_blocks();
    REQUIRE(blocks.size() == 1);
    CHECK_THROWS_AS(blocks[0]->get_bounding_box(), std::runtime_error);
  }
}
