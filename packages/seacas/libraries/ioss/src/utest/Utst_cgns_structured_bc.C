// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// CGNS structured boundary-condition reads must validate the point-set shape
// before reading into a fixed PointRange buffer.

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>

#include "Ionit_Initializer.h"
#include "Ioss_DBUsage.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_PropertyManager.h"
#include "Ioss_Region.h"
#include "Ioss_ScopeGuard.h"
#include "cgns/Iocgns_DatabaseIO.h"

#include <cgnslib.h>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

namespace {
const char *filename = "iocgns-structured-bc.cgns";

void cgns_check(int error)
{
  REQUIRE(error == CG_OK);
}

void create_structured_file(CGNS_ENUMT(PointSetType_t) point_set_type, cgsize_t point_count)
{
  int file = 0;
  cgns_check(cg_open(filename, CG_MODE_WRITE, &file));

  int base = 0;
  cgns_check(cg_base_write(file, "Base", 3, 3, &base));

  cgsize_t size[9] = {3, 3, 3, 2, 2, 2, 0, 0, 0};
  int      zone    = 0;
  cgns_check(cg_zone_write(file, base, "Zone1", size, CGNS_ENUMV(Structured), &zone));

  std::vector<cgsize_t> point_set;
  if (point_set_type == CGNS_ENUMV(PointRange)) {
    point_set = {1, 1, 1, 1, 3, 3};
  }
  else {
    point_set.resize(point_count * 3);
    for (cgsize_t i = 0; i < point_count; i++) {
      point_set[3 * i + 0] = 1;
      point_set[3 * i + 1] = i + 1;
      point_set[3 * i + 2] = 1;
    }
  }

  int boco = 0;
  cgns_check(cg_boco_write(file, base, zone, "wall", CGNS_ENUMV(BCWall), point_set_type,
                           point_count, point_set.data(), &boco));
  cgns_check(cg_close(file));
}

std::unique_ptr<Ioss::Region> open_region()
{
  Ioss::PropertyManager properties;
  auto *db_io = new Iocgns::DatabaseIO(nullptr, filename, Ioss::READ_MODEL,
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

TEST_CASE("Iocgns validates structured boundary-condition point-set shape")
{
  Ioss::Init::Initializer init_db;

  SECTION("PointRange with two points is accepted")
  {
    create_structured_file(CGNS_ENUMV(PointRange), 2);
    CHECK_NOTHROW(open_region());
  }

  SECTION("PointList is rejected before reading into a PointRange buffer")
  {
    create_structured_file(CGNS_ENUMV(PointList), 3);
    CHECK_THROWS_AS(open_region(), std::runtime_error);
  }

  SECTION("single-entry PointList is rejected before reading into a PointRange buffer")
  {
    create_structured_file(CGNS_ENUMV(PointList), 1);
    CHECK_THROWS_AS(open_region(), std::runtime_error);
  }
}
