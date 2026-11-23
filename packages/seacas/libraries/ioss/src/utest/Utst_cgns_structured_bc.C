// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// CGNS structured boundary-condition reads must validate the point-set shape
// before reading into a fixed PointRange buffer.

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>

#include "Ioss_CodeTypes.h"
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

int create_file_with_base()
{
  int file = 0;
  cgns_check(cg_open(filename, CG_MODE_WRITE, &file));

  int base = 0;
  cgns_check(cg_base_write(file, "Base", 3, 3, &base));
  return file;
}

void create_structured_file(CGNS_ENUMT(PointSetType_t) point_set_type, cgsize_t point_count)
{
  int file = create_file_with_base();

  cgsize_t size[9] = {3, 3, 3, 2, 2, 2, 0, 0, 0};
  int      zone    = 0;
  cgns_check(cg_zone_write(file, 1, "Zone1", size, CGNS_ENUMV(Structured), &zone));

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
  cgns_check(cg_boco_write(file, 1, zone, "wall", CGNS_ENUMV(BCWall), point_set_type,
                           point_count, point_set.data(), &boco));
  cgns_check(cg_close(file));
}

void create_transient_file(int declared_steps, const std::vector<double> &time_values)
{
  int file = create_file_with_base();
  cgns_check(cg_biter_write(file, 1, "TimeIterValues", declared_steps));
  cgns_check(cg_goto(file, 1, "BaseIterativeData_t", 1, "end"));
  cgsize_t time_value_count = time_values.size();
  cgns_check(cg_array_write("TimeValues", CGNS_ENUMV(RealDouble), 1, &time_value_count,
                            time_values.data()));
  cgns_check(cg_close(file));
}

void create_unstructured_adjacency_file(cgsize_t point, cgsize_t donor)
{
  int file = create_file_with_base();

  cgsize_t size[3] = {2, 0, 0};
  int      zone1   = 0;
  int      zone2   = 0;
  cgns_check(cg_zone_write(file, 1, "donor", size, CGNS_ENUMV(Unstructured), &zone1));
  cgns_check(cg_zone_write(file, 1, "receiver", size, CGNS_ENUMV(Unstructured), &zone2));

  int conn = 0;
  cgns_check(cg_conn_write(file, 1, zone2, "receiver_to_donor", CGNS_ENUMV(Vertex),
                           CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointList), 1, &point, "donor",
                           CGNS_ENUMV(Unstructured), CGNS_ENUMV(PointListDonor),
                           CGNS_ENUMV(DataTypeNull), 1, &donor, &conn));
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

std::vector<double> read_step_times()
{
  Ioss::PropertyManager properties;
  auto *db_io = new Iocgns::DatabaseIO(nullptr, filename, Ioss::READ_MODEL,
                                       Ioss::ParallelUtils::comm_world(), properties);
  REQUIRE(db_io->ok());
  return db_io->get_db_step_times();
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

TEST_CASE("Iocgns validates CGNS metadata before bounded reads")
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

  SECTION("TimeValues matching NumberOfSteps is accepted")
  {
    create_transient_file(2, {1.0, 2.0});
    std::vector<double> times;
    REQUIRE_NOTHROW(times = read_step_times());
    REQUIRE(times.size() == 2);
    CHECK(times[0] == 1.0);
    CHECK(times[1] == 2.0);
  }

  SECTION("oversized TimeValues array is rejected before reading into the timestep buffer")
  {
    create_transient_file(1, {1.0, 2.0, 3.0});
    CHECK_THROWS_AS(read_step_times(), std::runtime_error);
  }

  SECTION("valid unstructured PointList adjacency is accepted")
  {
    create_unstructured_adjacency_file(1, 2);
    CHECK_NOTHROW(open_region());
  }

  SECTION("out-of-range PointList entry is rejected before mapping block nodes")
  {
    create_unstructured_adjacency_file(3, 1);
    CHECK_THROWS_AS(open_region(), std::runtime_error);
  }

  SECTION("out-of-range PointListDonor entry is rejected before reading the donor map")
  {
    create_unstructured_adjacency_file(1, 3);
    CHECK_THROWS_AS(open_region(), std::runtime_error);
  }
}
