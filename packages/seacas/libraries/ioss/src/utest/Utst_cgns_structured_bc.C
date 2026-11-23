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
#include "Ioss_ElementBlock.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_PropertyManager.h"
#include "Ioss_Region.h"
#include "Ioss_ScopeGuard.h"
#include "Ioss_SideBlock.h"
#include "Ioss_SideSet.h"
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

void create_unstructured_element_file(const std::vector<cgsize_t> &connectivity)
{
  int file = create_file_with_base();

  cgsize_t size[3] = {8, 1, 0};
  int      zone    = 0;
  cgns_check(cg_zone_write(file, 1, "hex", size, CGNS_ENUMV(Unstructured), &zone));

  int section = 0;
  cgns_check(cg_section_write(file, 1, zone, "hexes", CGNS_ENUMV(HEXA_8), 1, 1, 0,
                              connectivity.data(), &section));
  cgns_check(cg_close(file));
}

void create_mixed_unstructured_element_file()
{
  int file = create_file_with_base();

  cgsize_t size[3] = {8, 1, 0};
  int      zone    = 0;
  cgns_check(cg_zone_write(file, 1, "mixed", size, CGNS_ENUMV(Unstructured), &zone));

  std::vector<cgsize_t> connectivity = {CGNS_ENUMV(HEXA_8), 1, 2, 3, 4, 5, 6, 7, 8};
  int                  section      = 0;
  cgns_check(cg_poly_section_write(file, 1, zone, "mixed", CGNS_ENUMV(MIXED), 1, 1, 0,
                                   connectivity.data(), nullptr, &section));
  cgns_check(cg_close(file));
}

void create_unstructured_side_file(const std::vector<cgsize_t> &face_connectivity,
                                   const std::vector<cgsize_t> &parent_data)
{
  int file = create_file_with_base();

  int family = 0;
  cgns_check(cg_family_write(file, 1, "wall", &family));
  int family_bc = 0;
  cgns_check(cg_fambc_write(file, 1, family, "wall", CGNS_ENUMV(BCWall), &family_bc));

  cgsize_t size[3] = {8, 1, 1};
  int      zone    = 0;
  cgns_check(cg_zone_write(file, 1, "hex", size, CGNS_ENUMV(Unstructured), &zone));

  cgsize_t point_range[2] = {2, 2};
  int      boco           = 0;
  cgns_check(cg_boco_write(file, 1, zone, "wall", CGNS_ENUMV(BCWall), CGNS_ENUMV(PointRange), 2,
                           point_range, &boco));
  cgns_check(cg_goto(file, 1, "Zone_t", zone, "ZoneBC_t", 1, "BC_t", boco, "end"));
  cgns_check(cg_famname_write("wall"));

  std::vector<cgsize_t> hex_connectivity = {1, 2, 3, 4, 5, 6, 7, 8};
  int                  section          = 0;
  cgns_check(cg_section_write(file, 1, zone, "hexes", CGNS_ENUMV(HEXA_8), 1, 1, 0,
                              hex_connectivity.data(), &section));
  cgns_check(cg_section_write(file, 1, zone, "faces", CGNS_ENUMV(QUAD_4), 2, 2, 1,
                              face_connectivity.data(), &section));
  cgns_check(cg_parent_data_write(file, 1, zone, section, parent_data.data()));
  cgns_check(cg_close(file));
}

void create_mixed_unstructured_side_file()
{
  int file = create_file_with_base();

  int family = 0;
  cgns_check(cg_family_write(file, 1, "wall", &family));
  int family_bc = 0;
  cgns_check(cg_fambc_write(file, 1, family, "wall", CGNS_ENUMV(BCWall), &family_bc));

  cgsize_t size[3] = {8, 1, 1};
  int      zone    = 0;
  cgns_check(cg_zone_write(file, 1, "hex", size, CGNS_ENUMV(Unstructured), &zone));

  cgsize_t point_range[2] = {2, 2};
  int      boco           = 0;
  cgns_check(cg_boco_write(file, 1, zone, "wall", CGNS_ENUMV(BCWall), CGNS_ENUMV(PointRange), 2,
                           point_range, &boco));
  cgns_check(cg_goto(file, 1, "Zone_t", zone, "ZoneBC_t", 1, "BC_t", boco, "end"));
  cgns_check(cg_famname_write("wall"));

  std::vector<cgsize_t> hex_connectivity = {1, 2, 3, 4, 5, 6, 7, 8};
  int                  section          = 0;
  cgns_check(cg_section_write(file, 1, zone, "hexes", CGNS_ENUMV(HEXA_8), 1, 1, 0,
                              hex_connectivity.data(), &section));

  std::vector<cgsize_t> face_connectivity = {CGNS_ENUMV(QUAD_4), 1, 2, 3, 4};
  cgns_check(cg_poly_section_write(file, 1, zone, "faces", CGNS_ENUMV(MIXED), 2, 2, 1,
                                   face_connectivity.data(), nullptr, &section));
  cgsize_t parent_data[4] = {1, 0, 1, 0};
  cgns_check(cg_parent_data_write(file, 1, zone, section, parent_data));
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

std::vector<int64_t> read_element_connectivity()
{
  auto region = open_region();
  auto blocks = region->get_element_blocks();
  REQUIRE(blocks.size() == 1);

  std::vector<int64_t> connectivity;
  blocks[0]->get_field_data("connectivity_raw", connectivity);
  return connectivity;
}

std::vector<int64_t> read_side_element_side()
{
  auto region = open_region();
  auto side_sets = region->get_sidesets();
  REQUIRE(side_sets.size() == 1);
  auto side_blocks = side_sets[0]->get_side_blocks();
  REQUIRE(side_blocks.size() == 1);

  std::vector<int64_t> element_side;
  side_blocks[0]->get_field_data("element_side", element_side);
  return element_side;
}

template <typename Callable>
void check_throws_message_containing(Callable &&call, const std::string &substring)
{
  try {
    call();
    FAIL("Expected std::runtime_error");
  }
  catch (const std::runtime_error &error) {
    CHECK(std::string(error.what()).find(substring) != std::string::npos);
  }
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

  SECTION("ElementConnectivity matching section topology is accepted")
  {
    create_unstructured_element_file({1, 2, 3, 4, 5, 6, 7, 8});
    std::vector<int64_t> connectivity;
    REQUIRE_NOTHROW(connectivity = read_element_connectivity());
    REQUIRE(connectivity.size() == 8);
    CHECK(connectivity[0] == 1);
    CHECK(connectivity[7] == 8);
  }

  SECTION("unsupported mixed-section connectivity size is rejected before cg_elements_read")
  {
    create_mixed_unstructured_element_file();
    check_throws_message_containing([]() { read_element_connectivity(); },
                                    "element connectivity entries");
  }

  SECTION("sideblock connectivity and parent-data sizes matching the section are accepted")
  {
    create_unstructured_side_file({1, 2, 3, 4}, {1, 0, 1, 0});
    std::vector<int64_t> element_side;
    REQUIRE_NOTHROW(element_side = read_side_element_side());
    REQUIRE(element_side.size() == 2);
    CHECK(element_side[0] == 1);
    CHECK(element_side[1] == 5);
  }

  SECTION("out-of-range ParentData face id is rejected before face-table lookup")
  {
    create_unstructured_side_file({1, 2, 3, 4}, {1, 0, 7, 0});
    check_throws_message_containing([]() { read_side_element_side(); }, "ParentData face id");
  }

  SECTION("unsupported mixed-side connectivity size is rejected before cg_elements_read")
  {
    create_mixed_unstructured_side_file();
    check_throws_message_containing([]() { read_side_element_side(); },
                                    "element connectivity entries");
  }
}
