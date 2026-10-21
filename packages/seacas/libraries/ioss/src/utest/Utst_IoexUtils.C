// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catch2/catch_test_macros.hpp>
#include <exodusII.h>
#include <netcdf.h>
#include <string>

#include "Ioex_Utils.h"

namespace {
const char *filename = "ioex-utils-attrs.exo";

void require_nc(int status) { REQUIRE(status == NC_NOERR); }

int create_file()
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  REQUIRE(exoid >= 0);
  REQUIRE(ex_put_init(exoid, "ioex utils attributes", 2, 0, 0, 0, 0, 0) == EX_NOERR);
  REQUIRE(ex_close(exoid) == EX_NOERR);
  return 0;
}

int open_file(int mode = EX_READ)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, mode, &cpu_word_size, &io_word_size, &version);
  REQUIRE(exoid >= 0);
  return exoid;
}

void write_processor_info_int(size_t len)
{
  int ncid;
  require_nc(nc_open(filename, NC_WRITE, &ncid));
  require_nc(nc_redef(ncid));
  int values[4] = {2, 1, 99, 100};
  require_nc(nc_put_att_int(ncid, NC_GLOBAL, "processor_info", NC_INT, len, values));
  require_nc(nc_enddef(ncid));
  require_nc(nc_close(ncid));
}

void write_processor_info_double()
{
  int ncid;
  require_nc(nc_open(filename, NC_WRITE, &ncid));
  require_nc(nc_redef(ncid));
  double value = 2.0;
  require_nc(nc_put_att_double(ncid, NC_GLOBAL, "processor_info", NC_DOUBLE, 1, &value));
  require_nc(nc_enddef(ncid));
  require_nc(nc_close(ncid));
}

void write_last_time_double(size_t len)
{
  int ncid;
  require_nc(nc_open(filename, NC_WRITE, &ncid));
  require_nc(nc_redef(ncid));
  double values[3] = {1.0, 2.0, 3.0};
  require_nc(nc_put_att_double(ncid, NC_GLOBAL, "last_written_time", NC_DOUBLE, len, values));
  require_nc(nc_enddef(ncid));
  require_nc(nc_close(ncid));
}

void write_last_time_int()
{
  int ncid;
  require_nc(nc_open(filename, NC_WRITE, &ncid));
  require_nc(nc_redef(ncid));
  int value = 1;
  require_nc(nc_put_att_int(ncid, NC_GLOBAL, "last_written_time", NC_INT, 1, &value));
  require_nc(nc_enddef(ncid));
  require_nc(nc_close(ncid));
}

void check_last_time_attribute_type_and_len(nc_type expected_type, size_t expected_len)
{
  int ncid;
  require_nc(nc_open(filename, NC_NOWRITE, &ncid));
  nc_type att_type = NC_NAT;
  size_t  att_len  = 0;
  require_nc(nc_inq_att(ncid, NC_GLOBAL, "last_written_time", &att_type, &att_len));
  CHECK(att_type == expected_type);
  CHECK(att_len == expected_len);
  require_nc(nc_close(ncid));
}
} // namespace

TEST_CASE("Ioex processor_info validates attribute length")
{
  SECTION("matching two-int attribute is checked")
  {
    create_file();
    write_processor_info_int(2);
    int exoid = open_file();
    CHECK(Ioex::check_processor_info(filename, exoid, 2, 1));
    REQUIRE(ex_close(exoid) == EX_NOERR);
  }

  SECTION("mismatched two-int attribute reports mismatch")
  {
    create_file();
    write_processor_info_int(2);
    int exoid = open_file();
    CHECK_FALSE(Ioex::check_processor_info(filename, exoid, 4, 0));
    REQUIRE(ex_close(exoid) == EX_NOERR);
  }

  SECTION("long processor_info attribute is ignored")
  {
    create_file();
    write_processor_info_int(4);
    int exoid = open_file();
    CHECK(Ioex::check_processor_info(filename, exoid, 4, 0));
    REQUIRE(ex_close(exoid) == EX_NOERR);
  }

  SECTION("wrong-typed processor_info attribute is ignored")
  {
    create_file();
    write_processor_info_double();
    int exoid = open_file();
    CHECK(Ioex::check_processor_info(filename, exoid, 4, 0));
    REQUIRE(ex_close(exoid) == EX_NOERR);
  }
}

TEST_CASE("Ioex last_written_time validates attribute length")
{
  SECTION("single double attribute is read and updated")
  {
    create_file();
    write_last_time_double(1);
    int    exoid = open_file(EX_WRITE);
    double value = -1.0;
    CHECK(Ioex::read_last_time_attribute(exoid, &value));
    CHECK(value == 1.0);
    Ioex::update_last_time_attribute(exoid, 4.0);
    REQUIRE(ex_close(exoid) == EX_NOERR);

    exoid = open_file();
    value = -1.0;
    CHECK(Ioex::read_last_time_attribute(exoid, &value));
    CHECK(value == 4.0);
    REQUIRE(ex_close(exoid) == EX_NOERR);
  }

  SECTION("long last_written_time attribute is not read before update")
  {
    create_file();
    write_last_time_double(3);
    int exoid = open_file(EX_WRITE);
    Ioex::update_last_time_attribute(exoid, 4.0);
    REQUIRE(ex_close(exoid) == EX_NOERR);
    check_last_time_attribute_type_and_len(NC_DOUBLE, 3);
  }

  SECTION("wrong-typed last_written_time attribute is not read before update")
  {
    create_file();
    write_last_time_int();
    int exoid = open_file(EX_WRITE);
    Ioex::update_last_time_attribute(exoid, 4.0);
    REQUIRE(ex_close(exoid) == EX_NOERR);
    check_last_time_attribute_type_and_len(NC_INT, 1);
  }
}
