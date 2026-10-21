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

int open_file()
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
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
