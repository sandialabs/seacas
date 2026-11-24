// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// Ioex distribution-factor reads must reject files whose distribution-factor
// count does not match the IOSS entity-set buffer size.

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>

#include "Ionit_Initializer.h"
#include "Ioss_ConcreteVariableType.h"
#include "Ioss_DBUsage.h"
#include "Ioss_ElementSet.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"
#include "Ioss_PropertyManager.h"
#include "Ioss_Region.h"
#include "Ioss_ScopeGuard.h"
#include "exodus/Ioex_DatabaseIO.h"

#include <exodusII.h>
#include <memory>
#include <netcdf.h>
#include <stdexcept>
#include <string>
#include <vector>

namespace {
  const char *filename = "ioex-distribution-factor-count.exo";

  void require_nc(int status) { REQUIRE(status == NC_NOERR); }

  void create_element_set_file(int element_count, int distribution_factor_count)
  {
    int cpu_word_size = 8;
    int io_word_size  = 8;
    int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
    REQUIRE(exoid >= 0);

    ex_init_params init{};
    ex_copy_string(init.title, "distribution factor count", MAX_LINE_LENGTH + 1);
    init.num_dim       = 3;
    init.num_nodes     = element_count * 2;
    init.num_elem      = element_count;
    init.num_elem_blk  = 1;
    init.num_elem_sets = 1;
    REQUIRE(ex_put_init_ext(exoid, &init) == EX_NOERR);
    REQUIRE(ex_put_block(exoid, EX_ELEM_BLOCK, 7, "BAR2", element_count, 2, 0, 0, 0) == EX_NOERR);

    std::vector<int> connectivity(element_count * 2);
    for (int i = 0; i < element_count * 2; i++) {
      connectivity[i] = i + 1;
    }
    REQUIRE(ex_put_conn(exoid, EX_ELEM_BLOCK, 7, connectivity.data(), nullptr, nullptr) ==
            EX_NOERR);

    REQUIRE(ex_put_set_param(exoid, EX_ELEM_SET, 11, element_count, element_count) == EX_NOERR);

    std::vector<int> elements(element_count);
    for (int i = 0; i < element_count; i++) {
      elements[i] = i + 1;
    }
    REQUIRE(ex_put_set(exoid, EX_ELEM_SET, 11, elements.data(), nullptr) == EX_NOERR);

    std::vector<double> distribution_factors(element_count);
    for (int i = 0; i < element_count; i++) {
      distribution_factors[i] = 2.0 + i;
    }
    REQUIRE(ex_put_set_dist_fact(exoid, EX_ELEM_SET, 11, distribution_factors.data()) == EX_NOERR);
    REQUIRE(ex_close(exoid) == EX_NOERR);

    int ncid = -1;
    require_nc(nc_open(filename, NC_WRITE, &ncid));
    require_nc(nc_redef(ncid));

    int old_varid = -1;
    require_nc(nc_inq_varid(ncid, "dist_fact_els1", &old_varid));
    require_nc(nc_rename_var(ncid, old_varid, "old_dist_fact_els1"));

    if (distribution_factor_count > 0) {
      int dimid = -1;
      require_nc(nc_def_dim(ncid, "num_df_els1", distribution_factor_count, &dimid));
      int varid = -1;
      require_nc(nc_def_var(ncid, "dist_fact_els1", NC_DOUBLE, 1, &dimid, &varid));
      require_nc(nc_enddef(ncid));

      distribution_factors.resize(distribution_factor_count);
      for (int i = 0; i < distribution_factor_count; i++) {
        distribution_factors[i] = 2.0 + i;
      }
      require_nc(nc_put_var_double(ncid, varid, distribution_factors.data()));
      require_nc(nc_close(ncid));
      return;
    }

    require_nc(nc_enddef(ncid));
    require_nc(nc_close(ncid));
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

TEST_CASE("Ioex validates set distribution-factor count against entity buffer size")
{
  Ioss::Init::Initializer  init_db;
  Ioss::StorageInitializer initialize_storage;

  SECTION("matching distribution-factor count reads successfully")
  {
    create_element_set_file(2, 2);
    auto region = open_region();

    const auto &sets = region->get_elementsets();
    REQUIRE(sets.size() == 1);

    std::vector<double> distribution_factors;
    REQUIRE_NOTHROW(sets[0]->get_field_data("distribution_factors", distribution_factors));
    REQUIRE(distribution_factors.size() == 2);
    CHECK(distribution_factors[0] == 2.0);
    CHECK(distribution_factors[1] == 3.0);
  }

  SECTION("zero stored distribution factors substitute ones")
  {
    create_element_set_file(2, 0);
    auto region = open_region();

    const auto &sets = region->get_elementsets();
    REQUIRE(sets.size() == 1);

    std::vector<double> distribution_factors;
    REQUIRE_NOTHROW(sets[0]->get_field_data("distribution_factors", distribution_factors));
    REQUIRE(distribution_factors.size() == 2);
    CHECK(distribution_factors[0] == 1.0);
    CHECK(distribution_factors[1] == 1.0);
  }

  SECTION("stored distribution-factor count larger than entity count throws before reading")
  {
    create_element_set_file(2, 3);
    auto region = open_region();

    const auto &sets = region->get_elementsets();
    REQUIRE(sets.size() == 1);
    CHECK(sets[0]->entity_count() == 2);

    std::vector<double> distribution_factors;
    CHECK_THROWS_AS(sets[0]->get_field_data("distribution_factors", distribution_factors),
                    std::runtime_error);
  }
}
