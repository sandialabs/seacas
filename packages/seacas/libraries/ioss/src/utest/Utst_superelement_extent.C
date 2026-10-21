// Copyright(C) 2026 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// SuperElement whole-variable reads must validate that each netCDF variable's
// extent matches the buffer size implied by the sizing dimensions.

#include <catch2/catch_test_macros.hpp>

#include "Ioss_ConcreteVariableType.h"
#include "Ioss_Field.h"
#include "Ioss_Property.h"
#include "Ioss_Utils.h"
#include "exodus/Ioex_SuperElement.h"

#include <netcdf.h>
#include <stdexcept>
#include <string>
#include <vector>

namespace {
const char *filename = "superelement-extent.ncf";

void require_nc(int status) { REQUIRE(status == NC_NOERR); }

void create_superelement_file(size_t kr_extent)
{
  int ncid = -1;
  require_nc(nc_create(filename, NC_CLOBBER, &ncid));

  int num_dof_dim = -1;
  int num_eig_dim = -1;
  int kr_dim      = -1;
  require_nc(nc_def_dim(ncid, "NumDof", 1, &num_dof_dim));
  require_nc(nc_def_dim(ncid, "NumEig", 1, &num_eig_dim));
  require_nc(nc_def_dim(ncid, "KrExtent", kr_extent, &kr_dim));

  int kr_var = -1;
  int mr_var = -1;
  require_nc(nc_def_var(ncid, "Kr", NC_DOUBLE, 1, &kr_dim, &kr_var));
  require_nc(nc_def_var(ncid, "Mr", NC_DOUBLE, 1, &num_dof_dim, &mr_var));
  require_nc(nc_enddef(ncid));

  std::vector<double> kr_values(kr_extent, 2.0);
  if (kr_extent > 0) {
    require_nc(nc_put_var_double(ncid, kr_var, kr_values.data()));
  }
  double mr_value = 3.0;
  require_nc(nc_put_var_double(ncid, mr_var, &mr_value));

  require_nc(nc_close(ncid));
}
} // namespace

TEST_CASE("SuperElement validates netCDF variable extent before whole-variable reads")
{
  Ioss::StorageInitializer initialize_storage;

  SECTION("matched Kr extent reads successfully")
  {
    create_superelement_file(1);
    Ioex::SuperElement se(filename, "superelement");

    size_t num_dof = se.get_property("numDOF").get_int();
    REQUIRE(num_dof == 1);

    std::vector<double> kr(num_dof * num_dof, 0.0);
    size_t              count = se.get_field_data("Kr", kr);
    CHECK(count == 1);
    CHECK(kr[0] == 2.0);
  }

  SECTION("mismatched Kr extent throws before modifying the destination buffer")
  {
    create_superelement_file(2);
    Ioex::SuperElement se(filename, "superelement");

    std::vector<double> kr(1, 0.0);
    CHECK_THROWS_AS((void)se.get_field_data("Kr", kr), std::runtime_error);
    CHECK(kr[0] == 0.0);
  }
}
