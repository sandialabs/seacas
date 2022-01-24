// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catch.hpp>
#include <Ioss_Compare.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_IOFactory.h>
#include <Ioss_MeshCopyOptions.h>
#include <catalyst/Iocatalyst_DatabaseIO.h>
#include <catalyst/blockmesh/Iocatalyst_BlockMeshSet.h>

namespace {
  class BlockMeshSetFixture
  {
  public:
    Iocatalyst::BlockMeshSet bmSet;

    bool regionsAreEqual(const std::string &fileName, const std::string &catFileName)
    {
      Ioss::PropertyManager dbProps;
      dbProps.add(Ioss::Property("DECOMPOSITION_METHOD", "external"));

      Ioss::DatabaseIO *dbi =
          Ioss::IOFactory::create("cgns", fileName, Ioss::READ_RESTART, MPI_COMM_WORLD, dbProps);
      if (dbi == nullptr || !dbi->ok(true)) {
        return false;
      }

      Ioss::PropertyManager dbCatProps;
      dbCatProps.add(Ioss::Property("DECOMPOSITION_METHOD", "external"));
      Ioss::DatabaseIO *dbiCat = Ioss::IOFactory::create("cgns", catFileName, Ioss::READ_RESTART,
                                                         MPI_COMM_WORLD, dbCatProps);
      if (dbiCat == nullptr || !dbiCat->ok(true)) {
        return false;
      }

      Ioss::Region          ir(dbi);
      Ioss::Region          rCat(dbiCat);
      Ioss::MeshCopyOptions options;
      options.data_storage_type = 1;

      bool result = Ioss::Compare::compare_database(ir, rCat, options);
      if (!result) {
        std::cerr << Ioss::OUTPUT().rdbuf() << "\n";
        bmSet.printCatalystConduitNode();
      }
      return result;
    }
  };
} // namespace

TEST_CASE_METHOD(BlockMeshSetFixture, "BlockMeshSet", "[blockMeshSet]")
{
  SECTION("Write One Structured Mesh Block to File")
  {
    Iocatalyst::BlockMesh bm;
    bmSet.addBlockMesh(bm);
    bmSet.writeCGNSFile("foo.cgns");
    bmSet.writeCatalystCGNSFile("cat.cgns");
    REQUIRE(regionsAreEqual("foo.cgns", "cat.cgns"));
  }
}