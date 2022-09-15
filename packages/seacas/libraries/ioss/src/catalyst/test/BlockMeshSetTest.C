// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_Compare.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_IOFactory.h>
#include <Ioss_MeshCopyOptions.h>
#include <catalyst/Iocatalyst_DatabaseIO.h>
#include <catalyst/blockmesh/Iocatalyst_BlockMeshSet.h>
#include <catch.hpp>
#include <mpi.h>

namespace {
  class BlockMeshSetFixture
  {
  public:
    Iocatalyst::BlockMeshSet         bmSet;
    Iocatalyst::BlockMesh::Partition part;
    Iocatalyst::BlockMesh::Extent    numBlocks;

    BlockMeshSetFixture()
    {
      MPI_Comm_rank(MPI_COMM_WORLD, &part.id);
      MPI_Comm_size(MPI_COMM_WORLD, &part.size);
    }

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
        bmSet.printCatalystConduitNode();
        std::cerr << Ioss::OUTPUT().rdbuf() << "\n";
      }
      return result;
    }

    void runTest(const std::string &testName)
    {
      std::string cgnsFileName     = testName + ".cgns";
      std::string catalystFileName = "catalyst_" + testName + ".cgns";
      bmSet.writeCGNSFile(cgnsFileName);
      bmSet.writeCatalystCGNSFile(catalystFileName);
      REQUIRE(regionsAreEqual(cgnsFileName, catalystFileName));
    }

    void initBlock(Iocatalyst::BlockMesh &blockMesh, int x, int y, int z)
    {
      numBlocks.x = x;
      numBlocks.y = y;
      numBlocks.z = z;
      blockMesh.init(part, numBlocks);
    }
  };

} // namespace

TEST_CASE_METHOD(BlockMeshSetFixture, "BlockMeshSet", "[blockMeshSet]")
{
  SECTION("Write One Structured Mesh Block with One Cell")
  {
    Iocatalyst::BlockMesh bm;
    bmSet.addBlockMesh(bm);
    runTest("test_sb_1_cells_1");
  }

  SECTION("Write One Structured Mesh Blocks with 200 Cells")
  {
    Iocatalyst::BlockMesh bm;
    initBlock(bm, 10, 10, 2);
    bmSet.addBlockMesh(bm);
    runTest("test_sb_1_cells_200");
  }

  SECTION("Write Three Structured Mesh Blocks with 835 Cells")
  {
    Iocatalyst::BlockMesh bmOne;
    initBlock(bmOne, 5, 15, 3);
    Iocatalyst::BlockMesh bmTwo;
    initBlock(bmTwo, 8, 14, 4);
    Iocatalyst::BlockMesh bmThree;
    initBlock(bmThree, 3, 6, 9);
    bmSet.addBlockMesh(bmOne);
    bmSet.addBlockMesh(bmTwo);
    bmSet.addBlockMesh(bmThree);
    runTest("test_sb_3_cells_835");
  }

  SECTION("Write One Structured Mesh Block with One Cell and One Time Step")
  {
    Iocatalyst::BlockMesh bm;
    bmSet.setNumberOfTimeSteps(1);
    bmSet.addBlockMesh(bm);
    runTest("test_sb_1_cells_1_ts_1");
    //bmSet.writeCGNSFile("test_sb_1_cells_1_ts_1.cgns");
  }

  SECTION("Write One Structured Mesh Block with One Cell and Ten Time Steps")
  {
    Iocatalyst::BlockMesh bm;
    bmSet.setNumberOfTimeSteps(10);
    bmSet.addBlockMesh(bm);
    runTest("test_sb_1_cells_1_ts_10");
    //bmSet.writeCGNSFile("test_sb_1_cells_1_ts_1.cgns");
  }
}