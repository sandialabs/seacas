// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catalyst_tests/Iocatalyst_DatabaseIOTest.h>
#include <Ioss_Compare.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_IOFactory.h>
#include <Ioss_MeshCopyOptions.h>
//#include <catalyst/Iocatalyst_DatabaseIO.h>
#include <mpi.h>

Iocatalyst_DatabaseIOTest::Iocatalyst_DatabaseIOTest()
{
  MPI_Comm_rank(MPI_COMM_WORLD, &part.id);
  MPI_Comm_size(MPI_COMM_WORLD, &part.size);
}

bool Iocatalyst_DatabaseIOTest::regionsAreEqual(const std::string &fileName,
                                                const std::string &catFileName)
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
  Ioss::DatabaseIO *dbiCat =
      Ioss::IOFactory::create("cgns", catFileName, Ioss::READ_RESTART, MPI_COMM_WORLD, dbCatProps);
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

  void Iocatalyst_DatabaseIOTest::runTest(const std::string &testName)
  {
    std::string cgnsFileName     = testName + ".cgns";
    std::string catalystFileName = "catalyst_" + testName + ".cgns";
    bmSet.writeCGNSFile(cgnsFileName);
    bmSet.writeCatalystCGNSFile(catalystFileName);
    EXPECT_TRUE(regionsAreEqual(cgnsFileName, catalystFileName));
  }

  void Iocatalyst_DatabaseIOTest::initBlock(Iocatalyst::BlockMesh &blockMesh, int x, int y, int z)
  {
    numBlocks.x = x;
    numBlocks.y = y;
    numBlocks.z = z;
    blockMesh.init(part, numBlocks);
  }