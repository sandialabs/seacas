// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_Compare.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_IOFactory.h>
#include <Ioss_MeshCopyOptions.h>
#include <catalyst_tests/Iocatalyst_DatabaseIOTest.h>

Iocatalyst_DatabaseIOTest::Iocatalyst_DatabaseIOTest()
{
  part.id         = putils.parallel_rank();
  part.size       = putils.parallel_size();
  blockMeshSize.i = 2;
  blockMeshSize.j = 2;
  blockMeshSize.k = 2;
  origin.i        = 0;
  origin.j        = 0;
  origin.k        = 0;
}

bool Iocatalyst_DatabaseIOTest::regionsAreEqual(const std::string &fileName,
                                                const std::string &catFileName,
                                                const std::string &iossDatabaseType)
{

  Ioss::PropertyManager dbProps;
  Ioss::DatabaseIO *dbi = Ioss::IOFactory::create(iossDatabaseType, fileName, Ioss::READ_RESTART,
                                                  Ioss::ParallelUtils::comm_world(), dbProps);
  if (dbi == nullptr || !dbi->ok(true)) {
    return false;
  }

  Ioss::PropertyManager dbCatProps;
  Ioss::DatabaseIO     *dbiCat =
      Ioss::IOFactory::create(iossDatabaseType, catFileName, Ioss::READ_RESTART,
                              Ioss::ParallelUtils::comm_world(), dbCatProps);
  if (dbiCat == nullptr || !dbiCat->ok(true)) {
    return false;
  }

  Ioss::Region          ir(dbi);
  Ioss::Region          rCat(dbiCat);
  Ioss::MeshCopyOptions options;
  options.data_storage_type = 1;
  return Ioss::Compare::compare_database(ir, rCat, options);
}

void Iocatalyst_DatabaseIOTest::runStructuredTest(const std::string &testName)
{
  std::string cgnsFileName =
      testName + CATALYST_TEST_FILE_NP + std::to_string(part.size) + CGNS_FILE_EXTENSION;
  std::string catalystFileName = CATALYST_TEST_FILE_PREFIX + testName + CATALYST_TEST_FILE_NP +
                                 std::to_string(part.size) + CGNS_FILE_EXTENSION;
  bmSet.writeIOSSFile(cgnsFileName, CGNS_DATABASE_TYPE);
  bmSet.writeCatalystIOSSFile(catalystFileName, CGNS_DATABASE_TYPE);
  EXPECT_TRUE(regionsAreEqual(cgnsFileName, catalystFileName, CGNS_DATABASE_TYPE));
}

void Iocatalyst_DatabaseIOTest::runUnstructuredTest(const std::string &testName)
{
  std::string exodusFileName =
      testName + CATALYST_TEST_FILE_NP + std::to_string(part.size) + EXODUS_FILE_EXTENSION;
  std::string catalystFileName = CATALYST_TEST_FILE_PREFIX + testName + CATALYST_TEST_FILE_NP +
                                 std::to_string(part.size) + EXODUS_FILE_EXTENSION;
  bmSet.writeIOSSFile(exodusFileName, EXODUS_DATABASE_TYPE);
  bmSet.writeCatalystIOSSFile(catalystFileName, EXODUS_DATABASE_TYPE);
  EXPECT_TRUE(regionsAreEqual(exodusFileName, catalystFileName, EXODUS_DATABASE_TYPE));
}

void Iocatalyst_DatabaseIOTest::setBlockMeshSize(unsigned int i, unsigned int j, unsigned int k)
{
  blockMeshSize.i = i;
  blockMeshSize.j = j;
  blockMeshSize.k = k;
}

void Iocatalyst_DatabaseIOTest::setOrigin(unsigned int i, unsigned int j, unsigned int k)
{
  origin.i = i;
  origin.j = j;
  origin.k = k;
}

void Iocatalyst_DatabaseIOTest::addBlockMesh(Iocatalyst::BlockMesh &blockMesh)
{
  blockMesh.init(part, blockMeshSize, origin);
  bmSet.addBlockMesh(blockMesh);
}