// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture, "CatalystCGNSVariableComponents",
                 "[catalyst multiblock mesh]")
{

  runCatalystMultiBlockMeshTest("aero_blunt_wedge_test3.cgns");
  checkTestOutputFileExists("iossDatabaseCatalystMesh_time_0.vtm");
  checkTestOutputFileExists("iossDatabaseCatalystMesh_time_1.vtm");
  checkTestOutputFileExists("iossDatabaseCatalystMesh_time_2.vtm");
  checkTestOutputFileExists("iossDatabaseCatalystMesh_time_3.vtm");
  checkTestOutputFileExists("iossDatabaseCatalystMesh_time_4.vtm");

  CatalystTestFixture::VarAndCompCountVec cellVariables = {
      {"density", 1}, {"pressure", 1}, {"temperature", 1}, {"velocity", 3}};
  CatalystTestFixture::VarAndCompCountVec pointVariables;
  CatalystTestFixture::VarAndCompCountVec globalVariables;

  checkMeshOutputVariables("iossDatabaseCatalystMesh_time_0.vtm", cellVariables, pointVariables,
                           globalVariables);
}

TEST_CASE_METHOD(CatalystTestFixture, "CatalystExodusVariableComponents",
                 "[catalyst multiblock mesh]")
{

  runCatalystMultiBlockMeshTest("block_crush_1.ex2");
  checkTestOutputFileExists("iossDatabaseCatalystMesh_time_0.vtm");
  checkTestOutputFileExists("iossDatabaseCatalystMesh_time_10.vtm");

  CatalystTestFixture::VarAndCompCountVec cellVariables  = {{"vonmises", 1}, {"stress", 6}};
  CatalystTestFixture::VarAndCompCountVec pointVariables = {
      {"cetan", 1}, {"cftan", 1}, {"vel", 3}, {"displ", 3}};
  CatalystTestFixture::VarAndCompCountVec globalVariables = {{"momentum", 3}, {"kineticenergy", 1}};

  checkMeshOutputVariables("iossDatabaseCatalystMesh_time_0.vtm", cellVariables, pointVariables,
                           globalVariables);
}
