// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catalyst_tests/Iocatalyst_DatabaseIOTest.h>

TEST_F(Iocatalyst_DatabaseIOTest, GetNumLocalPointsInMeshSet)
{
  EXPECT_EQ(bmSet.getNumLocalPointsInMeshSet(), 0);
  Iocatalyst::BlockMesh bmOne;
  setBlockMeshSize(2, 2, 2);
  addBlockMesh(bmOne);
  EXPECT_EQ(bmSet.getNumLocalPointsInMeshSet(), 27);

  Iocatalyst::BlockMesh bmTwo;
  setBlockMeshSize(1, 1, 1);
  setOrigin(2, 0, 0);
  addBlockMesh(bmTwo);
  EXPECT_EQ(bmSet.getNumLocalPointsInMeshSet(), 31);

  Iocatalyst::BlockMesh bmThree;
  setBlockMeshSize(2, 2, 2);
  setOrigin(3, 0, 0);
  addBlockMesh(bmThree);
  EXPECT_EQ(bmSet.getNumLocalPointsInMeshSet(), 54);

  Iocatalyst::BlockMesh bmFour;
  setBlockMeshSize(8, 3, 3);
  setOrigin(5, 0, 0);
  addBlockMesh(bmFour);
  EXPECT_EQ(bmSet.getNumLocalPointsInMeshSet(), 189);
}

/*TEST_F(Iocatalyst_DatabaseIOTest, GetNumLocalBlocksInMeshSet)
{
  EXPECT_EQ(bmSet.getNumLocalBlocksInMeshSet(), 0);
  Iocatalyst::BlockMesh bmOne;
  setBlockMeshSize(3, 5, 4);
  addBlockMesh(bmOne);
  EXPECT_EQ(bmSet.getNumLocalBlocksInMeshSet(), 60);

  Iocatalyst::BlockMesh bmTwo;
  setBlockMeshSize(3, 7, 4);
  setOrigin(3, 0, 0);
  addBlockMesh(bmTwo);
  EXPECT_EQ(bmSet.getNumLocalBlocksInMeshSet(), 144);
}

TEST_F(Iocatalyst_DatabaseIOTest, getGlobalPointIDInMeshSet)
{
  Iocatalyst::BlockMesh bmOne;
  setBlockMeshSize(1, 1, 1);
  addBlockMesh(bmOne);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmOne, 1), 1);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmOne, 2), 2);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmOne, 3), 3);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmOne, 4), 4);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmOne, 8), 8);

  Iocatalyst::BlockMesh bmTwo;
  setBlockMeshSize(1, 1, 1);
  setOrigin(1, 0, 0);
  addBlockMesh(bmTwo);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmTwo, 1), 2);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmTwo, 2), 10);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmTwo, 3), 4);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmTwo, 4), 12);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmTwo, 5), 6);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmTwo, 6), 14);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmTwo, 7), 8);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmTwo, 8), 16);

  Iocatalyst::BlockMesh bmThree;
  setBlockMeshSize(2, 2, 2);
  setOrigin(2, 0, 0);
  addBlockMesh(bmThree);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 1), 10);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 2), 18);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 3), 19);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 4), 12);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 5), 21);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 6), 22);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 7), 23);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 8), 24);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 9), 25);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 10), 14);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 11), 27);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 12), 28);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 13), 16);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 14), 30);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 15), 31);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 16), 32);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 17), 33);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 18), 34);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 19), 35);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 20), 36);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 21), 37);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 22), 38);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 23), 39);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 24), 40);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 25), 41);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 26), 42);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmThree, 27), 43);

  Iocatalyst::BlockMesh bmFour;
  setBlockMeshSize(1, 1, 1);
  setOrigin(4, 0, 0);
  addBlockMesh(bmFour);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmFour, 1), 19);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmFour, 2), 45);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmFour, 3), 22);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmFour, 4), 47);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmFour, 5), 28);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmFour, 6), 49);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmFour, 7), 31);
  EXPECT_EQ(bmSet.getGlobalPointIDInMeshSet(bmFour, 8), 51);
}

TEST_F(Iocatalyst_DatabaseIOTest, getGlobalBlockIDInMeshSet)
{
  Iocatalyst::BlockMesh bmOne;
  setBlockMeshSize(3, 5, 4);
  addBlockMesh(bmOne);

  EXPECT_EQ(bmSet.getGlobalBlockIDInMeshSet(bmOne, 5), 5);

  Iocatalyst::BlockMesh bmTwo;
  setBlockMeshSize(6, 2, 3);
  setOrigin(3, 0, 0);
  addBlockMesh(bmTwo);
  EXPECT_EQ(bmSet.getGlobalBlockIDInMeshSet(bmTwo, 1), 61);
  EXPECT_EQ(bmSet.getGlobalBlockIDInMeshSet(bmTwo, 36), 96);

  Iocatalyst::BlockMesh bmThree;
  setBlockMeshSize(1, 1, 1);
  setOrigin(9, 0, 0);
  addBlockMesh(bmThree);
  EXPECT_EQ(bmSet.getGlobalBlockIDInMeshSet(bmThree, 1), 97);
}
*/
