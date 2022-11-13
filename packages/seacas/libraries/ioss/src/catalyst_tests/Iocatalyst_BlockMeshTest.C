// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "gtest/gtest.h"
#include <catalyst_tests/Iocatalyst_BlockMesh.h>

class BlockMeshTest : public ::testing::Test
{
protected:
  Iocatalyst::BlockMesh            bmOne;
  Iocatalyst::BlockMesh            bmTwo;
  Iocatalyst::BlockMesh            bmThree;
  Iocatalyst::BlockMesh            bmFour;
  Iocatalyst::BlockMesh::Partition part;
  Iocatalyst::BlockMesh::Extent    numBlocks;

  void checkLocalNumBlocks(const Iocatalyst::BlockMesh &bm, int x, int y, int z)
  {
    EXPECT_EQ(bm.getLocalNumBlocks().x, x);
    EXPECT_EQ(bm.getLocalNumBlocks().y, y);
    EXPECT_EQ(bm.getLocalNumBlocks().z, z);
  }

  void checkLocalBlockStart(const Iocatalyst::BlockMesh &bm, int x, int y, int z)
  {
    EXPECT_EQ(bm.getLocalBlockStart().x, x);
    EXPECT_EQ(bm.getLocalBlockStart().y, y);
    EXPECT_EQ(bm.getLocalBlockStart().z, z);
  }
};

TEST_F(BlockMeshTest, Defaults)
{

  EXPECT_EQ(bmOne.getPartition().id, 0);
  EXPECT_EQ(bmOne.getPartition().size, 1);

  ASSERT_DOUBLE_EQ(bmOne.getOrigin().x, 0.0);
  ASSERT_DOUBLE_EQ(bmOne.getOrigin().y, 0.0);
  ASSERT_DOUBLE_EQ(bmOne.getOrigin().z, 0.0);

  EXPECT_EQ(bmOne.getGlobalNumBlocks().x, 1);
  EXPECT_EQ(bmOne.getGlobalNumBlocks().y, 1);
  EXPECT_EQ(bmOne.getGlobalNumBlocks().z, 1);

  ASSERT_DOUBLE_EQ(bmOne.getBlockLength().x, 1.0);
  ASSERT_DOUBLE_EQ(bmOne.getBlockLength().y, 1.0);
  ASSERT_DOUBLE_EQ(bmOne.getBlockLength().z, 1.0);

  EXPECT_EQ(bmOne.getLocalNumBlocks().x, 1);
  EXPECT_EQ(bmOne.getLocalNumBlocks().y, 1);
  EXPECT_EQ(bmOne.getLocalNumBlocks().z, 1);

  EXPECT_EQ(bmOne.getLocalBlockStart().x, 0);
  EXPECT_EQ(bmOne.getLocalBlockStart().y, 0);
  EXPECT_EQ(bmOne.getLocalBlockStart().z, 0);

  EXPECT_FALSE(bmOne.isLocalBlockEmpty());
}

TEST_F(BlockMeshTest, SetOrigin)
{
  Iocatalyst::BlockMesh::Point origin;
  origin.x = 9.5;
  origin.y = -4.0;
  origin.z = 16.45;
  bmOne.setOrigin(origin);
  ASSERT_DOUBLE_EQ(bmOne.getOrigin().x, 9.5);
  ASSERT_DOUBLE_EQ(bmOne.getOrigin().y, -4.0);
  ASSERT_DOUBLE_EQ(bmOne.getOrigin().z, 16.45);
}

TEST_F(BlockMeshTest, SetBlockLength)
{
  Iocatalyst::BlockMesh::Point length;
  length.x = -33;
  length.y = -13;
  length.z = 100.7;
  EXPECT_ANY_THROW(bmOne.setBlockLength(length));

  length.x = 0.2;
  length.y = 99.0;
  length.z = 44;
  bmOne.setBlockLength(length);
  ASSERT_DOUBLE_EQ(bmOne.getBlockLength().x, 0.2);
  ASSERT_DOUBLE_EQ(bmOne.getBlockLength().y, 99.0);
  ASSERT_DOUBLE_EQ(bmOne.getBlockLength().z, 44);
}

TEST_F(BlockMeshTest, InitPartitionSizeOne)
{

  part.id     = 5;
  part.size   = 4;
  numBlocks.x = -12;
  numBlocks.y = 45;
  numBlocks.z = 176;
  EXPECT_ANY_THROW(bmOne.init(part, numBlocks));
  part.id   = 0;
  part.size = 1;
  EXPECT_ANY_THROW(bmOne.init(part, numBlocks));
  numBlocks.x = 12;

  bmOne.init(part, numBlocks);
  EXPECT_EQ(bmOne.getPartition().id, 0);
  EXPECT_EQ(bmOne.getPartition().size, 1);
  EXPECT_EQ(bmOne.getGlobalNumBlocks().x, 12);
  EXPECT_EQ(bmOne.getGlobalNumBlocks().y, 45);
  EXPECT_EQ(bmOne.getGlobalNumBlocks().z, 176);
  checkLocalNumBlocks(bmOne, 12, 45, 176);
  checkLocalBlockStart(bmOne, 0, 0, 0);
}

TEST_F(BlockMeshTest, InitPartitionSizeTwoSmallestGrid)
{

  part.id     = 0;
  part.size   = 2;
  numBlocks.x = 1;
  numBlocks.y = 1;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  checkLocalNumBlocks(bmOne, 1, 1, 1);
  checkLocalBlockStart(bmOne, 0, 0, 0);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  EXPECT_TRUE(bmTwo.isLocalBlockEmpty());
}

TEST_F(BlockMeshTest, InitPartitionSizeTwoEvenExtents)
{

  part.id     = 0;
  part.size   = 2;
  numBlocks.x = 12;
  numBlocks.y = 6;
  numBlocks.z = 24;
  bmOne.init(part, numBlocks);
  checkLocalNumBlocks(bmOne, 12, 6, 12);
  checkLocalBlockStart(bmOne, 0, 0, 0);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  checkLocalNumBlocks(bmTwo, 12, 6, 12);
  checkLocalBlockStart(bmTwo, 0, 0, 12);
}

TEST_F(BlockMeshTest, InitPartitionSizeTwoOddExtents)
{

  part.id     = 0;
  part.size   = 2;
  numBlocks.x = 12;
  numBlocks.y = 6;
  numBlocks.z = 27;
  bmOne.init(part, numBlocks);
  checkLocalNumBlocks(bmOne, 12, 6, 13);
  checkLocalBlockStart(bmOne, 0, 0, 0);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  checkLocalNumBlocks(bmTwo, 12, 6, 14);
  checkLocalBlockStart(bmTwo, 0, 0, 13);
}

TEST_F(BlockMeshTest, InitPartitionSizeTwoZandYnumBlocksOne)
{
  part.id     = 0;
  part.size   = 2;
  numBlocks.x = 13;
  numBlocks.y = 1;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  checkLocalNumBlocks(bmOne, 6, 1, 1);
  checkLocalBlockStart(bmOne, 0, 0, 0);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  checkLocalNumBlocks(bmTwo, 7, 1, 1);
  checkLocalBlockStart(bmTwo, 6, 0, 0);
}

TEST_F(BlockMeshTest, InitPartitionSizeThree)
{
  part.id     = 0;
  part.size   = 3;
  numBlocks.x = 2;
  numBlocks.y = 2;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  EXPECT_TRUE(bmOne.isLocalBlockEmpty());
  part.id = 1;
  bmTwo.init(part, numBlocks);
  checkLocalNumBlocks(bmTwo, 2, 1, 1);
  checkLocalBlockStart(bmTwo, 0, 0, 0);
  part.id = 2;
  bmThree.init(part, numBlocks);
  checkLocalNumBlocks(bmThree, 2, 1, 1);
  checkLocalBlockStart(bmThree, 0, 1, 0);
}
