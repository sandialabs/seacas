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

TEST_F(BlockMeshTest, GetLocalPointIDsSizeOneOneBlock)
{
  std::vector<int> points = {1, 2, 3, 4, 5, 6, 7, 8};
  EXPECT_EQ(bmOne.getLocalPointIDs(), points);
}

TEST_F(BlockMeshTest, GetLocalPointIDsSizeOneTwoBlocksInX)
{
  part.id     = 0;
  part.size   = 1;
  numBlocks.x = 2;
  numBlocks.y = 1;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  std::vector<int> points = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
  EXPECT_EQ(bmOne.getLocalPointIDs(), points);
}

TEST_F(BlockMeshTest, GetLocalPointIDsSizeTwoOneBlock)
{

  part.id     = 0;
  part.size   = 2;
  numBlocks.x = 1;
  numBlocks.y = 1;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  std::vector<int> points = {1, 2, 3, 4, 5, 6, 7, 8};
  EXPECT_EQ(bmOne.getLocalPointIDs(), points);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  points.clear();
  EXPECT_EQ(bmTwo.getLocalPointIDs(), points);
}

TEST_F(BlockMeshTest, GetLocalPointIDsSizeTwoTwoBlocksInY)
{
  part.id     = 0;
  part.size   = 2;
  numBlocks.x = 1;
  numBlocks.y = 2;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  std::vector<int> points = {1, 2, 3, 4, 7, 8, 9, 10};
  EXPECT_EQ(bmOne.getLocalPointIDs(), points);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  std::vector<int> pointsTwo = {3, 4, 5, 6, 9, 10, 11, 12};
  EXPECT_EQ(bmTwo.getLocalPointIDs(), pointsTwo);
}

TEST_F(BlockMeshTest, GetLocalPointIDsSizeFourEightBlocks)
{
  part.id     = 0;
  part.size   = 4;
  numBlocks.x = 2;
  numBlocks.y = 2;
  numBlocks.z = 2;
  bmOne.init(part, numBlocks);
  std::vector<int> points = {1, 2, 3, 4, 5, 6, 10, 11, 12, 13, 14, 15};
  EXPECT_EQ(bmOne.getLocalPointIDs(), points);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  std::vector<int> pointsTwo = {4, 5, 6, 7, 8, 9, 13, 14, 15, 16, 17, 18};
  EXPECT_EQ(bmTwo.getLocalPointIDs(), pointsTwo);
  part.id = 2;
  bmThree.init(part, numBlocks);
  std::vector<int> pointsThree = {10, 11, 12, 13, 14, 15, 19, 20, 21, 22, 23, 24};
  EXPECT_EQ(bmThree.getLocalPointIDs(), pointsThree);
  part.id = 3;
  bmFour.init(part, numBlocks);
  std::vector<int> pointsFour = {13, 14, 15, 16, 17, 18, 22, 23, 24, 25, 26, 27};
  EXPECT_EQ(bmFour.getLocalPointIDs(), pointsFour);
}

TEST_F(BlockMeshTest, GetPointCoordsForGlobalIDSizeOneOneBlock)
{
  Iocatalyst::BlockMesh::Point p = bmOne.getPointCoordsForGlobalPointID(8);
  EXPECT_DOUBLE_EQ(p.x, 1.0);
  EXPECT_DOUBLE_EQ(p.y, 1.0);
  EXPECT_DOUBLE_EQ(p.z, 1.0);
}

TEST_F(BlockMeshTest, GetPointCoordsForGlobalIDSizeOneEightBlocks)
{
  part.id     = 0;
  part.size   = 1;
  numBlocks.x = 2;
  numBlocks.y = 2;
  numBlocks.z = 2;
  bmOne.init(part, numBlocks);
  Iocatalyst::BlockMesh::Point length;
  length.x = 2.0;
  length.y = 2.0;
  length.z = 2.0;
  bmOne.setBlockLength(length);
  bmOne.setOrigin(length);
  Iocatalyst::BlockMesh::Point p = bmOne.getPointCoordsForGlobalPointID(27);
  EXPECT_DOUBLE_EQ(p.x, 6.0);
  EXPECT_DOUBLE_EQ(p.y, 6.0);
  EXPECT_DOUBLE_EQ(p.z, 6.0);
  p = bmOne.getPointCoordsForGlobalPointID(1);
  EXPECT_DOUBLE_EQ(p.x, 2.0);
  EXPECT_DOUBLE_EQ(p.y, 2.0);
  EXPECT_DOUBLE_EQ(p.z, 2.0);
  p = bmOne.getPointCoordsForGlobalPointID(10);
  EXPECT_DOUBLE_EQ(p.x, 2.0);
  EXPECT_DOUBLE_EQ(p.y, 2.0);
  EXPECT_DOUBLE_EQ(p.z, 4.0);
}

TEST_F(BlockMeshTest, GetLocalBlockIDsSizeOneOneBlock)
{
  std::vector<int> points = {1};
  EXPECT_EQ(bmOne.getLocalBlockIDs(), points);
}

TEST_F(BlockMeshTest, GetLocalBlockIDsSizeOneTwoBlocksInX)
{
  part.id     = 0;
  part.size   = 1;
  numBlocks.x = 2;
  numBlocks.y = 1;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  std::vector<int> ids = {1, 2};
  EXPECT_EQ(bmOne.getLocalBlockIDs(), ids);
}

TEST_F(BlockMeshTest, GetLocalBlockIDsSizeTwoOneBlock)
{

  part.id     = 0;
  part.size   = 2;
  numBlocks.x = 1;
  numBlocks.y = 1;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  std::vector<int> ids = {1};
  EXPECT_EQ(bmOne.getLocalBlockIDs(), ids);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  ids.clear();
  EXPECT_EQ(bmTwo.getLocalBlockIDs(), ids);
}

TEST_F(BlockMeshTest, GetLocalBlockIDsSizeTwoTwoBlocksInY)
{
  part.id     = 0;
  part.size   = 2;
  numBlocks.x = 1;
  numBlocks.y = 2;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  std::vector<int> ids = {1};
  EXPECT_EQ(bmOne.getLocalBlockIDs(), ids);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  std::vector<int> idsTwo = {2};
  EXPECT_EQ(bmTwo.getLocalBlockIDs(), idsTwo);
}

TEST_F(BlockMeshTest, GetLocalBlockIDsSizeFourEightBlocks)
{
  part.id     = 0;
  part.size   = 4;
  numBlocks.x = 2;
  numBlocks.y = 2;
  numBlocks.z = 2;
  bmOne.init(part, numBlocks);
  std::vector<int> ids = {1,2};
  EXPECT_EQ(bmOne.getLocalBlockIDs(), ids);
  part.id = 1;
  bmTwo.init(part, numBlocks);
  std::vector<int> idsTwo = {3,4};
  EXPECT_EQ(bmTwo.getLocalBlockIDs(), idsTwo);
  part.id = 2;
  bmThree.init(part, numBlocks);
  std::vector<int> idsThree = {5,6};
  EXPECT_EQ(bmThree.getLocalBlockIDs(), idsThree);
  part.id = 3;
  bmFour.init(part, numBlocks);
  std::vector<int> idsFour = {7,8};
  EXPECT_EQ(bmFour.getLocalBlockIDs(), idsFour);
}

TEST_F(BlockMeshTest, GetBlockGlobalPointIDsSizeOneOneBlock)
{
  Iocatalyst::BlockMesh::BlockConn ids = {1,2,4,3,5,6,8,7};
  EXPECT_EQ(bmOne.getBlockConnectivityGlobalPointIDs(1), ids);
}

TEST_F(BlockMeshTest, GetBlockGlobalPointIDsSizeOneTwoBlocksInX)
{
  part.id     = 0;
  part.size   = 1;
  numBlocks.x = 2;
  numBlocks.y = 1;
  numBlocks.z = 1;
  bmOne.init(part, numBlocks);
  Iocatalyst::BlockMesh::BlockConn ids = {2,3,6,5,8,9,12,11};
  EXPECT_EQ(bmOne.getBlockConnectivityGlobalPointIDs(2), ids);
}

TEST_F(BlockMeshTest, GetBlockGlobalPointIDsSizeOneEightBlocks)
{
  part.id     = 0;
  part.size   = 1;
  numBlocks.x = 2;
  numBlocks.y = 2;
  numBlocks.z = 2;
  bmOne.init(part, numBlocks);
  Iocatalyst::BlockMesh::BlockConn ids = {14,15,18,17, 23,24,27,26};
  EXPECT_EQ(bmOne.getBlockConnectivityGlobalPointIDs(8), ids);
}
