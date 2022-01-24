// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catch.hpp>

#include <catalyst/blockmesh/Iocatalyst_BlockMesh.h>

namespace {
  class BlockMeshFixture {
    public:
      Iocatalyst::BlockMesh bmOne;
      Iocatalyst::BlockMesh bmTwo;
      Iocatalyst::BlockMesh bmThree;
      Iocatalyst::BlockMesh bmFour;
      Iocatalyst::BlockMesh::Partition part;
      Iocatalyst::BlockMesh::Extent numBlocks;

      void checkLocalNumBlocks(const Iocatalyst::BlockMesh& bm, 
        int x, int y, int z) {
        REQUIRE(bm.getLocalNumBlocks().x == x);
        REQUIRE(bm.getLocalNumBlocks().y == y);
        REQUIRE(bm.getLocalNumBlocks().z == z);
      }

      void checkLocalBlockStart(const Iocatalyst::BlockMesh& bm, 
        int x, int y, int z) {
        REQUIRE(bm.getLocalBlockStart().x == x);
        REQUIRE(bm.getLocalBlockStart().y == y);
        REQUIRE(bm.getLocalBlockStart().z == z);
      }
  };
}

TEST_CASE_METHOD(BlockMeshFixture, "BlockMesh", "[blockMesh]")
{
 
  SECTION("Defaults")
  {
    REQUIRE(bmOne.getPartition().id == 0);
    REQUIRE(bmOne.getPartition().size == 1);

    REQUIRE(bmOne.getOrigin().x == Approx(0.0));
    REQUIRE(bmOne.getOrigin().y == Approx(0.0));
    REQUIRE(bmOne.getOrigin().z == Approx(0.0));

    REQUIRE(bmOne.getGlobalNumBlocks().x == 1);
    REQUIRE(bmOne.getGlobalNumBlocks().y == 1);
    REQUIRE(bmOne.getGlobalNumBlocks().z == 1);

    REQUIRE(bmOne.getBlockLength().x == Approx(1.0));
    REQUIRE(bmOne.getBlockLength().y == Approx(1.0));
    REQUIRE(bmOne.getBlockLength().z == Approx(1.0));

    REQUIRE(bmOne.getLocalNumBlocks().x == 1);
    REQUIRE(bmOne.getLocalNumBlocks().y == 1);
    REQUIRE(bmOne.getLocalNumBlocks().z == 1);

    REQUIRE(bmOne.getLocalBlockStart().x == 0);
    REQUIRE(bmOne.getLocalBlockStart().y == 0);
    REQUIRE(bmOne.getLocalBlockStart().z == 0);

    REQUIRE(!bmOne.isLocalBlockEmpty());
  }

  SECTION("Set origin") {
    Iocatalyst::BlockMesh::Point origin;
    origin.x = 9.5;
    origin.y = -4.0;
    origin.z = 16.45;
    bmOne.setOrigin(origin);
    REQUIRE(bmOne.getOrigin().x == Approx(9.5));
    REQUIRE(bmOne.getOrigin().y == Approx(-4.0));
    REQUIRE(bmOne.getOrigin().z == Approx(16.45));
  }

  SECTION("Set block length") {
    Iocatalyst::BlockMesh::Point length;
    length.x = -33;
    length.y = -13;
    length.z = 100.7;
    REQUIRE_THROWS(bmOne.setBlockLength(length));

    length.x = 0.2;
    length.y = 99.0;
    length.z = 44;
    bmOne.setBlockLength(length);
    REQUIRE(bmOne.getBlockLength().x == 0.2);
    REQUIRE(bmOne.getBlockLength().y == 99.0);
    REQUIRE(bmOne.getBlockLength().z == 44);
  }

  SECTION("Init partition size one") {
    part.id = 5;
    part.size = 4;
    numBlocks.x = -12;
    numBlocks.y = 45;
    numBlocks.z = 176;
    REQUIRE_THROWS(bmOne.init(part, numBlocks));
    part.id = 0;
    part.size = 1;
    REQUIRE_THROWS(bmOne.init(part, numBlocks));
    numBlocks.x = 12;

    bmOne.init(part, numBlocks);
    REQUIRE(bmOne.getPartition().id == 0);
    REQUIRE(bmOne.getPartition().size == 1);
    REQUIRE(bmOne.getGlobalNumBlocks().x == 12);
    REQUIRE(bmOne.getGlobalNumBlocks().y == 45);
    REQUIRE(bmOne.getGlobalNumBlocks().z == 176);
    checkLocalNumBlocks(bmOne, 12, 45, 176);
    checkLocalBlockStart(bmOne, 0, 0, 0);
  }

  SECTION("Init partition size two, smallest grid") {
    part.id = 0;
    part.size = 2;
    numBlocks.x = 1;
    numBlocks.y = 1;
    numBlocks.z = 1;
    bmOne.init(part, numBlocks);
    checkLocalNumBlocks(bmOne, 1, 1, 1);
    checkLocalBlockStart(bmOne, 0, 0, 0);
    part.id = 1;
    bmTwo.init(part, numBlocks);
    REQUIRE(bmTwo.isLocalBlockEmpty());
  }

  SECTION("Init partition size two, even extents") {
    part.id = 0;
    part.size = 2;
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

  SECTION("Init partition size two, odd extents") {
    part.id = 0;
    part.size = 2;
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

  SECTION("Init partition size two, z and y numBlocks one") {
    part.id = 0;
    part.size = 2;
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

  SECTION("Init partition size three") {
    part.id = 0;
    part.size = 3;
    numBlocks.x = 2;
    numBlocks.y = 2;
    numBlocks.z = 1;
    bmOne.init(part, numBlocks);
    REQUIRE(bmOne.isLocalBlockEmpty());
    part.id = 1;
    bmTwo.init(part, numBlocks);
    checkLocalNumBlocks(bmTwo, 2, 1, 1);
    checkLocalBlockStart(bmTwo, 0, 0, 0);
    part.id = 2;
    bmThree.init(part, numBlocks);
    checkLocalNumBlocks(bmThree, 2, 1, 1);
    checkLocalBlockStart(bmThree, 0, 1, 0);
  }

  SECTION("Init partition size four") {
    part.id = 0;
    part.size = 4;
    numBlocks.x = 11;
    numBlocks.y = 7;
    numBlocks.z = 3;
    bmOne.init(part, numBlocks);
    checkLocalNumBlocks(bmOne, 5, 3, 3);
    checkLocalBlockStart(bmOne, 0, 0, 0);
    part.id = 1;
    bmTwo.init(part, numBlocks);
    checkLocalNumBlocks(bmTwo, 5, 4, 3);
    checkLocalBlockStart(bmTwo, 0, 3, 0);
    part.id = 2;
    bmThree.init(part, numBlocks);
    checkLocalNumBlocks(bmThree, 6, 3, 3);
    checkLocalBlockStart(bmThree, 5, 0, 0);
    part.id = 3;
    bmFour.init(part, numBlocks);
    checkLocalNumBlocks(bmFour, 6, 4, 3);
    checkLocalBlockStart(bmFour, 5, 3, 0);
  }
}