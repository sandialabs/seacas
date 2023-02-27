// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catalyst_tests/Iocatalyst_DatabaseIOTest.h>

TEST_F(Iocatalyst_DatabaseIOTest, WriteOneStructuredBlockWith8Cells)
{
  Iocatalyst::BlockMesh bm;
  initBlock(bm, 2, 2, 2);
  bmSet.addBlockMesh(bm);
  runStructuredTest("test_sb_1_cells_8");
}

TEST_F(Iocatalyst_DatabaseIOTest, WriteOneStructuredBlockWith200Cells)
{
  Iocatalyst::BlockMesh bm;
  initBlock(bm, 10, 10, 2);
  bmSet.addBlockMesh(bm);
  runStructuredTest("test_sb_1_cells_200");
}

TEST_F(Iocatalyst_DatabaseIOTest, WriteThreeStructuredBlocksWith835Cells)
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
  runStructuredTest("test_sb_3_cells_835");
}