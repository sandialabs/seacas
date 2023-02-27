// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catalyst_tests/Iocatalyst_DatabaseIOTest.h>

TEST_F(Iocatalyst_DatabaseIOTest, WriteOneElementBlockWith8Cells)
{
  Iocatalyst::BlockMesh bm;
  initBlock(bm, 2, 2, 2);
  bmSet.addBlockMesh(bm);
  runUnstructuredTest("test_eb_1_cells_8");
}
