// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include "iocatalyst_export.h"

#include "gtest/gtest.h"
#include <catalyst_tests/Iocatalyst_BlockMeshSet.h>

class IOCATALYST_EXPORT Iocatalyst_DatabaseIOTest : public ::testing::Test
{
protected:
  Iocatalyst::BlockMeshSet         bmSet;
  Iocatalyst::BlockMesh::Partition part;
  Iocatalyst::BlockMesh::Extent    numBlocks;
  Ioss::ParallelUtils              putils;

  Iocatalyst_DatabaseIOTest();

  bool regionsAreEqual(const std::string &fileName, const std::string &catFileName);

  void runTest(const std::string &testName);

  void initBlock(Iocatalyst::BlockMesh &blockMesh, int x, int y, int z);
};
