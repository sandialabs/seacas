// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include "iocatalyst_export.h"

namespace Iocatalyst {

  class IOCATALYST_EXPORT BlockMesh
  {
  public:
    struct Partition
    {
      int id;
      int size;
    };

    struct Point
    {
      double x;
      double y;
      double z;
    };

    struct Extent
    {
      int x;
      int y;
      int z;
    };

    BlockMesh();
    ~BlockMesh();

    void init(const Partition &part, const Extent &numBlocks);

    const Partition &getPartition() const { return partition; }

    const Point &getOrigin() const { return origin; }
    void         setOrigin(const Point &origin) { this->origin = origin; }

    const Extent &getGlobalNumBlocks() const { return globalNumBlocks; }

    const Point &getBlockLength() const { return blockLength; }
    void         setBlockLength(const Point &length);

    const Extent &getLocalNumBlocks() const { return localNumBlocks; }
    const Extent &getLocalBlockStart() const { return localBlockStart; }

    bool isLocalBlockEmpty();

  private:
    Partition partition;
    Point     origin;
    Extent    globalNumBlocks;
    Point     blockLength;
    Extent    localNumBlocks;
    Extent    localBlockStart;
    void      splitBlock();
    void      fillExtents(int *ext);
    void      setLocalBlockFromExtents(int *ext);
    void      setLocalBlockEmpty();
  };

} // namespace Iocatalyst
