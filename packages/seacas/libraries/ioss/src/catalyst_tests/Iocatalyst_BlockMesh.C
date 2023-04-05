// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_Utils.h>
#include <catalyst_tests/Iocatalyst_BlockMesh.h>

namespace Iocatalyst {

  BlockMesh::BlockMesh()
  {
    partition.id      = 0;
    partition.size    = 1;
    origin.x          = 0.0;
    origin.y          = 0.0;
    origin.z          = 0.0;
    globalNumBlocks.x = 1;
    globalNumBlocks.y = 1;
    globalNumBlocks.z = 1;
    blockLength.x     = 1.0;
    blockLength.y     = 1.0;
    blockLength.z     = 1.0;
    localNumBlocks.x  = 1;
    localNumBlocks.y  = 1;
    localNumBlocks.z  = 1;
    localBlockStart.x = 0;
    localBlockStart.y = 0;
    localBlockStart.z = 0;
  }

  BlockMesh::~BlockMesh() {}

  void BlockMesh::init(const Partition &part, const Extent &numBlocks)
  {
    if (part.id < 0 || part.size < 0 || part.id >= part.size) {
      std::ostringstream errmsg;
      errmsg << "Invalid partition: id = " << part.id << std::string(", size = ") << part.size
             << "\n";
      IOSS_ERROR(errmsg);
    }
    if (numBlocks.x <= 0 || numBlocks.y <= 0 || numBlocks.z <= 0) {
      std::ostringstream errmsg;
      errmsg << "Invalid numBlocks: x = " << numBlocks.x << std::string(", y = ") << numBlocks.y
             << std::string(", z = ") << numBlocks.z << "\n";
      IOSS_ERROR(errmsg);
    }
    this->partition       = part;
    this->globalNumBlocks = numBlocks;
    splitBlock();
  }

  void BlockMesh::setBlockLength(const Point &length)
  {
    if (length.x < 0 || length.y < 0 || length.z < 0) {
      std::ostringstream errmsg;
      errmsg << "Invalid length: x = " << length.x << std::string(", y = ") << length.y
             << std::string(", z = ") << length.z << "\n";
      IOSS_ERROR(errmsg);
    }
    this->blockLength = length;
  }

  void BlockMesh::splitBlock()
  {
    // Split algorithm from vtkExtentTranslator.cxx SplitExtent()

    unsigned long size[3];
    int           numPiecesInFirstHalf;
    int           splitAxis;
    long int      mid;
    int           ext[6];
    int           numPieces = getPartition().size;
    int           piece     = getPartition().id;
    fillExtents(ext);
    setLocalBlockFromExtents(ext);

    while (numPieces > 1) {
      size[0] = ext[1] - ext[0];
      size[1] = ext[3] - ext[2];
      size[2] = ext[5] - ext[4];

      if (size[2] >= size[1] && size[2] >= size[0] && size[2] / 2 >= 1) {
        splitAxis = 2;
      }
      else if (size[1] >= size[0] && size[1] / 2 >= 1) {
        splitAxis = 1;
      }
      else if (size[0] / 2 >= 1) {
        splitAxis = 0;
      }
      else {
        splitAxis = -1;
      }

      if (splitAxis == -1) {
        if (piece == 0) {
          numPieces = 1;
        }
        else {
          setLocalBlockEmpty();
          return;
        }
      }
      else {
        numPiecesInFirstHalf = (numPieces / 2);
        mid                  = size[splitAxis];
        mid                  = (mid * numPiecesInFirstHalf) / numPieces + ext[splitAxis * 2];
        if (piece < numPiecesInFirstHalf) {
          ext[splitAxis * 2 + 1] = mid;
          numPieces              = numPiecesInFirstHalf;
        }
        else {
          ext[splitAxis * 2] = mid;
          numPieces          = numPieces - numPiecesInFirstHalf;
          piece -= numPiecesInFirstHalf;
        }
      }
    }
    setLocalBlockFromExtents(ext);
  }

  void BlockMesh::fillExtents(int *ext)
  {
    if (getGlobalNumBlocks().x == 0) {
      ext[0] = 0;
      ext[1] = -1;
    }
    else {
      ext[0] = 0;
      ext[1] = getGlobalNumBlocks().x;
    }

    if (getGlobalNumBlocks().y == 0) {
      ext[2] = 0;
      ext[3] = -1;
    }
    else {
      ext[2] = 0;
      ext[3] = getGlobalNumBlocks().y;
    }

    if (getGlobalNumBlocks().z == 0) {
      ext[4] = 0;
      ext[5] = -1;
    }
    else {
      ext[4] = 0;
      ext[5] = getGlobalNumBlocks().z;
    }
  }

  void BlockMesh::setLocalBlockFromExtents(int *ext)
  {
    int sizeX = ext[1] - ext[0];
    int sizeY = ext[3] - ext[2];
    int sizeZ = ext[5] - ext[4];
    if (sizeX <= 0 || sizeY <= 0 || sizeZ <= 0) {
      setLocalBlockEmpty();
      return;
    }

    localNumBlocks.x  = sizeX;
    localBlockStart.x = ext[0];

    localNumBlocks.y  = sizeY;
    localBlockStart.y = ext[2];

    localNumBlocks.z  = sizeZ;
    localBlockStart.z = ext[4];
  }

  void BlockMesh::setLocalBlockEmpty()
  {
    localNumBlocks.x  = 0;
    localNumBlocks.y  = 0;
    localNumBlocks.z  = 0;
    localBlockStart.x = 0;
    localBlockStart.y = 0;
    localBlockStart.z = 0;
  }

  bool BlockMesh::isLocalBlockEmpty()
  {
    return localNumBlocks.x == 0 || localNumBlocks.y == 0 || localNumBlocks.z == 0;
  }

  BlockMesh::IDList BlockMesh::getLocalPointIDs() { return getLocalIDs(POINT_OFFSET); }

  int BlockMesh::getGlobalPointIDfromCoords(int i, int j, int k)
  {
    return getGlobalIDfromCoords(i, j, k, POINT_OFFSET);
  }

  BlockMesh::Point BlockMesh::getPointCoordsForGlobalPointID(int globalPointID)
  {
    BlockMesh::Point p;
    BlockMesh::IJK   ijk = getCoordsForGlobalID(globalPointID, POINT_OFFSET);
    int              i   = ijk[0];
    int              j   = ijk[1];
    int              k   = ijk[2];
    p.x                  = origin.x + i * blockLength.x;
    p.y                  = origin.y + j * blockLength.y;
    p.z                  = origin.z + k * blockLength.z;
    return p;
  }

  BlockMesh::IDList BlockMesh::getLocalBlockIDs() { return getLocalIDs(BLOCK_OFFSET); }

  BlockMesh::BlockConn BlockMesh::getBlockConnectivityGlobalPointIDs(int globalBlockID)
  {
    BlockMesh::BlockConn conn;
    BlockMesh::IJK       ijk = getCoordsForGlobalID(globalBlockID, BLOCK_OFFSET);
    int                  i   = ijk[0];
    int                  j   = ijk[1];
    int                  k   = ijk[2];
    conn[0]                  = getGlobalPointIDfromCoords(i, j, k);
    conn[1]                  = getGlobalPointIDfromCoords(i + 1, j, k);
    conn[2]                  = getGlobalPointIDfromCoords(i + 1, j + 1, k);
    conn[3]                  = getGlobalPointIDfromCoords(i, j + 1, k);
    conn[4]                  = getGlobalPointIDfromCoords(i, j, k + 1);
    conn[5]                  = getGlobalPointIDfromCoords(i + 1, j, k + 1);
    conn[6]                  = getGlobalPointIDfromCoords(i + 1, j + 1, k + 1);
    conn[7]                  = getGlobalPointIDfromCoords(i, j + 1, k + 1);
    return conn;
  }

  BlockMesh::IDList BlockMesh::getLocalIDs(int offset)
  {
    BlockMesh::IDList ids;
    if (!isLocalBlockEmpty()) {
      for (int k = localBlockStart.z; k < localBlockStart.z + localNumBlocks.z + offset; k++) {
        for (int j = localBlockStart.y; j < localBlockStart.y + localNumBlocks.y + offset; j++) {
          for (int i = localBlockStart.x; i < localBlockStart.x + localNumBlocks.x + offset; i++) {
            ids.push_back(getGlobalIDfromCoords(i, j, k, offset));
          }
        }
      }
    }
    return ids;
  }

  int BlockMesh::getGlobalIDfromCoords(int i, int j, int k, int offset)
  {
    int numPointsX = globalNumBlocks.x + offset;
    int numPointsY = globalNumBlocks.y + offset;
    return k * numPointsX * numPointsY + j * numPointsX + i + 1;
  }

  BlockMesh::IJK BlockMesh::getCoordsForGlobalID(int globalID, int offset)
  {
    BlockMesh::IJK ijk;
    int            zeroBasedID = globalID - 1;
    int            numBlocksX  = globalNumBlocks.x + offset;
    int            numBlocksY  = globalNumBlocks.y + offset;
    int            numBlocksXY = numBlocksX * numBlocksY;
    ijk[2]                     = zeroBasedID / numBlocksXY;
    ijk[1]                     = (zeroBasedID - ijk[2] * numBlocksXY) / numBlocksX;
    ijk[0]                     = zeroBasedID - ijk[2] * numBlocksXY - ijk[1] * numBlocksX;
    return ijk;
  }

} // namespace Iocatalyst
