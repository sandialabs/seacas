// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include "iocatalyst_export.h"

#include <Ioss_Region.h>
#include <catalyst.hpp>
#include <catalyst_tests/Iocatalyst_BlockMesh.h>
#include <vector>

namespace Iocatalyst {

  class IOCATALYST_EXPORT BlockMeshSet
  {
  public:
    BlockMeshSet();
    ~BlockMeshSet();

    void  addBlockMesh(const BlockMesh &blockMesh);
    void  writeIOSSFile(const std::string &fileName, const std::string &dbType);
    void  writeCatalystIOSSFile(const std::string &fileName, const std::string &dbType);
    void  printCatalystConduitNode();
    void *getCatalystConduitNode();
    int   getNumLocalPointsInMeshSet();

  private:
    std::vector<BlockMesh>        bms;
    Ioss::DatabaseIO             *databaseIO;
    bool                          isWriteCatalyst;
    bool                          isWriteStructured;
    std::unique_ptr<Ioss::Region> region;
    conduit_cpp::Node             conduitNode;

    void openIOSSDatabase(const std::string &fileName, const std::string &dbType);
    void closeIOSSDatabase();

    void switchStateDefineModel();
    void switchStateModel();
    void switchStateDefineTransient();
    void switchStateTransient();

    void writeStructuredBlockDefinitions();
    void writeStructuredBlockBulkData();
    void writeStructuredTransientFieldDefinitions();
    void writeStructuredTransientBulkData();

    void writeUnstructuredBlockDefinitions();
    void writeUnstructuredBlockBulkData();
    void writeUnstructuredTransientFieldDefinitions();
    void writeUnstructuredTransientBulkData();

    void saveConduitNode();

    std::string getStructuredBlockName(int index);
    std::string getStructuredNodeBlockName(int index);

    std::string getUnstructuredBlockName(int index);

    const std::string CGNS_DATABASE_TYPE      = "cgns";
    const std::string EXODUS_DATABASE_TYPE    = "exodus";
    const std::string CATALYST_DATABASE_TYPE  = "catalyst";
    const std::string CATALYST_DUMMY_DATABASE = "dummy.db";
    const std::string IOSS_CELL_FIELD         = "cell";
    const std::string IOSS_POINT_FIELD        = "point";
    const std::string IOSS_SCALAR_STORAGE     = "scalar";
  };

} // namespace Iocatalyst
