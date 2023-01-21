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

namespace Iocatalyst {

  class IOCATALYST_EXPORT BlockMeshSet
  {
  public:
    BlockMeshSet();
    ~BlockMeshSet();
    void  addBlockMesh(const BlockMesh &blockMesh);
    void  writeCGNSFile(const std::string &fileName);
    void  writeCatalystCGNSFile(const std::string &fileName);
    void  printCatalystConduitNode();
    void *getCatalystConduitNode();

  private:
    std::vector<BlockMesh>        bms;
    std::unique_ptr<Ioss::Region> region;
    Ioss::DatabaseIO             *databaseIO;
    conduit_cpp::Node             conduitNode;

    void writeFile(const std::string &fileName, const std::string &dbType);
    void openIOSSDatabase(const std::string &fileName, const std::string &dbType);
    void switchStateDefineModel();
    void switchStateModel();
    void switchStateDefineTransient();
    void switchStateTransient();
    void closeIOSSDatabase();
    void writeStructuredBlockDefinitions();
    void writeStructuredBlockBulkData();
    void writeTransientFieldDefinitions();
    void writeTransientBulkData();
    void saveConduitNode();
    bool isIOSSDatabaseCatalystType;

    std::string getStructuredBlockName(int index);
    std::string getStructuredNodeBlockName(int index);
  };

} // namespace Iocatalyst
