// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#ifndef IOSS_Iocatalyst_Block_Mesh_Set_h
#define IOSS_Iocatalyst_Block_Mesh_Set_h

#include <Ioss_Region.h>
#include <catalyst.hpp>
#include <catalyst_tests/Iocatalyst_BlockMesh.h>

namespace Iocatalyst {

  class BlockMeshSet
  {
  public:
    BlockMeshSet();
    ~BlockMeshSet();
    void  addBlockMesh(const BlockMesh &blockMesh);
    void  setNumberOfTimeSteps(int numTimeSteps);
    void  writeCGNSFile(const std::string &fileName);
    void  writeCatalystCGNSFile(const std::string &fileName);
    void  printCatalystConduitNode();
    void *getCatalystConduitNode() { return conduit_cpp::c_node(&conduitNode); }

  private:
    std::vector<BlockMesh>         bms;
    std::unique_ptr<Ioss::Region>  region;
    Ioss::DatabaseIO              *databaseIO;
    int                            numTimeSteps;
    std::vector<conduit_cpp::Node> conduitNodes;
    conduit_cpp::Node              conduitNode;

    void        writeFile(const std::string &fileName, const std::string &dbType);
    void        openIOSSDatabase(const std::string &fileName, const std::string &dbType);
    void        switchStateDefineModel();
    void        switchStateModel();
    void        switchStateDefineTransient();
    void        switchStateTransient();
    void        closeIOSSDatabase();
    void        writeStructuredBlockDefinitions();
    void        writeStructuredBlockBulkData();
    std::string getStructuredBlockName(int index);
    std::string getStructuredNodeBlockName(int index);
  };

} // namespace Iocatalyst
#endif