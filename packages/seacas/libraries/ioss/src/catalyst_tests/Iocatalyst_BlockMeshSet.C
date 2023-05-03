// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_CopyDatabase.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_IOFactory.h>
#include <Ioss_MeshCopyOptions.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_StructuredBlock.h>
#include <Ioss_Utils.h>
#include <catalyst/Iocatalyst_DatabaseIO.h>
#include <catalyst_tests/Iocatalyst_BlockMeshSet.h>
#include <unordered_set>

namespace Iocatalyst {

  BlockMeshSet::BlockMeshSet()
      : databaseIO(nullptr), isWriteCatalyst(false), isWriteStructured(true), region(nullptr)
  {
  }

  BlockMeshSet::~BlockMeshSet()
  {
    region     = nullptr;
    databaseIO = nullptr;
  }

  void BlockMeshSet::printCatalystConduitNode() { conduitNode.print_detailed(); }

  void BlockMeshSet::addBlockMesh(const BlockMesh &blockMesh) { bms.push_back(blockMesh); }

  void *BlockMeshSet::getCatalystConduitNode() { return conduit_cpp::c_node(&conduitNode); }

  int BlockMeshSet::getNumLocalPointsInMeshSet()
  {
    std::unordered_set<BlockMesh::ID> pids;
    for (auto bm : bms) {
      BlockMesh::IDList ids = bm.getPartitionPointIDs();
      for (auto id : ids) {
        auto gid = bm.getGlobalIDForPointID(id);
        if (pids.find(gid) == pids.end()) {
          pids.insert(gid);
        }
      }
    }
    return pids.size();
  }

  void BlockMeshSet::writeCatalystIOSSFile(const std::string &fileName, const std::string &dbType)
  {
    if (dbType == EXODUS_DATABASE_TYPE) {
      isWriteStructured = false;
    }
    isWriteCatalyst = true;
    writeIOSSFile(CATALYST_DUMMY_DATABASE, CATALYST_DATABASE_TYPE);
    Ioss::PropertyManager cdbProps;
    cdbProps.add(Ioss::Property("CATALYST_CONDUIT_NODE", getCatalystConduitNode()));

    Ioss::DatabaseIO *cdbi =
        Ioss::IOFactory::create(CATALYST_DATABASE_TYPE, CATALYST_DUMMY_DATABASE, Ioss::READ_RESTART,
                                Ioss::ParallelUtils::comm_world(), cdbProps);
    if (cdbi == nullptr || !cdbi->ok(true)) {
      return;
    }

    Ioss::PropertyManager properties;
    Ioss::DatabaseIO     *cdbo = Ioss::IOFactory::create(dbType, fileName, Ioss::WRITE_RESULTS,
                                                         Ioss::ParallelUtils::comm_world(), properties);

    if (cdbo == nullptr || !cdbo->ok(true)) {
      std::ostringstream errmsg;
      errmsg << "Unable to open IOSS database " + fileName + "\n";
      IOSS_ERROR(errmsg);
    }

    Ioss::Region          cor(cdbo);
    Ioss::Region          cir(cdbi);
    Ioss::MeshCopyOptions options;
    options.data_storage_type = 1;
    Ioss::copy_database(cir, cor, options);
  }

  void BlockMeshSet::writeIOSSFile(const std::string &fileName, const std::string &dbType)
  {
    if (dbType == EXODUS_DATABASE_TYPE) {
      isWriteStructured = false;
    }
    openIOSSDatabase(fileName, dbType);
    switchStateDefineModel();
    switchStateModel();
    switchStateDefineTransient();
    switchStateTransient();
    closeIOSSDatabase();
  }

  void BlockMeshSet::openIOSSDatabase(const std::string &fileName, const std::string &dbType)
  {
    Ioss::PropertyManager properties;
    databaseIO = Ioss::IOFactory::create(dbType, fileName, Ioss::WRITE_RESULTS,
                                         Ioss::ParallelUtils::comm_world(), properties);

    if (databaseIO == nullptr || !databaseIO->ok(true)) {
      std::ostringstream errmsg;
      errmsg << "Unable to open IOSS database " + fileName + "\n";
      IOSS_ERROR(errmsg);
    }
    region = std::unique_ptr<Ioss::Region>(new Ioss::Region(databaseIO));
  }

  void BlockMeshSet::closeIOSSDatabase()
  {
    region.reset();
    databaseIO        = nullptr;
    isWriteCatalyst   = false;
    isWriteStructured = true;
  }

  void BlockMeshSet::switchStateDefineModel()
  {
    region->begin_mode(Ioss::STATE_DEFINE_MODEL);
    if (isWriteStructured) {
      writeStructuredBlockDefinitions();
    }
    else {
      writeUnstructuredBlockDefinitions();
    }
    region->end_mode(Ioss::STATE_DEFINE_MODEL);
  }

  void BlockMeshSet::switchStateModel()
  {
    region->begin_mode(Ioss::STATE_MODEL);
    if (isWriteStructured) {
      writeStructuredBlockBulkData();
    }
    else {
      writeUnstructuredBlockBulkData();
    }
    if (isWriteCatalyst) {
      saveConduitNode();
    }
    region->end_mode(Ioss::STATE_MODEL);
  }

  void BlockMeshSet::switchStateDefineTransient()
  {
    region->begin_mode(Ioss::STATE_DEFINE_TRANSIENT);
    if (isWriteStructured) {
      writeStructuredTransientFieldDefinitions();
    }
    else {
      writeUnstructuredTransientFieldDefinitions();
    }
    region->end_mode(Ioss::STATE_DEFINE_TRANSIENT);
  }
  void BlockMeshSet::switchStateTransient()
  {
    region->begin_mode(Ioss::STATE_TRANSIENT);
    int tstep = region->add_state(0.0);
    region->begin_state(tstep);
    if (isWriteStructured) {
      writeStructuredTransientBulkData();
    }
    else {
      writeUnstructuredTransientBulkData();
    }
    region->end_state(tstep);
    if (isWriteCatalyst) {
      saveConduitNode();
    }
    region->end_mode(Ioss::STATE_TRANSIENT);
  }

  void BlockMeshSet::saveConduitNode()
  {
    auto c_node = reinterpret_cast<conduit_node *>(
        ((Iocatalyst::DatabaseIO *)databaseIO)->get_catalyst_conduit_node());
    conduit_node_set_node(conduit_cpp::c_node(&conduitNode), c_node);
  }

  void BlockMeshSet::writeStructuredBlockDefinitions()
  {
    int spatialDims = 3;
    for (auto bm : bms) {
      Ioss::IJK_t parentOffsets = {
          {bm.getPartitionStart().i, bm.getPartitionStart().j, bm.getPartitionStart().k}};
      Ioss::IJK_t globalSizes = {{bm.getExtents().i, bm.getExtents().j, bm.getExtents().k}};
      Ioss::IJK_t localSizes  = {
          {bm.getPartitionExtents().i, bm.getPartitionExtents().j, bm.getPartitionExtents().k}};
      Ioss::StructuredBlock *iossBlock =
          new Ioss::StructuredBlock(databaseIO, getStructuredBlockName(bm.getID()), spatialDims,
                                    localSizes, parentOffsets, globalSizes);
      int node_count = (bm.getPartitionExtents().i + 1) * (bm.getPartitionExtents().j + 1) *
                       (bm.getPartitionExtents().k + 1);
      Ioss::NodeBlock *nodeBlock = new Ioss::NodeBlock(
          databaseIO, getStructuredNodeBlockName(bm.getID()), node_count, spatialDims);
      region->add(iossBlock);
      region->add(nodeBlock);
    }
  }

  void BlockMeshSet::writeStructuredBlockBulkData()
  {
    std::vector<double> coordx;
    std::vector<double> coordy;
    std::vector<double> coordz;
    BlockMesh::Point    origin;
    origin.x = 0.0;
    origin.y = 0.0;
    origin.z = 0.0;
    for (auto bm : bms) {
      const int numI      = bm.getPartitionExtents().i + 1;
      const int numJ      = bm.getPartitionExtents().j + 1;
      const int numK      = bm.getPartitionExtents().k + 1;
      const int numPoints = numI * numJ * numK;

      coordx.resize(numPoints);
      coordy.resize(numPoints);
      coordz.resize(numPoints);

      for (int k = 0; k < numK; ++k) {
        const int kOffset = k * numI * numJ;
        for (int j = 0; j < numJ; ++j) {
          const int kjOffset = kOffset + j * numI;
          for (int i = 0; i < numI; ++i) {
            coordx[kjOffset + i] =
                i * bm.BLOCK_LENGTH + bm.getOrigin().i + bm.getPartitionStart().i;
            coordy[kjOffset + i] =
                j * bm.BLOCK_LENGTH + bm.getOrigin().j + bm.getPartitionStart().j;
            coordz[kjOffset + i] =
                k * bm.BLOCK_LENGTH + bm.getOrigin().k + bm.getPartitionStart().k;
          }
        }
      }

      auto iossBlock = region->get_structured_block(getStructuredBlockName(bm.getID()));
      iossBlock->put_field_data("mesh_model_coordinates_x", coordx);
      iossBlock->put_field_data("mesh_model_coordinates_y", coordy);
      iossBlock->put_field_data("mesh_model_coordinates_z", coordz);
    }
  }

  void BlockMeshSet::writeStructuredTransientFieldDefinitions()
  {
    for (auto bm : bms) {
      auto iossBlock = region->get_structured_block(getStructuredBlockName(bm.getID()));
      iossBlock->field_add(Ioss::Field(IOSS_CELL_FIELD, Ioss::Field::REAL, IOSS_SCALAR_STORAGE,
                                       Ioss::Field::TRANSIENT));
      iossBlock->get_node_block().field_add(Ioss::Field(
          IOSS_POINT_FIELD, Ioss::Field::REAL, IOSS_SCALAR_STORAGE, Ioss::Field::TRANSIENT));
    }
  }

  void BlockMeshSet::writeStructuredTransientBulkData()
  {

    std::vector<double> values;
    for (auto bm : bms) {
      values.clear();
      auto iossBlock = region->get_structured_block(getStructuredBlockName(bm.getID()));
      for (int j = 0; j < iossBlock->get_field(IOSS_CELL_FIELD).raw_count(); j++) {
        values.push_back(bm.getPartition().id);
      }
      iossBlock->put_field_data(IOSS_CELL_FIELD, values);

      values.clear();
      for (int j = 0; j < iossBlock->get_node_block().get_field(IOSS_POINT_FIELD).raw_count();
           j++) {
        values.push_back(bm.getPartition().id);
      }
      iossBlock->get_node_block().put_field_data(IOSS_POINT_FIELD, values);
    }
  }

  void BlockMeshSet::writeUnstructuredBlockDefinitions()
  {
    int              spatialDims = 3;
    Ioss::NodeBlock *nodeBlock =
        new Ioss::NodeBlock(databaseIO, "nodeblock", getNumLocalPointsInMeshSet(), spatialDims);
    region->add(nodeBlock);
    for (auto bm : bms) {
      Ioss::ElementBlock *elemBlock = new Ioss::ElementBlock(
          databaseIO, getUnstructuredBlockName(bm.getID()), "hex8", bm.getNumPartitionBlocks());
      region->add(elemBlock);
    }
  }

  void BlockMeshSet::writeUnstructuredBlockBulkData()
  {
    Ioss::NodeBlock                  *nodeBlock = region->get_node_block("nodeblock");
    std::vector<double>               coordx;
    std::vector<double>               coordy;
    std::vector<double>               coordz;
    BlockMesh::IDList                 globalPointIds;
    std::unordered_set<BlockMesh::ID> pids;
    for (auto bm : bms) {
      BlockMesh::IDList ids = bm.getPartitionPointIDs();
      for (auto id : ids) {
        auto gid = bm.getGlobalIDForPointID(id);
        if (pids.find(gid) == pids.end()) {
          BlockMesh::Point point = bm.getPointCoordsForPointID(id);
          coordx.push_back(point.x);
          coordy.push_back(point.y);
          coordz.push_back(point.z);
          globalPointIds.push_back(gid);
          pids.insert(gid);
        }
      }
    }
    nodeBlock->put_field_data("mesh_model_coordinates_x", coordx);
    nodeBlock->put_field_data("mesh_model_coordinates_y", coordy);
    nodeBlock->put_field_data("mesh_model_coordinates_z", coordz);
    nodeBlock->put_field_data("ids", globalPointIds);

    for (auto bm : bms) {
      Ioss::ElementBlock *elemBlock =
          region->get_element_block(getUnstructuredBlockName(bm.getID()));

      std::vector<int>  connectivity(8 * bm.getNumPartitionBlocks());
      BlockMesh::IDList globalElemIds;
      BlockMesh::IDList ids = bm.getPartitionBlockIDs();

      for (int i = 0; i < ids.size(); i++) {
        BlockMesh::BlockConn conn = bm.getBlockConnectivityPointIDs(ids[i]);
        globalElemIds.push_back(bm.getGlobalIDForBlockID(ids[i]));
        for (int j = 0; j < conn.size(); j++) {
          connectivity[(i * 8) + j] = bm.getGlobalIDForPointID(conn[j]);
        }
      }
      elemBlock->put_field_data("connectivity", connectivity);
      elemBlock->put_field_data("ids", globalElemIds);
    }
  }

  void BlockMeshSet::writeUnstructuredTransientFieldDefinitions()
  {
    for (auto bm : bms) {
      auto elemBlock = region->get_element_block(getUnstructuredBlockName(bm.getID()));
      elemBlock->field_add(Ioss::Field(IOSS_CELL_FIELD, Ioss::Field::REAL, IOSS_SCALAR_STORAGE,
                                       Ioss::Field::TRANSIENT));
      auto nodeBlock = region->get_node_block("nodeblock");
      nodeBlock->field_add(Ioss::Field(IOSS_POINT_FIELD, Ioss::Field::REAL, IOSS_SCALAR_STORAGE,
                                       Ioss::Field::TRANSIENT));
    }
  }

  void BlockMeshSet::writeUnstructuredTransientBulkData()
  {
    std::vector<double> values;
    for (auto bm : bms) {
      values.clear();
      auto elemBlock = region->get_element_block(getUnstructuredBlockName(bm.getID()));
      for (int j = 0; j < elemBlock->get_field(IOSS_CELL_FIELD).raw_count(); j++) {
        values.push_back(bm.getPartition().id);
      }
      elemBlock->put_field_data(IOSS_CELL_FIELD, values);

      auto nodeBlock = region->get_node_block("nodeblock");
      values.clear();
      for (int j = 0; j < nodeBlock->get_field(IOSS_POINT_FIELD).raw_count(); j++) {
        values.push_back(bm.getPartition().id);
      }
      nodeBlock->put_field_data(IOSS_POINT_FIELD, values);
    }
  }

  std::string BlockMeshSet::getStructuredBlockName(int index)
  {
    return "StructuredBlock" + std::to_string(index);
  }

  std::string BlockMeshSet::getStructuredNodeBlockName(int index)
  {
    return "StructuredNodeBlock" + std::to_string(index);
  }

  std::string BlockMeshSet::getUnstructuredBlockName(int index)
  {
    return "UnstructuredBlock" + std::to_string(index);
  }

} // namespace Iocatalyst
