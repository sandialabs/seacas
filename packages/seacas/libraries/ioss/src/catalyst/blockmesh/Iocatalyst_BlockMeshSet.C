// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_CopyDatabase.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_IOFactory.h>
#include <Ioss_MeshCopyOptions.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_StructuredBlock.h>
#include <Ioss_Utils.h>
#include <catalyst/Iocatalyst_DatabaseIO.h>
#include <catalyst/blockmesh/Iocatalyst_BlockMeshSet.h>

namespace Iocatalyst {

  BlockMeshSet::BlockMeshSet() : region(nullptr), databaseIO(nullptr), numTimeSteps(0) {}

  BlockMeshSet::~BlockMeshSet()
  {
    region     = nullptr;
    databaseIO = nullptr;
  }

  void BlockMeshSet::printCatalystConduitNode() { conduitNode.print_detailed(); }

  void BlockMeshSet::addBlockMesh(const BlockMesh &blockMesh) { bms.push_back(blockMesh); }

  void BlockMeshSet::writeCGNSFile(const std::string &fileName) { writeFile(fileName, "cgns"); }

  void BlockMeshSet::setNumberOfTimeSteps(int numTimeSteps) { this->numTimeSteps = numTimeSteps; }

  void BlockMeshSet::writeCatalystCGNSFile(const std::string &fileName)
  {
    writeFile("dummy.db", "catalyst");
    Ioss::PropertyManager cdbProps;
    cdbProps.add(Ioss::Property("INTEGER_SIZE_API", 8));
    cdbProps.add(Ioss::Property("INTEGER_SIZE_DB", 8));
    cdbProps.add(Ioss::Property("CATALYST_CONDUIT_NODE", getCatalystConduitNode()));

    Ioss::DatabaseIO *cdbi = Ioss::IOFactory::create("catalyst", "dummy.db", Ioss::READ_RESTART,
                                                     MPI_COMM_WORLD, cdbProps);
    if (cdbi == nullptr || !cdbi->ok(true)) {
      return;
    }

    Ioss::PropertyManager properties;
    properties.add(Ioss::Property("INTEGER_SIZE_API", 8));
    properties.add(Ioss::Property("INTEGER_SIZE_DB", 8));
    properties.add(Ioss::Property("COMPOSE_RESULTS", "NO"));
    Ioss::DatabaseIO *cdbo =
        Ioss::IOFactory::create("cgns", fileName, Ioss::WRITE_RESULTS, MPI_COMM_WORLD, properties);

    if (cdbo == nullptr || !cdbo->ok(true)) {
      std::ostringstream errmsg;
      errmsg << "Unable to open IOSS database cat.cgns\n";
      IOSS_ERROR(errmsg);
    }

    Ioss::Region          cir(cdbi);
    Ioss::Region          cor(cdbo);
    Ioss::MeshCopyOptions options;
    options.data_storage_type = 1;
    Ioss::copy_database(cir, cor, options);
  }

  void BlockMeshSet::writeFile(const std::string &fileName, const std::string &dbType)
  {
    openIOSSDatabase(fileName, dbType);
    switchStateDefineModel();
    switchStateModel();
    switchStateDefineTransient();
    switchStateTransient();
    if (dbType == "catalyst") {
      auto c_node = reinterpret_cast<conduit_node *>(
          ((Iocatalyst::DatabaseIO *)databaseIO)->get_catalyst_conduit_node());
      conduit_node_set_node(conduit_cpp::c_node(&conduitNode), c_node);
    }
    closeIOSSDatabase();
  }

  void BlockMeshSet::openIOSSDatabase(const std::string &fileName, const std::string &dbType)
  {
    Ioss::PropertyManager properties;
    properties.add(Ioss::Property("INTEGER_SIZE_API", 8));
    properties.add(Ioss::Property("INTEGER_SIZE_DB", 8));
    properties.add(Ioss::Property("COMPOSE_RESULTS", "NO"));
    databaseIO =
        Ioss::IOFactory::create(dbType, fileName, Ioss::WRITE_RESULTS, MPI_COMM_WORLD, properties);

    if (databaseIO == nullptr || !databaseIO->ok(true)) {
      std::ostringstream errmsg;
      errmsg << "Unable to open IOSS database " + fileName + "\n";
      IOSS_ERROR(errmsg);
    }
    region = std::unique_ptr<Ioss::Region>(new Ioss::Region(databaseIO));
  }

  void BlockMeshSet::closeIOSSDatabase()
  {
    region     = nullptr;
    databaseIO = nullptr;
  }

  void BlockMeshSet::switchStateDefineModel()
  {
    region->begin_mode(Ioss::STATE_DEFINE_MODEL);
    writeStructuredBlockDefinitions();
    region->end_mode(Ioss::STATE_DEFINE_MODEL);
  }

  void BlockMeshSet::switchStateModel()
  {
    region->begin_mode(Ioss::STATE_MODEL);
    writeStructuredBlockBulkData();
    region->end_mode(Ioss::STATE_MODEL);
  }

  void BlockMeshSet::switchStateDefineTransient()
  {
    region->begin_mode(Ioss::STATE_DEFINE_TRANSIENT);
    region->end_mode(Ioss::STATE_DEFINE_TRANSIENT);
  }
  void BlockMeshSet::switchStateTransient() {
    region->begin_mode(Ioss::STATE_TRANSIENT);
    for(int time=0;time<numTimeSteps;time++) {
      int tstep =  region->add_state(time);
      region->begin_state(tstep);
      region->end_state(tstep);
    }
    region->end_mode(Ioss::STATE_TRANSIENT);
  }

  void BlockMeshSet::writeStructuredBlockDefinitions()
  {
    int spatialDims = 3;
    for (int i = 0; i < bms.size(); i++) {
      BlockMesh & bm            = bms[i];
      Ioss::IJK_t parentOffsets = {
          {bm.getLocalBlockStart().x, bm.getLocalBlockStart().y, bm.getLocalBlockStart().z}};
      Ioss::IJK_t globalSizes = {
          {bm.getGlobalNumBlocks().x, bm.getGlobalNumBlocks().y, bm.getGlobalNumBlocks().z}};
      Ioss::IJK_t localSizes = {
          {bm.getLocalNumBlocks().x, bm.getLocalNumBlocks().y, bm.getLocalNumBlocks().z}};
      Ioss::StructuredBlock *iossBlock =
          new Ioss::StructuredBlock(databaseIO, getStructuredBlockName(i), spatialDims, localSizes,
                                    parentOffsets, globalSizes);
      int node_count = (bm.getLocalNumBlocks().x + 1) * (bm.getLocalNumBlocks().y + 1) *
                       (bm.getLocalNumBlocks().z + 1);
      Ioss::NodeBlock *nodeBlock =
          new Ioss::NodeBlock(databaseIO, getStructuredNodeBlockName(i), node_count, spatialDims);
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
    for (int i = 0; i < bms.size(); i++) {
      BlockMesh &bm = bms[i];

      const int numI      = bm.getLocalNumBlocks().x + 1;
      const int numJ      = bm.getLocalNumBlocks().y + 1;
      const int numK      = bm.getLocalNumBlocks().z + 1;
      const int numPoints = numI * numJ * numK;

      coordx.resize(numPoints);
      coordy.resize(numPoints);
      coordz.resize(numPoints);

      for (int k = 0; k < numK; ++k) {
        const int kOffset = k * numI * numJ;
        for (int j = 0; j < numJ; ++j) {
          const int kjOffset = kOffset + j * numI;
          for (int i = 0; i < numI; ++i) {
            coordx[kjOffset + i] = i + origin.x + bm.getLocalBlockStart().x;
            coordy[kjOffset + i] = j + origin.y + bm.getLocalBlockStart().y;
            coordz[kjOffset + i] = k + origin.z + bm.getLocalBlockStart().z;
          }
        }
      }

      auto iossBlock = region->get_structured_block(getStructuredBlockName(i));
      iossBlock->put_field_data("mesh_model_coordinates_x", coordx);
      iossBlock->put_field_data("mesh_model_coordinates_y", coordy);
      iossBlock->put_field_data("mesh_model_coordinates_z", coordz);
      origin.x += bm.getGlobalNumBlocks().x;
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

} // namespace Iocatalyst