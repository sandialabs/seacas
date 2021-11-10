// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ionit_Initializer.h>
#include <Ioss_DBUsage.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_PropertyManager.h>
#include <Ioss_Region.h>

#include "Ioss_DatabaseIO.h"      // for DatabaseIO
#include "Ioss_EntityType.h"      // for EntityType, etc
#include "Ioss_Field.h"           // for Field, etc
#include "Ioss_GroupingEntity.h"  // for GroupingEntity
#include "Ioss_IOFactory.h"       // for IOFactory
#include "Ioss_MeshType.h"        // for MeshType, etc

#include <gtest/gtest.h>
#include <mpi.h>
#include <string>
#include <unordered_map>

#include <fmt/ostream.h>

#include <string>
#include <vector>
#include <strings.h>

#include "Ioss_CommSet.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_StandardElementTypes.h"
#include "Ioss_ParallelUtils.h"

#include "text_mesh/Iotm_DatabaseIO.h"

#define ThrowRequireWithMsg(expr, message)                                   \
  do {                                                                       \
    if ( !(expr) ) {                                                         \
      std::ostringstream internal_throw_require_oss;                         \
      internal_throw_require_oss << message;                                 \
      IOSS_ERROR(internal_throw_require_oss );                               \
    }                                                                        \
  } while (false)

namespace
{
int get_parallel_size()
{
  return Ioss::ParallelUtils(MPI_COMM_WORLD).parallel_size();
}

int get_parallel_rank()
{
  return Ioss::ParallelUtils(MPI_COMM_WORLD).parallel_rank();
}

void filename_substitution(std::string &filename)
{
  // See if filename contains "%P" which is replaced by the number of processors...
  // Assumes that %P only occurs once...
  // filename is changed.
  size_t pos = filename.find("%P");
  if (pos != std::string::npos) {
    // Found the characters...  Replace with the processor count...
    int num_proc = std::max(1, get_parallel_size());
    std::string tmp(filename, 0, pos);
    tmp += std::to_string(num_proc);
    tmp += filename.substr(pos+2);
    filename = tmp;
  }
}

template <typename T>
size_t field_data_from_ioss(Ioss::GroupingEntity *io_entity,
                          const std::string &io_fld_name,
                          std::vector<T>& io_field_data)
{
  size_t io_entity_count = 0;
  if (io_entity->field_exists(io_fld_name)) {
    const Ioss::Field &io_field = io_entity->get_fieldref(io_fld_name);
    io_entity_count = io_entity->get_field_data(io_field.get_name(), io_field_data);
  }
  return io_entity_count;
}

class TextMeshFixture
{
 public:
  using EntityId = int64_t;
  using EntityIdVector = std::vector<EntityId>;
  using EntityIdSet = std::set<EntityId>;

  using PartNameId = std::pair<std::string, unsigned>;

  struct ElementInfo {
    const Ioss::ElementTopology* topology;
    EntityIdVector connectivity;

    ElementInfo() : topology(nullptr) { }
    ElementInfo(const Ioss::ElementTopology* topology_, const EntityIdVector& connectivity_)
    : topology(topology_)
    , connectivity(connectivity_) {}
  };

  struct PartInfo {
    std::string blockName;
    EntityIdSet ids;
  };

  TextMeshFixture()
  {
    Ioss::Init::Initializer io;
    Iotm::IOFactory::factory();
  }

  ~TextMeshFixture()
  {
    delete m_region;
  }

  void fill_mesh(const std::string& meshDesc)
  {
    std::pair<std::string, std::string> result = get_database_type_and_filename(meshDesc);

    std::string type = result.first;
    std::string filename = result.second;

    EXPECT_EQ("textmesh", type);

    create_database(filename, type);
    create_ioss_region();
  }

  MPI_Comm get_comm() const { return communicator;}
  void set_comm(MPI_Comm comm) {communicator = comm;}

  std::string get_mesh_desc(const std::string& textMeshDesc)
  {
    std::string header = "textmesh:";
    std::string meshDesc = header + textMeshDesc;
    return meshDesc;
  }

  std::string get_mesh_desc(const std::string& textMeshDesc, std::vector<double>& coordVec)
  {
    std::stringstream coords;
    coords << "|coordinates:";

    for (double coord : coordVec) {
      coords << coord << ",";
    }

    std::string meshDesc = get_mesh_desc(textMeshDesc) + coords.str();
    return meshDesc;
  }

  std::string get_mesh_desc(const std::string& textMeshDesc, unsigned dimension)
  {
    std::stringstream dim;
    dim << "|dimension:" << dimension;

    std::string meshDesc = get_mesh_desc(textMeshDesc) + dim.str();
    return meshDesc;
  }

  std::string get_mesh_desc(const std::string& textMeshDesc, std::vector<double>& coordVec, unsigned dimension)
  {
    std::stringstream dim;
    dim << "|dimension:" << dimension;

    std::string meshDesc = get_mesh_desc(textMeshDesc, coordVec) + dim.str();
    return meshDesc;
  }

  void verify_shared_nodes(const EntityIdVector& nodeIds, int sharingProc)
  {
    EXPECT_EQ(nodeIds.size(), get_node_sharing_count());

    for (EntityId nodeId : nodeIds) {
      EXPECT_TRUE(node_is_shared_with_proc(nodeId, sharingProc));
    }
  }

  void verify_num_elements(size_t goldCount)
  {
    size_t count = get_element_count();
    EXPECT_EQ(goldCount, count);
  }

  void verify_single_element(EntityId elemId, Ioss::ElementTopology* topology, const EntityIdVector& nodeIds)
  {
    ElementInfo info = get_element_info(elemId);
    EXPECT_TRUE(is_valid_element(info));
    EXPECT_EQ(topology, info.topology);
    verify_nodes_on_element(info, nodeIds);
  }

  void verify_part_membership(const std::vector<PartInfo> golds)
  {
    for (const PartInfo& gold : golds) {
      Ioss::ElementBlock* block = get_element_block(gold.blockName);

      verify_block(block);
      verify_elements_on_block(block, gold.ids);
    }
  }

  void verify_part_ids(const std::vector<PartNameId>& golds)
  {
    for (const PartNameId& gold : golds) {
      Ioss::ElementBlock* block = get_element_block(gold.first);

      verify_block(block);
      unsigned id = block->get_property("id").get_int();
      EXPECT_EQ(id, gold.second);
    }
  }

  void verify_nodes_on_element(const ElementInfo& info, const EntityIdVector& goldNodeIds)
  {
    EXPECT_EQ(goldNodeIds, info.connectivity);
  }

  void verify_coordinates(const EntityIdVector& goldNodeIds, const std::vector<double>& goldCoordinates)
  {
    CoordinateVerifier cv(*m_region, goldNodeIds, goldCoordinates);
    cv.verify();
  }

 protected:
  size_t db_api_int_size() const
  {
    assert(m_database != nullptr);
    return m_database->int_byte_size_api();
  }

  size_t get_node_sharing_count() const
  {
    ThrowRequireWithMsg(m_region != nullptr, "Ioss region has not been created");

    Ioss::CommSet* io_cs = m_region->get_commset("commset_node");
    size_t numSharings = io_cs->get_field("entity_processor").raw_count();

    return numSharings;
  }

  template<typename INT>
  EntityIdVector get_element_ids_from_block_impl(const Ioss::ElementBlock* block) const
  {
    EntityIdVector elemIds;

    std::vector<INT> ids ;

    block->get_field_data("ids", ids);

    for(INT id : ids) {
      elemIds.push_back(static_cast<EntityId>(id));
    }

    return elemIds;
  }

  EntityIdVector get_element_ids_from_block(const Ioss::ElementBlock* block) const
  {
    if(db_api_int_size() == 4) {
      return get_element_ids_from_block_impl<int>(block);
    } else {
      return get_element_ids_from_block_impl<int64_t>(block);
    }
  }

  template<typename INT>
  ElementInfo get_element_info_from_block_impl(EntityId elemId, const Ioss::ElementBlock* block) const
  {
    const Ioss::ElementTopology* topo = nullptr;
    EntityIdVector elemConn;

    std::vector<INT> connectivity ;
    std::vector<INT> elemIds ;

    block->get_field_data("ids", elemIds);
    block->get_field_data("connectivity", connectivity);

    topo = block->topology();

    size_t elementCount = elemIds.size();
    int nodesPerElem = topo->number_nodes();

    for(size_t i=0; i<elementCount; ++i) {
      INT *conn = &connectivity[i*nodesPerElem];
      EntityId id = static_cast<EntityId>(elemIds[i]);

      if(id == elemId) {
        for(int j=0; j<nodesPerElem; j++) {
          elemConn.push_back(conn[j]);
        }

        return ElementInfo(topo, elemConn);
      }
    }

    return ElementInfo(nullptr, EntityIdVector());
  }

  template<typename INT>
  ElementInfo get_element_info_impl(EntityId elemId) const
  {
    ThrowRequireWithMsg(m_region != nullptr, "Ioss region has not been created");
    ElementInfo elemInfo;

    EntityIdVector elemConn;

    const Ioss::ElementBlockContainer& elemBlocks = m_region->get_element_blocks();
    bool found = false;

    for(const Ioss::ElementBlock* block : elemBlocks) {
      ElementInfo info = get_element_info_from_block_impl<INT>(elemId, block);

      if(is_valid_element(info)) {
        ThrowRequireWithMsg(!found, "Element with id " << elemId << " exists in more than one block!");
        found = true;
        elemInfo = info;
      }
    }

    return elemInfo;
  }

  ElementInfo get_element_info(EntityId elemId) const
  {
    if(db_api_int_size() == 4) {
      return get_element_info_impl<int>(elemId);
    } else {
      return get_element_info_impl<int64_t>(elemId);
    }
  }

  template<typename INT>
  size_t get_element_count_impl() const
  {
    ThrowRequireWithMsg(m_region != nullptr, "Ioss region has not been created");

    const Ioss::ElementBlockContainer& elemBlocks = m_region->get_element_blocks();
    size_t count = 0;

    for(const Ioss::ElementBlock* block : elemBlocks) {
      std::vector<INT> elemIds ;
      block->get_field_data("ids", elemIds);
      count += elemIds.size();
    }

    return count;
  }

  size_t get_element_count() const
  {
    if(db_api_int_size() == 4) {
      return get_element_count_impl<int>();
    } else {
      return get_element_count_impl<int64_t>();
    }
  }

  template<typename INT>
  bool node_is_shared_with_proc_impl(EntityId nodeId, int sharingProc) const
  {
    ThrowRequireWithMsg(m_region != nullptr, "Ioss region has not been created");

    Ioss::CommSet* io_cs = m_region->get_commset("commset_node");
    size_t numSharings = io_cs->get_field("entity_processor").raw_count();

    std::vector<INT> entityProc;
    io_cs->get_field_data("entity_processor", entityProc);

    for (size_t i = 0; i < numSharings; ++i) {
        EntityId iossNodeId = entityProc[i*2];
        int iossSharingProc = entityProc[i*2+1];

        if(iossNodeId == nodeId && iossSharingProc == sharingProc){
          return true;
        }
    }

    return false;
  }

  bool node_is_shared_with_proc(EntityId nodeId, int sharingProc) const
  {
    if(db_api_int_size() == 4) {
      return node_is_shared_with_proc_impl<int>(nodeId, sharingProc);
    } else {
      return node_is_shared_with_proc_impl<int64_t>(nodeId, sharingProc);
    }
  }

  bool is_valid_element(const ElementInfo& info) const
  {
    bool validTopology = info.topology != nullptr && info.topology != Ioss::ElementTopology::factory(Ioss::Unknown::name);
    bool validConnectivitySize = info.connectivity.size() != 0;
    bool validNumNodes = validTopology ? info.topology->number_nodes() == static_cast<int>(info.connectivity.size()) : false;

    return validConnectivitySize && validNumNodes;
  }

  Ioss::ElementBlock* get_element_block(const std::string& blockName) const
  {
    ThrowRequireWithMsg(m_region != nullptr, "Ioss region has not been created");

    const Ioss::ElementBlockContainer& elemBlocks = m_region->get_element_blocks();
    Ioss::ElementBlock* elemBlock = nullptr;

    for(Ioss::ElementBlock* block : elemBlocks) {
      if(strcasecmp(block->name().c_str(), blockName.c_str()) == 0) {
        elemBlock = block;
      }
    }

    return elemBlock;
  }

  void verify_block(Ioss::ElementBlock* block)
  {
    ASSERT_TRUE(block != nullptr);
  }

  void verify_elements_on_block(const Ioss::ElementBlock* block, const std::set<EntityId>& goldIds)
  {
    EntityIdVector elemIds = get_element_ids_from_block(block);

    ASSERT_EQ(goldIds.size(), elemIds.size());
    for (EntityId elemId : elemIds) {
      EXPECT_EQ(1u, goldIds.count(elemId));
    }
  }

  void create_ioss_region()
  {
    if (m_region == nullptr) {
      EXPECT_TRUE(m_database != nullptr);
      m_region = new Ioss::Region(m_database, "input_model");
      EXPECT_TRUE(m_region != nullptr);
    }
  }

  void create_database(const std::string& fileName, const std::string& meshType)
  {
    if(m_database == nullptr) {
      Ioss::DatabaseUsage db_usage = Ioss::READ_MODEL;

      std::string meshFileName = fileName;
      filename_substitution(meshFileName);
      m_database = Ioss::IOFactory::create(meshType, meshFileName, db_usage, get_comm(), m_propertyManager);
      EXPECT_TRUE(m_database != nullptr);
      EXPECT_TRUE(m_database->ok(true));
      EXPECT_EQ(m_database->get_format(), "TextMesh");
    }
  }

  std::pair<std::string, std::string> get_database_type_and_filename(const std::string& meshDesc)
  {
    std::string type;
    std::string filename;

    size_t colon = meshDesc.find(':');
    if (colon != std::string::npos && colon > 0) {
      type = meshDesc.substr(0, colon);
      filename = meshDesc.substr(colon + 1);
    } else {
      type = "textmesh";
      filename = meshDesc;
    }

    return std::make_pair(type, filename);
  }

  class CoordinateVerifier
  {
   public:
    CoordinateVerifier(const Ioss::Region& r, const EntityIdVector& ids, const std::vector<double>& coords)
    : region(r),
      spatialDim(region.get_property("spatial_dimension").get_int()),
      goldNodeIds(ids),
      goldCoordinates(coords)
    {
      fill_coordinates_from_ioss();
    }

    void verify()
    {
      verify_num_nodes();

      for (size_t nodeIndex = 0; nodeIndex < goldNodeIds.size(); nodeIndex++) {
        EntityId nodeId = goldNodeIds[nodeIndex];
        EXPECT_TRUE(is_valid_node(nodeId));

        const double* nodalCoords = get_nodal_coordinates(nodeId);
        const double* goldCoords = &goldCoordinates[nodeIndex * spatialDim];

        verify_nodal_coordinates(nodeId, goldCoords, nodalCoords);
      }
    }

   private:
    size_t db_api_int_size() const
    {
      return region.get_database()->int_byte_size_api();
    }

    template <typename INT>
    EntityIdVector get_node_ids_impl() const
    {
      const Ioss::NodeBlockContainer& node_blocks = region.get_node_blocks();
      assert(node_blocks.size() == 1);

      Ioss::NodeBlock *nb = node_blocks[0];

      std::vector<INT> ids;
      nb->get_field_data("ids", ids);

      EntityIdVector nodeIds;
      for(INT id : ids) {
        nodeIds.push_back(static_cast<EntityId>(id));
      }

      return nodeIds;
    }

    EntityIdVector get_node_ids() const
    {
      if(db_api_int_size() == 4) {
        return get_node_ids_impl<int>();
      } else {
        return get_node_ids_impl<int64_t>();
      }
    }

    template <typename INT>
    bool is_valid_node_impl(EntityId nodeId) const
    {
      EntityIdVector ids = get_node_ids_impl<INT>();
      auto iter = std::find(ids.begin(), ids.end(), INT(nodeId));
      return iter != ids.end();
    }

    bool is_valid_node(EntityId nodeId) const
    {
      if(db_api_int_size() == 4) {
        return is_valid_node_impl<int>(nodeId);
      } else {
        return is_valid_node_impl<int64_t>(nodeId);
      }
    }

    void verify_num_nodes()
    {
      EntityIdVector ids = get_node_ids();
      EXPECT_EQ(goldNodeIds.size(), ids.size());
    }

    void fill_coordinate_map(const EntityIdVector& nodeIds, const std::vector<double>& coordinates)
    {
      std::vector<double>::const_iterator coordIter = coordinates.begin();
      for (const EntityId& nodeId : nodeIds) {
        m_nodalCoords[nodeId] = std::vector<double>(coordIter, coordIter + spatialDim);
        coordIter += spatialDim;
      }
    }

    void fill_coordinates_from_ioss()
    {
      std::vector<double> iossCoordinates;

      const Ioss::NodeBlockContainer& node_blocks = region.get_node_blocks();
      assert(node_blocks.size() == 1);

      Ioss::NodeBlock *nb = node_blocks[0];

      size_t node_count = nb->get_property("entity_count").get_int();

      EntityIdVector nodeIds = get_node_ids();

      size_t numIossNodes = field_data_from_ioss<double>(nb, "mesh_model_coordinates", iossCoordinates);
      ThrowRequireWithMsg(node_count == numIossNodes, "Node count mismatch");
      ThrowRequireWithMsg(iossCoordinates.size() == numIossNodes*spatialDim, "Invalid coordinate data size");

      fill_coordinate_map(nodeIds, iossCoordinates);
    }

    const std::vector<double>& operator[](const EntityId nodeId) const
    {
      auto it(m_nodalCoords.find(nodeId));
      return it->second;
    }

    const double* get_nodal_coordinates(const EntityId& nodeId) const
    {
      return (*this)[nodeId].data();
    }

    void verify_nodal_coordinates(const EntityId& nodeId, const double* goldCoords, const double* nodalCoords)
    {
      for (unsigned i = 0; i < spatialDim; i++) {
        EXPECT_NEAR(goldCoords[i], nodalCoords[i], 1.0e-9) << error_message(nodeId, i);
      }
    }

    std::string error_message(const EntityId& nodeId, unsigned coordIndex)
    {
      std::stringstream message;
      message << "Proc " << region.get_database()->util().parallel_rank() << ", Node ID " << nodeId << ", coord index " << coordIndex;
      return message.str();
    }

    const Ioss::Region& region;

    const unsigned spatialDim;

    const EntityIdVector& goldNodeIds;
    const std::vector<double>& goldCoordinates;
    std::unordered_map<EntityId, std::vector<double>> m_nodalCoords;
  };

  Ioss::PropertyManager m_propertyManager;
  Ioss::DatabaseIO* m_database = nullptr;
  Ioss::Region* m_region = nullptr;
  MPI_Comm communicator = MPI_COMM_WORLD;
};

using EntityIdVector = TextMeshFixture::EntityIdVector;

TEST(IossTextMesh, singleHex)
{
  if (get_parallel_size() != 1) return;

  std::string textMeshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8";
  EntityIdVector nodeIds = EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8};

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(textMeshDesc));

  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), nodeIds);
}

TEST(IossTextMesh, singleHexWithCoordinates)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8";
  EntityIdVector nodeIds = EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8};
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1};

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), nodeIds);
  fixture.verify_coordinates(nodeIds, coordinates);
}

TEST(IossTextMesh, singleHexWithCoordinates_separatedNodeIds)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,HEX_8,1,2,3,4,11,12,13,14";
  EntityIdVector nodeIds = EntityIdVector{1, 2, 3, 4, 11, 12, 13, 14};
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1};

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), nodeIds);
  fixture.verify_coordinates(nodeIds, coordinates);
}

TEST(IossTextMesh, twoHexesSerial)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1,2,3,4,5,6,7,8\n"
      "0,2,HEX_8,5,6,7,8,9,10,11,12";

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{5, 6, 7, 8, 9, 10, 11, 12});
}

TEST(IossTextMesh, twoTet10Serial)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,TET_10,1,2,3,4,5,6,7,8,9,10\n"
      "0,2,TET_10,2,11,3,4,12,13,6,9,14,10";
  //                                       1       2      3        4          5          6         7           8
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 0.5, 1, 0, 0.5, 0.5, 1, 0.5, 0, 0, 0.75, 0.5, 0, 0.25, 0.5, 0,
      0.25, 0.25, 0.5,
      //                                       9              10            11         12           13        14
      0.75, 0.25, 0.5, 0.5, 0.75, 0.5, 1.5, 0.5, 0, 1.25, 0.25, 0, 1, 0.75, 0, 1, 0.5, 0.5};

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Tet10::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8, 9, 10});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Tet10::name), EntityIdVector{2, 11, 3, 4, 12, 13, 6, 9, 14, 10});
}

TEST(IossTextMesh, twoHexDisconnectedWithCoordinatesAndParts)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,block_1\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,block_2";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1,
      0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 2, 1, 0, 2, 1, 1, 2, 0, 1, 2};

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{9, 10, 11, 12, 13, 14, 15, 16});
  fixture.verify_coordinates(EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}, coordinates);
  fixture.verify_part_membership({{"block_1", {1u}}, {"block_2", {2u}}});
}

TEST(IossTextMesh, twoHexDisconnectedWithCoordinatesAndParts_reversedBlockmembership)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,block_2\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,block_1";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1,
      0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 2, 1, 0, 2, 1, 1, 2, 0, 1, 2};

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{9, 10, 11, 12, 13, 14, 15, 16});
  fixture.verify_coordinates(EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}, coordinates);
  fixture.verify_part_membership({{"block_2", {1u}}, {"block_1", {2u}}});
}

TEST(IossTextMesh, twoHexDisconnectedWithCoordinatesAndSameParts_reversedElementListing)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,2,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,block_1\n"
      "0,1,HEX_8,9,10,11,12,13,14,15,16,block_1";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1,
      0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 2, 1, 0, 2, 1, 1, 2, 0, 1, 2};

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{9, 10, 11, 12, 13, 14, 15, 16});
  fixture.verify_coordinates(EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}, coordinates);
  fixture.verify_part_membership({{"block_1", {1u, 2u}}});
}

TEST(IossTextMesh, twoHexDisconnectedWithDefaultParts)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16";

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{9, 10, 11, 12, 13, 14, 15, 16});
  fixture.verify_part_membership({{"block_HEX8", {1u, 2u}}});
}

TEST(IossTextMesh, threeTriShellsWithCoordinatesAndParts)
{
  //      4-----5             //
  //      |\  2 |\            //
  //      |  \  |  \          //
  //      | 1  \| 3  \        //
  //      1-----2-----3       //

  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,SHELL_TRI_3,1,2,4,block_1\n"
      "0,2,SHELL_TRI_3,2,5,4,block_2\n"
      "0,3,SHELL_TRI_3,2,3,5,block_2";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 1, 0, 1, 1, 0};

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(3);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{1, 2, 4});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{2, 5, 4});
  fixture.verify_single_element(3u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{2, 3, 5});
  fixture.verify_coordinates(EntityIdVector{1, 2, 3, 4, 5}, coordinates);
  fixture.verify_part_membership({{"block_1", {1u}}, {"block_2", {2u, 3u}}});
}

TEST(IossTextMesh, threeTriShellsWithDefaultParts)
{
  //      4-----5             //
  //      |\  2 |\            //
  //      |  \  |  \          //
  //      | 1  \| 3  \        //
  //      1-----2-----3       //

  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,SHELL_TRI_3,1,2,4\n"
      "0,2,SHELL_TRI_3,2,5,4\n"
      "0,3,SHELL_TRI_3,2,3,5";

  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_num_elements(3);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{1, 2, 4});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{2, 5, 4});
  fixture.verify_single_element(3u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{2, 3, 5});
  fixture.verify_part_membership({{"block_trishell3", {1u, 2u, 3u}}});
}

TEST(IossTextMesh, partIds_oneDefaultPartOneElem)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_HEX8", 1u}});
}

TEST(IossTextMesh, partIds_oneDefaultPartTwoElems)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_HEX8", 1u}});
}

TEST(IossTextMesh, partIds_twoDefaultParts)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16\n"
      "0,3,SHELL_QUAD_4,17,18,19,20";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_HEX8", 1u}, {"block_SHELL4", 2u}});
}

TEST(IossTextMesh, partIds_oneDefaultPartOneUserSpecifiedPart)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,my_cool_part";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_HEX8", 1u}, {"my_cool_part", 2u}});
}

TEST(IossTextMesh, partIds_samePartTwice)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,my_cool_part\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,my_cool_part";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"my_cool_part", 1u}});
}

TEST(IossTextMesh, partIds_orderingIsByLine)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,2,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,partOne\n"
      "0,1,HEX_8,9,10,11,12,13,14,15,16,partTwo";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"partOne", 1u}, {"partTwo", 2u}});
}

TEST(IossTextMesh, partIds_respectExodusNamingConvention_onePart)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,block_101";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_101", 101u}});
}

TEST(IossTextMesh, partIds_respectExodusNamingConvention_twoParts)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,block_101\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,block_201";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_101", 101u}, {"block_201", 201u}});
}

TEST(IossTextMesh, partIds_respectExodusNamingConvention_withDefaultPartFirst)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,block_101";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_HEX8", 1u}, {"block_101", 101u}});
}

TEST(IossTextMesh, partIds_respectExodusNamingConvention_withDefaultPartSecond)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,block_101\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_101", 101u}, {"block_HEX8", 1u}});
}

TEST(IossTextMesh, partIds_respectExodusNamingConvention_withDefaultPartIdCollision)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,block_1";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_HEX8", 2u}, {"block_1", 1u}});
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_onePart)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,partThree,3";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"partThree", 3u}});
  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
  fixture.verify_part_membership({{"partThree", {1u}}});
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_twoParts)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,partThree,3\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,partFive,5";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"partThree", 3u}, {"partFive", 5u}});
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_twoPartsSameId)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,partFour,4\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,partFour,4";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"partFour", 4u}});
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_samePartDifferentIds)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,partFour,4\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,partFour,5";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_withDefaultPartIdCollision)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,partOne,1";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"block_HEX8", 2u}, {"partOne", 1u}});
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_withDefaultPart)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,partThree,3\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"partThree", 3u}, {"block_HEX8", 1u}});
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_withExodusPart)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,partThree,3\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,block_4";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"partThree", 3u}, {"block_4", 4u}});
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_collidesWithExodusPart)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,partThree,3\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,block_3";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_collidesWithPreviousSpec)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,partThreeA,3\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,partThreeB,3";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, partIds_userSpecifiedPartId_forExodusPart)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,block_2,3\n";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, partIds_shortPartNamesAreValid)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,HEX_8,1, 2, 3, 4, 5, 6, 7, 8,a\n"
      "0,2,HEX_8,9,10,11,12,13,14,15,16,b,3";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_part_ids({{"a", 1u}, {"b", 3u}});
}

TEST(IossTextMesh, partIds_integerPartNamesAreInvalid)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,9";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, twoHexesParallel)
{
  if (get_parallel_size() != 2) return;
  int rank = get_parallel_rank();

  std::string meshDesc =
      "0,1,HEX_8,1,2,3,4,5,6,7,8\n"
      "1,2,HEX_8,5,6,7,8,9,10,11,12";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  if (rank == 0) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
    fixture.verify_shared_nodes(EntityIdVector{5, 6, 7, 8}, 1);
  } else if (rank == 1) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{5, 6, 7, 8, 9, 10, 11, 12});
    fixture.verify_shared_nodes(EntityIdVector{5, 6, 7, 8}, 0);
  }
}

TEST(IossTextMesh, twoQuadShellsWithCoordinatesParallel)
{
  //      4-----5-----6
  //      |     |     |
  //      |  1  |  2  |
  //      |     |     |
  //      1-----2-----3

  if (get_parallel_size() != 2) return;
  int rank = get_parallel_rank();

  std::string meshDesc =
      "0,1,SHELL_QUAD_4,1,2,5,4\n"
      "1,2,SHELL_QUAD_4,2,3,6,5";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 1, 0, 1, 1, 0, 2, 1, 0};
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  if (rank == 0) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Shell4::name), EntityIdVector{1, 2, 5, 4});
    fixture.verify_coordinates(EntityIdVector{1, 2, 4, 5}, {0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0});
  } else if (rank == 1) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Shell4::name), EntityIdVector{2, 3, 6, 5});
    fixture.verify_coordinates(EntityIdVector{2, 3, 5, 6}, {1, 0, 0, 2, 0, 0, 1, 1, 0, 2, 1, 0});
  }
}

TEST(IossTextMesh, threeTriShellsWithCoordinatesParallel)
{
  //      4-----5             //
  //      |\  2 |\            //
  //      |  \  |  \          //
  //      | 1  \| 3  \        //
  //      1-----2-----3       //

  if (get_parallel_size() != 2) return;
  int rank = get_parallel_rank();

  std::string meshDesc =
      "0,1,SHELL_TRI_3,1,2,4\n"
      "0,2,SHELL_TRI_3,2,5,4\n"
      "1,3,SHELL_TRI_3,2,3,5";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 1, 0, 1, 1, 0};
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  if (rank == 0) {
    fixture.verify_num_elements(2);
    fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{1, 2, 4});
    fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{2, 5, 4});
    fixture.verify_coordinates(EntityIdVector{1, 2, 4, 5}, {0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0});
  } else if (rank == 1) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(3u, Ioss::ElementTopology::factory(Ioss::TriShell3::name), EntityIdVector{2, 3, 5});
    fixture.verify_coordinates(EntityIdVector{2, 3, 5}, {1, 0, 0, 2, 0, 0, 1, 1, 0});
  }
}

TEST(IossTextMesh, singleHexWithSpaces)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0, 1, HEX_8, 1, 2, 3, 4, 5, 6, 7, 8";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
}

TEST(IossTextMesh, singleHexWithCoordinatesAndSpaces)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0, 1, HEX_8, 1, 2, 3, 4, 5, 6, 7, 8";
  EntityIdVector nodeIds = EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8};
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1};
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), nodeIds);
  fixture.verify_coordinates(nodeIds, coordinates);
}

TEST(IossTextMesh, singleHexWithLowerCase)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,Hex_8,1,2,3,4,5,6,7,8";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc));

  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8});
}

TEST(IossTextMesh, singleHexWithCoordinatesAndLowerCase)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,Hex_8,1,2,3,4,5,6,7,8";
  EntityIdVector nodeIds = EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8};
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1};
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates));

  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Hex8::name), nodeIds);
  fixture.verify_coordinates(nodeIds, coordinates);
}

TEST(IossTextMesh, tooFewNodes)
{
  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, tooFewNodesWithCoordinates)
{
  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1};
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)), std::runtime_error);
}

TEST(IossTextMesh, tooFewCoordinates)
{
  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1};
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)), std::runtime_error);
}

TEST(IossTextMesh, tooManyNodes)
{
  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,9,10";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, tooManyNodesWithCoordinates)
{
  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,9,10";
  std::vector<double> coordinates = {
      0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 2, 0, 0, 2, 1, 0};
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)), std::runtime_error);
}

TEST(IossTextMesh, tooManyCoordinates)
{
  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 52};
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)), std::runtime_error);
}

TEST(IossTextMesh, tooLittleData_empty)
{
  std::string meshDesc = "";
  TextMeshFixture fixture;
  EXPECT_NO_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)));
}

TEST(IossTextMesh, tooLittleData_startsWithString)
{
  std::string meshDesc = "hi";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, tooLittleData_noGlobalId)
{
  std::string meshDesc = "0 ";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, tooLittleData_noTopology)
{
  std::string meshDesc = "0,1,";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, tooLittleData_noNodes)
{
  std::string meshDesc = "0,1,HEX_8";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, tooLittleDataWithCoordinates)
{
  std::string meshDesc = "0,1,";
  std::vector<double> coordinates;
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)), std::runtime_error);
}

TEST(IossTextMesh, invalidTopology)
{
  std::string meshDesc = "0,1,invalid,1";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, invalidTopologyWithCoordinates)
{
  std::string meshDesc = "0,1,invalid,1";
  std::vector<double> coordinates = {0, 0, 0};
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)), std::runtime_error);
}

TEST(IossTextMesh, mixedSpatialDim)
{
  std::string meshDesc =
      "0,1,HEX_8,1,2,3,4,5,6,7,8\n"
      "0,2,QUAD_4_2D,5,6,7,8";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, mixedSpatialDimWithCoordinates)
{
  std::string meshDesc =
      "0,1,HEX_8,1,2,3,4,5,6,7,8\n"
      "0,2,QUAD_4_2D,5,6,7,8";
  std::vector<double> coordinates = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1};
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)), std::runtime_error);
}

TEST(IossTextMesh, spatialDimInconsistentWithMetaData)
{
  std::string meshDesc = "0,1,QUAD_4_2D,1,2,3,4";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, spatialDimInconsistentWithMetaDataWithCoordinates)
{
  std::string meshDesc = "0,1,QUAD_4_2D,1,2,3,4";
  std::vector<double> coordinates = {0, 0, 1, 0, 1, 1, 0, 1};
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)), std::runtime_error);
}

TEST(IossTextMesh, endingWithNewlineIsOk)
{
  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8\n";
  TextMeshFixture fixture;
  EXPECT_NO_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)));
}

TEST(IossTextMesh, stringAfterPartNameIsError)
{
  std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,block_1,bogus\n";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)), std::runtime_error);
}

TEST(IossTextMesh, particleHex)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,PARTICLE,1\n"
      "0,2,HEX_8,2,3,4,5,6,7,8,9";
  TextMeshFixture fixture;
  EXPECT_NO_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc)));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Sphere::name), EntityIdVector{1});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{2, 3, 4, 5, 6, 7, 8, 9});
}

TEST(IossTextMesh, particleHexWithCoordinates)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,PARTICLE,1\n"
      "0,2,HEX_8,2,3,4,5,6,7,8,9";
  std::vector<double> coordinates = {2, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1};
  TextMeshFixture fixture;
  EXPECT_NO_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates)));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Sphere::name), EntityIdVector{1});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Hex8::name), EntityIdVector{2, 3, 4, 5, 6, 7, 8, 9});
  fixture.verify_coordinates(EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8, 9}, coordinates);
}

TEST(IossTextMesh2d, singleQuad)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,QUAD_4_2D,1,2,3,4";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, 2u));

  fixture.verify_num_elements(1);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Quad4::name), EntityIdVector{1, 2, 3, 4});
}

TEST(IossTextMesh2d, twoSprings)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,SPRING_2,1,2\n"
      "0,2,SPRING_2,2,3";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, 2u));

  fixture.verify_num_elements(2);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Spring2::name), EntityIdVector{1, 2});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Spring2::name), EntityIdVector{2, 3});
}

TEST(IossTextMesh2d, threeQuadsWithCoordinates)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc =
      "0,1,QUAD_4_2D,1,2,3,4\n"
      "0,2,QUAD_4_2D,2,3,5,6\n"
      "0,3,QUAD_4_2D,5,7,8,6";
  std::vector<double> coordinates = {0, 0, 1, 0, 1, 1, 0, 1, 2, 0, 2, 1, 3, 0, 3, 1};
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates, 2u));

  fixture.verify_num_elements(3);
  fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Quad4::name), EntityIdVector{1, 2, 3, 4});
  fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Quad4::name), EntityIdVector{2, 3, 5, 6});
  fixture.verify_single_element(3u, Ioss::ElementTopology::factory(Ioss::Quad4::name), EntityIdVector{5, 7, 8, 6});
  fixture.verify_coordinates(EntityIdVector{1, 2, 3, 4, 5, 6, 7, 8}, coordinates);
}

TEST(IossTextMesh2d, twoQuadsWithCoordinatesParallel)
{
  //      4-----5-----6
  //      |     |     |
  //      |  1  |  2  |
  //      |     |     |
  //      1-----2-----3

  if (get_parallel_size() != 2) return;
  int rank = get_parallel_rank();

  std::string meshDesc =
      "0,1,QUAD_4_2D,1,2,5,4\n"
      "1,2,QUAD_4_2D,2,3,6,5";
  std::vector<double> coordinates = {0, 0, 1, 0, 2, 0, 0, 1, 1, 1, 2, 1};
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, coordinates, 2u));

  if (rank == 0) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Quad4::name), EntityIdVector{1, 2, 5, 4});
    fixture.verify_shared_nodes(EntityIdVector{2, 5}, 1);
    fixture.verify_coordinates(EntityIdVector{1, 2, 4, 5}, {0, 0, 1, 0, 0, 1, 1, 1});
  } else if (rank == 1) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Quad4::name), EntityIdVector{2, 3, 6, 5});
    fixture.verify_shared_nodes(EntityIdVector{2, 5}, 0);
    fixture.verify_coordinates(EntityIdVector{2, 3, 5, 6}, {1, 0, 2, 0, 1, 1, 2, 1});
  }
}

TEST(IossTextMesh2d, twoQuadOneShellParallel)
{
  if (get_parallel_size() != 3) return;
  int rank = get_parallel_rank();

  std::string meshDesc =
      "0,1,QUAD_4_2D,1,2,3,4\n"
      "1,2,QUAD_4_2D,3,4,5,6\n"
      "2,3,SHELL_LINE_2,3,4";
  TextMeshFixture fixture;
  fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, 2u));

  if (rank == 0) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(1u, Ioss::ElementTopology::factory(Ioss::Quad4::name), EntityIdVector{1, 2, 3, 4});
    fixture.verify_shared_nodes(EntityIdVector{3, 4}, 1);
    fixture.verify_shared_nodes(EntityIdVector{3, 4}, 2);
  } else if (rank == 1) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(2u, Ioss::ElementTopology::factory(Ioss::Quad4::name), EntityIdVector{3, 4, 5, 6});
    fixture.verify_shared_nodes(EntityIdVector{3, 4}, 0);
    fixture.verify_shared_nodes(EntityIdVector{3, 4}, 2);
  } else if (rank == 2) {
    fixture.verify_num_elements(1);
    fixture.verify_single_element(3u, Ioss::ElementTopology::factory(Ioss::ShellLine2D2::name), EntityIdVector{3, 4});
    fixture.verify_shared_nodes(EntityIdVector{3, 4}, 0);
    fixture.verify_shared_nodes(EntityIdVector{3, 4}, 1);
  }
}

TEST(IossTextMesh1d, oneDimensionNotSupported)
{
  if (get_parallel_size() != 1) return;

  std::string meshDesc = "0,1,LINE_2_1D,1,2";
  TextMeshFixture fixture;
  EXPECT_THROW(fixture.fill_mesh(fixture.get_mesh_desc(meshDesc, 1u)), std::runtime_error);
}
}  // namespace



