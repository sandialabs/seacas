// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <cstddef>
#include <tokenize.h>

#include "Ioss_CommSet.h"         // for CommSet
#include "Ioss_DBUsage.h"         // for DatabaseUsage, etc
#include "Ioss_DatabaseIO.h"      // for DatabaseIO
#include "Ioss_EdgeBlock.h"       // for EdgeBlock
#include "Ioss_EdgeSet.h"         // for EdgeSet
#include "Ioss_ElementBlock.h"    // for ElementBlock
#include "Ioss_ElementSet.h"      // for ElementSet
#include "Ioss_EntityType.h"      // for EntityType::ELEMENTBLOCK
#include "Ioss_FaceBlock.h"       // for FaceBlock
#include "Ioss_FaceSet.h"         // for FaceSet
#include "Ioss_FileInfo.h"        // for FileInfo
#include "Ioss_Map.h"             // for Map, MapContainer
#include "Ioss_NodeBlock.h"       // for NodeBlock
#include "Ioss_NodeSet.h"         // for NodeSet
#include "Ioss_Property.h"        // for Property
#include "Ioss_SideBlock.h"       // for SideBlock
#include <Ioss_Assembly.h>        // for Assembly
#include <Ioss_CodeTypes.h>       // for HAVE_MPI
#include <Ioss_ElementTopology.h> // for NameList
#include <Ioss_ParallelUtils.h>   // for ParallelUtils, etc
#include <Ioss_Region.h>          // for Region
#include <Ioss_SerializeIO.h>     // for SerializeIO
#include <Ioss_StructuredBlock.h> // for StructuredBlock

#include <Ioss_Utils.h> // for Utils, IOSS_ERROR, etc
#include <climits>
#include <cstdlib>
#include <fmt/ostream.h>
#include <map>

#include <catalyst.hpp>
#include <catalyst/Iocatalyst_DatabaseIO.h>

namespace Ioss {
  class PropertyManager;
}

namespace Iocatalyst {

  namespace detail {

    template <typename GroupingEntityT>
    GroupingEntityT *createEntityGroup(const conduit_cpp::Node &node, Ioss::DatabaseIO *dbase);

    template <>
    Ioss::NodeBlock *createEntityGroup<Ioss::NodeBlock>(const conduit_cpp::Node &node,
                                                        Ioss::DatabaseIO        *dbase)
    {
      return new Ioss::NodeBlock(dbase, node["properties/name/value"].as_string(),
                                 node["properties/entity_count/value"].as_int64(),
                                 node["properties/component_degree/value"].as_int64());
    }

    template <>
    Ioss::ElementBlock *createEntityGroup<Ioss::ElementBlock>(const conduit_cpp::Node &node,
                                                              Ioss::DatabaseIO        *dbase)
    {
      return new Ioss::ElementBlock(dbase, node["properties/name/value"].as_string(),
                                    node["properties/topology_type/value"].as_string(),
                                    node["properties/entity_count/value"].as_int64());
    }

    template <>
    Ioss::NodeSet *createEntityGroup<Ioss::NodeSet>(const conduit_cpp::Node &node,
                                                    Ioss::DatabaseIO        *dbase)
    {
      return new Ioss::NodeSet(dbase, node["properties/name/value"].as_string(),
                               node["properties/entity_count/value"].as_int64());
    }

    template <>
    Ioss::SideBlock *createEntityGroup<Ioss::SideBlock>(const conduit_cpp::Node &node,
                                                        Ioss::DatabaseIO        *dbase)
    {
      return new Ioss::SideBlock(dbase, node["properties/name/value"].as_string(),
                                 node["properties/topology_type/value"].as_string(),
                                 node["properties/parent_topology_type/value"].as_string(),
                                 node["properties/entity_count/value"].as_int64());
    }

    template <>
    Ioss::SideSet *createEntityGroup<Ioss::SideSet>(const conduit_cpp::Node &node,
                                                    Ioss::DatabaseIO        *dbase)
    {
      return new Ioss::SideSet(dbase, node["properties/name/value"].as_string());
    }

    template <>
    Ioss::StructuredBlock *createEntityGroup<Ioss::StructuredBlock>(const conduit_cpp::Node &node,
                                                                    Ioss::DatabaseIO        *dbase)
    {
      Ioss::IJK_t localSizes    = {{(int)node["properties/ni/value"].as_int64(),
                                    (int)node["properties/nj/value"].as_int64(),
                                    (int)node["properties/nk/value"].as_int64()}};
      Ioss::IJK_t globalSizes   = {{(int)node["properties/ni_global/value"].as_int64(),
                                    (int)node["properties/nj_global/value"].as_int64(),
                                    (int)node["properties/nk_global/value"].as_int64()}};
      Ioss::IJK_t parentOffsets = {{(int)node["properties/offset_i/value"].as_int64(),
                                    (int)node["properties/offset_j/value"].as_int64(),
                                    (int)node["properties/offset_k/value"].as_int64()}};
      return new Ioss::StructuredBlock(dbase, node["properties/name/value"].as_string(),
                                       node["properties/component_degree/value"].as_int64(),
                                       localSizes, parentOffsets, globalSizes);
    }

  } // namespace detail

  class DatabaseIO::ImplementationT
  {
    conduit_cpp::Node                        Root;
    conduit_cpp::Node                        DBNode;
    mutable Ioss::Map                        NodeMap;
    std::map<std::string, Ioss::SideBlock *> sideBlocks;

  public:
    conduit_cpp::Node &databaseNode() { return this->DBNode; }
    void              *catalystConduitNode() { return conduit_cpp::c_node(&this->DBNode); }
    void               setDatabaseNode(conduit_node *c_node)
    {
      this->DBNode = conduit_cpp::Node();
      conduit_node_set_external_node(conduit_cpp::c_node(&this->DBNode), c_node);
    }
    conduit_cpp::Node &root() { return this->Root; }

    void print()
    {
      auto &node = this->DBNode;
      node.print_detailed();
    }

    bool defineModel(Ioss::Region *region)
    {
      assert(region->model_defined());

      auto &node = this->DBNode;
      node       = conduit_cpp::Node();

      node["database/int_byte_size_api"].set_int8(region->get_database()->int_byte_size_api());
      RegionContainer rc;
      rc.push_back(region);
      this->defineEntityGroup(node["region"], rc);
      this->defineEntityGroup(node["nodeblocks"], region->get_node_blocks());
      this->defineEntityGroup(node["edgeblocks"], region->get_edge_blocks());
      this->defineEntityGroup(node["faceblocks"], region->get_face_blocks());
      this->defineEntityGroup(node["elementblocks"], region->get_element_blocks());
      this->defineEntityGroup(node["sidesets"], region->get_sidesets());
      this->defineEntityGroup(node["nodesets"], region->get_nodesets());
      this->defineEntityGroup(node["edgesets"], region->get_edgesets());
      this->defineEntityGroup(node["facesets"], region->get_facesets());
      this->defineEntityGroup(node["elementsets"], region->get_elementsets());
      this->defineEntityGroup(node["structuredblocks"], region->get_structured_blocks());
      this->defineEntityGroup(node["assemblies"], region->get_assemblies());
      return true;
    }

    bool readModel(Ioss::Region *region);

    bool readTime(Ioss::Region *region)
    {
      auto &node = this->DBNode;
      if (node.has_child("state_time")) {
        const auto time = node["state_time"].as_float64();
        region->add_state(time);
      }
      return true;
    }

    int64_t putField(const std::string &containerName, const Ioss::GroupingEntity *entityGroup,
                     const Ioss::Field &field, void *data, size_t data_size, bool deep_copy)
    {
      const auto groupName      = getName(entityGroup);
      const auto num_to_get     = field.verify(data_size);
      const auto num_components = field.raw_storage()->component_count();
      if (num_to_get > 0) {
        auto &&node = this->DBNode[getFieldPath(containerName, groupName, field.get_name())];
        node["role"].set(static_cast<std::int8_t>(field.get_role()));
        node["type"].set(static_cast<std::int8_t>(field.get_type()));
        node["count"].set(static_cast<std::int64_t>(field.verify(data_size)));
        node["index"].set(static_cast<std::int64_t>(field.get_index()));
        node["component_count"].set(static_cast<std::int64_t>(num_components));
        node["storage"].set(field.raw_storage()->name());
        switch (field.get_type()) {
        case Ioss::Field::BasicType::DOUBLE:
          if (deep_copy) {
            node["value"].set(static_cast<double *>(data), num_to_get * num_components);
          }
          else {
            node["value"].set_external(static_cast<double *>(data), num_to_get * num_components);
          }
          break;
        case Ioss::Field::BasicType::INT32:
          if (deep_copy) {
            node["value"].set(static_cast<std::int32_t *>(data), num_to_get * num_components);
          }
          else {
            node["value"].set_external(static_cast<std::int32_t *>(data),
                                       num_to_get * num_components);
          }
          break;
        case Ioss::Field::BasicType::INT64:
          if (deep_copy) {
            node["value"].set(static_cast<std::int64_t *>(data), num_to_get * num_components);
          }
          else {
            node["value"].set_external(static_cast<std::int64_t *>(data),
                                       num_to_get * num_components);
          }
          break;
        case Ioss::Field::BasicType::CHARACTER:
          if (deep_copy) {
            node["value"].set(static_cast<std::int8_t *>(data), num_to_get * num_components);
          }
          else {
            node["value"].set_external(static_cast<std::int8_t *>(data),
                                       num_to_get * num_components);
          }
          break;
        default:
          fmt::print(stderr, "ERROR in {} {}: {} ({}), unsupported field type: {}\n", __func__,
                     containerName, field.get_name(), num_to_get, field.type_string());
        }
      }
      fmt::print(stderr, "put_field {}: {} ({})\n", containerName, field.get_name(), num_to_get);
      return num_to_get;
    }

    int64_t getField(const std::string &containerName, const Ioss::GroupingEntity *entityGroup,
                     const Ioss::Field &field, void *data, size_t data_size)
    {
      const auto groupName      = getName(entityGroup);
      auto       num_to_get     = field.verify(data_size);
      const auto num_components = field.raw_storage()->component_count();
      if (num_to_get > 0) {
        auto         path = getFieldPath(containerName, groupName, field.get_name()) + "/value";
        const auto &&node = this->DBNode[path];
        switch (field.get_type()) {
        case Ioss::Field::BasicType::DOUBLE:
          std::copy_n(reinterpret_cast<const double *>(node.element_ptr(0)),
                      num_to_get * num_components, reinterpret_cast<double *>(data));
          break;

        case Ioss::Field::BasicType::INT32:
          std::copy_n(reinterpret_cast<const int32_t *>(node.element_ptr(0)),
                      num_to_get * num_components, reinterpret_cast<int32_t *>(data));
          break;

        case Ioss::Field::BasicType::INT64:
          std::copy_n(reinterpret_cast<const int64_t *>(node.element_ptr(0)),
                      num_to_get * num_components, reinterpret_cast<int64_t *>(data));
          break;

        case Ioss::Field::BasicType::CHARACTER:
          std::copy_n(reinterpret_cast<const char *>(node.element_ptr(0)),
                      num_to_get * num_components, reinterpret_cast<char *>(data));
          break;
        default:
          fmt::print(stderr, "ERROR in {} {}: {} ({}), unsupported field type: {}\n", __func__,
                     containerName, field.get_name(), num_to_get, field.type_string());
        }
      }
      fmt::print(stderr, "get_field {}: {} ({})\n", containerName, field.get_name(), num_to_get);
      return num_to_get;
    }

    int64_t getFieldZeroCopy(const std::string          &containerName,
                             const Ioss::GroupingEntity *entityGroup, const Ioss::Field &field,
                             void **data, size_t *data_size)

    {
      *data      = nullptr;
      *data_size = 0;
      if (!hasField(containerName, entityGroup, field.get_name())) {
        fmt::print(stderr, "WARNING in {} : {}\n", __func__,
                   "field not available, " + field.get_name() + ", in container " + containerName +
                       "\n");
        return -1;
      }

      const auto groupName      = getName(entityGroup);
      auto       num_to_get     = field.verify(0);
      const auto num_components = field.raw_storage()->component_count();
      if (num_to_get > 0) {
        auto path = getFieldPath(containerName, groupName, field.get_name()) + "/value";

        const auto &&node = this->DBNode[path];
        *data_size        = num_to_get * num_components;
        switch (field.get_type()) {
        case Ioss::Field::BasicType::DOUBLE:
          *data = const_cast<double *>(node.as_double_ptr());
          break;

        case Ioss::Field::BasicType::INT32:
          *data = const_cast<int32_t *>(node.as_int32_ptr());
          break;

        case Ioss::Field::BasicType::INT64:
          *data = const_cast<int64_t *>(node.as_int64_ptr());
          break;

        case Ioss::Field::BasicType::CHARACTER:
          *data = const_cast<char *>(node.as_char_ptr());
          break;
        default:
          fmt::print(stderr, "ERROR in {} {}: {} ({}), unsupported field type: {}\n", __func__,
                     containerName, field.get_name(), num_to_get, field.type_string());
        }
      }
      fmt::print(stderr, "get_field {}: {} ({})\n", containerName, field.get_name(), num_to_get);
      return num_to_get;
    }

    int64_t getMeshModelCoordinates(const std::string          &containerName,
                                    const Ioss::GroupingEntity *entityGroup,
                                    const Ioss::Field &field, void *data, size_t data_size)
    {
      const auto groupName      = getName(entityGroup);
      auto       num_to_get     = field.verify(data_size);
      const auto num_components = field.raw_storage()->component_count();
      if (num_to_get > 0) {
        auto    path = getPropertyPath(containerName, groupName, "component_degree") + "/value";
        int64_t component_degree = this->DBNode[path].as_int64();
        double *rdata            = static_cast<double *>(data);

        auto coord_lambda = [&](const std::string &coord_name, int ordinal) {
          path          = getFieldPath(containerName, groupName, coord_name) + "/count";
          int64_t count = this->DBNode[path].as_int64();

          path = getFieldPath(containerName, groupName, coord_name) + "/value";
          const double *mesh_coords =
              reinterpret_cast<const double *>(this->DBNode[path].element_ptr(0));

          for (size_t i = 0; i < count; i++) {
            rdata[component_degree * i + ordinal] = mesh_coords[i];
          }
        };

        coord_lambda("mesh_model_coordinates_x", 0);

        if (component_degree >= 2) {
          coord_lambda("mesh_model_coordinates_y", 1);
        }

        if (component_degree == 3) {
          coord_lambda("mesh_model_coordinates_z", 2);
        }
      }
      fmt::print(stderr, "get_mesh_model_coordinates {}: {} ({})\n", containerName,
                 field.get_name(), num_to_get);
      return num_to_get;
    }

    bool hasField(const std::string &containerName, const Ioss::GroupingEntity *entityGroup,
                  const std::string &fieldName)
    {
      const auto groupName = getName(entityGroup);
      return this->DBNode.has_path(getFieldPath(containerName, groupName, fieldName));
    }

    std::string getFieldPath(const std::string &containerName, const std::string &groupName,
                             const std::string &fieldName)
    {
      return containerName + "/" + groupName + "/fields/" + fieldName;
    }

    bool hasProperty(const std::string &containerName, const Ioss::GroupingEntity *entityGroup,
                     const std::string &propertyName)
    {
      const auto groupName = getName(entityGroup);
      return this->DBNode.has_path(getPropertyPath(containerName, groupName, propertyName));
    }

    std::string getPropertyPath(const std::string &containerName, const std::string &groupName,
                                const std::string &propertyName)
    {
      return containerName + "/" + groupName + "/properties/" + propertyName;
    }

    std::string getName(const Ioss::GroupingEntity *entityGroup)
    {
      std::string retVal = entityGroup->name();
      if (dynamic_cast<const Ioss::Region *>(entityGroup) != nullptr) {
        retVal = "region_0";
      }
      else if (retVal.empty()) {
        retVal = entityGroup->generic_name();
      }
      return retVal;
    }

    Ioss::Map &get_node_map(const Ioss::DatabaseIO *dbase) const
    {
      if (this->NodeMap.defined()) {
        return this->NodeMap;
      }

      auto &&idsNode  = this->DBNode["nodeblocks/nodeblock_1/fields/ids"];
      auto   node_ids = const_cast<void *>(idsNode["value"].element_ptr(0));
      this->NodeMap.set_size(idsNode["count"].as_int64());
      if (idsNode["type"].as_int8() == Ioss::Field::BasicType::INT32) {
        this->NodeMap.set_map(reinterpret_cast<int32_t *>(node_ids), idsNode["count"].as_int64(),
                              0);
      }
      if (idsNode["type"].as_int8() == Ioss::Field::BasicType::INT64) {
        this->NodeMap.set_map(reinterpret_cast<int64_t *>(node_ids), idsNode["count"].as_int64(),
                              0);
      }

      this->NodeMap.set_defined(true);
      return this->NodeMap;
    }

  private:
    template <typename GroupingEntityT>
    bool defineEntityGroup(conduit_cpp::Node                     parent,
                           const std::vector<GroupingEntityT *> &container)
    {
      for (auto group : container) {
        this->addProperties(parent[getName(group)], group);
      }
      return true;
    }

    bool defineEntityGroup(conduit_cpp::Node parent, const Ioss::SideSetContainer &container)
    {
      for (auto group : container) {
        this->addProperties(parent[getName(group)], group);
        for (auto sb : group->get_side_blocks()) {
          parent[getName(group) + "/sideblocks"].append().set(sb->name());
        }
        auto &node = this->DBNode;
        this->defineEntityGroup(node["sideblocks"], group->get_side_blocks());
      }
      return true;
    }

    template <typename GroupingEntityT>
    bool addProperties(conduit_cpp::Node parent, GroupingEntityT *entityGroup)
    {
      Ioss::NameList names;
      // skip implicit properties.
      entityGroup->property_describe(Ioss::Property::INTERNAL, &names);
      entityGroup->property_describe(Ioss::Property::EXTERNAL, &names);
      entityGroup->property_describe(Ioss::Property::ATTRIBUTE, &names);
      entityGroup->property_describe(Ioss::Property::IMPLICIT, &names);

      auto &&propertiesNode = parent["properties"];
      for (const auto &name : names) {
        auto property = entityGroup->get_property(name);

        auto &&node = propertiesNode[name];
        node["type"].set(static_cast<std::int8_t>(property.get_type()));
        node["origin"].set(static_cast<std::int8_t>(property.get_origin()));
        switch (property.get_type()) {
        case Ioss::Property::BasicType::REAL: node["value"].set(property.get_real()); break;

        case Ioss::Property::BasicType::INTEGER: node["value"].set(property.get_int()); break;

        case Ioss::Property::BasicType::STRING: node["value"].set(property.get_string()); break;

        case Ioss::Property::BasicType::VEC_INTEGER:
          node["value"].set(property.get_vec_int());
          break;

        case Ioss::Property::BasicType::VEC_DOUBLE:
          node["value"].set(property.get_vec_double());
          break;

        case Ioss::Property::BasicType::POINTER:
        case Ioss::Property::BasicType::INVALID:
        default: return false;
        }
      }
      return true;
    }

    template <typename GroupingEntityT>
    bool readEntityGroup(conduit_cpp::Node &&parent, Ioss::Region *region)
    {
      for (conduit_index_t idx = 0, max = parent.number_of_children(); idx < max; ++idx) {
        auto &&child = parent[idx];
        auto   block = detail::createEntityGroup<GroupingEntityT>(child, region->get_database());
        region->add(block);
        this->readProperties(child["properties"], block);

        // read fields (meta-data only)
        this->readFields(child["fields"], block);
      }
      return true;
    }

    template <typename GroupingEntityT>
    bool readProperties(const conduit_cpp::Node &&parent, GroupingEntityT *block) const
    {
      for (conduit_index_t idx = 0, max = parent.number_of_children(); idx < max; ++idx) {
        auto     &&child  = parent[idx];
        const auto name   = child.name();
        const auto origin = static_cast<Ioss::Property::Origin>(child["origin"].as_int8());
        if (block->property_exists(name) && block->get_property(name).is_implicit()) {
          continue;
        }
        switch (child["type"].as_int8()) {
        // TODO: missing origin
        case Ioss::Property::BasicType::REAL:
          block->property_add(Ioss::Property(name, child["value"].as_float64(), origin));
          break;

        case Ioss::Property::BasicType::INTEGER:
          block->property_add(Ioss::Property(name, child["value"].as_int64(), origin));
          break;

        case Ioss::Property::BasicType::STRING:
          block->property_add(Ioss::Property(name, child["value"].as_string(), origin));
          break;

        case Ioss::Property::BasicType::VEC_INTEGER:
        case Ioss::Property::BasicType::VEC_DOUBLE: abort(); // TODO:
        }
      }
      return true;
    }

    template <typename GroupingEntityT>
    bool readFields(const conduit_cpp::Node &&parent, GroupingEntityT *block) const
    {
      for (conduit_index_t idx = 0, max = parent.number_of_children(); idx < max; ++idx) {
        auto     &&child   = parent[idx];
        const auto name    = child.name();
        const auto type    = static_cast<Ioss::Field::BasicType>(child["type"].as_int8());
        const auto role    = static_cast<Ioss::Field::RoleType>(child["role"].as_int8());
        const auto count   = child["count"].as_int64();
        const auto index   = child["index"].as_int64();
        const auto storage = child["storage"].as_string();
        if (!block->field_exists(name)) {
          block->field_add(
              Ioss::Field(name, type, storage, role, count, index).set_zero_copy_enabled());
        }
        else {
          // TODO verify field details.
          auto field = block->get_fieldref(name);
          if (!field.has_transform()) {
            block->get_fieldref(name).set_zero_copy_enabled();
          }
          assert(field.get_type() == type);
          auto f = block->get_fieldref(name);
        }
      }

      return true;
    }
  };

  template <>
  bool DatabaseIO::ImplementationT::readEntityGroup<Ioss::Region>(conduit_cpp::Node &&parent,
                                                                  Ioss::Region       *region)
  {
    for (conduit_index_t idx = 0, max = parent.number_of_children(); idx < max; ++idx) {
      auto &&child = parent[idx];
      this->readProperties(child["properties"], region);

      // read fields (meta-data only)
      this->readFields(child["fields"], region);
    }
    return true;
  }

  template <>
  bool DatabaseIO::ImplementationT::readEntityGroup<Ioss::SideBlock>(conduit_cpp::Node &&parent,
                                                                     Ioss::Region       *region)
  {
    sideBlocks.clear();
    for (conduit_index_t idx = 0, max = parent.number_of_children(); idx < max; ++idx) {
      auto &&child = parent[idx];
      auto   block = detail::createEntityGroup<Ioss::SideBlock>(child, region->get_database());
      if (sideBlocks.find(block->name()) == sideBlocks.end()) {
        sideBlocks[block->name()] = block;
      }
      else {
        fmt::print(stderr, "ERROR in {} {}: side block name used twice.\n", __func__,
                   block->name());
      }

      this->readProperties(child["properties"], block);

      // read fields (meta-data only)
      this->readFields(child["fields"], block);
    }
    return true;
  }

  template <>
  bool DatabaseIO::ImplementationT::readEntityGroup<Ioss::SideSet>(conduit_cpp::Node &&parent,
                                                                   Ioss::Region       *region)
  {
    for (conduit_index_t idx = 0, max = parent.number_of_children(); idx < max; ++idx) {
      auto &&child = parent[idx];
      auto   block = detail::createEntityGroup<Ioss::SideSet>(child, region->get_database());
      for (int i = 0; i < child["sideblocks"].number_of_children(); i++) {
        auto name = child["sideblocks"].child(i).as_string();
        if (sideBlocks.find(name) != sideBlocks.end()) {
          block->add(sideBlocks[name]);
        }
        else {
          fmt::print(stderr, "ERROR in {} {}: side block name not available.\n", __func__, name);
        }
      }
      region->add(block);
      this->readProperties(child["properties"], block);

      // read fields (meta-data only)
      this->readFields(child["fields"], block);
    }
    sideBlocks.clear();
    return true;
  }

  template <>
  bool
  DatabaseIO::ImplementationT::readEntityGroup<Ioss::StructuredBlock>(conduit_cpp::Node &&parent,
                                                                      Ioss::Region       *region)
  {
    for (conduit_index_t idx = 0, max = parent.number_of_children(); idx < max; ++idx) {
      auto &&child = parent[idx];
      auto block = detail::createEntityGroup<Ioss::StructuredBlock>(child, region->get_database());
      region->add(block);
      auto parent = block->get_node_block().get_property("IOSS_INTERNAL_CONTAINED_IN");
      this->readProperties(child["properties"], block);
      this->readProperties(child[getName(&block->get_node_block()) + "/properties"],
                           &block->get_node_block());
      block->get_node_block().property_add(parent);

      // read fields (meta-data only)
      this->readFields(child["fields"], block);
      this->readFields(child[getName(&block->get_node_block()) + "/fields"],
                       &block->get_node_block());
    }
    return true;
  }

  bool DatabaseIO::ImplementationT::readModel(Ioss::Region *region)
  {
    auto &node = this->DBNode;
    region->get_database()->set_int_byte_size_api(
        static_cast<Ioss::DataSize>(node["database/int_byte_size_api"].as_int8()));
    if (node.has_path("region/time")) {
      region->add_state(node["region/time"].to_float64());
    }
    this->readEntityGroup<Ioss::Region>(node["region"], region);
    this->readEntityGroup<Ioss::NodeBlock>(node["nodeblocks"], region);
    this->readEntityGroup<Ioss::ElementBlock>(node["elementblocks"], region);
    // this->readEntityGroup<Ioss::EdgeBlock>(node["edgeblocks"], region);
    // this->readEntityGroup<Ioss::FaceBlock>(node["faceblocks"], region);
    this->readEntityGroup<Ioss::SideBlock>(node["sideblocks"], region);
    this->readEntityGroup<Ioss::SideSet>(node["sidesets"], region);
    //  this->readEntityGroup<Ioss::NodeSet>(node["nodesets"], region);
    //  this->readEntityGroup<Ioss::EdgeSet>(node["edgesets"], region);
    //  this->readEntityGroup<Ioss::FaceSet>(node["facesets"], region);
    //  this->readEntityGroup<Ioss::ElementSet>(node["elementsets"], region);
    this->readEntityGroup<Ioss::StructuredBlock>(node["structuredblocks"], region);
    // this->readEntityGroup<Ioss::Assembly>(node["assemblies"], region);
    return true;
  }

  DatabaseIO::DatabaseIO(Ioss::Region *region, const std::string &filename,
                         Ioss::DatabaseUsage db_usage, Ioss_MPI_Comm communicator,
                         const Ioss::PropertyManager &props)
      : Ioss::DatabaseIO(region, filename, db_usage, communicator, props),
        Impl(new DatabaseIO::ImplementationT()), useDeepCopy(true)

  {
    dbState = Ioss::STATE_UNKNOWN;
    //// Always 64 bits
    // dbIntSizeAPI = Ioss::USE_INT64_API;
    // dbIntSizeAPI = Ioss::USE_INT32_API;

    bool shallowCopy = false;
    if (Ioss::Utils::check_set_bool_property(properties, "SHALLOW_COPY_FIELDS", shallowCopy)) {
      this->useDeepCopy = !shallowCopy;
    }

    if (is_input()) {
      auto &pm = get_property_manager();
      if (pm.exists("CATALYST_CONDUIT_NODE")) {
        auto c_node_ptr = reinterpret_cast<conduit_node *>(
            get_property_manager().get("CATALYST_CONDUIT_NODE").get_pointer());
        this->Impl->setDatabaseNode(c_node_ptr);
      }
      else {
        // we'll use filename as the location for the data dumps and read those.
        std::ostringstream path;
        path << get_catalyst_dump_dir() << "execute_invc" << filename << "_params.conduit_bin."
             << util().parallel_size() << "." << util().parallel_rank();
        auto &root  = this->Impl->root();
        auto &dbase = this->Impl->databaseNode();
        conduit_node_load(conduit_cpp::c_node(&root), path.str().c_str(), "conduit_bin");
        conduit_node_set_external_node(
            conduit_cpp::c_node(&dbase),
            conduit_node_fetch(conduit_cpp::c_node(&root), "catalyst/channels/dataset/data"));
      }
    }
    else {
      // in output-mode, we're pass data on to Catalyst.
      conduit_cpp::Node node;

      // TODO: Here, we need to pass pipeline scripts to execute to the Catalyst
      // implementation. ParaView Catalyst supports multiple scripts with args.
      // We could support that via the property manager. There are several
      // options:
      //.
      // We could support that via the property manager. There are several
      // options:
      //
      // 1. the simulation can add bunch of properties that indicate which scripts
      //    to run and we use those.
      // 2. we could use the filename passed to the database as the
      //    configuration file that provides the scripts to runs.
      // 3. we could simply treat the filename passed to the database as the
      //    pipeline script.
      //
      // Here, I am using the simplest option #3. I do that since that makes it
      // easy for me to use existing tools like `io_shell` to test this database
      node["catalyst"]["scripts"]["sample0"]["filename"].set(filename);
      catalyst_initialize(conduit_cpp::c_node(&node));
    }
  }

  DatabaseIO::~DatabaseIO()
  {
    if (!is_input()) {
      conduit_cpp::Node node;
      catalyst_finalize(conduit_cpp::c_node(&node));
    }
  }

  bool DatabaseIO::begin__(Ioss::State state)
  {
    this->dbState = state;
    if (is_input()) {
      if (state == Ioss::STATE_TRANSIENT) {

        auto &impl = (*this->Impl.get());
        // this->get_region()->add_state(impl.databaseNode()["region/time"].to_float64());
      }
    }
    return true;
  }

  bool DatabaseIO::end__(Ioss::State state)
  {
    assert(this->dbState == state);

    if (!is_input()) {
      auto region = this->get_region();
      assert(region != nullptr);

      auto &impl = (*this->Impl.get());
      switch (state) {
      case Ioss::STATE_DEFINE_MODEL:
        // here the basic structure for the model is defined i.e.
        // number of blocks/sets/names etc.
        if (!impl.defineModel(region)) {
          return false;
        }
        break;

      case Ioss::STATE_MODEL:
        // here the model has meshdata e.g mesh fields, ids, coordinates etc.
        // impl.print();
        break;

      case Ioss::STATE_DEFINE_TRANSIENT: break;

      case Ioss::STATE_TRANSIENT: break;

      case Ioss::STATE_LAST_ENTRY: break;
      case Ioss::STATE_UNKNOWN:
      case Ioss::STATE_INVALID:
      case Ioss::STATE_READONLY:
      case Ioss::STATE_CLOSED: break;
      }
    }

    dbState = Ioss::STATE_UNKNOWN;
    return true;
  }

  bool DatabaseIO::begin_state__(int state, double time) { return true; }

  // common
  bool DatabaseIO::end_state__(int state, double time)
  {
    if (this->is_input()) {}
    else {
      // invoke catalyst.
      auto &impl = (*this->Impl.get());

      auto &dbaseNode = this->Impl->databaseNode();
      dbaseNode["region/time"].set_float64(time);

      // state is 1-based, need to offset by 1 to make it 0-based.
      // timesteps start with 0.
      conduit_cpp::Node node;
      node["catalyst/state/timestep"].set(state - 1);
      node["catalyst/state/cycle"].set(state - 1);
      node["catalyst/state/time"].set(time);
      node["catalyst/channels/dataset/type"].set(std::string("ioss"));
      node["catalyst/channels/dataset/data"].set_external(impl.databaseNode());
      node["catalyst/channels/dataset/data/state_time"].set(time);
      catalyst_execute(conduit_cpp::c_node(&node));
    }
    return true;
  }

  unsigned DatabaseIO::entity_field_support() const
  {
    return Ioss::NODEBLOCK | Ioss::EDGEBLOCK | Ioss::FACEBLOCK | Ioss::ELEMENTBLOCK |
           Ioss::NODESET | Ioss::EDGESET | Ioss::FACESET | Ioss::ELEMENTSET | Ioss::SIDESET |
           Ioss::SIDEBLOCK | Ioss::STRUCTUREDBLOCK | Ioss::REGION;
  }

  void DatabaseIO::read_meta_data__()
  {
    auto region = this->get_region();
    assert(region != nullptr);

    auto &impl = (*this->Impl.get());
    impl.readModel(region);
  }

  void DatabaseIO::get_step_times__()
  {
    auto region = this->get_region();
    assert(region != nullptr);

    auto &impl = (*this->Impl.get());
    impl.readTime(region);
  }

  void *DatabaseIO::get_catalyst_conduit_node()
  {
    auto &impl = (*this->Impl.get());
    return impl.catalystConduitNode();
  }

  void DatabaseIO::print_catalyst_conduit_node()
  {
    auto &impl = (*this->Impl.get());
    impl.print();
  }

  std::string DatabaseIO::get_catalyst_dump_dir() const
  {
    std::string retVal;
    auto        catalystDumpDir = std::getenv("CATALYST_DATA_DUMP_DIRECTORY");
    if (catalystDumpDir) {
      retVal = catalystDumpDir;
    }
    if (!retVal.empty() && retVal.back() != '/') {
      retVal += '/';
    }
    return retVal;
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::Region *reg, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("region", reg, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::NodeBlock *nb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto       &impl      = (*this->Impl.get());
    std::string blockPath = "nodeblocks";
    if (nb->is_nonglobal_nodeblock()) {
      blockPath = "structuredblocks/" + impl.getName(nb->contained_in());
    }
    return impl.putField(blockPath, nb, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::EdgeBlock *eb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("edgeblocks", eb, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::FaceBlock *fb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("faceblocks", fb, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::ElementBlock *eb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("elementblocks", eb, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::SideBlock *sb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("sideblocks", sb, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::NodeSet *ns, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("nodesets", ns, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::EdgeSet *es, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("edgesets", es, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::FaceSet *fs, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("facesets", fs, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::ElementSet *es, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("elementsets", es, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::SideSet *ss, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("sidesets", ss, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::CommSet *cs, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    return -1;
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::Assembly * /*as*/,
                                         const Ioss::Field & /*field*/, void * /*data*/,
                                         size_t /*data_size*/) const
  {
    return -1;
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::Blob * /*bl*/, const Ioss::Field & /*field*/,
                                         void * /*data*/, size_t /*data_size*/) const
  {
    return -1;
  }

  int64_t DatabaseIO::put_field_internal(const Ioss::StructuredBlock *sb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.putField("structuredblocks", sb, field, data, data_size, this->deep_copy());
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::Region *reg, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("region", reg, field, data, data_size);
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::NodeBlock *nb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());

    std::string blockPath = "nodeblocks";
    if (nb->is_nonglobal_nodeblock()) {
      blockPath = "structuredblocks/" + impl.getName(nb->contained_in());
    }

    if (impl.hasField(blockPath, nb, field.get_name())) {
      return impl.getField(blockPath, nb, field, data, data_size);
    }
    else if ((field.get_name() == "mesh_model_coordinates") &&
             (impl.hasField(blockPath, nb, "mesh_model_coordinates_x") &&
              impl.hasField(blockPath, nb, "mesh_model_coordinates_y") &&
              impl.hasField(blockPath, nb, "mesh_model_coordinates_z"))) {
      return impl.getMeshModelCoordinates(blockPath, nb, field, data, data_size);
    }
    else {
      fmt::print(stderr, "WARNING in {} : {}\n", __func__,
                 "field not available, " + field.get_name() + ", in container " + blockPath + "\n");
      return -1;
    }
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::ElementBlock *eb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    if (impl.hasField("elementblocks", eb, field.get_name())) {
      return impl.getField("elementblocks", eb, field, data, data_size);
    }
    else {
      if (field.get_name() == "connectivity_raw" &&
          impl.hasField("elementblocks", eb, "connectivity")) {
        // maybe the data has 'connectivity' provided, so we convert it to 'connectivity_raw'.
        auto count = this->get_field_internal(eb, eb->get_field("connectivity"), data, data_size);
        if (count <= 0) {
          return count;
        }

        impl.get_node_map(this).reverse_map_data(
            data, field, field.verify(data_size) * field.raw_storage()->component_count());
        return count;
      }
      else if (field.get_name() == "connectivity" &&
               impl.hasField("elementblocks", eb, "connectivity_raw")) {
        // maybe the data has 'connectivity_raw' is provided, so we convert it to 'connectivity.
        auto count =
            this->get_field_internal(eb, eb->get_field("connectivity_raw"), data, data_size);
        if (count <= 0) {
          return count;
        }

        impl.get_node_map(this).map_data(
            data, field, field.verify(data_size) * field.raw_storage()->component_count());
        return count;
      }

      fmt::print(stderr, "WARNING in {} : {}\n", __func__,
                 "field not available, " + field.get_name() + ", in container elementblocks\n");
      return -1;
    }
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::EdgeBlock *eb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("edgeblocks", eb, field, data, data_size);
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::FaceBlock *fb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("faceblocks", fb, field, data, data_size);
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::SideBlock *sb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("sideblocks", sb, field, data, data_size);
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::NodeSet *ns, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("nodesets", ns, field, data, data_size);
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::EdgeSet *ns, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("edgesets", ns, field, data, data_size);
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::FaceSet *ns, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("facesets", ns, field, data, data_size);
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::ElementSet *ns, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("elementsets", ns, field, data, data_size);
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::SideSet *fs, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    auto &impl = (*this->Impl.get());
    return impl.getField("sidesets", fs, field, data, data_size);
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::CommSet *cs, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    return -1;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::Assembly * /*as*/,
                                         const Ioss::Field & /*field*/, void * /*data*/,
                                         size_t /*data_size*/) const
  {
    return -1;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::Blob * /*bl*/, const Ioss::Field & /*field*/,
                                         void * /*data*/, size_t /*data_size*/) const
  {
    return -1;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::StructuredBlock *sb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    std::string blockPath = "structuredblocks";
    auto       &impl      = (*this->Impl.get());
    if (impl.hasField(blockPath, sb, field.get_name())) {
      return impl.getField(blockPath, sb, field, data, data_size);
    }
    else if ((field.get_name() == "mesh_model_coordinates") &&
             (impl.hasField(blockPath, sb, "mesh_model_coordinates_x") &&
              impl.hasField(blockPath, sb, "mesh_model_coordinates_y") &&
              impl.hasField(blockPath, sb, "mesh_model_coordinates_z"))) {
      return impl.getMeshModelCoordinates(blockPath, sb, field, data, data_size);
    }
    else {
      fmt::print(stderr, "WARNING in {} : {}\n", __func__,
                 "field not available, " + field.get_name() + ", in container structuredblocks\n");
      return -1;
    }
  }

  int64_t DatabaseIO::get_zc_field_internal(const Ioss::Region *reg, const Ioss::Field &field,
                                            void **data, size_t *data_size) const
  {
    std::string blockPath = "region";
    auto       &impl      = (*this->Impl.get());
    return impl.getFieldZeroCopy(blockPath, reg, field, data, data_size);
  }
  int64_t DatabaseIO::get_zc_field_internal(const Ioss::NodeBlock *nb, const Ioss::Field &field,
                                            void **data, size_t *data_size) const
  {

    auto       &impl      = (*this->Impl.get());
    std::string blockPath = "nodeblocks";
    if (nb->is_nonglobal_nodeblock()) {
      blockPath = "structuredblocks/" + impl.getName(nb->contained_in());
    }

    return impl.getFieldZeroCopy(blockPath, nb, field, data, data_size);
  }
  int64_t DatabaseIO::get_zc_field_internal(const Ioss::ElementBlock *eb, const Ioss::Field &field,
                                            void **data, size_t *data_size) const
  {
    std::string blockPath = "elementblocks";
    auto       &impl      = (*this->Impl.get());
    return impl.getFieldZeroCopy(blockPath, eb, field, data, data_size);
  }
  int64_t DatabaseIO::get_zc_field_internal(const Ioss::StructuredBlock *sb,
                                            const Ioss::Field &field, void **data,
                                            size_t *data_size) const
  {
    std::string blockPath = "structuredblocks";
    auto       &impl      = (*this->Impl.get());
    return impl.getFieldZeroCopy(blockPath, sb, field, data, data_size);
  }

} // namespace Iocatalyst
