// Copyright(C) 1999-2025 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "Ioss_CodeTypes.h" // for Int64Vector, IntVector
#include "Ioss_SideBlock.h" // for SideBlock
#include "Ioss_Utils.h"     // for Utils, IOSS_ERROR
#include "generated/Iogn_DatabaseIO.h"
#include "generated/Iogn_GeneratedMesh.h" // for GeneratedMesh
#include <cassert>                        // for assert
#include <cmath>                          // for sqrt
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <stdlib.h>
#include <string> // for string, operator==, etc

#include "Ioss_CommSet.h"         // for CommSet
#include "Ioss_DBUsage.h"         // for DatabaseUsage
#include "Ioss_DatabaseIO.h"      // for DatabaseIO
#include "Ioss_ElementBlock.h"    // for ElementBlock
#include "Ioss_EntityType.h"      // for EntityType, etc
#include "Ioss_Field.h"           // for Field, etc
#include "Ioss_GroupingEntity.h"  // for GroupingEntity
#include "Ioss_IOFactory.h"       // for IOFactory
#include "Ioss_Map.h"             // for Map, MapContainer
#include "Ioss_NodeBlock.h"       // for NodeBlock
#include "Ioss_NodeSet.h"         // for NodeSet
#include "Ioss_ParallelUtils.h"   // for ParallelUtils
#include "Ioss_Property.h"        // for Property
#include "Ioss_PropertyManager.h" // for PropertyManager
#include "Ioss_Region.h"          // for Region
#include "Ioss_SideSet.h"         // for SideSet
#include "Ioss_VariableType.h"    // for VariableType

namespace {
  template <typename INT>
  void map_global_to_local(const Ioss::Map &map, size_t count, size_t stride, INT *data)
  {
    for (size_t i = 0; i < count; i += stride) {
      int64_t local = map.global_to_local(data[i], true);
      data[i]       = static_cast<INT>(local);
    }
  }

  template <typename INT>
  void fill_transient_data(size_t component_count, double *data, INT *ids, size_t count,
                           double offset = 0.0)
  {
    if (component_count == 1) {
      for (size_t i = 0; i < count; i++) {
        data[i] = std::sqrt((double)ids[i]) + offset;
      }
    }
    else {
      for (size_t i = 0; i < count; i++) {
        for (size_t j = 0; j < component_count; j++) {
          data[i * component_count + j] = j + std::sqrt((double)ids[i]) + offset;
        }
      }
    }
  }

  void fill_transient_data(const Ioss::GroupingEntity *entity, const Ioss::Field &field, void *data,
                           void *id_data, size_t count, double offset = 0.0)
  {
    const Ioss::Field &ids = entity->get_fieldref("ids");
    if (ids.is_type(Ioss::Field::INTEGER)) {
      fill_transient_data(field.raw_storage()->component_count(), reinterpret_cast<double *>(data),
                          reinterpret_cast<int *>(id_data), count, offset);
    }
    else {
      fill_transient_data(field.raw_storage()->component_count(), reinterpret_cast<double *>(data),
                          reinterpret_cast<int64_t *>(id_data), count, offset);
    }
  }

  void fill_constant_data(const Ioss::Field &field, void *data, double value)
  {
    auto  *rdata           = reinterpret_cast<double *>(data);
    size_t count           = field.raw_count();
    size_t component_count = field.raw_storage()->component_count();
    for (size_t i = 0; i < count * component_count; i++) {
      rdata[i] = value;
    }
  }
} // namespace
namespace Iogn {

  // ========================================================================
  const IOFactory *IOFactory::factory()
  {
    static IOFactory registerThis;
    return &registerThis;
  }

  IOFactory::IOFactory() : Ioss::IOFactory("generated") {}

  Ioss::DatabaseIO *IOFactory::make_IO(const std::string &filename, Ioss::DatabaseUsage db_usage,
                                       Ioss_MPI_Comm                communicator,
                                       const Ioss::PropertyManager &props) const
  {
    return new DatabaseIO(nullptr, filename, db_usage, communicator, props);
  }

  // ========================================================================
  DatabaseIO::DatabaseIO(Ioss::Region *region, const std::string &filename,
                         Ioss::DatabaseUsage db_usage, Ioss_MPI_Comm communicator,
                         const Ioss::PropertyManager &props)
      : Ioss::DatabaseIO(region, filename, db_usage, communicator, props)
  {
    if (is_input()) {
      dbState = Ioss::STATE_UNKNOWN;
    }
    else {
      IOSS_ERROR("Generated mesh option is only valid for input mesh.");
    }
    if (props.exists("USE_CONSTANT_DF")) {
      m_useVariableDf = false;
    }
  }

  DatabaseIO::~DatabaseIO() { delete m_generatedMesh; }

  void DatabaseIO::read_meta_data_nl()
  {
    if (m_generatedMesh == nullptr) {
      if (get_filename() == "external") {
        IOSS_ERROR("ERROR: (generated mesh) 'external' specified for mesh, but "
                   "getGeneratedMesh was not called to set the external mesh.\n");
      }
      else {
        m_generatedMesh =
            new GeneratedMesh(get_filename(), util().parallel_size(), util().parallel_rank());
      }
    }

    assert(m_generatedMesh != nullptr);

    Ioss::Region *this_region     = get_region();
    auto          glob_node_count = m_generatedMesh->node_count();
    auto          glob_elem_count = m_generatedMesh->element_count();

    this_region->property_add(Ioss::Property("global_node_count", glob_node_count));
    this_region->property_add(Ioss::Property("global_element_count", glob_elem_count));

    const int64_t two_billion = 2ll << 30;
    if ((glob_node_count > two_billion || glob_elem_count > two_billion) &&
        int_byte_size_api() == 4) {
      IOSS_ERROR(fmt::format(
          "ERROR: The node count is {} and the element count is {}.\n"
          "       This exceeds the capacity of the 32-bit integers ({})\n"
          "       which are being requested by the client.\n"
          "       The mesh requires 64-bit integers which can be requested by setting the "
          "`INTEGER_SIZE_API=8` property.",
          fmt::group_digits(glob_node_count), fmt::group_digits(glob_elem_count),
          fmt::group_digits(two_billion)));
    }

    spatialDimension  = 3;
    nodeCount         = m_generatedMesh->node_count_proc();
    elementCount      = m_generatedMesh->element_count_proc();
    elementBlockCount = m_generatedMesh->block_count();
    nodesetCount      = m_generatedMesh->nodeset_count();
    sidesetCount      = m_generatedMesh->sideset_count();

    get_step_times_nl();

    add_transient_fields(this_region);
    get_nodeblocks();
    get_elemblocks();
    get_nodesets();
    get_sidesets();
    get_commsets();

    this_region->property_add(
        Ioss::Property(std::string("title"), std::string("GeneratedMesh: ") += get_filename()));
  }

  bool DatabaseIO::begin_nl(Ioss::State /* state */) { return true; }

  bool DatabaseIO::end_nl(Ioss::State /* state */) { return true; }

  bool DatabaseIO::begin_state_nl(int /* state */, double time)
  {
    currentTime = time;
    return true;
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::NodeBlock *nb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    size_t num_to_get = field.verify(data_size);

    Ioss::Field::RoleType role = field.get_role();
    if (role == Ioss::Field::MESH) {
      if (field.get_name() == "mesh_model_coordinates") {
        // Cast 'data' to correct size -- double
        auto *rdata = static_cast<double *>(data);
        m_generatedMesh->coordinates(rdata);
      }
      else if (field.get_name() == "mesh_model_coordinates_x") {
        // Cast 'data' to correct size -- double
        auto *rdata = static_cast<double *>(data);
        m_generatedMesh->coordinates(1, rdata);
      }
      else if (field.get_name() == "mesh_model_coordinates_y") {
        // Cast 'data' to correct size -- double
        auto *rdata = static_cast<double *>(data);
        m_generatedMesh->coordinates(2, rdata);
      }
      else if (field.get_name() == "mesh_model_coordinates_z") {
        // Cast 'data' to correct size -- double
        auto *rdata = static_cast<double *>(data);
        m_generatedMesh->coordinates(3, rdata);
      }

      // NOTE: The implicit_ids field is ONLY provided for backward-
      // compatibility and should not be used unless absolutely
      // required. For generated mesh, the implicit_ids and ids are the same.
      else if (field.get_name() == "ids" || field.get_name() == "implicit_ids") {
        // Map the local ids in this node block
        // (1...node_count) to global node ids.
        get_node_map().map_implicit_data(data, field, num_to_get, 0);
      }
      else if (field.get_name() == "owning_processor") {
        int *owner = static_cast<int *>(data);
        m_generatedMesh->owning_processor(owner, num_to_get);
      }
      else if (field.get_name() == "connectivity") {
        // Do nothing, just handles an idiosyncrasy of the GroupingEntity
      }
      else if (field.get_name() == "connectivity_raw") {
        // Do nothing, just handles an idiosyncrasy of the GroupingEntity
      }
      else {
        num_to_get = Ioss::Utils::field_warning(nb, field, "input");
      }
      return num_to_get;
    }

    const Ioss::Field &id_fld = nb->get_fieldref("ids");
    std::vector<char>  ids(id_fld.get_size());
    get_field_internal(nb, id_fld, Data(ids), id_fld.get_size());
    fill_transient_data(nb, field, data, Data(ids), num_to_get, currentTime);

    return num_to_get;
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::Region * /*region*/, const Ioss::Field &field,
                                         void *data, size_t /*data_size*/) const
  {
    Ioss::Field::RoleType role = field.get_role();
    if (role == Ioss::Field::TRANSIENT) {
      // Fill the field with arbitrary data...
      (reinterpret_cast<double *>(data))[0] = static_cast<double>(rand());
    }
    return 1;
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::ElementBlock *eb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    size_t num_to_get = field.verify(data_size);

    int64_t               id            = eb->get_property("id").get_int();
    int64_t               element_count = eb->entity_count();
    Ioss::Field::RoleType role          = field.get_role();

    if (role == Ioss::Field::MESH) {
      // Handle the MESH fields required for an ExodusII file model.
      // (The 'genesis' portion)

      if (field.get_name() == "connectivity" || field.get_name() == "connectivity_raw") {
        assert(field.raw_storage()->component_count() == m_generatedMesh->topology_type(id).second);

        // The generated mesh connectivity is returned in a vector.  Ids are global
        if (field.is_type(Ioss::Field::INTEGER)) {
          int *connect = static_cast<int *>(data);
          m_generatedMesh->connectivity(id, connect);
          if (field.get_name() == "connectivity_raw") {
            map_global_to_local(get_node_map(),
                                element_count * field.raw_storage()->component_count(), 1, connect);
          }
        }
        else {
          auto *connect = static_cast<int64_t *>(data);
          m_generatedMesh->connectivity(id, connect);
          if (field.get_name() == "connectivity_raw") {
            map_global_to_local(get_node_map(),
                                element_count * field.raw_storage()->component_count(), 1, connect);
          }
        }
      }
      else if (field.get_name() == "ids" || field.get_name() == "implicit_ids") {
        // Map the local ids in this element block
        // (eb_offset+1...eb_offset+1+element_count) to global element ids.
        get_element_map().map_implicit_data(data, field, num_to_get, eb->get_offset());
      }
      else {
        num_to_get = Ioss::Utils::field_warning(eb, field, "input");
      }
    }

    else if (role == Ioss::Field::ATTRIBUTE) {
      if (element_count > 0) {
        int64_t attribute_count = eb->get_property("attribute_count").get_int();
        if (attribute_count > 0) {
          auto *attr = static_cast<double *>(data);
          for (size_t i = 0; i < num_to_get; i++) {
            attr[i] = 1.0;
          }
        }
      }
    }

    else if (role == Ioss::Field::TRANSIENT) {
      // Fill the field with arbitrary data...
      const Ioss::Field &id_fld = eb->get_fieldref("ids");
      std::vector<char>  ids(id_fld.get_size());
      get_field_internal(eb, id_fld, Data(ids), id_fld.get_size());
      fill_transient_data(eb, field, data, Data(ids), num_to_get, currentTime);
    }
    else if (role == Ioss::Field::REDUCTION) {
      num_to_get = Ioss::Utils::field_warning(eb, field, "input reduction");
    }
    return num_to_get;
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::SideBlock *sd_blk, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    size_t num_to_get = field.verify(data_size);

    int64_t id           = sd_blk->get_property("id").get_int();
    size_t  entity_count = sd_blk->entity_count();
    if (num_to_get != entity_count) {
      IOSS_ERROR("Partial field input not implemented for side blocks");
    }

    Ioss::Field::RoleType role = field.get_role();
    if (role == Ioss::Field::MESH) {

      if (field.get_name() == "ids") {
        // A sideset' is basically an exodus sideset.  A
        // sideset has a list of elements and a corresponding local
        // element side (1-based) The side id is: side_id =
        // 10*element_id + local_side_number This assumes that all
        // sides in a sideset are boundary sides.
        std::vector<int64_t> elem_side;
        m_generatedMesh->sideset_elem_sides(id, elem_side);
        if (field.is_type(Ioss::Field::INTEGER)) {
          int *ids = static_cast<int *>(data);
          for (size_t i = 0; i < num_to_get; i++) {
            ids[i] = static_cast<int>(10 * elem_side[2 * i + 0] + elem_side[2 * i + 1] + 1);
          }
        }
        else {
          auto *ids = static_cast<int64_t *>(data);
          for (size_t i = 0; i < num_to_get; i++) {
            ids[i] = 10 * elem_side[2 * i + 0] + elem_side[2 * i + 1] + 1;
          }
        }
      }

      else if (field.get_name() == "element_side" || field.get_name() == "element_side_raw") {
        // Since we only have a single array, we need to allocate an extra
        // array to store all of the data.  Note also that the element_id
        // is the global id but only the local id is stored so we need to
        // map from local_to_global prior to generating the side id...

        std::vector<int64_t> elem_side;
        m_generatedMesh->sideset_elem_sides(id, elem_side);
        if (field.get_name() == "element_side_raw") {
          map_global_to_local(get_element_map(), elem_side.size(), 2, Data(elem_side));
        }

        if (field.is_type(Ioss::Field::INTEGER)) {
          int *element_side = static_cast<int *>(data);
          for (size_t i = 0; i < num_to_get; i++) {
            element_side[2 * i + 0] = static_cast<int>(elem_side[2 * i + 0]);
            element_side[2 * i + 1] = static_cast<int>(elem_side[2 * i + 1] + 1);
          }
        }
        else {
          auto *element_side = static_cast<int64_t *>(data);
          for (size_t i = 0; i < num_to_get; i++) {
            element_side[2 * i + 0] = elem_side[2 * i + 0];
            element_side[2 * i + 1] = elem_side[2 * i + 1] + 1;
          }
        }
      }

      else if (field.get_name() == "distribution_factors") {
        if (m_useVariableDf) {
          const Ioss::Field &id_fld = sd_blk->get_fieldref("ids");
          std::vector<char>  ids(id_fld.get_size());
          get_field_internal(sd_blk, id_fld, Data(ids), id_fld.get_size());
          fill_transient_data(sd_blk, field, data, Data(ids), num_to_get);
        }
        else {
          fill_constant_data(field, data, 1.0);
        }
      }

      else {
        num_to_get = Ioss::Utils::field_warning(sd_blk, field, "input");
      }
    }
    else if (role == Ioss::Field::TRANSIENT) {
      const Ioss::Field &id_fld = sd_blk->get_fieldref("ids");
      std::vector<char>  ids(id_fld.get_size());
      get_field_internal(sd_blk, id_fld, Data(ids), id_fld.get_size());
      fill_transient_data(sd_blk, field, data, Data(ids), num_to_get, currentTime);
    }
    return num_to_get;
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::NodeSet *ns, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    size_t num_to_get = field.verify(data_size);

    int64_t               id   = ns->get_property("id").get_int();
    Ioss::Field::RoleType role = field.get_role();
    if (role == Ioss::Field::MESH) {

      if (field.get_name() == "ids" || field.get_name() == "ids_raw") {
        std::vector<int64_t> nodes;
        m_generatedMesh->nodeset_nodes(id, nodes);
        if (field.get_name() == "ids_raw") {
          map_global_to_local(get_node_map(), nodes.size(), 1, Data(nodes));
        }

        if (field.is_type(Ioss::Field::INTEGER)) {
          int *ids = static_cast<int *>(data);
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4244)
#endif
          std::copy(nodes.begin(), nodes.end(), ids);
#ifdef _MSC_VER
#pragma warning(pop)
#endif
        }
        else {
          auto *ids = static_cast<int64_t *>(data);
          std::copy(nodes.begin(), nodes.end(), ids);
        }
      }
      else if (field.get_name() == "distribution_factors") {
        if (m_useVariableDf) {
          const Ioss::Field &id_fld = ns->get_fieldref("ids");
          std::vector<char>  ids(id_fld.get_size());
          get_field_internal(ns, id_fld, Data(ids), id_fld.get_size());
          fill_transient_data(ns, field, data, Data(ids), num_to_get);
        }
        else {
          fill_constant_data(field, data, 1.0);
        }
      }
      else {
        num_to_get = Ioss::Utils::field_warning(ns, field, "input");
      }
    }
    else if (role == Ioss::Field::TRANSIENT) {
      const Ioss::Field &id_fld = ns->get_fieldref("ids");
      std::vector<char>  ids(id_fld.get_size());
      get_field_internal(ns, id_fld, Data(ids), id_fld.get_size());
      fill_transient_data(ns, field, data, Data(ids), num_to_get, currentTime);
    }
    return num_to_get;
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::CommSet *cs, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    size_t num_to_get   = field.verify(data_size);
    size_t entity_count = cs->entity_count();
    assert(num_to_get == entity_count);

    // Return the <entity (node or face), processor> pair
    if (field.get_name() == "entity_processor" || field.get_name() == "entity_processor_raw") {

      // Check type -- node or face
      std::string type = cs->get_property("entity_type").get_string();

      if (type == "node") {
        // Allocate temporary storage space
        Ioss::Int64Vector entities(num_to_get);
        Ioss::IntVector   procs(num_to_get);
        m_generatedMesh->node_communication_map(entities, procs);

        // and store in 'data' ...
        if (field.is_type(Ioss::Field::INTEGER)) {
          int *entity_proc = static_cast<int *>(data);

          size_t j = 0;
          for (size_t i = 0; i < entity_count; i++) {
            assert(entities[i] > 0);
            entity_proc[j++] = static_cast<int>(entities[i]);
            entity_proc[j++] = procs[i];
          }

          if (field.get_name() == "entity_processor_raw") {
            map_global_to_local(get_node_map(), 2 * entity_count, 2, entity_proc);
          }
        }
        else {
          auto *entity_proc = static_cast<int64_t *>(data);

          size_t j = 0;
          for (size_t i = 0; i < entity_count; i++) {
            assert(entities[i] > 0);
            entity_proc[j++] = entities[i];
            entity_proc[j++] = procs[i];
          }

          if (field.get_name() == "entity_processor_raw") {
            map_global_to_local(get_node_map(), 2 * entity_count, 2, entity_proc);
          }
        }
      }
      else {
        IOSS_ERROR(fmt::format("Invalid commset type {}", type));
      }
    }
    else if (field.get_name() == "ids") {
      // Do nothing, just handles an idiosyncrasy of the GroupingEntity
    }
    else {
      num_to_get = Ioss::Utils::field_warning(cs, field, "input");
    }
    return num_to_get;
  }

  const Ioss::Map &DatabaseIO::get_node_map() const
  {
    // Allocate space for node number map and read it in...
    // Can be called multiple times, allocate 1 time only
    if (nodeMap.map().empty()) {
      nodeMap.set_size(nodeCount);
      std::vector<int64_t> map;
      m_generatedMesh->node_map(map);
      nodeMap.set_map(Data(map), map.size(), 0, true);
    }
    return nodeMap;
  }

  const Ioss::Map &DatabaseIO::get_element_map() const
  {
    // Allocate space for element number map and read it in...
    // Can be called multiple times, allocate 1 time only
    if (elemMap.map().empty()) {
      elemMap.set_size(elementCount);
      std::vector<int64_t> map;
      m_generatedMesh->element_map(map);
      elemMap.set_map(Data(map), map.size(), 0, true);
    }
    return elemMap;
  }

  void DatabaseIO::get_nodeblocks()
  {
    std::string block_name = "nodeblock_1";
    auto block = new Ioss::NodeBlock(this, block_name, m_generatedMesh->node_count_proc(), 3);
    block->property_add(Ioss::Property("id", 1));
    block->property_add(Ioss::Property("guid", util().generate_guid(1)));
    get_region()->add(block);
    add_transient_fields(block);
  }

  void DatabaseIO::get_step_times_nl()
  {
    auto time_step_count = m_generatedMesh->timestep_count();
    for (int i = 0; i < time_step_count; i++) {
      double time = m_generatedMesh->timestep_initial() + i * m_generatedMesh->timestep_interval();
      get_region()->add_state(time);
    }
  }

  std::vector<double> DatabaseIO::get_db_step_times_nl()
  {
    std::vector<double> timesteps;

    int time_step_count = m_generatedMesh->timestep_count();
    timesteps.reserve(time_step_count);
    for (int i = 0; i < time_step_count; i++) {
      double time = m_generatedMesh->timestep_initial() + i * m_generatedMesh->timestep_interval();
      timesteps.push_back(time);
    }

    return timesteps;
  }

  void DatabaseIO::get_elemblocks()
  {
    // Attributes of an element block are:
    // -- id
    // -- name
    // -- element type
    // -- number of elements
    // -- number of attributes per element
    // -- number of nodes per element (derivable from type)
    // -- number of faces per element (derivable from type)
    // -- number of edges per element (derivable from type)

    auto block_count = m_generatedMesh->block_count();
    for (int i = 0; i < block_count; i++) {
      std::string name          = Ioss::Utils::encode_entity_name("block", i + 1);
      std::string type          = m_generatedMesh->topology_type(i + 1).first;
      size_t      element_count = m_generatedMesh->element_count_proc(i + 1);
      auto        block         = new Ioss::ElementBlock(this, name, type, element_count);

      block->property_add(Ioss::Property("id", i + 1));
      block->property_add(Ioss::Property("guid", util().generate_guid(i + 1)));
      block->property_add(Ioss::Property("original_block_order", i));

      block->property_add(
          Ioss::Property("global_entity_count", m_generatedMesh->element_count(i + 1)));

      if (type == "shell4" || type == "tri3") {
        block->field_add(
            Ioss::Field("thickness", Ioss::Field::REAL, "scalar", Ioss::Field::ATTRIBUTE));
      }

      get_region()->add(block);
      add_transient_fields(block);
    }
  }

  void DatabaseIO::get_nodesets()
  {
    // Attributes of a nodeset are:
    // -- id
    // -- name
    // -- number of nodes
    // -- number of distribution factors (see next comment)
    // ----the #distribution factors should equal #nodes or 0, any
    //     other value does not make sense. If it is 0, then a substitute
    //     list will be created returning 1.0 for the factor

    // In a parallel execution, it is possible that a nodeset will have
    // no nodes or distribution factors on a particular processor...

    // Get nodeset metadata
    for (int ins = 0; ins < nodesetCount; ins++) {
      int64_t number_nodes = m_generatedMesh->nodeset_node_count_proc(ins + 1);

      std::string name    = Ioss::Utils::encode_entity_name("nodelist", ins + 1);
      auto        nodeset = new Ioss::NodeSet(this, name, number_nodes);
      nodeset->property_add(Ioss::Property("id", ins + 1));
      nodeset->property_add(Ioss::Property("guid", util().generate_guid(ins + 1)));
      get_region()->add(nodeset);
      add_transient_fields(nodeset);
    }
  }

  std::string DatabaseIO::get_sideset_topology() const
  {
    return m_generatedMesh->get_sideset_topology();
  }

  void DatabaseIO::get_sidesets()
  {
    const std::string face_topo = get_sideset_topology();

    m_sideset_names.reserve(sidesetCount);
    for (int iss = 0; iss < sidesetCount; iss++) {
      std::string name = Ioss::Utils::encode_entity_name("surface", iss + 1);
      m_sideset_names.push_back(name);
      auto sideset = new Ioss::SideSet(this, name);
      sideset->property_add(Ioss::Property("id", iss + 1));
      sideset->property_add(Ioss::Property("guid", util().generate_guid(iss + 1)));
      get_region()->add(sideset);

      Ioss::NameList touching_blocks = m_generatedMesh->sideset_touching_blocks(iss + 1);
      if (touching_blocks.size() == 1) {
        std::string sd_block_name = name;
        sd_block_name += "_";
        sd_block_name += face_topo;
        const std::string &side_topo_name = face_topo;
        std::string        elem_topo_name = "unknown";
        int64_t            number_faces   = m_generatedMesh->sideset_side_count_proc(iss + 1);

        auto sd_block =
            new Ioss::SideBlock(this, sd_block_name, side_topo_name, elem_topo_name, number_faces);
        sideset->add(sd_block);
        sd_block->property_add(Ioss::Property("id", iss + 1));
        sd_block->property_add(Ioss::Property("guid", util().generate_guid(iss + 1)));

        std::string storage = "Real[";
        storage += face_topo == "quad4" ? std::to_string(4) : std::to_string(3);
        storage += "]";
        sd_block->field_add(
            Ioss::Field("distribution_factors", Ioss::Field::REAL, storage, Ioss::Field::MESH));

        Ioss::ElementBlock *el_block = get_region()->get_element_block(touching_blocks[0]);
        sd_block->set_parent_element_block(el_block);
        add_transient_fields(sd_block);
      }
      else {
        for (auto &touching_block : touching_blocks) {
          std::string sd_block_name =
              "surface_" + touching_block + "_edge2_" + std::to_string(iss + 1);
          const std::string &side_topo_name = face_topo;
          std::string        elem_topo_name = "unknown";
          int64_t            number_faces   = m_generatedMesh->sideset_side_count_proc(iss + 1);

          auto sd_block = new Ioss::SideBlock(this, sd_block_name, side_topo_name, elem_topo_name,
                                              number_faces);
          sideset->add(sd_block);
          sd_block->property_add(Ioss::Property("id", iss + 1));
          sd_block->property_add(Ioss::Property("guid", util().generate_guid(iss + 1)));

          std::string storage = "Real[";
          storage += face_topo == "quad4" ? std::to_string(4) : std::to_string(3);
          storage += "]";
          sd_block->field_add(
              Ioss::Field("distribution_factors", Ioss::Field::REAL, storage, Ioss::Field::MESH));

          Ioss::ElementBlock *el_block = get_region()->get_element_block(touching_block);
          sd_block->set_parent_element_block(el_block);
          add_transient_fields(sd_block);
        }
      }
    }
  }

  void DatabaseIO::get_commsets()
  {
    if (util().parallel_size() > 1) {
      // Get size of communication map...
      size_t my_node_count = m_generatedMesh->communication_node_count_proc();

      // Create a single node commset
      auto *commset = new Ioss::CommSet(this, "commset_node", "node", my_node_count);
      commset->property_add(Ioss::Property("id", 1));
      commset->property_add(Ioss::Property("guid", util().generate_guid(1)));
      get_region()->add(commset);
    }
  }

  unsigned DatabaseIO::entity_field_support() const
  {
    return Ioss::NODEBLOCK | Ioss::ELEMENTBLOCK | Ioss::REGION | Ioss::NODESET | Ioss::SIDESET;
  }

  void DatabaseIO::add_transient_fields(Ioss::GroupingEntity *entity)
  {
    Ioss::EntityType type      = entity->type();
    size_t           var_count = m_generatedMesh->get_variable_count(type);
    for (size_t i = 0; i < var_count; i++) {
      std::string var_name = entity->type_string() + "_" + std::to_string(i + 1);
      entity->field_add(
          Ioss::Field(std::move(var_name), Ioss::Field::REAL, "scalar", Ioss::Field::TRANSIENT));
    }
  }
} // namespace Iogn
