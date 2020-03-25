// Copyright(C) 1999-2010
// Sandia Corporation. Under the terms of Contract
// DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
// certain rights in this software.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Sandia Corporation nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "faodel/Iofaodel_DatabaseIO.h"
#include "faodel/Iofaodel_Utils.h"
#include "faodel/Iofaodel_PropertySerialization.h"
#include "faodel/Iofaodel_FieldSerialization.h"

#include <Ioss_CodeTypes.h>
#include <Ioss_SubSystem.h>
#include <Ioss_Utils.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_NodeSet.h>
#include <Ioss_SideSet.h>

#include <algorithm>
#include <cctype>
#include <cfloat>
#include <climits>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <fmt/ostream.h>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "faodel-services/MPISyncStart.hh"
// #include "Iofaodel_Serialize.h"

#if 1
#define PRINT_KEYS
#endif

namespace {
  // Output a message that the operation is unsupported and die...
  void unsupported(const char *operation)
  {
    std::cerr << "ERROR: Unsupported functionality called: " << operation << '\n';
    std::abort();
  }

  int get_file_pointer() { return 0; }

  const char *Version() { return "Iofaodel_DatabaseIO.C 2010/09/22"; }

  void faodel_error(int exoid, int lineno, int /* processor */)
  {
    std::ostringstream errmsg;

    errmsg << "Faodel error at line " << lineno << " in file '" << Version()
      << "' Please report to gdsjaar@sandia.gov if you need help.";

    IOSS_ERROR(errmsg);
  }
} // namespace

namespace Iofaodel {

  std::atomic<int> DatabaseIO::instanceCount{0};

  std::string kelpie_config_string = R"EOS(
dirman.type centralized

# MPI tests will need to have a standard networking base
kelpie.type standard

lunasa.lazy_memory_manager malloc
lunasa.eager_memory_manager malloc

#config.additional_files.env_name.if_defined FAODEL_CONFIG

mpisyncstart.enable true
dirman.root_node_mpi 0

#bootstrap.debug true
#whookie.debug true
#opbox.debug true
#dirman.debug true
#kelpie.debug true
)EOS";

  // ========================================================================
  const IOFactory *IOFactory::factory()
  {
    static IOFactory registerThis;
    return &registerThis;
  }

  IOFactory::IOFactory() : Ioss::IOFactory("faodel") {}

  Ioss::DatabaseIO *IOFactory::make_IO(const std::string &filename, Ioss::DatabaseUsage db_usage,
      MPI_Comm                     communicator,
      const Ioss::PropertyManager &properties) const
  {
    return new DatabaseIO(nullptr, filename, db_usage, communicator, properties);
  }


  // ========================================================================
  DatabaseIO::DatabaseIO(Ioss::Region *region, const std::string &filename,
      Ioss::DatabaseUsage db_usage, MPI_Comm communicator,
      const Ioss::PropertyManager &props)
    : Ioss::DatabaseIO(region, filename, db_usage, communicator, props), spatialDimension(3),
    nodeBlockCount(0), elementBlockCount(0), nodesetCount(0), sidesetCount(0),
    commsetNodeCount(0), commsetElemCount(0)
  {
    faodel_config.AppendFromReferences();

    /* CREATE a DHT that is distributed across all of the ranks of the communicator. */
    if( instanceCount++ == 0 ) {

      std::string resource;
      faodel_config.GetString(&resource, "dirman.resources.0", "dht:/ioss/dht");
      std::string mpi_resource = "dirman.resources_mpi[] " + resource;

      int size;
      MPI_Comm_size(communicator, &size);
      if( size == 1 ) {
        faodel_config.Append(mpi_resource + " 0");
      } else {
        faodel_config.Append(mpi_resource + " 0-" + std::to_string(size-1));
      }

      faodel_config.Append( kelpie_config_string );
      faodel::mpisyncstart::bootstrap();
      faodel::bootstrap::Start( faodel_config, kelpie::bootstrap );
    }

#ifdef JOB_TO_JOB_KELPIE
    /*
       We will worry about job-to-job later
       */
    char *kelpie_url = std::getenv( "IOSS_KELPIE_URL" );
    pool = kelpie::Connect( kelpie_url );
#endif

    std::string s;
    faodel_config.GetString(&s, "dirman.resources.0");
    if( s.empty() ) {
      std::ostringstream errmsg;
      fmt::print(
          errmsg,
          "ERROR: Unable to locate Faodel resource to connect to\n");
      IOSS_ERROR(errmsg);
    } else {
      pool = kelpie::Connect( s );
    }
    dbState = Ioss::STATE_UNKNOWN;
  }

  DatabaseIO::~DatabaseIO()
  {
    // Shut down Faodel gracefully
    if( --instanceCount == 0 ) {
      faodel::bootstrap::Finish();
    }
  }


  bool DatabaseIO::put_properties() {
    // TODO add check to see what's been published before publishing again
    map_properties(*(get_region()),
        [this](const Ioss::Region & r,
          const Ioss::GroupingEntity & e, const Ioss::Property & p) {
          this->pool.Publish(
              make_key(parallel_rank(), r, e, p),
              pack_property(r, e, p)
              );
        }
        );
    return true;
  }


  void DatabaseIO::finalize_database()
  {
    if(this->usage() == Ioss::DatabaseUsage::WRITE_RESTART ||
        this->usage() == Ioss::DatabaseUsage::WRITE_RESULTS ||
        this->usage() == Ioss::DatabaseUsage::WRITE_HISTORY ||
        this->usage() == Ioss::DatabaseUsage::WRITE_HEARTBEAT) {

      // write states to LDO
      pool.Publish(
          make_states_key(parallel_rank(), *get_region()),
          pack_states(*get_region()));
 
      // write properties to LDOs and publish
      this->put_properties();
    }
  }

  const std::string DatabaseIO::get_format() const { return "faodel"; }


  bool DatabaseIO::begin_state__(int /* state */, double /* time */)
  {

    return false;
  }


  bool DatabaseIO::end_state__(int /* state */, double /* time */)
  {

    return false;
  }


  void DatabaseIO::read_meta_data__()
  {
    this->get_step_times__();

    this->read_region();

    this->get_edgeblocks();
    this->get_elemblocks();
    this->get_faceblocks();
    this->get_nodeblocks();

    this->get_edgesets();
    this->get_elemsets();
    this->get_facesets();
    this->get_nodesets();
    this->get_sidesets();
  }

  void DatabaseIO::get_step_times__()
  {
    auto search_key = make_states_search_key(parallel_rank(), *get_region());
    kelpie::ObjectCapacities oc;
    pool.List(search_key, &oc);
    if(oc.keys.size() == 1)
    {
      lunasa::DataObject ldo(0, oc.capacities[0],
          lunasa::DataObject::AllocatorType::eager);
      pool.Need(oc.keys[0], oc.capacities[0], &ldo);

      auto meta = static_cast<Iofaodel::meta_entry_t*>(ldo.GetMetaPtr());

      auto entry = static_cast<Iofaodel::state_entry_t*>(
          static_cast<void*>(
            static_cast<char*>(ldo.GetDataPtr()) + meta->value.offset));

      auto data = static_cast<Iofaodel::state_entry_t::basic_type*>(
          static_cast<void*>(entry->data + entry->value.offset));

      for(auto state(1); state <= entry->count; state++)
        get_region()->add_state(data[state-1]);
    }
    // TODO
    // else {
    // Report error of not having 1 set of time steps
    // }
  }

  void DatabaseIO::read_region() {

    {
      // Region Properties
      kelpie::ObjectCapacities oc;
      auto search_key = property_search_key(parallel_rank(), *(get_region()), *(get_region()));
      pool.List(search_key, &oc);
      this->read_entity_properties(oc, *(this->get_region()));
    }

    {
      // Region Fields
      kelpie::ObjectCapacities oc;
      auto search_key = field_search_key(parallel_rank(), *(get_region()), *(get_region()));
      pool.List(search_key, &oc);
      this->read_entity_fields(oc, *(this->get_region()));
    }

    {    
      // Region TRANSIENT Fields
      kelpie::ObjectCapacities oc;
      auto search_key = field_search_key(parallel_rank(), 1, *(get_region()), *(get_region()));
      pool.List(search_key, &oc);
      this->read_entity_fields(oc, *(this->get_region()));
    }    
  }

  void DatabaseIO::read_entity_properties(kelpie::ObjectCapacities oc,
      Ioss::GroupingEntity & entity)
  {
    // TODO do we need to update default properties upon construction?
    // Properties
    for(size_t i=0; i<oc.keys.size(); i++) {
      lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
      pool.Need(oc.keys[i], oc.capacities[i], &ldo);

      auto meta(static_cast<meta_entry_t*>(ldo.GetMetaPtr()));
      auto prop = static_cast<Iofaodel::property_entry_t*>(
          static_cast<void*>(
            static_cast<char*>(ldo.GetDataPtr()) + meta->value.offset
            )
          );

      std::string property_name(prop->data + prop->name.offset, prop->name.size);

      auto value_ptr = static_cast<void*>(prop->data + prop->value.offset);
      if(prop->basic_type == Ioss::Property::BasicType::STRING) {
        std::string value(prop->data + prop->value.offset, prop->value.size);
        entity.property_update(property_name, value);
      } else if(prop->basic_type == Ioss::Property::BasicType::INTEGER) {
        double val = 0.0;
        entity.property_update(property_name, *(reinterpret_cast<int64_t*>(value_ptr)));
      } else if(prop->basic_type == Ioss::Property::BasicType::REAL) {
        entity.property_update(property_name, *(reinterpret_cast<double*>(value_ptr)));
      }
    }
  }

  void DatabaseIO::read_entity_fields(kelpie::ObjectCapacities oc, Ioss::GroupingEntity & entity)
  {
    // Fields
    for(size_t i=0; i<oc.keys.size(); i++) {
      lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
      pool.Need(oc.keys[i], oc.capacities[i], &ldo);

      auto meta(static_cast<meta_entry_t*>(ldo.GetMetaPtr()));

      auto field = static_cast<field_entry_t*>(
          static_cast<void*>(
            static_cast<char*>(ldo.GetDataPtr())
            + meta->value.offset)
          );

      std::string field_name(field->data + field->name.offset, field->name.size);
      std::string field_storage(field->data + field->storage.offset, field->storage.size);

      if(!entity.field_exists(field_name)) {
        entity.field_add(
            Ioss::Field(field_name, field->basic_type,
              field_storage,
              field->role_type,
              field->raw_count)
            );
      }
    }
  }




  void DatabaseIO::get_edgeblocks()
  {
    std::string type_string("EdgeBlock");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {
      bool have_entity_count(false);
      int64_t entity_count(0);
      bool have_original_edge_type(false);
      std::string original_edge_type;

      for(int i = 0; i < oc.keys.size(); i++)
      {

        if(oc.keys[i].K2().find(entity_name) != std::string::npos) {

          if(oc.keys[i].K2().find("entity_count") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            entity_count = property_get_int(ldo);
            have_entity_count = true;
          }

          if(oc.keys[i].K2().find("original_edge_type") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            original_edge_type = property_get_int(ldo);
            have_original_edge_type = true;
          }

        }
      }

      if(have_entity_count && have_original_edge_type) {
        auto block = new Ioss::EdgeBlock(this, entity_name,
            original_edge_type, entity_count);

        // Add Properties that aren't created in the CTor
        auto property_search = property_search_key(parallel_rank(), *(get_region()), *block);
        kelpie::ObjectCapacities property_oc;
        pool.List(property_search, &property_oc);
        this->read_entity_properties(property_oc, *block);


        // Add fields that aren't created in the CTor
        auto field_search = field_search_key(parallel_rank(), *(get_region()), *block);
        kelpie::ObjectCapacities field_oc;
        pool.List(field_search, &field_oc);
        this->read_entity_fields(field_oc, *block);

        this->get_region()->add(block);
      }
    }
  }



  void DatabaseIO::get_elemblocks()
  {
    std::string type_string("ElementBlock");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {
      bool have_entity_count(false);
      int64_t entity_count(0);
      bool have_original_topology_type(false);
      std::string original_topology_type;

      for(int i = 0; i < oc.keys.size(); i++)
      {

        if(oc.keys[i].K2().find(entity_name) != std::string::npos) {

          if(oc.keys[i].K2().find("entity_count") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            entity_count = property_get_int(ldo);
            have_entity_count = true;
          }

          if(oc.keys[i].K2().find("original_topology_type") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            original_topology_type = property_get_string(ldo);
            have_original_topology_type = true;
          }

        }
      }

      if(have_entity_count && have_original_topology_type) {
        auto block = new Ioss::ElementBlock(this, entity_name,
            original_topology_type, entity_count);

        // Add Properties that aren't created in the CTor
        auto property_search = property_search_key(parallel_rank(), *(get_region()), *block);
        kelpie::ObjectCapacities property_oc;
        pool.List(property_search, &property_oc);
        this->read_entity_properties(property_oc, *block);


        // Add fields that aren't created in the CTor
        auto field_search = field_search_key(parallel_rank(), *(get_region()), *block);
        kelpie::ObjectCapacities field_oc;
        pool.List(field_search, &field_oc);
        this->read_entity_fields(field_oc, *block);

        // Add TRANSIENT fields that aren't created in the CTor 
        auto field_search_debug = field_search_key(parallel_rank(), 1, *(get_region()), *block);
        kelpie::ObjectCapacities field_oc_debug;
        pool.List(field_search_debug, &field_oc_debug);
        this->read_entity_fields(field_oc_debug, *block);

        this->get_region()->add(block);
      }
    }
  }


  void DatabaseIO::get_faceblocks()
  {
    std::string type_string("FaceBlock");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {
      bool have_entity_count(false);
      int64_t entity_count(0);
      bool have_original_topology_type(false);
      std::string original_topology_type;

      for(int i = 0; i < oc.keys.size(); i++)
      {

        if(oc.keys[i].K2().find(entity_name) != std::string::npos) {

          if(oc.keys[i].K2().find("entity_count") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            entity_count = property_get_int(ldo);
            have_entity_count = true;
          }

          if(oc.keys[i].K2().find("original_topology_type") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            original_topology_type = property_get_string(ldo);
            have_original_topology_type = true;
          }

        }
      }

      if(have_entity_count && have_original_topology_type) {
        auto block = new Ioss::FaceBlock(this, entity_name,
            original_topology_type, entity_count);

        // Add Properties that aren't created in the CTor
        auto property_search = property_search_key(parallel_rank(), *(get_region()), *block);
        kelpie::ObjectCapacities property_oc;
        pool.List(property_search, &property_oc);
        this->read_entity_properties(property_oc, *block);


        // Add fields that aren't created in the CTor
        auto field_search = field_search_key(parallel_rank(), *(get_region()), *block);
        kelpie::ObjectCapacities field_oc;
        pool.List(field_search, &field_oc);
        this->read_entity_fields(field_oc, *block);

        this->get_region()->add(block);
      }
    }
  }


  void DatabaseIO::get_nodeblocks()
  {
    std::string type_string("NodeBlock");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {
      bool have_entity_count(false);
      int64_t entity_count(0);
      bool have_component_degree(false);
      int64_t component_degree(0);

      for(int i = 0; i < oc.keys.size(); i++)
      {

        if(oc.keys[i].K2().find(entity_name) != std::string::npos) {

          if(oc.keys[i].K2().find("entity_count") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            entity_count = property_get_int(ldo);
            have_entity_count = true;
          }

          if(oc.keys[i].K2().find("component_degree") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            component_degree = property_get_int(ldo);
            have_component_degree = true;
          }

        }
      }

      if(have_entity_count && have_component_degree) {
        // Creates a new NodeBlock with default Properties and Fields
        auto block = new Ioss::NodeBlock(this, entity_name, entity_count, component_degree);

        // Add Properties that aren't created in the CTor
        auto property_search = property_search_key(parallel_rank(), *(get_region()), *block);
        kelpie::ObjectCapacities property_oc;
        pool.List(property_search, &property_oc);
        this->read_entity_properties(property_oc, *block);


        // Add fields that aren't created in the CTor
        auto field_search = field_search_key(parallel_rank(), *(get_region()), *block);
        kelpie::ObjectCapacities field_oc;
        pool.List(field_search, &field_oc);
        this->read_entity_fields(field_oc, *block);

        // Add TRANSIENT fields that aren't created in the CTor
        auto field_search_debug = field_search_key(parallel_rank(), 1, *(get_region()), *block);
        kelpie::ObjectCapacities field_oc_debug;
        pool.List(field_search_debug, &field_oc_debug);
        this->read_entity_fields(field_oc_debug, *block);

        this->get_region()->add(block);
      }
    }
  }


  void DatabaseIO::get_edgesets()
  {
    std::string type_string("EdgeSet");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {
      bool have_entity_count(false);
      int64_t entity_count(0);
      bool have_component_degree(false);
      int64_t component_degree(0);

      for(int i = 0; i < oc.keys.size(); i++)
      {

        if(oc.keys[i].K2().find(entity_name) != std::string::npos) {

          if(oc.keys[i].K2().find("entity_count") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            entity_count = property_get_int(ldo);
            have_entity_count = true;
          }

        }
      }

      if(have_entity_count && have_component_degree) {
        auto entity = new Ioss::EdgeSet(this, entity_name, entity_count);

        // Add Properties that aren't created in the CTor
        auto property_search = property_search_key(parallel_rank(), *(get_region()), *entity);
        kelpie::ObjectCapacities property_oc;
        pool.List(property_search, &property_oc);
        this->read_entity_properties(property_oc, *entity);


        // Add fields that aren't created in the CTor
        auto field_search = field_search_key(parallel_rank(), *(get_region()), *entity);
        kelpie::ObjectCapacities field_oc;
        pool.List(field_search, &field_oc);
        this->read_entity_fields(field_oc, *entity);

        this->get_region()->add(entity);
      }
    }
  }

  void DatabaseIO::get_elemsets()
  {
    std::string type_string("ElementSet");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {
      bool have_entity_count(false);
      int64_t entity_count(0);
      bool have_component_degree(false);
      int64_t component_degree(0);

      for(int i = 0; i < oc.keys.size(); i++)
      {

        if(oc.keys[i].K2().find(entity_name) != std::string::npos) {

          if(oc.keys[i].K2().find("entity_count") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            entity_count = property_get_int(ldo);
            have_entity_count = true;
          }

        }
      }

      if(have_entity_count && have_component_degree) {
        auto entity = new Ioss::ElementSet(this, entity_name, entity_count);

        // Add Properties that aren't created in the CTor
        auto property_search = property_search_key(parallel_rank(), *(get_region()), *entity);
        kelpie::ObjectCapacities property_oc;
        pool.List(property_search, &property_oc);
        this->read_entity_properties(property_oc, *entity);


        // Add fields that aren't created in the CTor
        auto field_search = field_search_key(parallel_rank(), *(get_region()), *entity);
        kelpie::ObjectCapacities field_oc;
        pool.List(field_search, &field_oc);
        this->read_entity_fields(field_oc, *entity);

        this->get_region()->add(entity);
      }
    }
  }

  void DatabaseIO::get_facesets()
  {
    std::string type_string("FaceSet");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {
      bool have_entity_count(false);
      int64_t entity_count(0);
      bool have_component_degree(false);
      int64_t component_degree(0);

      for(int i = 0; i < oc.keys.size(); i++)
      {

        if(oc.keys[i].K2().find(entity_name) != std::string::npos) {

          if(oc.keys[i].K2().find("entity_count") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            entity_count = property_get_int(ldo);
            have_entity_count = true;
          }

        }
      }

      if(have_entity_count && have_component_degree) {
        auto entity = new Ioss::FaceSet(this, entity_name, entity_count);

        // Add Properties that aren't created in the CTor
        auto property_search = property_search_key(parallel_rank(), *(get_region()), *entity);
        kelpie::ObjectCapacities property_oc;
        pool.List(property_search, &property_oc);
        this->read_entity_properties(property_oc, *entity);


        // Add fields that aren't created in the CTor
        auto field_search = field_search_key(parallel_rank(), *(get_region()), *entity);
        kelpie::ObjectCapacities field_oc;
        pool.List(field_search, &field_oc);
        this->read_entity_fields(field_oc, *entity);

        this->get_region()->add(entity);
      }
    }
  }

  void DatabaseIO::get_nodesets()
  {
    std::string type_string("NodeSet");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {
      bool have_entity_count(false);
      int64_t entity_count(0);

      for(int i = 0; i < oc.keys.size(); i++)
      {
        std::string entity_name_search = "/" + entity_name + "/";
        if(oc.keys[i].K2().find(entity_name_search) != std::string::npos) {

          if(oc.keys[i].K2().find("entity_count") != std::string::npos) {
            lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
            pool.Need(oc.keys[i], &ldo);
            entity_count = property_get_int(ldo);
            have_entity_count = true;
          }

        }
      }

      if(have_entity_count) {
        auto entity = new Ioss::NodeSet(this, entity_name, entity_count);

        // Add Properties that aren't created in the CTor
        auto property_search = property_search_key(parallel_rank(), *(get_region()), *entity);
        kelpie::ObjectCapacities property_oc;
        pool.List(property_search, &property_oc);
        this->read_entity_properties(property_oc, *entity);


        // Add fields that aren't created in the CTor
        auto field_search = field_search_key(parallel_rank(), *(get_region()), *entity);
        kelpie::ObjectCapacities field_oc;
        pool.List(field_search, &field_oc);
        this->read_entity_fields(field_oc, *entity);

        this->get_region()->add(entity);
      }
    }
  }

  void DatabaseIO::get_sidesets()
  {
    std::string type_string("SideSet");
    kelpie::ObjectCapacities oc;
    auto search_key = entity_search_key(parallel_rank(), *(get_region()), type_string);
    pool.List(search_key, &oc);

    auto entity_names = get_entity_names(oc.keys, type_string);
    for(auto entity_name : entity_names)
    {

      auto entity = new Ioss::SideSet(this, entity_name);

      // Add Properties that aren't created in the CTor
      auto property_search = property_search_key(parallel_rank(), *(get_region()), *entity);
      kelpie::ObjectCapacities property_oc;
      pool.List(property_search, &property_oc);
      this->read_entity_properties(property_oc, *entity);


      // Add fields that aren't created in the CTor
      auto field_search = field_search_key(parallel_rank(), *(get_region()), *entity);
      kelpie::ObjectCapacities field_oc;
      pool.List(field_search, &field_oc);
      this->read_entity_fields(field_oc, *entity);

      this->get_region()->add(entity);
    }
  }


  void DatabaseIO::read_communication_metadata() {}

  int64_t DatabaseIO::get_field_internal(const Ioss::Region *reg, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*reg, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::NodeBlock *nb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*nb, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::EdgeBlock *nb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*nb, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::FaceBlock *nb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*nb, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::ElementBlock *eb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*eb, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::SideBlock *fb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*fb, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::NodeSet *ns, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*ns, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::EdgeSet *ns, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*ns, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::FaceSet *ns, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*ns, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::ElementSet *ns, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*ns, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::SideSet *fs, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*fs, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::CommSet *cs, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*cs, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::StructuredBlock *sb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return get_field_internal(*sb, field, data, data_size);;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::Assembly *a, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return 0;
  }
  int64_t DatabaseIO::get_field_internal(const Ioss::Blob *b, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return 0;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::Region *reg, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*reg, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::NodeBlock *nb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*nb, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::EdgeBlock *eb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*eb, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::FaceBlock *nb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*nb, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::ElementBlock *eb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*eb, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::SideBlock *fb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*fb, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::NodeSet *ns, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*ns, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::EdgeSet *ns, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*ns, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::FaceSet *ns, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*ns, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::ElementSet *ns, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*ns, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::SideSet *fs, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*fs, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::CommSet *cs, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*cs, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::StructuredBlock *sb, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return put_field_internal(*sb, field, data, data_size);;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::Assembly *a, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return 0;
  }
  int64_t DatabaseIO::put_field_internal(const Ioss::Blob *b, const Ioss::Field &field,
      void *data, size_t data_size) const
  {
    return 0;
  }

  int64_t DatabaseIO::get_field_internal(const Ioss::GroupingEntity &e, const Ioss::Field &f,
      void *data, size_t data_size) const
  {
    lunasa::DataObject ldo;
    kelpie::Key k = make_key(parallel_rank(), *(get_region()), e, f);
    pool.Need( k, &ldo );

    /*
       unpack the LDO to retrieve the field data (user data) and set the output variables
       according to the field type information
       */

    auto meta_ptr(static_cast<meta_entry_t*>(ldo.GetMetaPtr()));

    auto field_ptr(static_cast<field_entry_t*>(
          static_cast<void*>(
            static_cast<char*>(ldo.GetDataPtr()) + meta_ptr->value.offset
            )));

    // TODO what other checks do we need here?
    if(data_size != field_ptr->value.size)
      return 1;

    std::memcpy(
        data, 
        static_cast<void*>(field_ptr->data + field_ptr->value.offset),
        field_ptr->value.size);

    return 0;
  }


  int64_t DatabaseIO::put_field_internal(const Ioss::GroupingEntity &e, const Ioss::Field &f,
      void *data, size_t data_size) const
  {
    auto key = make_key(parallel_rank(), *(get_region()), e, f);
    auto ldo = pack_field(*(get_region()), e, f, data, data_size);
    pool.Publish( key, ldo );
    return 0;
  }

  } // namespace Iofaodel
