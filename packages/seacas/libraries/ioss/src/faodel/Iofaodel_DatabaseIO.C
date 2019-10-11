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

#include <faodel/Iofaodel_DatabaseIO.h>

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
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "faodel-services/MPISyncStart.hh"
#include "Iofaodel_Serialize.h"

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

config.additional_files.env_name.if_defined FAODEL_CONFIG

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

    /* CREATE a DHT that is distributed across all of the ranks of the communicator. */
    if( instanceCount++ == 0 ) {
      int size;
      MPI_Comm_size(communicator, &size);
      if( size == 1 ) {
        faodel_config.Append("dirman.resources_mpi[] dht:/ioss/dht 0");
      } else {
        faodel_config.Append("dirman.resources_mpi[] dht:/ioss/dht 0-" + std::to_string(size-1));
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

    pool = kelpie::Connect( "dht:/ioss/dht" );
    dbState = Ioss::STATE_UNKNOWN;
  }

  DatabaseIO::~DatabaseIO()
  {
    // Shut down Faodel gracefully
    if( --instanceCount == 0 ) {
      faodel::bootstrap::Finish();
    }
  }

  const std::string DatabaseIO::get_format() const { return "faodel"; }

  void DatabaseIO::read_meta_data__()
  {
    this->read_region();

    this->get_step_times__();

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
    auto region(this->get_region());
    for(int state(1); state <= region->get_property("state_count").get_int(); state++)
      region->add_state(region->get_state_time(state));
  }

  void DatabaseIO::read_region() {
    const std::string pool_name("dht:/ioss/dht");
    pool = kelpie::Connect(pool_name);
    int rank(parallel_rank());

    auto region(this->get_region());
    {
      kelpie::ObjectCapacities oc;
      pool.List( kelpie::Key(std::to_string(rank), "/Region/can.ex2/State/-1/Entity/REGION//Properties*"), &oc);
      this->read_entity_properties(oc, *(this->get_region()));
    }

    {
      kelpie::ObjectCapacities oc;
      pool.List( kelpie::Key(std::to_string(rank), "/Region/can.ex2/State/-1/Entity/REGION//Fields*"), &oc);
      this->read_entity_fields(oc, *(this->get_region()));
    }

  }

  void DatabaseIO::read_entity_properties(kelpie::ObjectCapacities oc,
                               Ioss::GroupingEntity & entity)
  {
    // Properties
    for(size_t i=0; i<oc.keys.size(); i++) {
      // NOTE implicit properties will be overwritten later
      lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
      pool.Need(oc.keys[i], oc.capacities[i], &ldo);
      auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
      auto property_name(Iofaodel::get_string(meta->property.name, ldo));

      if(meta->property.type == Ioss::Property::BasicType::STRING) {
        entity.property_update(property_name,
                               Iofaodel::get_string(meta->property.value, ldo));

      } else if(meta->property.type == Ioss::Property::BasicType::INTEGER) {
        int state_count = *(Iofaodel::get_int_ptr(meta->property.value, ldo));
        entity.property_update(property_name,
                               *(Iofaodel::get_int_ptr(meta->property.value, ldo)));

      } else if(meta->property.type == Ioss::Property::BasicType::REAL) {
        entity.property_update(property_name,
                               *(Iofaodel::get_double_ptr(meta->property.value, ldo)));
      }
    }
  }

  void DatabaseIO::read_entity_fields(kelpie::ObjectCapacities oc, Ioss::GroupingEntity & entity)
  {
    // Fields
    for(size_t i=0; i<oc.keys.size(); i++) {
      lunasa::DataObject ldo(0, oc.capacities[i], lunasa::DataObject::AllocatorType::eager);
      pool.Need(oc.keys[i], oc.capacities[i], &ldo);
      auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
      auto field_name(Iofaodel::get_string(meta->field.name, ldo));
      if(!entity.field_exists(field_name)) {
        entity.field_add(
          Ioss::Field(field_name, meta->field.type,
                      IOSS_SCALAR(),
                      meta->field.role,
                      meta->field.value_count)
          );
      }
    }
  }


  lunasa::DataObject key_to_Entry( const std::vector< kelpie::Key >::iterator& key_iter,
				   kelpie::ObjectCapacities& oc,
				   kelpie::Pool pool,
				   Entry*& entry )
    {
      size_t id( key_iter - oc.keys.begin() );
      lunasa::DataObject ldo(0, oc.capacities[id], lunasa::DataObject::AllocatorType::eager);
      pool.Need(oc.keys[id], oc.capacities[id], &ldo);
      entry = static_cast<Entry*>(ldo.GetMetaPtr());
      return ldo;
    }


  std::vector<kelpie::Key>::iterator substr_to_iterator(const std::string & substr,
                                                        kelpie::ObjectCapacities & oc)
  {
    return
         std::find_if(oc.keys.begin(), oc.keys.end(),
                     [&substr](const kelpie::Key & k) {
                       if(k.K2().find(substr) != std::string::npos)
                         return true;
                       return false;
                     }
                     );
  }


  std::string key_to_string(const std::vector< kelpie::Key >::iterator& key,
                            kelpie::ObjectCapacities& oc,
                            kelpie::Pool pool
                           )
  {
    Entry * meta;
    auto ldo = key_to_Entry(key, oc, pool, meta);
    return get_string(meta->property.value, ldo);
  }



  int64_t key_to_int(const std::vector< kelpie::Key >::iterator& key,
                            kelpie::ObjectCapacities& oc,
                            kelpie::Pool pool
                           )
  {
    Entry * meta;
    auto ldo = key_to_Entry(key, oc, pool, meta);
    return *(get_int64_ptr(meta->property.value, ldo));
  }


  double key_to_double(const std::vector< kelpie::Key >::iterator& key,
                            kelpie::ObjectCapacities& oc,
                            kelpie::Pool pool
                           )
  {
    Entry * meta;
    auto ldo = key_to_Entry(key, oc, pool, meta);
    return *(get_double_ptr(meta->property.value, ldo));
  }


  void DatabaseIO::get_edgeblocks()
  {
    // Root of the second key
    std::string root("/Region/can.ex2/State/-1/Entity/FACEBLOCK");

    kelpie::ObjectCapacities oc;
    pool.List(kelpie::Key(std::to_string(parallel_rank()), root+ "*"), &oc);

    // for(auto key : block_keys) {
    for(auto key : oc.keys) {
      if(key.K2().find("/Properties/") == std::string::npos &&
         key.K2().find("/Fields/") == std::string::npos) {

        auto name = substr_to_iterator(key.K2() + "/Properties/name", oc);
        auto topo = substr_to_iterator(key.K2() + "/Properties/topology_type", oc);
        auto count = substr_to_iterator(key.K2() + "/Properties/entity_count", oc);

        // If these values exist we can create an Ioss::ElementBlock
        if(name != oc.keys.end() && topo != oc.keys.end() && count != oc.keys.end()) {

          auto *block = new Ioss::FaceBlock(this,
                                            key_to_string(name, oc, pool),
                                            key_to_string(topo, oc, pool),
                                            key_to_int(count, oc, pool));

          // Get Property keys for this ElementBlock
          kelpie::ObjectCapacities properties;
          pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Properties*"), &properties);
          this->read_entity_properties(properties, *block);

          // Get Field keys for this ElementBlock
          kelpie::ObjectCapacities fields;
          pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Fields*"), &fields);
          this->read_entity_properties(fields, *block);

          this->get_region()->add(block);
        }
      }
    }
  }



  void DatabaseIO::get_elemblocks()
  {
    // Root of the second key
    std::string root("/Region/can.ex2/State/-1/Entity/ELEMENTBLOCK");
    kelpie::ObjectCapacities oc;
    pool.List(kelpie::Key(std::to_string(parallel_rank()), root + "*"), &oc);

    for(auto key : oc.keys) {
      if(key.K2().find("/Properties/") != std::string::npos ||
         key.K2().find("/Fields/") != std::string::npos)
      {
        continue;
      }

      auto name = substr_to_iterator(key.K2() + "/Properties/name", oc);
      auto topo = substr_to_iterator(key.K2() + "/Properties/topology_type", oc);
      auto count = substr_to_iterator(key.K2() + "/Properties/entity_count", oc);

      // If these values exist we can create an Ioss::ElementBlock
      if(name != oc.keys.end() &&
         topo != oc.keys.end() &&
         count != oc.keys.end())
      {

        auto *block = new Ioss::ElementBlock(this,
                                             key_to_string(name, oc, pool),
                                             key_to_string(topo, oc, pool),
                                             key_to_int(count, oc, pool));

        // Get Property keys for this ElementBlock
        kelpie::ObjectCapacities properties;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Properties*"), &properties);
        this->read_entity_properties(properties, *block);

        // Get Field keys for this ElementBlock
        kelpie::ObjectCapacities fields;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Fields*"), &fields);
        this->read_entity_fields(fields, *block);

        this->get_region()->add(block);
      }
    }
  }

  void DatabaseIO::get_faceblocks()
  {
    // UNTESTED
#if 0
    // Root of the second key
    std::string root("/Region/can.ex2/State/-1/Entity/FACEBLOCK");

    kelpie::ObjectCapacities oc;
    pool.List(kelpie::Key(std::to_string(parallel_rank()), root+ "*"), &oc);

    // for(auto key : block_keys) {
    for(auto key : oc.keys) {
      if(key.K2().find("/Properties/") != std::string::npos ||
         key.K2().find("/Fields/") != std::string::npos)
      {
        continue;
      }

      auto name = substr_to_iterator(key.K2() + "/Properties/name", oc);
      auto dof = substr_to_iterator(key.K2() + "/Properties/component_degree", oc);
      auto count = substr_to_iterator(key.K2() + "/Properties/entity_count", oc);

      // If these values exist we can create an Ioss::ElementBlock
      if(name != oc.keys.end() &&
         dof != oc.keys.end() &&
         count != oc.keys.end())
      {

        auto *block = new Ioss::FaceBlock(this,
                                          key_to_string(name, oc, pool),
                                          key_to_int(count, oc, pool),
                                          key_to_int(dof, oc, pool));

        // Get Property keys for this ElementBlock
        kelpie::ObjectCapacities properties;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Properties*"), &properties);
        this->read_entity_properties(properties, *block);

        // Get Field keys for this ElementBlock
        kelpie::ObjectCapacities fields;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Fields*"), &fields);
        this->read_entity_properties(fields, *block);

        this->get_region()->add(block);
      }
    }
#endif
  }

  void DatabaseIO::get_nodeblocks()
  {
    // Root of the second key
    std::string root("/Region/can.ex2/State/-1/Entity/NODEBLOCK");

    kelpie::ObjectCapacities oc;
    pool.List(kelpie::Key(std::to_string(parallel_rank()), root+ "*"), &oc);

    // for(auto key : block_keys) {
    for(auto key : oc.keys) {
      if(key.K2().find("/Properties/") != std::string::npos ||
         key.K2().find("/Fields/") != std::string::npos)
      {
        continue;
      }

      auto name = substr_to_iterator(key.K2() + "/Properties/name", oc);
      auto dof = substr_to_iterator(key.K2() + "/Properties/component_degree", oc);
      auto count = substr_to_iterator(key.K2() + "/Properties/entity_count", oc);

      // If these values exist we can create an Ioss::ElementBlock
      if(name != oc.keys.end() &&
         dof != oc.keys.end() &&
         count != oc.keys.end())
      {

        auto *block = new Ioss::NodeBlock(this,
                                          key_to_string(name, oc, pool),
                                          key_to_int(count, oc, pool),
                                          key_to_int(dof, oc, pool));

        // Get Property keys for this ElementBlock
        kelpie::ObjectCapacities properties;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Properties*"), &properties);
        this->read_entity_properties(properties, *block);

        // Get Field keys for this ElementBlock
        kelpie::ObjectCapacities fields;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Fields*"), &fields);
        this->read_entity_properties(fields, *block);

        this->get_region()->add(block);
      }
    }
  }

  void DatabaseIO::get_edgesets()
  {
  }

  void DatabaseIO::get_elemsets()
  {
  }

  void DatabaseIO::get_facesets()
  {
  }

  void DatabaseIO::get_nodesets()
  {
    // Root of the second key
    std::string root("/Region/can.ex2/State/-1/Entity/NODESET");

    kelpie::ObjectCapacities oc;
    pool.List(kelpie::Key(std::to_string(parallel_rank()), root+ "*"), &oc);

    // for(auto key : block_keys) {
    for(auto key : oc.keys) {
      if(key.K2().find("/Properties/") != std::string::npos ||
         key.K2().find("/Fields/") != std::string::npos)
      {
        continue;
      }

      auto name = substr_to_iterator(key.K2() + "/Properties/name", oc);
      auto count = substr_to_iterator(key.K2() + "/Properties/entity_count", oc);

      // If these values exist we can create an Ioss::ElementBlock
      if(name != oc.keys.end() &&
         count != oc.keys.end())
      {

        auto *set = new Ioss::NodeSet(this,
                                      key_to_string(name, oc, pool),
                                      key_to_int(count, oc, pool));

        // Get Property keys for this Elementset
        kelpie::ObjectCapacities properties;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Properties*"), &properties);
        this->read_entity_properties(properties, *set);

        // Get Field keys for this Elementset
        kelpie::ObjectCapacities fields;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Fields*"), &fields);
        this->read_entity_properties(fields, *set);

        this->get_region()->add(set);
      }
    }
  }

  void DatabaseIO::get_sidesets()
  {
    // Root of the second key
    std::string root("/Region/can.ex2/State/-1/Entity/SIDESET");

    kelpie::ObjectCapacities oc;
    pool.List(kelpie::Key(std::to_string(parallel_rank()), root+ "*"), &oc);

    for(auto key : oc.keys) {
      if(key.K2().find("/Properties/") != std::string::npos ||
         key.K2().find("/Fields/") != std::string::npos)
      {
        continue;
      }

      auto name = substr_to_iterator(key.K2() + "/Properties/name", oc);

      // If these values exist we can create an Ioss::ElementBlock
      if(name != oc.keys.end())
      {

        auto *set = new Ioss::SideSet(this, key_to_string(name, oc, pool));

        // Get Property keys for this Elementset
        kelpie::ObjectCapacities properties;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Properties*"), &properties);
        this->read_entity_properties(properties, *set);

        // Get Field keys for this Elementset
        kelpie::ObjectCapacities fields;
        pool.List(kelpie::Key(std::to_string(parallel_rank()), key.K2() + "/Fields*"), &fields);
        this->read_entity_properties(fields, *set);

        this->get_region()->add(set);
      }
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
    kelpie::Key k;
    k.K1( std::to_string( parallel_rank() ) );
    k.K2(to_string(e) + to_string(f));
    pool.Need( k, &ldo );

    auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
    std::memcpy(data, get_ptr(meta->field.value, ldo), meta->field.value.size);

    /*
      unpack the LDO to retrieve the field data (user data) and set the output variables
      according to the field type information
    */
    return 0;
  }


  int64_t DatabaseIO::put_field_internal(const Ioss::GroupingEntity &e, const Ioss::Field &f,
      void *data, size_t data_size) const
  {
    lunasa::DataObject ldo = to_ldo( *(get_region()), e, f );
    kelpie::Key k;
    k.K1( std::to_string( parallel_rank() ) );
    k.K2(to_string(e) + to_string(f));
    pool.Publish( k, ldo );
    return 0;
  }

} // namespace Iofaodel
