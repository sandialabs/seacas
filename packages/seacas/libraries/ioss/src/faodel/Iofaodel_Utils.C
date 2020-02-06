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

#include "Iofaodel_Utils.h"
#include "Iofaodel_Serialize.h"

namespace Iofaodel {

  state_entry_t::state_entry_t(const Ioss::Region & r)
    : count(r.get_property("state_count").get_int()),
    value{0, count * sizeof(state_entry_t::basic_type)}
  {
  }

  lunasa::DataObject pack_states(const Ioss::Region & r)
  {
    // meta, entry, data, data_size
    auto state_count = r.get_property("state_count").get_int();

    // std::cerr << "state count: " << state_count << std::endl;
    auto data_size = state_count * sizeof(state_entry_t::basic_type);

    auto ldo = lunasa::DataObject(
        sizeof(meta_entry_t),
        sizeof(state_entry_t) + data_size,
        lunasa::DataObject::AllocatorType::eager);

    auto meta = static_cast<meta_entry_t*>(ldo.GetMetaPtr());
    meta->ioss_type = meta_entry_t::IossType::IofaodelStates;
    meta->value.offset = 0;
    meta->value.size = sizeof(state_entry_t) + data_size;

    auto entry = static_cast<state_entry_t*>(
        static_cast<void*>(
          static_cast<char*>(ldo.GetDataPtr()) + meta->value.offset
          )
        );
    entry->count = state_count;
    entry->value.offset = 0;
    entry->value.size = data_size;

    auto data = static_cast<Iofaodel::state_entry_t::basic_type*>(
        static_cast<void*>(entry->data + entry->value.offset));

    for(auto state(1); state <= entry->count; state++)
      data[state-1] = r.get_state_time(state);

    return ldo;
  };


  kelpie::Key make_states_key(int rank,
      const Ioss::Region & region)
  {
    auto region_name = region.name();
    if(region_name.empty()) {
      region_name="UNNAMED";
    }
    return kelpie::Key(
        std::to_string(rank),
        "/Region/" + region_name +
        "/TimeSteps/"
        );
  }


  kelpie::Key make_key(int rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & grouping_entity,
      const Ioss::Field & field)
  {
    auto region_name = region.name();
    if(region_name.empty()) {
      region_name="UNNAMED";
    }
    auto grouping_entity_name = grouping_entity.name();
    if(grouping_entity_name.empty()) {
      grouping_entity_name="UNNAMED";
    }
    return kelpie::Key(
        std::to_string(rank),
        "/Region/" + region_name +
        "/State/" + std::to_string(region.get_current_state()) +
        "/Entity/" + grouping_entity.type_string() + "/Name/" + grouping_entity_name +
        "/Field/RoleType/" + to_string(field.get_role()) +
        "/BasicType/" + to_string(field.get_type()) +
        "/Name/" + to_string(field.get_type()) + "/Name/" + field.get_name()
        );
  }

  kelpie::Key make_key(int rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & grouping_entity,
      const Ioss::Property & property)
  {
    auto region_name = region.name();
    if(region_name.empty()) {
      region_name="UNNAMED";
    }
    auto grouping_entity_name = grouping_entity.name();
    if(grouping_entity_name.empty()) {
      grouping_entity_name="UNNAMED";
    }
    return kelpie::Key(
        std::to_string(rank),
        "/Region/" + region_name +
        "/State/" + std::to_string(region.get_current_state()) +
        "/Entity/" + grouping_entity.type_string() + "/Name/" + grouping_entity_name +
        "/Property/BasicType/" + to_string(property.get_type()) + "/Name/" + property.get_name()
        );
  }

  kelpie::Key make_key(int rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & grouping_entity)
  {
    auto region_name = region.name();
    if(region_name.empty()) {
      region_name="UNNAMED";
    }
    auto grouping_entity_name = grouping_entity.name();
    if(grouping_entity_name.empty()) {
      grouping_entity_name="UNNAMED";
    }
    return kelpie::Key(
        std::to_string(rank),
        "/Region/" + region_name +
        "/State/" + std::to_string(region.get_current_state()) +
        "/Entity/" + grouping_entity.type_string() + "/Name/" + grouping_entity_name
        );
  }


  std::string to_string(const Ioss::Property::BasicType & t)
  {
    switch(t) {
      case Ioss::Property::BasicType::INVALID: return std::string("INVALID");
      case Ioss::Property::BasicType::REAL:    return std::string("REAL");
      case Ioss::Property::BasicType::INTEGER: return std::string("INTEGER");
      case Ioss::Property::BasicType::POINTER: return std::string("POINTER");
      case Ioss::Property::BasicType::STRING:  return std::string("STRING");
      default:                                 return std::string("INVALID");
    }

  }

  std::string to_string(const Ioss::Field::BasicType & t)
  {
    // INTEGER == INT32
    // DOUBLE == REAL
    switch(t) {
      case Ioss::Field::BasicType::INVALID:   return std::string("INVALID");
      case Ioss::Field::BasicType::REAL:      return std::string("REAL");
      case Ioss::Field::BasicType::INTEGER:   return std::string("INTEGER");
      case Ioss::Field::BasicType::INT64:     return std::string("INT64");
      case Ioss::Field::BasicType::COMPLEX:   return std::string("COMPLEX,");
      case Ioss::Field::BasicType::STRING:    return std::string("STRING,");
      case Ioss::Field::BasicType::CHARACTER: return std::string("CHARACTER");
      default:                                return std::string("INVALID");
    }

  }



  std::string to_string(const Ioss::Field::RoleType & t)
  {
    // INTEGER == INT32
    // DOUBLE == REAL
    switch(t) {
      case Ioss::Field::RoleType::INTERNAL:      return std::string("INTERNAL");
      case Ioss::Field::RoleType::MESH:          return std::string("MESH");
      case Ioss::Field::RoleType::ATTRIBUTE:     return std::string("ATTRIBUTE");
      case Ioss::Field::RoleType::COMMUNICATION: return std::string("COMMUNICATION");
      case Ioss::Field::RoleType::INFORMATION:   return std::string("INFORMATION");
      case Ioss::Field::RoleType::REDUCTION:     return std::string("REDUCTION");
      case Ioss::Field::RoleType::TRANSIENT:     return std::string("TRANSIENT");
      default:                                   return std::string("INVALIDE");
    }

  }

  std::string to_string(const Ioss::EntityType & t)
  {
    switch(t) {
      case Ioss::EntityType::NODEBLOCK:       return std::string("NODEBLOCK");
      case Ioss::EntityType::EDGEBLOCK:       return std::string("EDGEBLOCK");
      case Ioss::EntityType::FACEBLOCK:       return std::string("FACEBLOCK");
      case Ioss::EntityType::ELEMENTBLOCK:    return std::string("ELEMENTBLOCK");
      case Ioss::EntityType::NODESET:         return std::string("NODESET");
      case Ioss::EntityType::EDGESET:         return std::string("EDGESET");
      case Ioss::EntityType::FACESET:         return std::string("FACESET");
      case Ioss::EntityType::ELEMENTSET:      return std::string("ELEMENTSET");
      case Ioss::EntityType::SIDESET:         return std::string("SIDESET");
                                              // Ioss::EntityType::SIDESET == Ioss::EntityType::SURFACE
                                              // case Ioss::EntityType::SURFACE: return std::string("SURFACE");
      case Ioss::EntityType::COMMSET:         return std::string("COMMSET");
      case Ioss::EntityType::SIDEBLOCK:       return std::string("SIDEBLOCK");
      case Ioss::EntityType::REGION:          return std::string("REGION");
      case Ioss::EntityType::SUPERELEMENT:    return std::string("SUPERELEMENT");
      case Ioss::EntityType::STRUCTUREDBLOCK: return std::string("STRUCTUREDBLOCK");
      case Ioss::EntityType::INVALID_TYPE:    return std::string("INVALID_TYPE");
      default:                                return std::string("INVALID_TYPE");
    };
  }

} // namespace Iofaodel
