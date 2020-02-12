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

#ifndef Iofaodel_Utils_h
#define Iofaodel_Utils_h

#include <Ioss_Region.h>
#include <Ioss_GroupingEntity.h>
#include <Ioss_Property.h>

#include <kelpie/Key.hh>
#include <lunasa/DataObject.hh>

#include <set>

namespace Iofaodel {

  // Keys and LDOs go together, the pair below makes publishing easy
  using DataPair = std::pair<kelpie::Key, lunasa::DataObject>;


  struct value_entry_t {
    size_t offset, size;
  };


  struct meta_entry_t {
    enum class IossType {IossProperty, IossField, IofaodelStates};
    IossType ioss_type;
    value_entry_t value; // offset from LDO::GetDataPtr and size
    // NOTE an added char data[0] would point to the next meta_entry_T
  };


  struct state_entry_t {
    using basic_type = double;
    size_t count;
    value_entry_t value;
    char data[0];

    explicit state_entry_t(const Ioss::Region & r);
  };

  
  lunasa::DataObject pack_states(const Ioss::Region & r);


  kelpie::Key make_states_key(int parallel_rank,
      const Ioss::Region & region);

  kelpie::Key make_key(int parallel_rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & grouping_entity,
      const Ioss::Field & field);

  kelpie::Key make_key(int parallel_rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & grouping_entity,
      const Ioss::Property & property);

  kelpie::Key make_key(int parallel_rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & grouping_entity);

  kelpie::Key entity_search_key(int rank,
      const Ioss::Region & region,
      const std::string & entity_name);

  kelpie::Key entity_search_key(int rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & entity);

  kelpie::Key property_search_key(int parallel_rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & grouping_entity);

  kelpie::Key field_search_key(int parallel_rank,
      const Ioss::Region & region,
      const Ioss::GroupingEntity & grouping_entity);

  std::string to_string(const Ioss::Property::BasicType & t);
  std::string to_string(const Ioss::Field::BasicType & t);
  std::string to_string(const Ioss::Field::RoleType & t);
  std::string to_string(const Ioss::EntityType & t);

  std::set<std::string> get_entity_names(const std::vector<kelpie::Key> & keys,
      std::string target);

} // namespace


#endif
