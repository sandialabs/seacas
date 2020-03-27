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

#ifndef Iofaodel_Serialize_h
#define Iofaodel_Serialize_h

#include <Ioss_DatabaseIO.h>
#include <Ioss_DatabaseIO.h> // for DatabaseIO
#include <Ioss_EdgeBlock.h>
#include <Ioss_EdgeSet.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_ElementSet.h>
#include <Ioss_FaceBlock.h>
#include <Ioss_FaceSet.h>
#include <Ioss_Field.h>   // for Region
#include <Ioss_IOFactory.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_NodeSet.h>
#include <Ioss_Property.h>   // for Region
#include <Ioss_Region.h>
#include <Ioss_Region.h>   // for Region
#include <Ioss_SideBlock.h>
#include <Ioss_SideSet.h>
#include <Ioss_State.h>      // for State
#include <Ioss_StructuredBlock.h>

#include <cstddef>           // for size_t
#include <cstdint>           // for int64_t
#include <string>            // for string
#include <vector>            // for vector

#include <kelpie/Kelpie.hh>

namespace Ioss {
class CommSet;
class EdgeBlock;
class EdgeSet;
class ElementBlock;
class ElementSet;
class EntityBlock;
class FaceBlock;
class FaceSet;
class Field;
class GroupingEntity;
class NodeBlock;
class NodeSet;
class PropertyManager;
class Region;
class SideBlock;
class SideSet;
class StructuredBlock;
} // namespace Ioss


namespace Iofaodel {
#if 0

  // Map operations for Ioss types --------------------------------------------
  // Function that access Ioss types and return void
  // NOTE: Lambda and functors can have state that can store
  // data derived from these arguments
  using EntityOperator = std::function<
    void (const Ioss::Region & r, const Ioss::GroupingEntity& e)>;

  using PropertyOperator = std::function<
    void (const Ioss::Region & r, const Ioss::GroupingEntity& e, const Ioss::Property & p)>;

  using FieldOperator = std::function<
    void (const Ioss::Region & r, const Ioss::GroupingEntity& e, const Ioss::Field& f)>;


  void map_ioss(const Ioss::Region & region,
                const Ioss::GroupingEntity & entity,
                EntityOperator eop,
                PropertyOperator pop,
                FieldOperator fop);

  void map_ioss(const Ioss::Region & r,
                EntityOperator eop,
                PropertyOperator pop,
                FieldOperator fop);

  // Track Serialized Data ----------------------------------------------------
  // Location and length of LDO data entry
  struct ValueEntry    { size_t offset; size_t size; };

  // Memento of sorts:
  // Store info about the original Ioss types
  //  - Everything has a name
  struct RegionEntry   { ValueEntry name; int state; };
  struct EntityEntry   { ValueEntry name; Ioss::EntityType type; };
  struct PropertyEntry { ValueEntry name; ValueEntry value; Ioss::Property::BasicType type;};
  struct FieldEntry    { ValueEntry name; ValueEntry value;
    ValueEntry storage;
    size_t value_count;
    Ioss::Field::BasicType type;
    Ioss::Field::RoleType role;};


  // Given an LDO and ValueEntry provide access to data
  int* get_int_ptr(const ValueEntry & entry, lunasa::DataObject & ldo);
  int64_t* get_int64_ptr(const ValueEntry & entry, lunasa::DataObject & ldo);
  float* get_float_ptr(const ValueEntry & entry, lunasa::DataObject & ldo);
  double* get_double_ptr(const ValueEntry & entry, lunasa::DataObject & ldo);
  double* get_real_ptr(const ValueEntry & entry, lunasa::DataObject & ldo);
  void* get_ptr(const ValueEntry & entry, lunasa::DataObject & ldo);
  std::string get_string(const ValueEntry & entry, lunasa::DataObject & ldo);


  // This is what gets put into the LDO MetaData section
  struct Entry {
    RegionEntry  region;
    EntityEntry  entity;

    // There is redundance here since each entry is derived from
    //   - Ioss::Property
    //   - Ioss::Field
    //   - Neither if the types is a subclass of Ioss::GroupingEntit
    PropertyEntry property;
    FieldEntry   field;
    Ioss::DatabaseUsage db_usage;
    size_t offset;
    size_t data_size;

    inline void init();

    // Accumulate sizes for LDO DataPtr entries
    inline void add_data(const Ioss::Property& p);
    inline void add_data(const Ioss::Field& f);
    inline void add_data(const Ioss::Region & r);
    inline void add_data(const Ioss::GroupingEntity& e);
    inline void add_data(const Ioss::Region & r,
                         const Ioss::GroupingEntity& e,
                         const Ioss::Property& p);
    inline void add_data(const Ioss::Region & r,
                         const Ioss::GroupingEntity& e,
                         const Ioss::Field & f);


    // Do the actual data writes
    inline void pack(const Ioss::Region & r, lunasa::DataObject ldo);
    inline void pack(const Ioss::GroupingEntity & e, lunasa::DataObject ldo);
    inline void pack(const Ioss::Property & p, lunasa::DataObject ldo);
    inline void pack(const Ioss::Field & f, lunasa::DataObject ldo);
    inline void pack(const Ioss::Region & r,
                     const Ioss::GroupingEntity& e,
                     lunasa::DataObject ldo);
    inline void pack(const Ioss::Region & r,
                     const Ioss::GroupingEntity& e,
                     const Ioss::Property& p, lunasa::DataObject ldo);
    inline void pack(const Ioss::Region & r,
                     const Ioss::GroupingEntity& e,
                     const Ioss::Field& f, lunasa::DataObject ldo);
    inline void pack(const Ioss::Region & r,
                     const Ioss::GroupingEntity& e,
                     const Ioss::Field& f, lunasa::DataObject ldo,
                     const void *data_ptr, const size_t data_size);

    inline void pack_field_data(lunasa::DataObject ldo, 
                                const void *data_ptr_, 
                                const size_t data_size_);
  };


  // This helps the visual debugger!
  std::ostream & operator<<(std::ostream & os, const ValueEntry & entry);
  std::ostream & operator<<(std::ostream & os, const RegionEntry & entry);
  std::ostream & operator<<(std::ostream & os, const EntityEntry & entry);
  std::ostream & operator<<(std::ostream & os, const PropertyEntry & entry);
  std::ostream & operator<<(std::ostream & os, const FieldEntry & entry);
  std::ostream & operator<<(std::ostream & os, const Entry & entry);
  std::ostream & operator<<(std::ostream & os, lunasa::DataObject ldo);

  std::string to_string(const Ioss::Region & r);
  std::string to_string(const Ioss::GroupingEntity & e);
  std::string to_string(const Ioss::Property & p);
  std::string to_string(const Ioss::Field & p);

  std::string to_string(const RegionEntry & r, lunasa::DataObject & ldo);
  std::string to_string(const EntityEntry & e, lunasa::DataObject & ldo);
  std::string to_string(const PropertyEntry & p, lunasa::DataObject & ldo);
  std::string to_string(const FieldEntry & p, lunasa::DataObject & ldo);

  // Mostly for debugging
  std::string to_string(const Ioss::Property::BasicType & t);
  std::string to_string(const Ioss::Field::BasicType & t);
  std::string to_string(const Ioss::Field::RoleType & t);
  std::string to_string(const Ioss::EntityType & t);

  // Serialization Helpers ----------------------------------------------------
  // Return byte size of data in each type
  size_t data_size(const Ioss::Property & p);
  size_t data_size(const Ioss::Field & f);

  EntityOperator SerializeEntity(std::vector<lunasa::DataObject> & ldos);
  PropertyOperator SerializeProperty(std::vector<lunasa::DataObject> & ldos);
#if 0
  FieldOperator SerializeField(std::vector<lunasa::DataObject> & ldos);
#endif
  FieldOperator SerializeField(std::vector<lunasa::DataObject> & ldos,
                               std::vector<Ioss::Field::RoleType> roles);

  lunasa::DataObject to_ldo(
    const Ioss::Region & r,
    const Ioss::GroupingEntity & e,
    const Ioss::Property & p);

  lunasa::DataObject to_ldo(
    const Ioss::Region & r,
    const Ioss::GroupingEntity & e,
    const Ioss::Field & f);

  lunasa::DataObject to_ldo(
    const Ioss::Region & r,
    const Ioss::GroupingEntity & e,
    const Ioss::Field & f, 
    void *data,
    size_t data_size);


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


#endif
} // namespace Iofaodel
#endif // Iofaodel_Serialize
