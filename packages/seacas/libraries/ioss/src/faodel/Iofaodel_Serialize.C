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

#include "Iofaodel_Serialize.h"

#include <Ioss_NodeBlock.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_FaceBlock.h>
#include <Ioss_EdgeBlock.h>
#include <Ioss_StructuredBlock.h>

#include <Ioss_NodeSet.h>
#include <Ioss_ElementSet.h>
#include <Ioss_FaceSet.h>
#include <Ioss_EdgeSet.h>
#include <Ioss_SideSet.h>

namespace Iofaodel {
#if 0

  size_t data_size(const Ioss::Property & p) {
    auto type = p.get_type();
    if(      type == Ioss::Property::BasicType::REAL)    return sizeof(double);
    else if( type == Ioss::Property::BasicType::INTEGER) return sizeof(int64_t);
    else if( type == Ioss::Property::BasicType::POINTER) return sizeof(int64_t);
    else if( type == Ioss::Property::BasicType::STRING)  return p.get_string().size();
    else                                                 return 0;
  }

  size_t data_size(const Ioss::Field & f) {
    return f.get_size();
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


  int* get_int_ptr(const ValueEntry & entry, lunasa::DataObject & ldo) {
    auto ptr = static_cast<char*>(ldo.GetDataPtr()) + entry.offset;
    return static_cast<int*>( static_cast<void*>(ptr) );
  }

  int64_t* get_int64_ptr(const ValueEntry & entry, lunasa::DataObject & ldo) {
    auto ptr = static_cast<char*>(ldo.GetDataPtr()) + entry.offset;
    return static_cast<int64_t*>( static_cast<void*>(ptr) );
  }

  float* get_float_ptr(const ValueEntry & entry, lunasa::DataObject & ldo) {
    auto ptr = static_cast<char*>(ldo.GetDataPtr()) + entry.offset;
    return static_cast<float*>( static_cast<void*>(ptr) );
  }

  double* get_double_ptr(const ValueEntry & entry, lunasa::DataObject & ldo) {
    auto ptr = static_cast<char*>(ldo.GetDataPtr()) + entry.offset;
    return static_cast<double*>( static_cast<void*>(ptr) );
  }

  // TODO this probably is only right for Ioss::Property, but that's all we have so far
  double* get_real_ptr(const ValueEntry & entry, lunasa::DataObject & ldo) {
    return get_double_ptr(entry, ldo);
  }

  char* get_char_ptr(const ValueEntry & entry, lunasa::DataObject & ldo) {
    return static_cast<char*>(ldo.GetDataPtr()) + entry.offset;
  }

  void* get_ptr(const ValueEntry & entry, lunasa::DataObject & ldo) {
    return static_cast<void*>(
      static_cast<char*>(ldo.GetDataPtr()) + entry.offset
      );
  }

  std::string get_string(const ValueEntry & entry, lunasa::DataObject & ldo) {
    return std::string(
      static_cast<char*>(ldo.GetDataPtr()) + entry.offset,
      entry.size);
  }


  inline void Entry::init() {
    this->region.name.offset    = 0;
    this->region.name.size      = 0;
    this->region.state          = 0;

    this->entity.name.offset    = 0;
    this->entity.name.size      = 0;
    this->entity.type           = Ioss::EntityType::INVALID_TYPE;

    this->property.name.offset  = 0;
    this->property.name.size    = 0;

    this->property.value.offset = 0;
    this->property.value.size   = 0;
    this->property.type         = Ioss::Property::BasicType::INVALID;

    this->field.name.offset     = 0;
    this->field.name.size       = 0;

    this->field.value.offset    = 0;
    this->field.value.size      = 0;
    this->field.type            = Ioss::Field::BasicType::INVALID;

    this->offset                = 0;
    this->data_size             = 0;
  }


  inline void Entry::add_data(const Ioss::Region & r) {
    auto filename(r.get_database()->get_filename());
    size_t loc(filename.find_last_of('/'));
    filename.erase(0,loc+1);

    // always the first data value
    this->region.name.offset = this->offset;
    this->region.name.size   = filename.size();
    this->region.state       = r.get_current_state();

    this->data_size += this->region.name.size;
  }


  inline void Entry::add_data(const Ioss::GroupingEntity& e) {
    this->entity.name.offset = this->region.name.offset + this->region.name.size;
    this->entity.name.size   = e.name().size();
    this->entity.type        = e.type();

    this->data_size += this->entity.name.size;
  }


  inline void Entry::add_data(const Ioss::Property& p) {
    this->property.name.offset  = this->entity.name.offset + this->entity.name.size;
    this->property.name.size    = p.get_name().size();

    this->property.value.offset = this->property.name.offset + this->property.name.size;
    this->property.value.size   = Iofaodel::data_size(p);

    this->property.type         = p.get_type();

    this->data_size += this->property.name.size + this->property.value.size;
  }


  inline void Entry::add_data(const Ioss::Field& f) {
    this->field.name.offset  = this->entity.name.offset + this->entity.name.size;
    this->field.name.size    = f.get_name().size();

    this->field.value.offset = this->field.name.offset + this->field.name.size;
    this->field.value.size   = f.get_size();

    this->field.storage.offset = this->field.value.offset + this->field.value.size;
    this->field.storage.size   = f.raw_storage()->name().size();

    this->field.value_count  = f.raw_count();
    this->field.type         = f.get_type();
    this->field.role         = f.get_role();

    this->data_size += this->field.name.size
      + this->field.value.size
      + this->field.storage.size;
  }

  inline void Entry::add_data(const Ioss::Region & r,
                              const Ioss::GroupingEntity& e,
                              const Ioss::Property& p) {
    this->add_data(r); this->add_data(e); this->add_data(p);
  }


  inline void Entry::add_data(const Ioss::Region & r,
                              const Ioss::GroupingEntity& e,
                              const Ioss::Field & f) {
    this->add_data(r); this->add_data(e); this->add_data(f);
  }



  inline void Entry::pack(const Ioss::Region & r, lunasa::DataObject ldo) {
    auto filename(r.get_database()->get_filename());
    size_t loc(filename.find_last_of('/'));
    filename.erase(0,loc+1);

    auto data(static_cast<char*>(ldo.GetDataPtr()) + this->offset);
    std::memcpy(data + this->region.name.offset,
                filename.data(),
                filename.size());
  }

  inline void Entry::pack(const Ioss::GroupingEntity & e, lunasa::DataObject ldo) {
    auto data(static_cast<char*>(ldo.GetDataPtr()) + this->offset);
    std::memcpy(data + this->entity.name.offset,
                e.name().data(),
                e.name().size());
  }

  inline void Entry::pack(const Ioss::Property & p, lunasa::DataObject ldo) {
    auto data(static_cast<char*>(ldo.GetDataPtr()) + this->offset);
    std::memcpy(data + this->property.name.offset,
                p.get_name().data(),
                p.get_name().size());
  }

  inline void Entry::pack(const Ioss::Field & f, lunasa::DataObject ldo) {
    auto data(static_cast<char*>(ldo.GetDataPtr()) + this->offset);
    std::memcpy(data + this->field.name.offset,
                f.get_name().data(),
                f.get_name().size());
    std::memcpy(data + this->field.storage.offset,
                f.raw_storage()->name().data(),
                f.raw_storage()->name().size());
  }

  inline void Entry::pack(const Ioss::Region & r,
                          const Ioss::GroupingEntity& e,
                          lunasa::DataObject ldo) {
    this->pack(r, ldo); this->pack(e, ldo);
  }

  inline void Entry::pack(const Ioss::Region & r,
                          const Ioss::GroupingEntity& e,
                          const Ioss::Property& p, lunasa::DataObject ldo) {
    this->pack(r, ldo); this->pack(e, ldo); this->pack(p, ldo);

    auto data(static_cast<char*>(ldo.GetDataPtr()) + this->offset);
    switch(p.get_type()) {
      case Ioss::Property::BasicType::REAL:
        *get_real_ptr(this->property.value, ldo) = p.get_real();
        break;
      case Ioss::Property::BasicType::INTEGER:
        *get_int64_ptr(this->property.value, ldo) = p.get_int();
        break;
#if 0
      case Ioss::Property::BasicType::POINTER:
        *get_ptr(this->property.value, ldo) = p.get_pointer();
        break;
#endif
      case Ioss::Property::BasicType::STRING:
        std::memcpy(data + this->property.value.offset,
                    p.get_string().data(),
                    p.get_string().size());
        break;
      default:
        break;
    }
  }

  inline void Entry::pack(const Ioss::Region & r,
                          const Ioss::GroupingEntity& e,
                          const Ioss::Field& f, lunasa::DataObject ldo) {
    this->pack(r, ldo); this->pack(e, ldo); this->pack(f, ldo);
  }

  inline void Entry::pack(const Ioss::Region & r,
                          const Ioss::GroupingEntity& e,
                          const Ioss::Field& f, lunasa::DataObject ldo, 
                          const void *data_ptr_, const size_t data_size_) {
    this->pack(r, ldo); this->pack(e, ldo); this->pack(f, ldo);

    // TODO Is check necessary?
    // Write TRANSIENT and REDUCTION fields if we're at a real State
    if(r.get_current_state() > 0) {
      if(f.get_role() == Ioss::Field::RoleType::TRANSIENT ||
         f.get_role() == Ioss::Field::RoleType::REDUCTION) {
        this->pack_field_data(ldo, data_ptr_, data_size_);
      }
    }
    // Write all other fields if we're not at a read State
    else {
      if(f.get_role() != Ioss::Field::RoleType::TRANSIENT &&
         f.get_role() != Ioss::Field::RoleType::REDUCTION) {
        this->pack_field_data(ldo, data_ptr_, data_size_);
      }
    }
  }

  inline void Entry::pack_field_data(lunasa::DataObject ldo, 
                                     const void *data_ptr_, 
                                     const size_t data_size_) 
  {
    auto data(static_cast<char*>(ldo.GetDataPtr()) + this->offset);
    std::memcpy(data + this->field.value.offset, 
                data_ptr_,  
                data_size_);
  }

  std::ostream & operator<<(std::ostream & os, const ValueEntry & entry) {
    os << "{" << entry.offset << ", " << entry.size << "}";
    return os;
  }

  std::ostream & operator<<(std::ostream & os, const RegionEntry & entry) {
    os << "{Region: " << entry.name << " @ " << entry.state << "}";
    return os;
  }

  std::ostream & operator<<(std::ostream & os, const EntityEntry & entry) {
    os << "{Entity: " << Iofaodel::to_string(entry.type) << ", " << entry.name << "}";
    return os;
  }

  std::ostream & operator<<(std::ostream & os, const PropertyEntry & entry) {
    os << "{Property: " << Iofaodel::to_string(entry.type) << ", " << entry.name << ", value=" << entry.value << "}";
    return os;
  }

  std::ostream & operator<<(std::ostream & os, const FieldEntry & entry) {
    os << "{Field: " << Iofaodel::to_string(entry.type) << ", " << entry.name << ", value=" << entry.value << "}";
    return os;
  }


  std::ostream & operator<<(std::ostream & os, const Entry & entry) {
    os << "*** ";
    os << entry.region;
    os << ", " << entry.entity;
    os << ", " << entry.property;
    os << ", " << entry.field;
    return os;
  }

  std::ostream & operator<<(std::ostream & os, lunasa::DataObject ldo) {
    auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));

    os << get_string(meta->region.name,   ldo) << " " << meta->region<< " | ";
    os << get_string(meta->entity.name,   ldo) << " " << meta->entity<< " | ";

    if(meta->property.type != Ioss::Property::BasicType::INVALID) {
      os << get_string(meta->property.name, ldo) << " " << meta->property << " = ";
      if(meta->property.type == Ioss::Property::BasicType::STRING) {
        os << get_string(meta->property.value, ldo);
      } else if(meta->property.type == Ioss::Property::BasicType::INTEGER) {
        os << *get_int64_ptr(meta->property.value, ldo);
      } else if(meta->property.type == Ioss::Property::BasicType::REAL) {
        os << *get_double_ptr(meta->property.value, ldo);
      }
    }

    int print_count(3);
    if(meta->field.type != Ioss::Field::BasicType::INVALID) {
      os << get_string(meta->field.name, ldo) << " " << meta->field << " = ";
      if(meta->field.type == Ioss::Field::BasicType::REAL) {
        auto ptr(get_double_ptr(meta->field.value, ldo));
        for(auto i(0); i < print_count; i++)
          std::cout << ptr[i] << ", ";
      } else if(meta->field.type == Ioss::Field::BasicType::INTEGER) {
        auto ptr(get_int_ptr(meta->field.value, ldo));
        for(auto i(0); i < print_count; i++)
          std::cout << ptr[i] << ", ";
      } else if(meta->field.type == Ioss::Field::BasicType::INT64) {
        auto ptr(get_int64_ptr(meta->field.value, ldo));
        for(auto i(0); i < print_count; i++)
          std::cout << ptr[i] << ", ";
      } else if(meta->field.type == Ioss::Field::BasicType::CHARACTER) {
        auto ptr(get_char_ptr(meta->field.value, ldo));
        for(auto i(0); i < print_count; i++)
          std::cout << ptr[i] << ", ";
      }
    }

    return os;
  }


  EntityOperator SerializeEntity(std::vector<lunasa::DataObject> & ldos) {
    return [&ldos](const Ioss::Region& r, const Ioss::GroupingEntity& e){
      Entry entry;
      entry.init();
      entry.add_data(r);
      entry.add_data(e);

      ldos.push_back(
        lunasa::DataObject(sizeof(Entry), entry.data_size,
                           lunasa::DataObject::AllocatorType::eager));

      auto ldo(ldos.back());

      auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
      *meta = entry;
      assert(meta->property.name.size == entry.property.name.size);
      meta->pack(r, e, ldo);

      return;
    };
  }

  PropertyOperator SerializeProperty(std::vector<lunasa::DataObject> & ldos) {
    return [&ldos](const Ioss::Region& r, const Ioss::GroupingEntity& e, const Ioss::Property & p){
      Entry entry;
      entry.init();
      entry.add_data(r, e, p);

      ldos.push_back(
        lunasa::DataObject(sizeof(Entry), entry.data_size,
                           lunasa::DataObject::AllocatorType::eager));

      auto ldo(ldos.back());

      auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
      *meta = entry;
      assert(meta->property.name.size == entry.property.name.size);
      meta->pack(r, e, p, ldo);
    };
  }

#if 0
  FieldOperator SerializeField(std::vector<lunasa::DataObject> & ldos) {
    return [&ldos](const Ioss::Region& r, const Ioss::GroupingEntity& e, const Ioss::Field & f){
      Entry entry;
      entry.init();
      // TODO Check field type
      entry.add_data(r, e, f);

      ldos.push_back(
        lunasa::DataObject(sizeof(Entry), entry.data_size,
                           lunasa::DataObject::AllocatorType::eager));

      auto ldo(ldos.back());

      auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
      *meta = entry;
      assert(meta->property.name.size == entry.property.name.size);
      meta->pack(r, e, f, ldo);
    };
  }
#endif

  FieldOperator SerializeField(std::vector<lunasa::DataObject> & ldos,
                               std::vector<Ioss::Field::RoleType> roles) {
    return [&ldos, &roles](const Ioss::Region& r, const Ioss::GroupingEntity& e, const Ioss::Field & f){
      if(std::find(roles.begin(), roles.end(), f.get_role()) != roles.end()) {
        Entry entry;
        entry.init();
        // TODO Check field type
        entry.add_data(r, e, f);

        ldos.push_back(
          lunasa::DataObject(sizeof(Entry), entry.data_size,
                             lunasa::DataObject::AllocatorType::eager));

        auto ldo(ldos.back());

        auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
        *meta = entry;
        assert(meta->property.name.size == entry.property.name.size);
        meta->pack(r, e, f, ldo);
      }
    };
  }


  void map_ioss(const Ioss::Region & region,
                const Ioss::GroupingEntity & entity,
                EntityOperator eop,
                PropertyOperator pop,
                FieldOperator fop)
  {
    eop(region, entity);

    std::vector<std::string> names;
    entity.property_describe(&names);
    for(auto name : names)
      pop(region, entity, entity.get_property(name));

    names.clear();
    entity.field_describe(&names);
    for(auto name : names)
      fop(region, entity, entity.get_field(name));

  }


  void map_ioss(const Ioss::Region & r,
                EntityOperator eop,
                PropertyOperator pop,
                FieldOperator fop)
  {
    map_ioss(r, r, eop, pop, fop);

    for(auto e : r.get_edge_blocks())    map_ioss(r, *e, eop, pop, fop);
    for(auto e : r.get_element_blocks()) map_ioss(r, *e, eop, pop, fop);
    for(auto e : r.get_face_blocks())    map_ioss(r, *e, eop, pop, fop);
    for(auto e : r.get_node_blocks())    map_ioss(r, *e, eop, pop, fop);

    for(auto e : r.get_edgesets())       map_ioss(r, *e, eop, pop, fop);
    for(auto e : r.get_elementsets())    map_ioss(r, *e, eop, pop, fop);
    for(auto e : r.get_facesets())       map_ioss(r, *e, eop, pop, fop);
    for(auto e : r.get_nodesets())       map_ioss(r, *e, eop, pop, fop);
    for(auto e : r.get_sidesets())       map_ioss(r, *e, eop, pop, fop);
  }


  lunasa::DataObject to_ldo(
    const Ioss::Region & r,
    const Ioss::GroupingEntity & e,
    const Ioss::Property & p)
  {
    Entry entry;
    entry.init();
    entry.add_data(r, e, p);

    auto ldo = lunasa::DataObject(
      sizeof(Entry), entry.data_size,
      lunasa::DataObject::AllocatorType::eager);

    auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
    *meta = entry;
    meta->pack(r, e, p, ldo);

    return ldo;
  }

  lunasa::DataObject to_ldo(
    const Ioss::Region & r,
    const Ioss::GroupingEntity & e,
    const Ioss::Field & f)
  {
    Entry entry;
    entry.init();
    entry.add_data(r, e, f);

    auto ldo = lunasa::DataObject(
      sizeof(Entry), entry.data_size,
      lunasa::DataObject::AllocatorType::eager);

    auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
    *meta = entry;
    meta->pack(r, e, f, ldo);
    return ldo;
  }

  lunasa::DataObject to_ldo(
    const Ioss::Region & r,
    const Ioss::GroupingEntity & e,
    const Ioss::Field & f,
    void *data, size_t data_size)
  {
    Entry entry;
    entry.init();
    entry.add_data(r, e, f);

    auto ldo = lunasa::DataObject(
      sizeof(Entry), entry.data_size,
      lunasa::DataObject::AllocatorType::eager);

    auto meta(static_cast<Entry*>(ldo.GetMetaPtr()));
    *meta = entry;
    meta->pack(r, e, f, ldo, data, data_size);
    return ldo;
  }

  std::string to_string(const Ioss::Region & r)
  {
    auto filename(r.get_database()->get_filename());
    size_t loc(filename.find_last_of('/'));
    filename.erase(0,loc+1);

    return std::string(
      "/Region/" + filename +
      "/State/" + std::to_string(r.get_current_state())
      );
  }
  std::string to_string(const Ioss::GroupingEntity & e)
  {
      // Don't use the region name as it may be unreliable
      // That is, it's not property of the Region.
      if(e.type_string() == "Region") {
          return std::string(
              "/Entity/" + e.type_string()
              );
      }
      else
      {
          return std::string(
              "/Entity/" + e.type_string() +
              "/" + e.name()
              );
      }
  }
  std::string to_string(const Ioss::Property & p)
  {
    return std::string(
      "/Properties/" + p.get_name()
                      );
  }
  std::string to_string(const Ioss::Field & p)
  {
    return std::string(
      "/Fields/" + p.get_name() + "/" + to_string(p.get_role())
                      );
  }

  std::string to_string(const RegionEntry & r, lunasa::DataObject & ldo)
  {
    return std::string(
      "/Region/" + get_string(r.name, ldo) +
      "/State/" + std::to_string(r.state)
      );
  }
  std::string to_string(const EntityEntry & e, lunasa::DataObject & ldo)
  {
    return std::string(
      "/Entity/" + to_string(e.type) +
      "/" + get_string(e.name, ldo)
      );
  }
  std::string to_string(const PropertyEntry & p, lunasa::DataObject & ldo)
  {
    return std::string(
      "/Properties/" + Iofaodel::get_string(p.name, ldo)
      );
  }
  std::string to_string(const FieldEntry & f, lunasa::DataObject & ldo)
  {
    return std::string(
      "/Fields/" + Iofaodel::get_string(f.name, ldo) + "/" + to_string(f.role)
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

#endif
} // namespace Iofaodel
