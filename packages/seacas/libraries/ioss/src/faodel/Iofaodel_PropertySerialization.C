#include "Iofaodel_PropertySerialization.h"
#include "Iofaodel_Utils.h"
#include <Iofaodel_Serialize.h>

#ifdef NDEBUG
#undef NDEBUG
#endif
#include <assert.h>


namespace Iofaodel {

  size_t data_size(const Ioss::Property & p) {
    auto type = p.get_type();
    if(      type == Ioss::Property::BasicType::REAL)    return sizeof(double);
    else if( type == Ioss::Property::BasicType::INTEGER) return sizeof(int64_t);
    else if( type == Ioss::Property::BasicType::POINTER) return sizeof(int64_t);
    else if( type == Ioss::Property::BasicType::STRING)  return p.get_string().size();
    else                                                 return 0;
  }


  void map_properties(
      const Ioss::Region & region,
      const Ioss::GroupingEntity & entity,
      PropertyFunction op)
  {
    std::vector<std::string> description;
    entity.property_describe(&description);
    for(auto name : description)
      op(region, entity, entity.get_property(name));
  }


  void map_properties( const Ioss::Region & region, PropertyFunction op)
  {
    map_properties(region, region, op);

    for(auto entity : region.get_edge_blocks())
      map_properties(region, *entity, op);

    for(auto entity : region.get_element_blocks())
      map_properties(region, *entity, op);

    for(auto entity : region.get_face_blocks())
      map_properties(region, *entity, op);

    for(auto entity : region.get_node_blocks())
      map_properties(region, *entity, op);

    for(auto entity : region.get_edgesets())
      map_properties(region, *entity, op);

    for(auto entity : region.get_elementsets())
      map_properties(region, *entity, op);

    for(auto entity : region.get_facesets())
      map_properties(region, *entity, op);

    for(auto entity : region.get_nodesets())
      map_properties(region, *entity, op);

    for(auto entity : region.get_sidesets())
      map_properties(region, *entity, op);
  }


  // Put this in the meta data section of the LDO
  property_entry_t::property_entry_t(
      const Ioss::Property & property, const size_t start)
    : basic_type(property.get_type()),

    is_implicit(property.is_implicit()),
    is_valid(property.is_valid()),

    name{start, property.get_name().size()},
    value{name.offset + name.size, Iofaodel::data_size(property)},
    data_size(name.size + value.size)
  {
  }


  lunasa::DataObject pack_property(const Ioss::Region & region,
      const Ioss::GroupingEntity & entity, const Ioss::Property & property)
  {
    property_entry_t property_entry(property);

    meta_entry_t meta_entry{meta_entry_t::IossType::IossProperty, 0,
      property_entry.data_size};

    auto ldo = lunasa::DataObject(
        sizeof(meta_entry_t),
        sizeof(property_entry_t) + property_entry.data_size,
        lunasa::DataObject::AllocatorType::eager);

    // copy meta_entry_t to meta section
    std::memcpy(static_cast<char*>(ldo.GetMetaPtr()), &meta_entry,
        sizeof(meta_entry_t));

    // copy property_entry_t to meta section
    std::memcpy(static_cast<char*>(ldo.GetDataPtr()), &property_entry,
        sizeof(property_entry_t)
        );

    auto entry = static_cast<property_entry_t*>(ldo.GetDataPtr());
    auto name_ptr = static_cast<char*>(entry->data) + entry->name.offset;
    auto value_ptr = static_cast<void*>(
        static_cast<char*>(entry->data) + entry->value.offset);

    // copy name to data section
    std::memcpy(name_ptr, property.get_name().data(), entry->name.size);

    // copy value to data section
    if(property.get_type() == Ioss::Property::BasicType::INTEGER) {
      auto value = static_cast<int64_t*>(value_ptr);
      *value = property.get_int();
    }
    else if(property.get_type() == Ioss::Property::BasicType::REAL) {
      auto value = static_cast<double*>(value_ptr);
      *value = property.get_int();
    }
    else if(property.get_type() == Ioss::Property::BasicType::STRING) {
      std::memcpy(
          static_cast<char*>(value_ptr),
          property.get_string().data(),
          property.get_string().size());
    }

    if(entry->value.size != Iofaodel::data_size(property))
      std::cerr << "value.size mismatch: " << entry->value.size << " ?= " <<
        Iofaodel::data_size(property);
    return ldo;
  }


} // namespace
