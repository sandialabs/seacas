// Copyright(C) 1999-2025 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "Ioss_ElementTopology.h"
#include "Ioss_Region.h"
#include "Ioss_SmartAssert.h"
#include "Ioss_Utils.h"
#include "Ioss_VariableType.h"
#include "exonull/Ioexnl_Utils.h"
#include <cstring>
#include <exodusII.h>
#include <exodusII_int.h>
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <iosfwd>
#include <netcdf.h>
#include <tokenize.h>

#include "Ioss_CoordinateFrame.h"
#include "Ioss_DatabaseIO.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_Field.h"
#include "Ioss_GroupingEntity.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"

namespace {
  size_t match(const std::string &name1, const std::string &name2)
  {
    size_t l1  = name1.size();
    size_t l2  = name2.size();
    size_t len = std::min(l1, l2);
    for (size_t i = 0; i < len; i++) {
      if (name1[i] != name2[i]) {
        while (i > 0 && (isdigit(name1[i - 1]) != 0) && (isdigit(name2[i - 1]) != 0)) {
          i--;
          // Back up to first non-digit so to handle "evar0000, evar0001, ..., evar 1123"
        }
        return i;
      }
    }
    return len;
  }

  template <typename INT>
  void internal_write_coordinate_frames(int exoid, const Ioss::CoordinateFrameContainer &frames,
                                        INT /*dummy*/)
  {
    // Query number of coordinate frames...
    int nframes = static_cast<int>(frames.size());
    if (nframes > 0) {
      std::vector<char>   tags(nframes);
      std::vector<double> coordinates(nframes * 9);
      std::vector<INT>    ids(nframes);

      for (size_t i = 0; i < frames.size(); i++) {
        ids[i]              = frames[i].id();
        tags[i]             = frames[i].tag();
        const double *coord = frames[i].coordinates();
        for (size_t j = 0; j < 9; j++) {
          coordinates[9 * i + j] = coord[j];
        }
      }
      int ierr = ex_put_coordinate_frames(exoid, nframes, Data(ids), Data(coordinates), Data(tags));
      if (ierr < 0) {
        Ioexnl::exodus_error(exoid, __LINE__, __func__, __FILE__);
      }
    }
  }

} // namespace

namespace Ioexnl {
  void update_last_time_attribute(int exodusFilePtr, double value)
  {
    double tmp    = 0.0;
    int    rootid = static_cast<unsigned>(exodusFilePtr) & EX_FILE_ID_MASK;
    int    status = nc_get_att_double(rootid, NC_GLOBAL, "last_written_time", &tmp);

    if (status == EX_NOERR && value > tmp) {
      status = nc_put_att_double(rootid, NC_GLOBAL, "last_written_time", NC_DOUBLE, 1, &value);
      if (status != EX_NOERR) {
        ex_opts(EX_VERBOSE);
        auto errmsg = fmt::format(
            "Error: failed to define 'last_written_time' attribute to file id {}", exodusFilePtr);
        ex_err_fn(exodusFilePtr, __func__, errmsg.c_str(), status);
      }
    }
  }

  Ioss::EntityType map_exodus_type(ex_entity_type type)
  {
    switch (type) {
    case EX_ASSEMBLY: return Ioss::ASSEMBLY;
    case EX_BLOB: return Ioss::BLOB;
    case EX_EDGE_BLOCK: return Ioss::EDGEBLOCK;
    case EX_EDGE_SET: return Ioss::EDGESET;
    case EX_ELEM_BLOCK: return Ioss::ELEMENTBLOCK;
    case EX_ELEM_SET: return Ioss::ELEMENTSET;
    case EX_FACE_BLOCK: return Ioss::FACEBLOCK;
    case EX_FACE_SET: return Ioss::FACESET;
    case EX_NODAL: return Ioss::NODEBLOCK;
    case EX_NODE_SET: return Ioss::NODESET;
    case EX_SIDE_SET: return Ioss::SIDESET;
    case EX_GLOBAL: return Ioss::REGION;
    default: return Ioss::INVALID_TYPE;
    }
  }

  ex_entity_type map_exodus_type(Ioss::EntityType type)
  {
    switch (type) {
    case Ioss::REGION: return EX_GLOBAL;
    case Ioss::ASSEMBLY: return EX_ASSEMBLY;
    case Ioss::BLOB: return EX_BLOB;
    case Ioss::EDGEBLOCK: return EX_EDGE_BLOCK;
    case Ioss::EDGESET: return EX_EDGE_SET;
    case Ioss::ELEMENTBLOCK: return EX_ELEM_BLOCK;
    case Ioss::ELEMENTSET: return EX_ELEM_SET;
    case Ioss::FACEBLOCK: return EX_FACE_BLOCK;
    case Ioss::FACESET: return EX_FACE_SET;
    case Ioss::NODEBLOCK: return EX_NODAL;
    case Ioss::NODESET: return EX_NODE_SET;
    case Ioss::SIDESET: return EX_SIDE_SET;
    case Ioss::SIDEBLOCK: return EX_SIDE_SET;
    case Ioss::COMMSET: return static_cast<ex_entity_type>(0);
    default: return EX_INVALID;
    }
  }

  bool read_last_time_attribute(int exodusFilePtr, double *value)
  {
    // Check whether the "last_written_time" attribute exists.  If it does,
    // return the value of the attribute in 'value' and return 'true'.
    // If not, don't change 'value' and return 'false'.
    bool found = false;

    int     rootid   = static_cast<unsigned>(exodusFilePtr) & EX_FILE_ID_MASK;
    nc_type att_type = NC_NAT;
    size_t  att_len  = 0;
    int     status   = nc_inq_att(rootid, NC_GLOBAL, "last_written_time", &att_type, &att_len);
    if (status == EX_NOERR && att_type == NC_DOUBLE) {
      // Attribute exists on this database, read it...
      double tmp = 0.0;
      status     = nc_get_att_double(rootid, NC_GLOBAL, "last_written_time", &tmp);
      if (status == EX_NOERR) {
        *value = tmp;
        found  = true;
      }
      else {
        ex_opts(EX_VERBOSE);
        auto errmsg = fmt::format(
            "Error: failed to read last_written_time attribute from file id {}", exodusFilePtr);
        ex_err_fn(exodusFilePtr, __func__, errmsg.c_str(), status);
        found = false;
      }
    }
    return found;
  }

  bool check_processor_info(const std::string &filename, int exodusFilePtr, int processor_count,
                            int processor_id)
  {
    // A restart file may contain an attribute which contains
    // information about the processor count and current processor id
    // when the file was written.  This code checks whether that
    // information matches the current processor count and id.  If it
    // exists, but doesn't match, a warning message is printed.
    // Eventually, this will be used to determine whether certain
    // decomposition-related data in the file is valid or has been
    // invalidated by a join/re-spread to a different number of
    // processors.
    bool matches = true;

    nc_type att_type = NC_NAT;
    size_t  att_len  = 0;
    int     status   = nc_inq_att(exodusFilePtr, NC_GLOBAL, "processor_info", &att_type, &att_len);
    if (status == EX_NOERR && att_type == NC_INT) {
      // Attribute exists on this database, read it and check that the information
      // matches the current processor count and processor id.
      int proc_info[2];
      status = nc_get_att_int(exodusFilePtr, NC_GLOBAL, "processor_info", proc_info);
      if (status == EX_NOERR) {
        if (proc_info[0] != processor_count && proc_info[0] > 1) {
          fmt::print(Ioss::WarnOut(),
                     "Processor decomposition count in file ({}) does not match current "
                     "processor count ({}) in file named '{}'.\n",
                     proc_info[0], processor_count, filename);
          matches = false;
        }
        if (proc_info[1] != processor_id) {
          fmt::print(
              Ioss::WarnOut(),
              "The file '{}' was originally written on processor {}, but is now being read on "
              "processor {}.\n"
              "This may cause problems if there is any processor-dependent data on the file.\n",
              filename, proc_info[1], processor_id);
          matches = false;
        }
      }
      else {
        ex_opts(EX_VERBOSE);
        auto errmsg =
            fmt::format("Error: failed to read processor info attribute from file {}", filename);
        ex_err_fn(exodusFilePtr, __func__, errmsg.c_str(), status);
        return EX_FATAL != 0;
      }
    }
    return matches;
  }

  bool type_match(const std::string &type, const char *substring)
  {
    // Returns true if 'substring' is a sub-string of 'type'.
    // The comparisons are case-insensitive
    // 'substring' is required to be in all lowercase.
    const char *s = substring;
    const char *t = type.c_str();

    SMART_ASSERT(s != nullptr);
    while (*s != '\0' && *t != '\0') {
      if (*s++ != tolower(*t++)) {
        return false;
      }
    }
    return true;
  }

  bool set_id(const Ioss::GroupingEntity *entity, Ioexnl::EntityIdSet *idset)
  {
    // See description of 'get_id' function.  This function just primes
    // the idset with existing ids so that when we start generating ids,
    // we don't overwrite an existing one.

    // Avoid a few string constructors/destructors
    static std::string id_prop("id");

    bool succeed = false;
    if (entity->property_exists(id_prop)) {
      int64_t id = entity->get_property(id_prop).get_int();

      // See whether it already exists...
      auto type = map_exodus_type(entity->type());
      succeed   = idset->insert(std::make_pair(static_cast<int>(type), id)).second;
      if (!succeed) {
        // Need to remove the property so it doesn't cause problems
        // later...
        auto *new_entity = const_cast<Ioss::GroupingEntity *>(entity);
        new_entity->property_erase(id_prop);
        SMART_ASSERT(!entity->property_exists(id_prop))(id_prop);
      }
    }
    return succeed;
  }

  // Potentially extract the id from a name possibly of the form name_id.
  // If not of this form, return 0;
  int64_t extract_id(const std::string &name_id)
  {
    auto tokens = Ioss::tokenize(name_id, "_");

    if (tokens.size() == 1) {
      return 0;
    }

    // Check whether last token is an integer...
    std::string str_id = tokens.back();
    std::size_t found  = str_id.find_first_not_of("0123456789");
    if (found == std::string::npos) {
      // All digits...
      return std::stoll(str_id);
    }

    return 0;
  }

  int64_t get_id(const Ioss::GroupingEntity *entity, Ioexnl::EntityIdSet *idset)
  {
    // Sierra uses names to refer to grouping entities; however,
    // exodusII requires integer ids.  When reading an exodusII file,
    // the DatabaseIO creates a name by concatenating the entity
    // type (e.g., 'block') and the id separated by an underscore.  For
    // example, an exodusII element block with an id of 100 would be
    // encoded into "block_100"

    // This routine tries to determine the id of the entity using 3
    // approaches:
    //
    // 1. If the entity contains a property named 'id', this is used.
    // The DatabaseIO actually stores the id in the "id" property;
    // however, other grouping entity creators are not required to do
    // this so the property is not guaranteed to exist.
    //
    // 2.If property does not exist, it tries to decode the entity name
    // based on the above encoding.  Again, it is not required that the
    // name follow this convention so success is not guaranteed.
    //
    // 3. If all other schemes fail, the routine picks an id for the entity
    // and returns it.  It also stores this id in the "id" property so an
    // entity will always return the same id for multiple calls.
    // Note that this violates the 'const'ness of the entity so we use
    // a const-cast.

    // Avoid a few string constructors/destructors
    static std::string prop_name("name");
    static std::string id_prop("id");

    int64_t id = 1;

    if (entity->property_exists(id_prop)) {
      id = entity->get_property(id_prop).get_int();
      return id;
    }

    // Try to decode an id from the name.
    std::string name_string = entity->get_property(prop_name).get_string();
    std::string type_name   = entity->short_type_string();
    if (std::strncmp(type_name.c_str(), name_string.c_str(), type_name.size()) == 0) {
      id = extract_id(name_string);
      if (id <= 0) {
        id = 1;
      }
    }

    // At this point, we either have an id equal to '1' or we have an id
    // extracted from the entities name. Increment it until it is
    // unique...
    ex_entity_type type = map_exodus_type(entity->type());
    while (idset->find(std::make_pair(static_cast<int>(type), id)) != idset->end()) {
      ++id;
    }

    // 'id' is a unique id for this entity type...
    idset->insert(std::make_pair(static_cast<int>(type), id));
    auto *new_entity = const_cast<Ioss::GroupingEntity *>(entity);
    new_entity->property_add(Ioss::Property(id_prop, id));
    new_entity->property_update("guid", entity->get_database()->util().generate_guid(id));
    return id;
  }

  bool find_displacement_field(Ioss::NameList &fields, const Ioss::GroupingEntity *block, int ndim,
                               std::string *disp_name)
  {
    // This is a kluge to work with many of the SEACAS codes.  The
    // convention used (in Blot and others) is that the first 'ndim'
    // nodal variables are assumed to be displacements *if* the first
    // character of the names is 'D' and the last characters match the
    // coordinate labels (typically 'X', 'Y', and 'Z').  This routine
    // looks for the field that has the longest match with the string
    // "displacement" and is of the correct storage type (VECTOR_2D or
    // VECTOR_3D).  If found, it returns the name.
    //

    static const std::string displace = "displacement";

    size_t max_span = 0;
    for (const auto &name : fields) {
      std::string lc_name(name);

      Ioss::Utils::fixup_name(lc_name);
      size_t span = match(lc_name, displace);
      if (span > max_span) {
        const Ioss::VariableType *var_type   = block->get_field(name).transformed_storage();
        int                       comp_count = var_type->component_count();
        if (comp_count == ndim) {
          max_span   = span;
          *disp_name = name;
        }
      }
    }
    return max_span > 0;
  }

  void fix_bad_name(char *name)
  {
    SMART_ASSERT(name != nullptr);

    size_t len = std::strlen(name);
    for (size_t i = 0; i < len; i++) {
      if (name[i] < 32 || name[i] > 126) {
        // Zero out entire name if a bad character found anywhere in the name.
        for (size_t j = 0; j < len; j++) {
          name[j] = '\0';
        }
        return;
      }
    }
  }

  void exodus_error(int exoid, int lineno, const char *function, const char *filename)
  {
    std::string empty{};
    exodus_error(exoid, lineno, function, filename, empty);
  }

  void exodus_error(int exoid, int lineno, const char *function, const char *filename,
                    const std::string &extra)
  {
    std::ostringstream errmsg;
    // Create errmsg here so that the exerrval doesn't get cleared by
    // the ex_close call.
    int status;
    ex_get_err(nullptr, nullptr, &status);
    fmt::print(errmsg, "Exodus error ({}) {} at line {} of file '{}' in function '{}'.", status,
               ex_strerror(status), lineno, filename, function);

    if (!extra.empty()) {
      fmt::print(errmsg, " {}", extra);
    }
    fmt::print(errmsg, " Please report to gdsjaar@sandia.gov if you need help.");

    ex_err_fn(exoid, nullptr, nullptr, EX_PRTLASTMSG);
    IOSS_ERROR(errmsg);
  }

  void write_coordinate_frames(int exoid, const Ioss::CoordinateFrameContainer &frames)
  {
    if ((ex_int64_status(exoid) & EX_BULK_INT64_API) != 0) {
      internal_write_coordinate_frames(exoid, frames, static_cast<int64_t>(0));
    }
    else {
      internal_write_coordinate_frames(exoid, frames, 0);
    }
  }

  bool filter_node_list(Ioss::Int64Vector                &nodes,
                        const std::vector<unsigned char> &node_connectivity_status)
  {
    // Iterate through 'nodes' and determine which of the nodes are
    // not connected to any non-omitted blocks. The index of these
    // nodes is then put in the 'nodes' list.
    // Assumes that there is at least one omitted element block.  The
    // 'nodes' list on entry contains 1-based local node ids, not global.
    // On return, the nodes list contains indices.  To filter a nodeset list:
    // for (size_t i = 0; i < nodes.size(); i++) {
    //    active_values[i] = some_nset_values[nodes[i]];
    // }

    size_t orig_size = nodes.size();
    size_t active    = 0;
    for (size_t i = 0; i < orig_size; i++) {
      if (node_connectivity_status[nodes[i] - 1] >= 2) {
        // Node is connected to at least 1 active element...
        nodes[active++] = i;
      }
    }
    nodes.resize(active);
    nodes.shrink_to_fit(); // shrink to fit
    return active != orig_size;
  }

  void filter_element_list(Ioss::Region *region, Ioss::Int64Vector &elements,
                           Ioss::Int64Vector &sides, bool remove_omitted_elements)
  {
    // Iterate through 'elements' and remove the elements which are in an omitted block.
    // Precondition is that there is at least one omitted element block.
    // The 'elements' list contains local element ids, not global.
    // Since there are typically a small number of omitted blocks, do
    // the following:
    // For each omitted block, determine the min and max element id in
    // that block.  Iterate 'elements' vector and set the id to zero if
    // min <= id <= max.  Once all omitted blocks have been processed,
    // then iterate the vector and compress out all zeros.  Keep 'sides'
    // array consistent.

    // Get all element blocks in region...
    bool                               omitted        = false;
    const Ioss::ElementBlockContainer &element_blocks = region->get_element_blocks();
    for (const auto &block : element_blocks) {

      if (Ioss::Utils::block_is_omitted(block)) {
        int64_t min_id = block->get_offset() + 1;
        int64_t max_id = min_id + block->entity_count() - 1;
        for (size_t i = 0; i < elements.size(); i++) {
          if (min_id <= elements[i] && elements[i] <= max_id) {
            omitted     = true;
            elements[i] = 0;
            sides[i]    = 0;
          }
        }
      }
    }
    if (remove_omitted_elements && omitted) {
      elements.erase(std::remove(elements.begin(), elements.end(), 0), elements.end());
      sides.erase(std::remove(sides.begin(), sides.end(), 0), sides.end());
    }
  }

  void separate_surface_element_sides(Ioss::Int64Vector &element, Ioss::Int64Vector &sides,
                                      Ioss::Region *region, Ioexnl::TopologyMap &topo_map,
                                      Ioexnl::TopologyMap   &side_map,
                                      Ioss::SurfaceSplitType split_type,
                                      const std::string     &surface_name)
  {
    if (!element.empty()) {
      Ioss::ElementBlock *block = nullptr;
      // Topology of sides in current element block
      const Ioss::ElementTopology *common_ftopo = nullptr;
      const Ioss::ElementTopology *topo         = nullptr; // Topology of current side
      int64_t                      current_side = -1;

      for (size_t iel = 0; iel < element.size(); iel++) {
        int64_t elem_id = element[iel];
        if (elem_id <= 0) {
          std::ostringstream errmsg;
          fmt::print(errmsg,
                     "ERROR: In sideset/surface '{}' an element with id {} is specified.  Element "
                     "ids must be greater than zero. ({})",
                     surface_name, elem_id, __func__);
          IOSS_ERROR(errmsg);
        }
        if (block == nullptr || !block->contains(elem_id)) {
          block = region->get_element_block(elem_id);
          SMART_ASSERT(block != nullptr);
          SMART_ASSERT(!Ioss::Utils::block_is_omitted(block)); // Filtered out above.

          // nullptr if hetero sides on element
          common_ftopo = block->topology()->boundary_type(0);
          if (common_ftopo != nullptr) {
            topo = common_ftopo;
          }
          current_side = -1;
        }

        if (common_ftopo == nullptr && sides[iel] != current_side) {
          current_side = sides[iel];
          if (current_side <= 0 || current_side > block->topology()->number_boundaries()) {
            std::ostringstream errmsg;
            fmt::print(
                errmsg,
                "ERROR: In sideset/surface '{}' for the element with id {} of topology '{}';\n\t"
                "an invalid face index '{}' is specified.\n\tFace indices "
                "must be between 1 and {}. ({})",
                surface_name, fmt::group_digits(elem_id), block->topology()->name(), current_side,
                block->topology()->number_boundaries(), __func__);
            IOSS_ERROR(errmsg);
          }
          topo = block->topology()->boundary_type(sides[iel]);
          SMART_ASSERT(topo != nullptr);
        }
        std::pair<std::string, const Ioss::ElementTopology *> name_topo;
        if (split_type == Ioss::SPLIT_BY_TOPOLOGIES) {
          name_topo = std::make_pair(block->topology()->name(), topo);
        }
        else if (split_type == Ioss::SPLIT_BY_ELEMENT_BLOCK) {
          name_topo = std::make_pair(block->name(), topo);
        }
        topo_map[name_topo]++;
        if (side_map[name_topo] == 0) {
          side_map[name_topo] = sides[iel];
        }
        else if (side_map[name_topo] != sides[iel]) {
          // Not a consistent side for all sides in this
          // sideset. Set to large number. Note that maximum
          // sides/element is 6, so don't have to worry about
          // a valid element having 999 sides (unless go to
          // arbitrary polyhedra some time...) Using a large
          // number instead of -1 makes it easier to check the
          // parallel consistency...
          side_map[name_topo] = 999;
        }
      }
    }
  }

  void write_reduction_attributes(int exoid, const Ioss::GroupingEntity *ge)
  {
    Ioss::NameList properties = ge->property_describe(Ioss::Property::Origin::ATTRIBUTE);

    auto type = Ioexnl::map_exodus_type(ge->type());
    auto id   = ge->get_optional_property("id", 0);

    double  rval = 0.0;
    int64_t ival = 0;
    for (const auto &property_name : properties) {
      auto prop = ge->get_property(property_name);

      switch (prop.get_type()) {
      case Ioss::Property::BasicType::REAL:
        rval = prop.get_real();
        ex_put_double_attribute(exoid, type, id, property_name.c_str(), 1, &rval);
        break;
      case Ioss::Property::BasicType::INTEGER:
        ival = prop.get_int();
        ex_put_integer_attribute(exoid, type, id, property_name.c_str(), 1, &ival);
        break;
      case Ioss::Property::BasicType::STRING:
        ex_put_text_attribute(exoid, type, id, property_name.c_str(), prop.get_string().c_str());
        break;
      case Ioss::Property::BasicType::VEC_INTEGER:
        ex_put_integer_attribute(exoid, type, id, property_name.c_str(), prop.get_vec_int().size(),
                                 Data(prop.get_vec_int()));
        break;
      case Ioss::Property::BasicType::VEC_DOUBLE:
        ex_put_double_attribute(exoid, type, id, property_name.c_str(),
                                prop.get_vec_double().size(), Data(prop.get_vec_double()));
        break;
      default:; // Do nothing
      }
    }
  }
} // namespace Ioexnl
