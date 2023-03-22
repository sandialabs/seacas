// Copyright(C) 1999-2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_Assembly.h>
#include <Ioss_ElementTopology.h>
#include <Ioss_Region.h>
#include <Ioss_SmartAssert.h>
#include <Ioss_Utils.h>
#include <Ioss_VariableType.h>
#include <algorithm>
#include <cstring>
#include <fmt/ostream.h>
#include <null/Ionull_Utils.h>
#include <tokenize.h>

namespace {
  size_t match(const char *name1, const char *name2)
  {
    size_t l1  = std::strlen(name1);
    size_t l2  = std::strlen(name2);
    size_t len = l1 < l2 ? l1 : l2;
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

  void internal_write_coordinate_frames(int exoid, const Ioss::CoordinateFrameContainer &frames)
  {
    // Query number of coordinate frames...
    int nframes = static_cast<int>(frames.size());
    if (nframes > 0) {
      std::vector<char>   tags(nframes);
      std::vector<double> coordinates(nframes * 9);
      std::vector<int>    ids(nframes);

      for (size_t i = 0; i < frames.size(); i++) {
        ids[i]              = frames[i].id();
        tags[i]             = frames[i].tag();
        const double *coord = frames[i].coordinates();
        for (size_t j = 0; j < 9; j++) {
          coordinates[9 * i + j] = coord[j];
        }
      }
    }
  }
} // namespace

namespace Ionull {
  void update_last_time_attribute(int nullFilePtr, double value) {}

  bool type_match(const std::string &type, const char *substring)
  {
    // Returns true if 'substring' is a sub-string of 'type'.
    // The comparisons are case-insensitive
    // 'substring' is required to be in all lowercase.
    const char *s = substring;
    const char *t = type.c_str();

    SMART_ASSERT(s != nullptr && t != nullptr);
    while (*s != '\0' && *t != '\0') {
      if (*s++ != tolower(*t++)) {
        return false;
      }
    }
    return true;
  }

  void decode_surface_name(Ionull::SideSetMap &fs_map, Ionull::SideSetSet &fs_set,
                           const std::string &name)
  {
    auto tokens = Ioss::tokenize(name, "_");
    if (tokens.size() >= 4) {
      // Name of form: "name_eltopo_sidetopo_id" or
      // "name_block_id_sidetopo_id" "name" is typically "surface".
      // The sideset containing this should then be called "name_id"

      // Check whether the second-last token is a side topology and
      // the third-last token is an element topology.
      const Ioss::ElementTopology *side_topo =
          Ioss::ElementTopology::factory(tokens[tokens.size() - 2], true);
      if (side_topo != nullptr) {
        const Ioss::ElementTopology *element_topo =
            Ioss::ElementTopology::factory(tokens[tokens.size() - 3], true);
        if (element_topo != nullptr || tokens[tokens.size() - 4] == "block") {
          // The remainder of the tokens will be used to create
          // a side set name and then this sideset will be
          // a side block in that set.
          std::string fs_name;
          size_t      last_token = tokens.size() - 3;
          if (element_topo == nullptr) {
            last_token--;
          }
          for (size_t tok = 0; tok < last_token; tok++) {
            fs_name += tokens[tok];
          }
          fs_name += "_";
          fs_name += tokens[tokens.size() - 1]; // Add on the id.

          fs_set.insert(fs_name);
          fs_map.insert(Ionull::SideSetMap::value_type(name, fs_name));
        }
      }
    }
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

    static char displace[] = "displacement";

    size_t max_span = 0;
    for (const auto &name : fields) {
      std::string lc_name(name);

      Ioss::Utils::fixup_name(lc_name);
      size_t span = match(lc_name.c_str(), displace);
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

  void throw_error(int exoid, int lineno, const char *function, const char *filename)
  {
    std::string empty{};
    throw_error(exoid, lineno, function, filename, empty);
  }

  void throw_error(int exoid, int lineno, const char *function, const char *filename,
                   const std::string &extra)
  {
    std::ostringstream errmsg;
    int                status;
    fmt::print(errmsg, "NullDB error ({}) at line {} of file '{}' in function '{}'.", status,
               lineno, filename, function);

    if (!extra.empty()) {
      fmt::print(errmsg, " {}", extra);
    }
    fmt::print(errmsg, " Please report to gdsjaar@sandia.gov if you need help.");
    IOSS_ERROR(errmsg);
  }

  void write_coordinate_frames(int exoid, const Ioss::CoordinateFrameContainer &frames)
  {
    internal_write_coordinate_frames(exoid, frames);
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
    return (active != orig_size);
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

  void write_reduction_attributes(int /*exoid*/, const Ioss::GroupingEntity * /*ge*/) {}
} // namespace Ionull
