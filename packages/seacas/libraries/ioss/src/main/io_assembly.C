// Copyright(C) 1999-2017, 2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
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
//
//     * Neither the name of NTESS nor the names of its
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

#include "assembly_interface.h"

#include <cassert>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <regex>
#include <string>
#include <unistd.h>
#include <utility>
#include <vector>
#if defined(SEACAS_HAVE_EXODUS)
#include <exodusII.h>
#endif

#include <Ioex_Internals.h>
#include <Ionit_Initializer.h>
#include <Ioss_Assembly.h>
#include <Ioss_Blob.h>
#include <Ioss_CodeTypes.h>
#include <Ioss_CommSet.h>
#include <Ioss_CoordinateFrame.h>
#include <Ioss_DBUsage.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_EdgeBlock.h>
#include <Ioss_EdgeSet.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_ElementSet.h>
#include <Ioss_ElementTopology.h>
#include <Ioss_FaceBlock.h>
#include <Ioss_FaceSet.h>
#include <Ioss_Field.h>
#include <Ioss_FileInfo.h>
#include <Ioss_Getline.h>
#include <Ioss_GroupingEntity.h>
#include <Ioss_IOFactory.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_NodeSet.h>
#include <Ioss_Property.h>
#include <Ioss_Region.h>
#include <Ioss_SideBlock.h>
#include <Ioss_SideSet.h>
#include <Ioss_StructuredBlock.h>
#include <Ioss_SurfaceSplit.h>
#include <Ioss_Utils.h>
#include <Ioss_VariableType.h>
#include <tokenize.h>

#include <fmt/format.h>
#include <fmt/ostream.h>
#if defined(SEACAS_HAVE_CGNS)
#include <cgnslib.h>
#endif

// ========================================================================

namespace {
  std::string codename;
  std::string version = "0.5";

  Ioss::EntityType get_entity_type(const std::string &type)
  {
    if (Ioss::Utils::substr_equal(type, "elementblock")) {
      return Ioss::ELEMENTBLOCK;
    }
    else if (Ioss::Utils::substr_equal(type, "block")) {
      return Ioss::ELEMENTBLOCK;
    }
    else if (Ioss::Utils::substr_equal(type, "nodeset")) {
      return Ioss::NODESET;
    }
    else if (Ioss::Utils::substr_equal(type, "nodelist")) {
      return Ioss::NODESET;
    }
    else if (Ioss::Utils::substr_equal(type, "sideset")) {
      return Ioss::SIDESET;
    }
    else if (Ioss::Utils::substr_equal(type, "surface")) {
      return Ioss::SIDESET;
    }
    else if (Ioss::Utils::substr_equal(type, "assembly")) {
      return Ioss::ASSEMBLY;
    }
    else if (Ioss::Utils::substr_equal(type, "blob")) {
      return Ioss::BLOB;
    }
    return Ioss::INVALID_TYPE;
  }

  Ioss::NameList get_name_list(const Ioss::Region &region, Ioss::EntityType type);
  void           handle_help(const std::string &tokens);
  void           handle_list(const std::vector<std::string> &tokens, const Ioss::Region &region);
  bool handle_assm(const std::vector<std::string> &tokens, Ioss::Region &region, bool allow_modify);
  void update_assembly_info(Ioss::Region &region, const Assembly::Interface &interFace);

  void set_db_properties(const Assembly::Interface &interFace, Ioss::DatabaseIO *dbi);

  void info_elementblock(const Ioss::Region &region);
#if 0
  void info_structuredblock(const Ioss::Region &region);
  void info_aliases(const Ioss::Region &region, const Ioss::GroupingEntity *ige, bool nl_pre,
                    bool nl_post);
#endif
  void info_nodesets(const Ioss::Region &region);

  void info_sidesets(const Ioss::Region &region);
  void info_assemblies(const Ioss::Region &region);
  void info_blobs(const Ioss::Region &region);

  std::string name(const Ioss::GroupingEntity *entity)
  {
    return entity->type_string() + " '" + entity->name() + "'";
  }

  int64_t id(const Ioss::GroupingEntity *entity)
  {
    int64_t id = -1;
    if (entity->property_exists("id")) {
      id = entity->get_property("id").get_int();
    }
    return id;
  }

  int64_t get_next_assembly_id(const Ioss::Region &region)
  {
    static int64_t next_id = 0;
    if (next_id == 0) {
      const auto &assemblies = region.get_assemblies();
      for (const auto *assembly : assemblies) {
        auto my_id = id(assembly);
        next_id    = std::max(next_id, my_id);
      }
      next_id = (next_id / 100);
    }
    ++next_id;
    return next_id * 100;
  }

  Ioss::PropertyManager set_properties(const Assembly::Interface &interFace)
  {
    Ioss::PropertyManager properties{};
    return properties;
  }
} // namespace

int main(int argc, char *argv[])
{
#ifdef SEACAS_HAVE_MPI
  MPI_Init(&argc, &argv);
  ON_BLOCK_EXIT(MPI_Finalize);
#endif

  codename   = argv[0];
  size_t ind = codename.find_last_of('/', codename.size());
  if (ind != std::string::npos) {
    codename = codename.substr(ind + 1, codename.size());
  }

  fmt::print("\n *** {}, Version {}\n", codename, version);
  Assembly::Interface interFace;
  interFace.parse_options(argc, argv);

  Ioss::Init::Initializer io;

  std::string inpfile    = interFace.filename();
  std::string input_type = interFace.type();

  //========================================================================
  // INPUT ...
  // NOTE: The "READ_RESTART" mode ensures that the node and element ids will be mapped.
  //========================================================================
  Ioss::PropertyManager properties = set_properties(interFace);
  properties.add(Ioss::Property("APPEND_OUTPUT", Ioss::DB_APPEND));

  Ioss::DatabaseIO *dbi = Ioss::IOFactory::create(input_type, inpfile, Ioss::WRITE_RESTART,
                                                  (MPI_Comm)MPI_COMM_WORLD, properties);
  if (dbi == nullptr || !dbi->ok(true)) {
    std::exit(EXIT_FAILURE);
  }

  set_db_properties(interFace, dbi);

  // NOTE: 'region' owns 'db' pointer at this time...
  Ioss::Region region(dbi, "region_1");
  region.begin_mode(Ioss::STATE_DEFINE_MODEL);

  region.output_summary(std::cout, true);

  bool from_term = (isatty(0) != 0 && isatty(1) != 0);
  bool changed   = false;
  while (1) {
    std::string input;
    if (from_term) {
      const char *cinput = getline_int("\nCOMMAND> ");
      if (input[0] == '\0') {
        break;
      }
      if (cinput) {
        gl_histadd(cinput);
      }
      input = cinput;
    }
    else {
      std::getline(std::cin, input);
    }

    // NOTE: getline_int returns the trailing '\n'
    auto tokens = Ioss::tokenize(std::string(input), " ,\n");
    if (tokens.empty()) {
      continue;
    }
    if (Ioss::Utils::substr_equal(tokens[0], "exit") ||
        Ioss::Utils::substr_equal(tokens[0], "end")) {
      break;
    }
    if (Ioss::Utils::substr_equal(tokens[0], "quit")) {
      changed = false;
      break;
    }
    if (Ioss::Utils::str_equal(tokens[0], "help")) {
      handle_help(tokens.back());
    }
    else if (Ioss::Utils::substr_equal(tokens[0], "list")) {
      handle_list(tokens, region);
    }
    else if (Ioss::Utils::substr_equal(tokens[0], "assembly")) {
      bool allow_modify = interFace.modify_existing_assembly();
      changed |= handle_assm(tokens, region, allow_modify);
    }
  }

  if (changed) {
    update_assembly_info(region, interFace);
  }
  else {
    fmt::print("\n\t*** Database unchanged. No update required.\n");
  }
  fmt::print("\n{} execution successful.\n", codename);
  return EXIT_SUCCESS;
}

namespace {
#if 0
  void info_structuredblock(const Ioss::Region &region)
  {
    bool                                  parallel = region.get_database()->is_parallel();
    const Ioss::StructuredBlockContainer &sbs      = region.get_structured_blocks();
    if (sbs.empty()) {
      fmt::print("\n\t*** There are no structured blocks in this model.\n");
      return;
    }
    for (auto sb : sbs) {
      int64_t num_cell = sb->get_property("cell_count").get_int();
      int64_t num_node = sb->get_property("node_count").get_int();
      int64_t num_dim  = sb->get_property("component_degree").get_int();

      fmt::print("\n{} {}", name(sb), sb->get_property("ni_global").get_int());
      if (num_dim > 1) {
        fmt::print("x{}", sb->get_property("nj_global").get_int());
      }
      if (num_dim > 2) {
        fmt::print("x{}", sb->get_property("nk_global").get_int());
      }

      if (parallel) {
        fmt::print(" [{}x{}x{}, Offset = {}, {}, {}] ", sb->get_property("ni").get_int(),
                   sb->get_property("nj").get_int(), sb->get_property("nk").get_int(),
                   sb->get_property("offset_i").get_int(), sb->get_property("offset_j").get_int(),
                   sb->get_property("offset_k").get_int());
      }
      fmt::print("{:14n} cells, {:14n} nodes\n", num_cell, num_node);

      if (!sb->m_zoneConnectivity.empty()) {
        fmt::print("\tConnectivity with other blocks:\n");
        for (const auto &zgc : sb->m_zoneConnectivity) {
          fmt::print("{}\n", zgc);
        }
      }
      if (!sb->m_boundaryConditions.empty()) {
        fmt::print("\tBoundary Conditions:\n");
        for (const auto &bc : sb->m_boundaryConditions) {
          fmt::print("{}\n", bc);
        }
      }
    }
  }
#endif

  void info_assemblies(const Ioss::Region &region)
  {
    const auto &assem = region.get_assemblies();
    if (assem.empty()) {
      fmt::print("\n\t*** There are no assemblies in this model.\n");
      return;
    }
    for (auto as : assem) {
      fmt::print("\n{} id: {:6d}, contains: {} member(s) of type {:>10s}.\n\tMembers: ", name(as),
                 id(as), as->member_count(), as->contains_string());
      for (const auto mem : as->get_members()) {
        fmt::print("'{}' ", mem->name());
      }
      fmt::print("\n");
    }
  }

  void info_blobs(const Ioss::Region &region)
  {
    const auto &blobs = region.get_blobs();
    if (blobs.empty()) {
      fmt::print("\n\t*** There are no blobs in this model.\n");
      return;
    }
    for (auto blob : blobs) {
      fmt::print("\n{} id: {:6d}, contains: {} item(s).\n", name(blob), id(blob),
                 blob->entity_count());
    }
  }

  void info_elementblock(const Ioss::Region &region)
  {
    const Ioss::ElementBlockContainer &ebs = region.get_element_blocks();
    if (ebs.empty()) {
      fmt::print("\n\t*** There are no element blocks in this model.\n");
      return;
    }
    for (auto eb : ebs) {
      int64_t num_elem = eb->entity_count();

      std::string type       = eb->get_property("topology_type").get_string();
      int64_t     num_attrib = eb->get_property("attribute_count").get_int();
      fmt::print("\n{} id: {:6d}, topology: {:>10s}, {:14n} elements, {:3d} attributes.\n",
                 name(eb), id(eb), type, num_elem, num_attrib);
    }
  }

  void info_sidesets(const Ioss::Region &region)
  {
    const Ioss::SideSetContainer &fss = region.get_sidesets();
    if (fss.empty()) {
      fmt::print("\n\t*** There are no side sets in this model.\n");
      return;
    }
    for (auto fs : fss) {
      fmt::print("\n{} id: {:6d}", name(fs), id(fs));
      if (fs->property_exists("bc_type")) {
#if defined(SEACAS_HAVE_CGNS)
        auto bc_type = fs->get_property("bc_type").get_int();
        fmt::print(", boundary condition type: {} ({})\n", BCTypeName[bc_type], bc_type);
#else
        fmt::print(", boundary condition type: {}\n", fs->get_property("bc_type").get_int());
#endif
      }
      const Ioss::SideBlockContainer &fbs = fs->get_side_blocks();
      for (auto fb : fbs) {
        int64_t count      = fb->entity_count();
        int64_t num_attrib = fb->get_property("attribute_count").get_int();
        int64_t num_dist   = fb->get_property("distribution_factor_count").get_int();
        fmt::print("\t{}, {:8n} sides, {:3d} attributes, {:8n} distribution factors.\n", name(fb),
                   count, num_attrib, num_dist);
      }
    }
  }

  void info_nodesets(const Ioss::Region &region)
  {
    const Ioss::NodeSetContainer &nss = region.get_nodesets();
    if (nss.empty()) {
      fmt::print("\n\t*** There are no node sets in this model.\n");
      return;
    }
    for (auto ns : nss) {
      int64_t count      = ns->entity_count();
      int64_t num_attrib = ns->get_property("attribute_count").get_int();
      int64_t num_dist   = ns->get_property("distribution_factor_count").get_int();
      fmt::print("\n{} id: {:6d}, {:8n} nodes, {:3d} attributes, {:8n} distribution factors.\n",
                 name(ns), id(ns), count, num_attrib, num_dist);
    }
  }

#if 0
  void info_aliases(const Ioss::Region &region, const Ioss::GroupingEntity *ige, bool nl_pre,
                    bool nl_post)
  {
    std::vector<std::string> aliases;
    if (region.get_aliases(ige->name(), aliases) > 0) {
      if (nl_pre) {
        fmt::print("\n");
      }
      fmt::print("\tAliases: ");
      for (size_t i = 0; i < aliases.size(); i++) {
        if (aliases[i] != ige->name()) {
          if (i > 0) {
            fmt::print(", ");
          }
          fmt::print("{}", aliases[i]);
        }
      }
      if (nl_post) {
        fmt::print("\n");
      }
    }
  }
#endif

  void set_db_properties(const Assembly::Interface &interFace, Ioss::DatabaseIO *dbi)
  {
    std::string inpfile = interFace.filename();

    if (dbi == nullptr || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }
  }

  void handle_help(const std::string &topic)
  {
    bool all = Ioss::Utils::substr_equal(topic, "help");
    if (all) {
      fmt::print("\n\tHELP [list | assembly]\n");
      fmt::print("\n\tEND | EXIT\n");
      fmt::print("\t\tEnd command input and output changed assembly definitions (if any).\n");
      fmt::print("\n\tQUIT\n");
      fmt::print("\t\tEnd command input and exit with no changes to database.\n");
    }
    if (all || Ioss::Utils::substr_equal(topic, "list")) {
      fmt::print("\n\tLIST summary|element block|assembly|nodeset|sideset|blobs\n\n");
    }
    if (all || Ioss::Utils::substr_equal(topic, "assembly")) {
      fmt::print("\n\tFor all commands, if an assembly named `name` does not exist, it will be "
                 "created.\n");
      fmt::print("\tASSEMBLY {{name}}\n");
      fmt::print("\t\tCreates an empty assembly named `name` if it does not exist.\n");

      fmt::print("\n\tASSEMBLY {{name}} ADD {{name1}} {{name2}} ... {{namen}}\n");
      fmt::print("\t\tAdds the specified entities to the assembly.  All entities must be the same "
                 "type.\n");

      fmt::print("\n\tASSEMBLY {{name}} REMOVE {{name1}} {{name2}} ... {{namen}}\n");
      fmt::print("\t\tRemoves the specified entities from the assembly.\n");

      fmt::print("\n\tASSEMBLY {{name}} TYPE {{type}} MATCHES {{regex}}\n");
      fmt::print("\t\tAdds the entities of the specified type to the assembly.\n"
                 "\t\tAll entities whose name matches the {{regex}} will be added.\n");

      fmt::print("\n\tASSEMBLY {{name}} TYPE {{type}} NAMED {{list of one or more names}}\n");
      fmt::print("\t\tAdds the entities of the specified type to the assembly.\n"
                 "\t\tAll entities whose names are listed will be added.\n");

      fmt::print("\n\tASSEMBLY {{name}} TYPE {{type}} RANGE {{id}} TO {{id}} BY {{step}}\n");
      fmt::print("\t\tAdds the entities of the specified type to the assembly.\n"
                 "\t\tAll entities whose id matches the specified range will be added.\n"
                 "\t\tNo message will be output for ids not matching an entity.\n");

      fmt::print("\n\tASSEMBLY {{name}} TYPE {{type}} IDS {{id}}, {{id2}}, ..., {{idn}}\n");
      fmt::print(
          "\t\tAdds the entities of the specified type to the assembly.\n"
          "\t\tAll entities whose id matches an id in the list will be added.\n"
          "\t\tA warning message will be output if there is no entity with the requested id.\n");
    }
  }

  void handle_list(const std::vector<std::string> &tokens, const Ioss::Region &region)
  {
    if (tokens.size() > 1) {
      if (Ioss::Utils::substr_equal(tokens[1], "summary")) {
        region.output_summary(std::cout);
      }
      else if (Ioss::Utils::substr_equal(tokens[1], "element") ||
               Ioss::Utils::substr_equal(tokens[1], "block")) {
        info_elementblock(region);
      }
      else if (Ioss::Utils::substr_equal(tokens[1], "assembly")) {
        info_assemblies(region);
      }
      else if (Ioss::Utils::substr_equal(tokens[1], "nodeset")) {
        info_nodesets(region);
      }
      else if (Ioss::Utils::substr_equal(tokens[1], "sideset")) {
        info_sidesets(region);
      }
      else if (Ioss::Utils::substr_equal(tokens[1], "blobs")) {
        info_blobs(region);
      }
      else {
        fmt::print(stderr, "\tWARNING: Unrecognized list option '{}'\n", tokens[1]);
        handle_help("list");
      }
    }
    else {
      handle_help("list");
    }
  }
  bool handle_assm(const std::vector<std::string> &tokens, Ioss::Region &region, bool allow_modify)
  {
    bool            changed = false;
    Ioss::Assembly *assem   = nullptr;

    if (tokens.size() > 1) {
      assem = region.get_assembly(tokens[1]);
      if (assem == nullptr) {
        // New assembly...
        assem      = new Ioss::Assembly(region.get_database(), tokens[1]);
        auto my_id = get_next_assembly_id(region);
        assem->property_add(Ioss::Property("id", my_id));
        assem->property_add(Ioss::Property("created", 1));
        region.add(assem);
        fmt::print("\t*** Created Assembly '{}' with id {}.\n", tokens[1], my_id);
        // Don't set changed to true; only set if members modified
      }
    }
    else {
      handle_help("assembly");
      return false;
    }

    if (assem == nullptr) {
      fmt::print(stderr, "ERROR: Unable to create or access assembly '{}'.\n", tokens[1]);
      return false;
    }

    bool created = false;
    if (assem->property_exists("created")) {
      created = assem->get_property("created").get_int() == 1;
    }
    if (!allow_modify && !created) {
      fmt::print(stderr,
                 "ERROR: Unable to modify an existing assembly '{}'.  Restart with "
                 "`--allow_modifications` option.\n",
                 tokens[1]);
      return false;
    }

    if (tokens.size() > 2) {
      try {
        if (Ioss::Utils::substr_equal(tokens[2], "add")) {
          // List of names ...
          for (size_t i = 3; i < tokens.size(); i++) {
            auto *member = region.get_entity(tokens[i]);
            if (member != nullptr) {
              if (assem->add(member)) {
                changed = true;
              }
            }
          }
        }
        else if (Ioss::Utils::substr_equal(tokens[2], "remove")) {
          // List of names ...
          for (size_t i = 3; i < tokens.size(); i++) {
            auto *member = region.get_entity(tokens[i]);
            if (member != nullptr) {
              if (assem->remove(member)) {
                changed = true;
              }
            }
          }
        }
        else if (Ioss::Utils::substr_equal(tokens[2], "type")) {
          // Determine type of add...
          Ioss::EntityType type = get_entity_type(tokens[3]);
          if (type == Ioss::INVALID_TYPE) {
            fmt::print(stderr, "ERROR: Unrecognized entity type: '{}'\n", tokens[3]);
            return changed;
          }
          if (Ioss::Utils::substr_equal(tokens[4], "matches")) {
            // regex match on names
            // Get list of all names for this entity type...
            Ioss::NameList names = get_name_list(region, type);

            std::regex reg(tokens[5], std::regex::extended);

            // Check for match against all names in list...
            for (const auto &name : names) {
              if (std::regex_match(name, reg)) {
                const auto *entity = region.get_entity(name, type);
                if (entity != nullptr) {
                  if (assem->add(entity)) {
                    changed = true;
                  }
                }
              }
            }
          }
          else if (Ioss::Utils::substr_equal(tokens[4], "named")) {
            // list of names
            for (size_t i = 5; i < tokens.size(); i++) {
              const auto *entity = region.get_entity(tokens[i], type);
              if (entity != nullptr) {
                if (assem->add(entity)) {
                  changed = true;
                }
              }
            }
          }
          else if (Ioss::Utils::substr_equal(tokens[4], "ids")) {
            // list of ids
            for (size_t i = 5; i < tokens.size(); i++) {
              size_t      id     = std::stod(tokens[i]);
              const auto *entity = region.get_entity(id, type);
              if (entity != nullptr) {
                if (assem->add(entity)) {
                  changed = true;
                }
              }
            }
          }
          else if (Ioss::Utils::substr_equal(tokens[4], "range")) {
            //     0        1     2     3      4      5    6    7    8     9
            // ASSEMBLY {{name}} TYPE {{type}} RANGE {{id}} TO {{id}} BY {{step}}
            size_t begin = std::stod(tokens[5]);
            size_t end   = begin;
            size_t step  = 1;
            if (tokens.size() >= 8 && Ioss::Utils::substr_equal(tokens[6], "to")) {
              end = std::stod(tokens[7]);
            }
            if (tokens.size() >= 10 && Ioss::Utils::substr_equal(tokens[8], "by")) {
              step = std::stod(tokens[9]);
            }
            for (size_t id = begin; id <= end; id += step) {
              const auto *entity = region.get_entity(id, type);
              if (entity != nullptr) {
                if (assem->add(entity)) {
                  changed = true;
                }
              }
            }
          }
        }
        else {
          fmt::print(stderr, "ERROR: Unrecognized assembly option '{}'.\n", tokens[2]);
          return changed;
        }
      }
      catch (const std::exception &x) {
        fmt::print("{}\n", x.what());
      }
    }

    if (changed) {
      assem->property_add(Ioss::Property("changed", true));
    }
    else {
      fmt::print(Ioss::WARNING(), "Command did not modify assembly '{}'\n", assem->name());
    }

    return changed;
  }

  template <typename T> Ioss::NameList get_entity_names(const std::vector<T *> &entity_list)
  {
    Ioss::NameList names;
    names.reserve(entity_list.size());

    for (const auto *entity : entity_list) {
      names.push_back(entity->name());
    }
    return names;
  }

  Ioss::NameList get_name_list(const Ioss::Region &region, Ioss::EntityType type)
  {
    Ioss::NameList names;
    switch (type) {
    case Ioss::ELEMENTBLOCK: {
      const auto &entities = region.get_element_blocks();
      names                = get_entity_names(entities);
    } break;
    case Ioss::NODESET: {
      const auto &entities = region.get_nodesets();
      names                = get_entity_names(entities);
    } break;
    case Ioss::SIDESET: {
      const auto &entities = region.get_sidesets();
      names                = get_entity_names(entities);
    } break;
    case Ioss::ASSEMBLY: {
      const auto &entities = region.get_assemblies();
      names                = get_entity_names(entities);
    } break;
    case Ioss::BLOB: {
      const auto &entities = region.get_blobs();
      names                = get_entity_names(entities);
    } break;
    default: break;
    }
    return names;
  }

  void update_assembly_info(Ioss::Region &region, const Assembly::Interface &interFace)
  {
    // Assembly ids -- maybe set at definition time so can add to other assemblies more easily.
    std::vector<Ioex::Assembly> ex_assemblies;

    bool modify_existing = false;

    region.end_mode(Ioss::STATE_DEFINE_MODEL);
    fmt::print("\n\t*** Database changed. Updating assembly definitions.\n");
    const auto &assemblies = region.get_assemblies();
    for (const auto *assembly : assemblies) {
      if (assembly->property_exists("changed")) {
        ex_assemblies.emplace_back(*assembly);
        if (!assembly->property_exists("created")) {
          fmt::print("\t*** Modifying assembly {}\n", assembly->name());
          modify_existing = true;
        }
        else {
          fmt::print("\t*** Creating assembly {}\n", assembly->name());
        }
      }
    }

    int exoid = region.get_database()->get_file_pointer();
    if (modify_existing) {
      // Need to create a temporary database to copy the database into.
      // Make sure has same int size as current.
      int                   byte_size = region.get_database()->int_byte_size_db();
      Ioss::PropertyManager properties;
      properties.add(Ioss::Property("INTEGER_SIZE_DB", byte_size));
      std::string       out_file  = interFace.filename() + ".mod";
      std::string       file_type = interFace.type();
      Ioss::DatabaseIO *dbo = Ioss::IOFactory::create(file_type, out_file, Ioss::WRITE_RESTART,
                                                      (MPI_Comm)MPI_COMM_WORLD, properties);

      if (dbo == nullptr || !dbo->ok(true)) {
        std::exit(EXIT_FAILURE);
      }
      // NOTE: 'region' owns 'db' pointer at this time...
      Ioss::Region reg_out(dbo, "region_tmp");

      int out_exoid = reg_out.get_database()->get_file_pointer();
      Ioex::Internals::update_assembly_data(out_exoid, ex_assemblies, 1);
      Ioex::Internals::copy_database(exoid, out_exoid);
      Ioex::Internals::update_assembly_data(out_exoid, ex_assemblies, 2);

      // Now, remove old file and replace with new...
      region.get_database()->closeDatabase();
      reg_out.get_database()->closeDatabase();
      Ioss::FileInfo in_file(interFace.filename());
      in_file.remove_file();

      if (std::rename(out_file.c_str(), interFace.filename().c_str()) != 0) {
        fmt::print(stderr, "ERROR: Could not update modified file {} to {}.\n", out_file,
                   interFace.filename());
        return;
      }
    }
    else {
      Ioex::Internals::update_assembly_data(exoid, ex_assemblies);
    }
  }
} // nameSpace
