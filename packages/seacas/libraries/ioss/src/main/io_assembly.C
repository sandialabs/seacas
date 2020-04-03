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

#include "info_interface.h"

#include <Ionit_Initializer.h>
#include <Ioss_CodeTypes.h>
#include <Ioss_Getline.h>
#include <Ioss_SurfaceSplit.h>
#include <Ioss_Utils.h>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <string>
#include <utility>
#include <vector>
#if defined(SEACAS_HAVE_EXODUS)
#include <exodusII.h>
#endif

#include <Ioss_Assembly.h>
#include <Ioss_Blob.h>
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
#include <Ioss_GroupingEntity.h>
#include <Ioss_IOFactory.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_NodeSet.h>
#include <Ioss_Property.h>
#include <Ioss_Region.h>
#include <Ioss_SideBlock.h>
#include <Ioss_SideSet.h>
#include <Ioss_StructuredBlock.h>
#include <Ioss_VariableType.h>
#include <tokenize.h>

#include <cassert>

#include <fmt/format.h>
#include <fmt/ostream.h>
#if defined(SEACAS_HAVE_CGNS)
#include <cgnslib.h>
#endif

// ========================================================================

namespace {
  std::string codename;
  std::string version = "1.05";

  // Data space shared by most field input/output routines...
  std::vector<char> data;

  void handle_help(const std::string &tokens);
  void handle_list(const std::vector<std::string> &tokens, const Ioss::Region &region);
  bool handle_assm(const std::vector<std::string> &tokens, Ioss::Region &region);

  void set_db_properties(const Info::Interface &interFace, Ioss::DatabaseIO *dbi);

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

  int64_t id(Ioss::GroupingEntity *entity)
  {
    int64_t id = -1;
    if (entity->property_exists("id")) {
      id = entity->get_property("id").get_int();
    }
    return id;
  }

  Ioss::PropertyManager set_properties(const Info::Interface &interFace)
  {
    Ioss::PropertyManager properties{};
    if (!interFace.decomp_method().empty()) {
      properties.add(Ioss::Property("DECOMPOSITION_METHOD", interFace.decomp_method()));
    }
    return properties;
  }
} // namespace

int main(int argc, char *argv[])
{
#ifdef SEACAS_HAVE_MPI
  MPI_Init(&argc, &argv);
  ON_BLOCK_EXIT(MPI_Finalize);
#endif

  Info::Interface interFace;
  interFace.parse_options(argc, argv);

  Ioss::Init::Initializer io;

  if (interFace.show_config()) {
    Ioss::IOFactory::show_configuration();
    exit(EXIT_SUCCESS);
  }

  codename   = argv[0];
  size_t ind = codename.find_last_of('/', codename.size());
  if (ind != std::string::npos) {
    codename = codename.substr(ind + 1, codename.size());
  }

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

  bool changed = false;
  while (1) {
    const char *input = getline_int("\nCOMMAND> ");
    if (input[0] == '\0') {
      break;
    }
    if (input) {
      gl_histadd(input);
    }

    // NOTE: getline_int returns the trailing '\n'
    auto tokens = Ioss::tokenize(std::string(input), " ,\n");
    if (tokens.empty()) {
      continue;
    }
    if (Ioss::Utils::substr_equal(tokens[0], "exit")) {
      break;
    }
    if (Ioss::Utils::str_equal(tokens[0], "help")) {
      handle_help(tokens.back());
    }
    else if (Ioss::Utils::substr_equal(tokens[0], "list")) {
      handle_list(tokens, region);
    }
    else if (Ioss::Utils::substr_equal(tokens[0], "assembly")) {
      changed |= handle_assm(tokens, region);
    }
  }

  if (changed) {
    region.end_mode(Ioss::STATE_DEFINE_MODEL);
  }
  if (changed) {
    fmt::print("\nDatabase changed. Updating assembly definitions.\n");
  }
  else {
    fmt::print("\nDatabase unchanged. No update required.\n");
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
      fmt::print("\n\tThere are no structured blocks in this model.\n");
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
      fmt::print("\n\tThere are no assemblies in this model.\n");
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
      fmt::print("\n\tThere are no blobs in this model.\n");
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
      fmt::print("\n\tThere are no element blocks in this model.\n");
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
      fmt::print("\n\tThere are no side sets in this model.\n");
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
      fmt::print("\n\tThere are no node sets in this model.\n");
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

  void set_db_properties(const Info::Interface &interFace, Ioss::DatabaseIO *dbi)
  {
    std::string inpfile = interFace.filename();

    if (dbi == nullptr || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    if (interFace.use_generic_names()) {
      dbi->set_use_generic_canonical_name(true);
    }

    dbi->set_surface_split_type(Ioss::int_to_surface_split(interFace.surface_split_scheme()));
  }

  void handle_help(const std::string &topic)
  {
    bool all = Ioss::Utils::substr_equal(topic, "help");
    if (all) {
      fmt::print("\n\tHELP [list | assembly]\n");
    }
    if (all || Ioss::Utils::substr_equal(topic, "list")) {
      fmt::print("\n\tLIST summary|element block|assembly|nodeset|sideset|blobs\n");
    }
    if (all || Ioss::Utils::substr_equal(topic, "assembly")) {
      fmt::print(
          "\tFor all commands, if an assembly named `name` does not exist, it will be created.\n");
      fmt::print("\n\tASSEMBLY {{name}}\n");
      fmt::print("\t\tCreates an empty assembly named `name` if it does not exist.\n");

      fmt::print("\n\tASSEMBLY {{name}} ADD {{name1}} {{name2}} ... {{namen}}\n");
      fmt::print("\t\tAdds the specified entities to the assembly.  All entities must be the same "
                 "type.\n");

      fmt::print("\n\tASSEMBLY {{name}} ADD {{type}} MATCH {{regex}}\n");
      fmt::print("\t\tAdds the entities of the specified type to the assembly.\n"
                 "\t\tAll entities whose name matches the {{regex}} will be added.\n");

      fmt::print("\n\tASSEMBLY {{name}} ADD {{type}} ID {{id}} TO {{id}} BY {{step}}\n");
      fmt::print("\t\tAdds the entities of the specified type to the assembly.\n"
                 "\t\tAll entities whose id matches the specified range will be added.\n");

      fmt::print("\n\tASSEMBLY {{name}} ADD {{type}} ID {{id}}, {{id2}}, ..., {{idn}}\n");
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
  bool handle_assm(const std::vector<std::string> &tokens, Ioss::Region &region)
  {
    bool            changed = false;
    Ioss::Assembly *assem   = nullptr;

    if (tokens.size() > 1) {
      assem = region.get_assembly(tokens[1]);
      if (assem == nullptr) {
        // New assembly...
        assem = new Ioss::Assembly(region.get_database(), tokens[1]);
        region.add(assem);
        changed = true;
      }
    }
    else {
      handle_help("assembly");
      return false;
    }

    if (assem == nullptr) {
      fmt::print(stderr, "ERROR: Unable to create or access assembly '{}'\n", tokens[1]);
      return false;
    }

    try {
      if (tokens.size() > 2) {
        fmt::print("{}\n", fmt::join(tokens.begin() + 3, tokens.end(), ", "));
        for (size_t i = 3; i < tokens.size(); i++) {
          auto *member = region.get_entity(tokens[i]);
          if (member != nullptr) {
            assem->add(member);
            changed = true;
            fmt::print("Added {}\n", tokens[i]);
          }
        }
      }
    }
    catch (const std::exception &x) {
      fmt::print("{}\n", x.what());
    }

    return changed;
  }
} // nameSpace
