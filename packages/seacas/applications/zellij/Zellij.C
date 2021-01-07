// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#include <cctype>
#include <cfloat>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <exception>
#include <iterator>
#include <limits>
#include <map>
#include <numeric>
#include <set>
#include <string>
#ifndef _MSC_VER
#include <sys/times.h>
#include <sys/utsname.h>
#endif
#include <unistd.h>
#include <vector>

#include "add_to_log.h"
#include "fmt/ostream.h"

#include <exodusII.h>

#include <Ionit_Initializer.h>
#include <Ioss_SmartAssert.h>
#include <Ioss_SubSystem.h>
#include <Ioss_Transform.h>

#include "Grid.h"
#include "GridEntry.h"
#include "ZE_SystemInterface.h"
#include "ZE_Version.h"

#ifdef SEACAS_HAVE_MPI
#include <mpi.h>
#endif

namespace {
  template <typename INT>
  Ioss::PropertyManager parse_properties(SystemInterface &interFace, INT int_size)
  {
    Ioss::PropertyManager properties;
    if (sizeof(int_size) == 8) {
      properties.add(Ioss::Property("INTEGER_SIZE_DB", 8));
      properties.add(Ioss::Property("INTEGER_SIZE_API", 8));
    }

    if (interFace.use_netcdf4()) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf4"));
    }

    if (interFace.compression_level() > 0 || interFace.szip()) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf4"));
      properties.add(Ioss::Property("COMPRESSION_LEVEL", interFace.compression_level()));
      properties.add(Ioss::Property("COMPRESSION_SHUFFLE", true));
      if (interFace.szip()) {
        properties.add(Ioss::Property("COMPRESSION_METHOD", "szip"));
      }
      else if (interFace.zlib()) {
        properties.add(Ioss::Property("COMPRESSION_METHOD", "zlib"));
      }
    }
    return properties;
  }

  void        transfer_field_data(Ioss::GroupingEntity *ige, Ioss::GroupingEntity *oge,
                                  Ioss::Field::RoleType role, const std::string &prefix = "",
                                  bool transfer_connectivity = true);
  std::string time_stamp(const std::string &format);
} // namespace

std::string  tsFormat    = "[%H:%M:%S] ";
unsigned int debug_level = 0;

template <typename INT> double zellij(SystemInterface &interFace, INT dummy);

int main(int argc, char *argv[])
{
#ifdef SEACAS_HAVE_MPI
  MPI_Init(&argc, &argv);
#endif

  try {
    SystemInterface::show_version();
    Ioss::Init::Initializer io;

    SystemInterface interFace;
    bool            ok = interFace.parse_options(argc, argv);

    if (!ok) {
      fmt::print(stderr, "\nERROR: Problems parsing command line arguments.\n\n");
      exit(EXIT_FAILURE);
    }

    double time = 0.0;
    if (interFace.ints64bit()) {
      time = zellij(interFace, static_cast<int64_t>(0));
    }
    else {
      time = zellij(interFace, 0);
    }

    add_to_log(argv[0], time);

#ifdef SEACAS_HAVE_MPI
    MPI_Finalize();
#endif
  }
  catch (std::exception &e) {
    fmt::print(stderr, "ERROR: Standard exception: {}\n", e.what());
  }
}

template <typename INT> double zellij(SystemInterface &interFace, INT /*dummy*/)
{
  double begin = Ioss::Utils::timer();

  debug_level = interFace.debug();

  if ((debug_level & 64) != 0U) {
    ex_opts(EX_VERBOSE | EX_DEBUG);
  }
  else {
    ex_opts(0);
  }

  int          int_byte_size = (interFace.ints64bit()) ? 8 : 4;
  size_t       part_count    = interFace.inputFiles_.size();
  RegionVector unit_cells(part_count);
  for (size_t p = 0; p < part_count; p++) {
    Ioss::DatabaseIO *dbi = Ioss::IOFactory::create("exodus", interFace.inputFiles_[p],
                                                    Ioss::READ_RESTART, (MPI_Comm)MPI_COMM_WORLD);
    if (dbi == nullptr || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    if (dbi->int_byte_size_api() > int_byte_size) {
      int_byte_size = dbi->int_byte_size_api();
    }

    dbi->set_surface_split_type(Ioss::SPLIT_BY_DONT_SPLIT);

    if (int_byte_size == 8) {
      dbi->set_int_byte_size_api(Ioss::USE_INT64_API);
    }

    // Generate a name for the region based on the part number...
    std::string name = "Region_" + std::to_string(p + 1);
    // NOTE: region owns database pointer at this time...
    unit_cells[p] = std::make_shared<Ioss::Region>(dbi, name);
    fmt::print(stderr, "\nUnit_Cell {}:", p);
    unit_cells[p]->output_summary(std::cerr);
  }

  Ioss::PropertyManager properties = parse_properties(interFace, INT(0));

  Ioss::DatabaseIO *dbo = Ioss::IOFactory::create(
      "exodus", interFace.outputName_, Ioss::WRITE_RESTART, (MPI_Comm)MPI_COMM_WORLD, properties);
  if (dbo == nullptr || !dbo->ok(true)) {
    std::exit(EXIT_FAILURE);
  }

  // NOTE: 'output_region' owns 'dbo' pointer at this time
  Ioss::Region output_region(dbo, "ejoin_output_region");

  output_region.begin_mode(Ioss::STATE_DEFINE_MODEL);

  output_region.property_add(Ioss::Property("code_name", qainfo[0]));
  output_region.property_add(Ioss::Property("code_version", qainfo[2]));

  if (debug_level & 1) {
    fmt::print(stderr, "{}", time_stamp(tsFormat));
  }

  // Now that we have the unit cells, we can start to place them in the overall grid...
  Grid grid(interFace.grid_i(), interFace.grid_j());

  // Until get a method of inputting the mapping of unit cells to the
  // IxJ grid, we will just distribute them round-robin so have something to test...
  size_t uidx = 0;
  size_t II   = grid.II();
  size_t JJ   = grid.JJ();
  for (size_t j = 0; j < JJ; j++) {
    for (size_t i = 0; i < II; i++) {
      size_t index = uidx++ % unit_cells.size();
      grid.initialize(i, j, unit_cells[index]);
    }
  }

  // All unit cells have been mapped into the IxJ grid, now calculate all node / element offsets
  // and the global node and element counts... (TODO: Parallel decomposition)
  //
  // Iterate through the grid starting with (0,0) and accumulate node and element counts...
  grid.finalize();

#if 0
  INT node_offset    = 0;
  INT element_offset = 0;
  for (auto &pm : unit_cells) {
    pm->property_add(Ioss::Property("node_offset", node_offset));
    pm->property_add(Ioss::Property("element_offset", element_offset));
    INT local_node_count = pm->get_property("node_count").get_int();
    INT local_elem_count = pm->get_property("element_count").get_int();
    node_offset += local_node_count;
    element_offset += local_elem_count;
  }

  INT              node_count = node_offset; // Sum of nodes in part meshes.
  std::vector<INT> local_node_map(node_count);
  std::vector<INT> global_node_map;

  node_count    = global_node_map.size();
  size_t merged = local_node_map.size() - global_node_map.size();
  if (merged > 0) {
    fmt::print("*** {:n} Nodes were merged/omitted.\n", merged);
  }

// Verify nodemap...
#ifndef NDEBUG
  std::vector<int> glob(node_count);
  for (auto id : local_node_map) {
    if (id >= 0) {
      glob[id] = 1;
    }
  }
  for (int i : glob) {
    SMART_ASSERT(i == 1);
  }
#endif

  // Transfer some common data...
  output_region.property_add(unit_cells[0]->get_property("title"));

  // Define a node block...
  std::string block_name        = "nodeblock_1";
  int         spatial_dimension = unit_cells[0]->get_property("spatial_dimension").get_int();
  auto        block =
      new Ioss::NodeBlock(output_region.get_database(), block_name, node_count, spatial_dimension);
  block->property_add(Ioss::Property("id", 1));

  output_region.add(block);

  // Add element blocks, nodesets, sidesets
  for (size_t p = 0; p < part_count; p++) {
    transfer_elementblock(unit_cells[p], output_region, false);
    if (!interFace.omit_nodesets()) {
      transfer_nodesets(unit_cells[p], output_region, false);
    }
    if (!interFace.omit_sidesets()) {
      transfer_sidesets(unit_cells[p], output_region, false);
    }
  }

  if (!interFace.information_record_parts().empty()) {
    const std::vector<int> &info_parts = interFace.information_record_parts();
    if (info_parts[0] == 0) {
      // Transfer info records from all parts...
      for (const auto &pm : unit_cells) {
        const StringVector &info = pm->get_information_records();
        output_region.add_information_records(info);
      }
    }
    else {
      for (int info_part : info_parts) {
        const StringVector &info = unit_cells[info_part - 1]->get_information_records();
        output_region.add_information_records(info);
      }
    }
  }

  output_region.end_mode(Ioss::STATE_DEFINE_MODEL);

  output_region.begin_mode(Ioss::STATE_MODEL);

  output_nodeblock(output_region, unit_cells, local_node_map, global_node_map);
  output_elementblock(output_region, unit_cells, local_node_map, local_element_map,
                      interFace.ignore_element_ids());
  output_nodal_nodeset(output_region, unit_cells, interFace, local_node_map);

  if (!interFace.omit_nodesets()) {
    output_nodeset(output_region, unit_cells, local_node_map);
  }
  if (!interFace.omit_sidesets()) {
    output_sideset(output_region, unit_cells, local_element_map);
  }

  output_region.end_mode(Ioss::STATE_MODEL);
#endif

  /*************************************************************************/
  // EXIT program
  if (debug_level & 1) {
    fmt::print(stderr, "{}", time_stamp(tsFormat));
  }
  output_region.output_summary(std::cout);
  double end = Ioss::Utils::timer();
  fmt::print("******* END *******\n");
  fmt::print(stderr, "\nTotal Execution time     = {:.5} seconds.\n", end - begin);
  return (end - begin);
}

namespace {
  std::string time_stamp(const std::string &format)
  {
    if (format == "") {
      return std::string("");
    }

    const int   length = 256;
    static char time_string[length];

    time_t     calendar_time = time(nullptr);
    struct tm *local_time    = localtime(&calendar_time);

    int error = strftime(time_string, length, format.c_str(), local_time);
    if (error != 0) {
      time_string[length - 1] = '\0';
      return std::string(time_string);
    }

    return std::string("[ERROR]");
  }

  void transfer_field_data(Ioss::GroupingEntity *ige, Ioss::GroupingEntity *oge,
                           Ioss::Field::RoleType role, const std::string &prefix,
                           bool transfer_connectivity)
  {
  }

  template <typename INT>
  void output_nodeblock(Ioss::Region &output_region, RegionVector &unit_cells,
                        const std::vector<INT> &local_node_map, std::vector<INT> &global_node_map)
  {
    Ioss::NodeBlock *onb = output_region.get_node_blocks()[0];
    SMART_ASSERT(onb != nullptr);

    onb->put_field_data("ids", global_node_map);

    int spatial_dimension = output_region.get_property("spatial_dimension").get_int();
    std::vector<double> coord(global_node_map.size() * spatial_dimension);
    for (const auto &pm : unit_cells) {
      Ioss::NodeBlock *nb = pm->get_node_blocks()[0];
      SMART_ASSERT(nb != nullptr);
      std::vector<double> coordinates;
      nb->get_field_data("mesh_model_coordinates", coordinates);
      size_t node_count = nb->entity_count();
      size_t offset     = pm->get_property("node_offset").get_int();
      for (size_t i = 0; i < node_count; i++) {
        ssize_t glob_pos = local_node_map[i + offset];
        if (glob_pos >= 0) {
          coord[glob_pos * spatial_dimension + 0] = coordinates[i * spatial_dimension + 0];
          coord[glob_pos * spatial_dimension + 1] = coordinates[i * spatial_dimension + 1];
          coord[glob_pos * spatial_dimension + 2] = coordinates[i * spatial_dimension + 2];
        }
      }
    }
    onb->put_field_data("mesh_model_coordinates", coord);
  }

  template <typename INT>
  void output_elementblock(Ioss::Region &output_region, RegionVector &unit_cells,
                           const std::vector<INT> &local_node_map,
                           const std::vector<INT> &local_element_map, bool ignore_element_ids)
  {

    const Ioss::ElementBlockContainer &ebs = output_region.get_element_blocks();

    size_t           element_count = output_region.get_property("element_count").get_int();
    std::vector<INT> ids(element_count);

    if (ignore_element_ids) {
      // Just generate 1..numel ids (much faster for large models)
      std::iota(ids.begin(), ids.end(), 1);
    }
    else {
      // Try to maintain the original element ids if possible...
      generate_element_ids(unit_cells, local_element_map, ids);
    }
    size_t element_offset = 0;
    for (auto eb : ebs) {
      eb->put_field_data("ids", &ids[element_offset], ids.size() * sizeof(int));
      element_offset += eb->entity_count();
    }

    SMART_ASSERT(element_offset == element_count);

    // Connectivity...
    for (const auto &pm : unit_cells) {
      const Ioss::ElementBlockContainer &iebs        = pm->get_element_blocks();
      size_t                             node_offset = pm->get_property("node_offset").get_int();

      for (auto ieb : iebs) {
        std::string         name = pm->name() + "_" + ieb->name();
        Ioss::ElementBlock *oeb  = output_region.get_element_block(name);
        if (oeb == nullptr) {
          name = ieb->name();
          oeb  = output_region.get_element_block(name);
        }
        if (oeb != nullptr) {
          std::vector<INT> connectivity;
          ieb->get_field_data("connectivity_raw", connectivity);

          SMART_ASSERT(ieb->entity_count() == oeb->entity_count());
          for (auto &node : connectivity) {
            // connectivity is in part-local node ids [1..num_node]
            // loc_node = the position of node in the local [0..num_node)
            // local_node_map[node_offset+loc_node] gives the position of this node in the global
            // list
            size_t loc_node = node - 1;
            SMART_ASSERT(node_offset + loc_node < local_node_map.size());
            ssize_t gpos = local_node_map[node_offset + loc_node];
            if (gpos >= 0) {
              node = gpos + 1;
            }
          }
          oeb->put_field_data("connectivity_raw", connectivity);
          transfer_field_data(ieb, oeb, Ioss::Field::ATTRIBUTE);
        }
      }
    }
  }

  template <typename INT>
  void output_nodeset(Ioss::Region &output_region, RegionVector &unit_cells,
                      const std::vector<INT> &local_node_map)
  {
    if (output_region.get_nodesets().empty()) {
      return;
    }

    for (const auto &pm : unit_cells) {
      size_t                        node_offset = pm->get_property("node_offset").get_int();
      const Ioss::NodeSetContainer &ins         = pm->get_nodesets();
      for (auto in : ins) {
        std::vector<INT> nodelist;
        in->get_field_data("ids", nodelist);

        std::string    name = pm->name() + "_" + in->name();
        Ioss::NodeSet *ons  = output_region.get_nodeset(name);
        if (ons == nullptr) {
          name = in->name();
          ons  = output_region.get_nodeset(name);
        }
        SMART_ASSERT(ons != nullptr)(name);
        SMART_ASSERT(in->entity_count() == ons->entity_count());

        // This needs to make sure that the nodelist comes back as local id (1..numnodes)
        for (auto &node : nodelist) {
          size_t  loc_node = pm->node_global_to_local(node, true) - 1;
          ssize_t gpos     = local_node_map[node_offset + loc_node];
          if (gpos >= 0) {
            node = gpos + 1;
          }
        }
        ons->put_field_data("ids", nodelist);

        std::vector<double> df;
        in->get_field_data("distribution_factors", df);
        ons->put_field_data("distribution_factors", df);
      }
    }
  }

  template <typename INT>
  void output_sideset(Ioss::Region &output_region, RegionVector &unit_cells,
                      const std::vector<INT> &local_element_map)
  {
    const Ioss::SideSetContainer &os = output_region.get_sidesets();

    Ioss::SideBlockContainer out_eb;
    // Put all output side blocks in the same list...
    for (auto oss : os) {
      const Ioss::SideBlockContainer &obs = oss->get_side_blocks();
      std::copy(obs.begin(), obs.end(), std::back_inserter(out_eb));
    }

    // Assuming (with checks) that the output side blocks will be
    // iterated in same order as input side blocks...
    Ioss::SideBlockContainer::const_iterator II = out_eb.begin();

    for (const auto &pm : unit_cells) {
      size_t element_offset = pm->get_property("element_offset").get_int();

      const Ioss::SideSetContainer &is = pm->get_sidesets();
      for (auto iss : is) {
        const Ioss::SideBlockContainer &ebs = iss->get_side_blocks();

        for (auto eb : ebs) {
          SMART_ASSERT((eb->name() == (*II)->name()) ||
                       (pm->name() + "_" + eb->name() == (*II)->name()))
          (eb->name())((*II)->name());
          SMART_ASSERT(eb->entity_count() == (*II)->entity_count());
          std::vector<INT> elem_side_list;
          eb->get_field_data("element_side_raw", elem_side_list);

          // The 'elem_side_list' contains
          // (local_element_position,side_ordinal) pairs. The
          // 'local_element_position' is 1-based offset in the
          // current part.  Need to map to its location in the
          // output region...
          for (size_t i = 0; i < elem_side_list.size();
               i += 2) { // just get the elem part of the pair...
            size_t  local_position = elem_side_list[i] - 1;
            ssize_t gpos           = local_element_map[element_offset + local_position];
            SMART_ASSERT(gpos >= 0)(gpos)(i); // Inactive elements should be filtered by Ioss
            elem_side_list[i] = gpos + 1;
          }
          (*II)->put_field_data("element_side_raw", elem_side_list);
          ++II;
        }
      }
    }
  }
} // namespace
