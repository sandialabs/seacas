// CGNS Assumptions:
// * All boundary conditions are listed as Family nodes at the "top" level.
// * Unstructured mesh only
// * Single Base.
// * ZoneGridConnectivity is 1to1 with point lists

// Copyright(C) 2015
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

#include <Ioss_CodeTypes.h>
#include <Ioss_Utils.h>
#include <assert.h>
#include <cgns/Iocgns_ParallelDatabaseIO.h>
#include <cgns/Iocgns_Utils.h>
#include <cgnslib.h>
#include <fstream>
#include <iostream>
#include <numeric>
#include <stddef.h>
#include <string>
#include <sys/select.h>
#include <time.h>
#include <vector>

#if !defined(CGNSLIB_H)
#error "Could not include cgnslib.h"
#endif

#include "Ioss_CommSet.h"
#include "Ioss_DBUsage.h"
#include "Ioss_DatabaseIO.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_EntityType.h"
#include "Ioss_NodeBlock.h"
#include "Ioss_SideBlock.h"
#include "Ioss_SideSet.h"
#include "Ioss_StructuredBlock.h"
#include "Ioss_TerminalColor.h"

#include "Ioss_Field.h"
#include "Ioss_IOFactory.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"
#include "Ioss_Region.h"
#include "Ioss_State.h"
#include "Ioss_Utils.h"
#include "Ioss_VariableType.h"


namespace {
  CG_ZoneType_t check_zone_type(int cgnsFilePtr)
  {
    // ========================================================================
    // Get the number of zones (element blocks) in the mesh...
    int base = 1;
    int num_zones = 0;
    cg_nzones(cgnsFilePtr, base, &num_zones);

    CG_ZoneType_t common_zone_type = CG_ZoneTypeNull;

    for (cgsize_t zone = 1; zone <= num_zones; zone++) {
      CG_ZoneType_t zone_type;
      cg_zone_type(cgnsFilePtr, base, zone, &zone_type);

      if (common_zone_type == CG_ZoneTypeNull) {
        common_zone_type = zone_type;
      }

      if (common_zone_type != zone_type) {
        std::ostringstream errmsg;
        errmsg << "ERROR: CGNS: Zone " << zone << " is not the same zone type as previous zones."
               << " This is currently not allowed or supported (hybrid mesh).";
        IOSS_ERROR(errmsg);
      }
    }
    return common_zone_type;
  }
}

namespace Iocgns {

  ParallelDatabaseIO::ParallelDatabaseIO(Ioss::Region *region, const std::string &filename,
                                         Ioss::DatabaseUsage db_usage, MPI_Comm communicator,
                                         const Ioss::PropertyManager &props)
      : Ioss::DatabaseIO(region, filename, db_usage, communicator, props), cgnsFilePtr(-1),
        nodeCount(0), elementCount(0), m_zoneType(CG_ZoneTypeNull)
  {
    dbState = Ioss::STATE_UNKNOWN;

    if (!is_input()) {
      std::ostringstream errmsg;
      errmsg << "ERROR: IOSS CGNS Currently only supports reading, not writing.\n";
      IOSS_ERROR(errmsg);
    }

    std::cout << "CGNS ParallelDatabaseIO using " << CG_SIZEOF_SIZE << "-bit integers.\n";
    if (CG_SIZEOF_SIZE == 64) {
      set_int_byte_size_api(Ioss::USE_INT64_API);
    }

    openDatabase();
  }

  ParallelDatabaseIO::~ParallelDatabaseIO() = default;
  void ParallelDatabaseIO::openDatabase() const
  {
    if (cgnsFilePtr < 0) {
      if (is_input()) {
        int ierr = cg_open(get_filename().c_str(), CG_MODE_READ, &cgnsFilePtr);
        if (ierr != CG_OK) {
          // NOTE: Code will not continue past this call...
          std::ostringstream errmsg;
          errmsg << "ERROR: Problem opening file '" << get_filename() << "' for read access.";
          IOSS_ERROR(errmsg);
        }
      }
    }
    assert(cgnsFilePtr >= 0);
  }

  void ParallelDatabaseIO::closeDatabase() const
  {
    if (cgnsFilePtr != -1) {
      cg_close(cgnsFilePtr);
    }
    cgnsFilePtr = -1;
  }

  void ParallelDatabaseIO::release_memory()
  {
    nodeMap.release_memory();
    elemMap.release_memory();
    try {
      decomp.reset();
    }
    catch (...) {
    }
  }

  int64_t ParallelDatabaseIO::node_global_to_local(int64_t global, bool must_exist) const
  {
    // TODO: Fix
    return global;
  }

  int64_t ParallelDatabaseIO::element_global_to_local(int64_t global) const
  {
    // TODO: Fix
    return global;
  }

  void ParallelDatabaseIO::read_meta_data()
  {
    openDatabase();

    // Determine the number of bases in the grid.
    // Currently only handle 1.
    cgsize_t n_bases = 0;
    cg_nbases(cgnsFilePtr, &n_bases);
    if (n_bases != 1) {
      std::ostringstream errmsg;
      errmsg << "CGNS: Too many bases; only support files with a single bases at this time";
      IOSS_ERROR(errmsg);
    }

    m_zoneType = check_zone_type(cgnsFilePtr);
    
    if (int_byte_size_api() == 8) {
      decomp = std::unique_ptr<DecompositionDataBase>(
          new DecompositionData<int64_t>(properties, util().communicator()));
    }
    else {
      decomp = std::unique_ptr<DecompositionDataBase>(
          new DecompositionData<int>(properties, util().communicator()));
    }
    assert(decomp != nullptr);
    decomp->decompose_model(cgnsFilePtr, m_zoneType);

    if (m_zoneType == CG_Structured) {
      handle_structured_blocks();
    }
    else if (m_zoneType == CG_Unstructured) {
      handle_unstructured_blocks();
    }
  }

  void ParallelDatabaseIO::handle_unstructured_blocks()
  {
    get_region()->property_add(
        Ioss::Property("global_node_count", (int64_t)decomp->global_node_count()));
    get_region()->property_add(
        Ioss::Property("global_element_count", (int64_t)decomp->global_elem_count()));

    nodeCount    = decomp->ioss_node_count();
    elementCount = decomp->ioss_elem_count();
    std::cerr << "Nodes, Cells = " << nodeCount << "\t" << elementCount << "\n";

    // ========================================================================
    // Get the number of families in the mesh...
    // Will treat these as sidesets if they are of the type "FamilyBC_t"
    cgsize_t base         = 1;
    cgsize_t num_families = 0;
    cg_nfamilies(cgnsFilePtr, base, &num_families);
    for (cgsize_t family = 1; family <= num_families; family++) {
      char     name[33];
      cgsize_t num_bc  = 0;
      cgsize_t num_geo = 0;
      cg_family_read(cgnsFilePtr, base, family, name, &num_bc, &num_geo);
#if defined(DEBUG_OUTPUT)
      std::cout << "Family " << family << " named " << name << " has " << num_bc << " BC, and "
                << num_geo << " geometry references\n";
#endif
      if (num_bc > 0) {
        // Create a sideset...
        std::string    ss_name(name);
        Ioss::SideSet *ss = new Ioss::SideSet(this, ss_name);
        get_region()->add(ss);
      }
    }

    // ========================================================================
    // Get the number of zones (element blocks) in the mesh...
    int i = 0;
    for (auto &block : decomp->m_elementBlocks) {
      std::string element_topo = block.topologyType;
#if defined(DEBUG_OUTPUT)
      std::cout << "Added block " << block.name() << ":, IOSS topology = '" << element_topo
                << "' with " << block.ioss_count() << " elements\n";
#endif
      auto *eblock = new Ioss::ElementBlock(this, block.name(), element_topo, block.ioss_count());
      eblock->property_add(Ioss::Property("base", base));
      eblock->property_add(Ioss::Property("zone", block.zone()));
      eblock->property_add(Ioss::Property("section", block.section()));
      eblock->property_add(Ioss::Property("original_block_order", i++));
      get_region()->add(eblock);
    }

    // ========================================================================
    // Have sidesets, now create sideblocks for each sideset...
    int id = 0;
    for (auto &sset : decomp->m_sideSets) {
      // See if there is an Ioss::SideSet with a matching name...
      Ioss::SideSet *ioss_sset = get_region()->get_sideset(sset.name());
      if (ioss_sset != NULL) {
        auto        zone = decomp->m_zones[sset.zone()];
        std::string block_name(zone.m_name);
        block_name += "/";
        block_name += sset.name();
        std::string face_topo = sset.topologyType;
#if defined(DEBUG_OUTPUT)
        std::cout << "Processor " << myProcessor << ": Added sideblock " << block_name
                  << " of topo " << face_topo << " with " << sset.ioss_count() << " faces\n";
#endif
        const auto &block = decomp->m_elementBlocks[sset.parentBlockIndex];

        std::string      parent_topo = block.topologyType;
        Ioss::SideBlock *sblk =
            new Ioss::SideBlock(this, block_name, face_topo, parent_topo, sset.ioss_count());
        sblk->property_add(Ioss::Property("id", id));
        sblk->property_add(Ioss::Property("base", 1));
        sblk->property_add(Ioss::Property("zone", sset.zone()));
        sblk->property_add(Ioss::Property("section", sset.section()));
        Ioss::ElementBlock *eblock = get_region()->get_element_block(block.name());
        if (eblock != NULL) {
          sblk->set_parent_element_block(eblock);
        }
        ioss_sset->add(sblk);
      }
      id++; // Really just index into m_sideSets list.
    }

    Ioss::NodeBlock *nblock =
        new Ioss::NodeBlock(this, "nodeblock_1", decomp->ioss_node_count(), 3);
    nblock->property_add(Ioss::Property("base", base));
    get_region()->add(nblock);

    // Create a single node commset
    Ioss::CommSet *commset =
        new Ioss::CommSet(this, "commset_node", "node", decomp->get_commset_node_size());
    commset->property_add(Ioss::Property("id", 1));
    get_region()->add(commset);
  }

  void ParallelDatabaseIO::create_structured_block(cgsize_t base, cgsize_t zone, size_t &num_node,
                                           size_t &num_cell)
  {
    cgsize_t size[9];
    char     zone_name[33];
    cg_zone_read(cgnsFilePtr, base, zone, zone_name, size);
    m_zoneNameMap[zone_name] = zone;

    assert(size[0] - 1 == size[3]);
    assert(size[1] - 1 == size[4]);
    assert(size[2] - 1 == size[5]);

    assert(size[6] == 0);
    assert(size[7] == 0);
    assert(size[8] == 0);

    cgsize_t index_dim = 0;
    cg_index_dim(cgnsFilePtr, base, zone, &index_dim);
    // An Ioss::StructuredBlock corresponds to a CG_Structured zone...
    Ioss::StructuredBlock *block =
        new Ioss::StructuredBlock(this, zone_name, index_dim, size[3], size[4], size[5]);

    block->property_add(Ioss::Property("base", base));
    block->property_add(Ioss::Property("zone", zone));
    get_region()->add(block);

    block->set_node_offset(num_node);
    block->set_cell_offset(num_cell);
    num_node += block->get_property("node_count").get_int();
    num_cell += block->get_property("cell_count").get_int();

    // Handle zone-grid-connectivity...
    int nconn = 0;
    cg_n1to1(cgnsFilePtr, base, zone, &nconn);
    for (int i = 0; i < nconn; i++) {
      char connectname[33];
      char donorname[33];
      std::array<cgsize_t, 6> range;
      std::array<cgsize_t, 6> donor_range;
      std::array<int, 3>      transform;

      cg_1to1_read(cgnsFilePtr, base, zone, i + 1, connectname, donorname, range.data(),
                   donor_range.data(), transform.data());

      // Get number of nodes shared with other "previous" zones...
      // A "previous" zone will have a lower zone number this this zone...
      int  donor_zone = -1;
      auto donor_iter = m_zoneNameMap.find(donorname);
      if (donor_iter != m_zoneNameMap.end()) {
        donor_zone = (*donor_iter).second;
      }
      std::array<cgsize_t, 3> range_beg{{range[0], range[1], range[2]}};
      std::array<cgsize_t, 3> range_end{{range[3], range[4], range[5]}};
      std::array<cgsize_t, 3> donor_beg{{donor_range[0], donor_range[1], donor_range[2]}};
      std::array<cgsize_t, 3> donor_end{{donor_range[3], donor_range[4], donor_range[5]}};

      block->m_zoneConnectivity.emplace_back(connectname, zone, donorname, donor_zone, transform,
                                             range_beg, range_end, donor_beg, donor_end);
    }
  }

  size_t ParallelDatabaseIO::finalize_structured_blocks()
  {
    const auto &blocks = get_region()->get_structured_blocks();

    // If there are any Structured blocks, need to iterate them and their 1-to-1 connections
    // and update the donor_zone id for zones that had not yet been processed at the time of
    // definition...
    for (auto &block : blocks) {
      for (auto &conn : block->m_zoneConnectivity) {
        if (conn.m_donorZone < 0) {
          auto donor_iter = m_zoneNameMap.find(conn.m_donorName);
          assert(donor_iter != m_zoneNameMap.end());
          conn.m_donorZone = (*donor_iter).second;
        }
      }
    }

    for (auto &block : blocks) {
      block->generate_shared_nodes(*get_region());
    }

    // Iterate all structured blocks and fill in the global ids:
    // Map from local node to global node block accounting for
    // shared nodes.
    //
    // Iterate the m_globalNodeIdList in each block.
    // If the entry is "ss_max", then this is an owned
    // node -- file with sequential global node block offsets.
    // If the entry is not "ss_max", then this node is shared
    // and its entry currently points to the location in the "global offset"
    // list of the owning node.  Need to get the "global id" value at that
    // location and put it in m_globalNodeIdList at that location.
    ssize_t ss_max = std::numeric_limits<ssize_t>::max();
    size_t  offset = 0;
    for (auto &block : blocks) {
      for (auto &node : block->m_globalNodeIdList) {
        if (node == ss_max) {
          node = offset++;
        }
        else {
          // Node is shared and the value points to the owner.
          // Determine which block contains the owner and get its value.
          auto owner_block = get_region()->get_structured_block(node);
          assert(owner_block != nullptr);
          node = owner_block->m_globalNodeIdList[node - owner_block->get_node_offset()];
        }
      }
    }
    return offset; // Number of 'equived' nodes in model
  }

  void ParallelDatabaseIO::handle_structured_blocks()
  {
    int base = 1;
    
    char     basename[33];
    cgsize_t cell_dimension = 0;
    cgsize_t phys_dimension = 0;
    cg_base_read(cgnsFilePtr, base, basename, &cell_dimension, &phys_dimension);

    std::cerr << "In handle structured blocks\n";
    // Iterate all structured blocks and set the intervals to zero
    // if the m_proc field does not match current processor...
    const auto &blocks = decomp->m_structuredBlocks;
    const auto &zones  = decomp->m_structuredZones;
    assert(blocks.size() == zones.size());
    
    size_t node_offset = 0;
    size_t cell_offset = 0;

    for (auto &zone : zones) {
      if (zone->m_adam == zone) {
	// This is a "root" zone from the undecomposed mesh...
	// Now see if there are any non-empty blocks with
	// this m_adam on this processor.  If exists, then create
	// a StructuredBlock; otherwise, create an empty block.
	auto block_name = zone->m_adam->m_structuredBlock->name();

	Ioss::StructuredBlock *new_block = nullptr;
	for (auto &pzone : zones) {
	  if (pzone->m_proc == myProcessor && pzone->m_adam == zone) {
	    // Create a non-empty structured block on this processor...
	    auto &block = pzone->m_structuredBlock;
	    assert(block != nullptr);
	    std::cerr << Ioss::trmclr::green
		      << "Creating non-empty " << block_name << " on processor "
		      << myProcessor
		      << "\t" << block->get_property("ni").get_int()
		      << "\t" << block->get_property("nj").get_int()
		      << "\t" << block->get_property("nk").get_int()
		      << "\n"
		      << Ioss::trmclr::normal;
	    new_block = new Ioss::StructuredBlock(this, block_name,
						  phys_dimension,
						  block->get_property("ni").get_int(),
						  block->get_property("nj").get_int(),
						  block->get_property("nk").get_int());
	    new_block->set_index_offset(zone->m_offset);
	    for (auto &zgc : block->m_zoneConnectivity) {
	      new_block->m_zoneConnectivity.push_back(zgc);
	    }
	    break;
	  }
	}
	if (new_block == nullptr) {
	  // There is no block on this processor corresponding to the m_adam
	  // block.  Create an empty block...
	  std::cerr << Ioss::trmclr::red
		    << "Creating empty " << block_name << " on processor "
		    << myProcessor << "\n"
		    << Ioss::trmclr::normal;
	  new_block = new Ioss::StructuredBlock(this, block_name,
						phys_dimension,
						0, 0, 0);
	}
	assert(new_block != nullptr);
	get_region()->add(new_block);
	
	new_block->property_add(Ioss::Property("base", base));
	new_block->property_add(Ioss::Property("zone", zone->m_adam->m_zone));

	new_block->set_node_offset(node_offset);
	new_block->set_cell_offset(cell_offset);
	node_offset += new_block->get_property("node_count").get_int();
	cell_offset += new_block->get_property("cell_count").get_int();
      }
    }

    auto *nblock = new Ioss::NodeBlock(this, "nodeblock_1", node_offset, phys_dimension);
    nblock->property_add(Ioss::Property("base", base));
    get_region()->add(nblock);
    std::cerr << myProcessor << "-Nodes/Cells = " << node_offset << "\t" << cell_offset << "\n";
  }

  bool ParallelDatabaseIO::begin(Ioss::State /* state */) { return true; }

  bool ParallelDatabaseIO::end(Ioss::State /* state */) { return true; }

  bool ParallelDatabaseIO::begin_state(Ioss::Region *region, int /* state */, double time)
  {
    return true;
  }

  bool ParallelDatabaseIO::end_state(Ioss::Region * /* region */, int /* state */,
                                     double /* time */)
  {
    return true;
  }

  const Ioss::Map &ParallelDatabaseIO::get_map(entity_type type) const
  {
    if (m_zoneType == CG_Unstructured) {
      switch (type) {
      case entity_type::NODE: {
	size_t offset = decomp->decomp_node_offset();
	size_t count  = decomp->decomp_node_count();
	return get_map(nodeMap, nodeCount, offset, count, entity_type::NODE);
      }
      case entity_type::ELEM: {
	size_t offset = decomp->decomp_elem_offset();
	size_t count  = decomp->decomp_elem_count();
	return get_map(elemMap, elementCount, offset, count, entity_type::ELEM);
      }
	
      default:
	std::ostringstream errmsg;
	errmsg << "INTERNAL ERROR: Invalid map type. "
	       << "Something is wrong in the Iocgns::ParallelDatabaseIO::get_map() function. "
	       << "Please report.\n";
	IOSS_ERROR(errmsg);
      }
    }
    else {
      std::cerr << "NodeCount = " << nodeCount << "\n";
    }
  }

  const Ioss::Map &ParallelDatabaseIO::get_map(Ioss::Map &entity_map, int64_t entityCount,
                                               int64_t file_offset, int64_t file_count,
                                               entity_type type) const

  {
    // Allocate space for node number map and read it in...
    // Can be called multiple times, allocate 1 time only
    if (entity_map.map.empty()) {
      entity_map.map.resize(entityCount + 1);

      if (is_input()) {
        Ioss::MapContainer file_data(file_count);

        // For cgns, my file_data is just nodes from file_offset to file_offset+file_count
        std::iota(file_data.begin(), file_data.end(), file_offset + 1);

        if (type == entity_type::NODE)
          decomp->communicate_node_data(TOPTR(file_data), &entity_map.map[1], 1);
        else if (type == entity_type::ELEM)
          decomp->communicate_element_data(TOPTR(file_data), &entity_map.map[1], 1);

        // Check for sequential node map.
        // If not, build the reverse G2L node map...
        entity_map.map[0] = -1;
        for (int64_t i = 1; i < entityCount + 1; i++) {
          if (i != entity_map.map[i]) {
            entity_map.map[0] = 1;
            break;
          }
        }

        entity_map.build_reverse_map();
      }
      else {
        // Output database; entity_map.map not set yet... Build a default map.
        for (int64_t i = 1; i < entityCount + 1; i++) {
          entity_map.map[i] = i;
        }
        // Sequential map
        entity_map.map[0] = -1;
      }
    }
    return entity_map;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::Region * /* reg */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::NodeBlock *nb,
                                                 const Ioss::Field &field, void *data,
                                                 size_t data_size) const
  {
    size_t num_to_get = field.verify(data_size);

    Ioss::Field::RoleType role = field.get_role();
    if (role == Ioss::Field::MESH) {
      if (field.get_name() == "mesh_model_coordinates_x" ||
          field.get_name() == "mesh_model_coordinates_y" ||
          field.get_name() == "mesh_model_coordinates_z" ||
          field.get_name() == "mesh_model_coordinates") {
        decomp->get_node_coordinates(cgnsFilePtr, (double *)data, field);
      }

      else if (field.get_name() == "ids") {
        // Map the local ids in this node block
        // (1...node_count) to global node ids.
        get_map(entity_type::NODE).map_implicit_data(data, field, num_to_get, 0);
      }
      // The 1..global_node_count id.  In a parallel-decomposed run,
      // it maps the node back to its implicit position in the serial
      // undecomposed mesh file.  This is ONLY provided for backward-
      // compatibility and should not be used unless absolutely required.
      else if (field.get_name() == "implicit_ids") {
        size_t offset = decomp->decomp_node_offset();
        size_t count  = decomp->decomp_node_count();
        if (int_byte_size_api() == 4) {
          std::vector<int> file_ids(count);
          std::iota(file_ids.begin(), file_ids.end(), offset + 1);
          decomp->communicate_node_data(TOPTR(file_ids), (int *)data, 1);
        }
        else {
          std::vector<int64_t> file_ids(count);
          std::iota(file_ids.begin(), file_ids.end(), offset + 1);
          decomp->communicate_node_data(TOPTR(file_ids), (int64_t *)data, 1);
        }
      }

      else if (field.get_name() == "connectivity") {
        // Do nothing, just handles an idiosyncracy of the GroupingEntity
      }
      else if (field.get_name() == "connectivity_raw") {
        // Do nothing, just handles an idiosyncracy of the GroupingEntity
      }
      else if (field.get_name() == "owning_processor") {
        // If parallel, then set the "locally_owned" property on the nodeblocks.
        Ioss::CommSet *css = get_region()->get_commset("commset_node");
        if (int_byte_size_api() == 8) {
          int64_t *idata = static_cast<int64_t *>(data);
          std::fill(idata, idata + nodeCount, myProcessor);

          std::vector<int64_t> ent_proc;
          css->get_field_data("entity_processor_raw", ent_proc);
          for (size_t i = 0; i < ent_proc.size(); i += 2) {
            int64_t node = ent_proc[i + 0];
            int64_t proc = ent_proc[i + 1];
            if (proc < idata[node - 1]) {
              idata[node - 1] = proc;
            }
          }
        }
        else {
          int *idata = static_cast<int *>(data);
          std::fill(idata, idata + nodeCount, myProcessor);

          std::vector<int> ent_proc;
          css->get_field_data("entity_processor_raw", ent_proc);
          for (size_t i = 0; i < ent_proc.size(); i += 2) {
            int node = ent_proc[i + 0];
            int proc = ent_proc[i + 1];
            if (proc < idata[node - 1]) {
              idata[node - 1] = proc;
            }
          }
        }
      }
      else {
        num_to_get = Ioss::Utils::field_warning(nb, field, "input");
      }
      return num_to_get;
    }
    return -1;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::EdgeBlock * /* nb */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::FaceBlock * /* nb */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::StructuredBlock *sb, const Ioss::Field &field,
                                         void *data, size_t data_size) const
  {
    Ioss::Field::RoleType role = field.get_role();
    cgsize_t              base = sb->get_property("base").get_int();
    cgsize_t              zone = sb->get_property("zone").get_int();

    cgsize_t num_to_get = field.verify(data_size);

    cgsize_t rmin[3] = {0, 0, 0};
    cgsize_t rmax[3] = {0, 0, 0};

    assert(num_to_get == sb->get_property("node_count").get_int());
    if (num_to_get > 0) {
      rmin[0] = sb->get_property("offset_i").get_int() +1 ;
      rmin[1] = sb->get_property("offset_j").get_int() +1 ;
      rmin[2] = sb->get_property("offset_k").get_int() +1 ;
      
      rmax[0] = rmin[0] + sb->get_property("ni").get_int();
      rmax[1] = rmin[1] + sb->get_property("nj").get_int();
      rmax[2] = rmin[2] + sb->get_property("nk").get_int();
    }

    if (role == Ioss::Field::MESH) {
      double *rdata = static_cast<double *>(data);
      if (field.get_name() == "mesh_model_coordinates_x") {
        int ierr =
            cg_coord_read(cgnsFilePtr, base, zone, "CoordinateX", CG_RealDouble, rmin, rmax, rdata);
        if (ierr < 0) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }
      }

      else if (field.get_name() == "mesh_model_coordinates_y") {
        int ierr =
            cg_coord_read(cgnsFilePtr, base, zone, "CoordinateY", CG_RealDouble, rmin, rmax, rdata);
        if (ierr < 0) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }
      }

      else if (field.get_name() == "mesh_model_coordinates_z") {
        int ierr =
            cg_coord_read(cgnsFilePtr, base, zone, "CoordinateZ", CG_RealDouble, rmin, rmax, rdata);
        if (ierr < 0) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }
      }

      else if (field.get_name() == "mesh_model_coordinates") {
        char     basename[33];
        cgsize_t cell_dimension = 0;
        cgsize_t phys_dimension = 0;
        cg_base_read(cgnsFilePtr, base, basename, &cell_dimension, &phys_dimension);

        // Data required by upper classes store x0, y0, z0, ... xn,
        // yn, zn. Data stored in cgns file is x0, ..., xn, y0,
        // ..., yn, z0, ..., zn so we have to allocate some scratch
        // memory to read in the data and then map into supplied
        // 'data'
        std::vector<double> coord(num_to_get);
        int ierr = cg_coord_read(cgnsFilePtr, base, zone, "CoordinateX", CG_RealDouble, rmin, rmax,
                                 TOPTR(coord));
        if (ierr < 0) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }

        // Map to global coordinate position...
        for (cgsize_t i = 0; i < num_to_get; i++) {
          rdata[phys_dimension * i + 0] = coord[i];
        }

        if (phys_dimension >= 2) {
          ierr = cg_coord_read(cgnsFilePtr, base, zone, "CoordinateY", CG_RealDouble, rmin, rmax,
                               TOPTR(coord));
          if (ierr < 0) {
            Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
          }

          // Map to global coordinate position...
          for (cgsize_t i = 0; i < num_to_get; i++) {
            rdata[phys_dimension * i + 1] = coord[i];
          }
        }

        if (phys_dimension == 3) {
          ierr = cg_coord_read(cgnsFilePtr, base, zone, "CoordinateZ", CG_RealDouble, rmin, rmax,
                               TOPTR(coord));
          if (ierr < 0) {
            Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
          }

          // Map to global coordinate position...
          for (cgsize_t i = 0; i < num_to_get; i++) {
            rdata[phys_dimension * i + 2] = coord[i];
          }
        }
      }
      else if (field.get_name() == "cell_node_ids") {

        // Map the local ids in this node block
        // (1...node_count) to global 1-based cell-node ids.
        size_t node_offset = sb->get_node_offset();
        size_t node_count  = sb->get_property("node_count").get_int();

        if (field.get_type() == Ioss::Field::INT64) {
          int64_t *idata = static_cast<int64_t *>(data);
          std::iota(idata, idata + node_count, node_offset + 1);
        }
        else {
          assert(field.get_type() == Ioss::Field::INT32);
          int *idata = static_cast<int *>(data);
          std::iota(idata, idata + node_count, node_offset + 1);
        }
      }
      else {
        num_to_get = Ioss::Utils::field_warning(sb, field, "input");
      }
      return num_to_get;
    }
    return -1;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::ElementBlock *eb,
                                                 const Ioss::Field &field, void *data,
                                                 size_t data_size) const
  {
    size_t                num_to_get = field.verify(data_size);
    Ioss::Field::RoleType role       = field.get_role();

    if (role == Ioss::Field::MESH) {
      // Handle the MESH fields required for a CGNS file model.
      // (The 'genesis' portion)

      if (field.get_name() == "connectivity_raw" || field.get_name() == "connectivity") {

        // The connectivity is stored in a 1D array.
        // The element_node index varies fastet
        int order = eb->get_property("original_block_order").get_int();
        decomp->get_block_connectivity(cgnsFilePtr, data, order);

#if 0
	int element_nodes = eb->topology()->number_nodes();
	assert(field.raw_storage()->component_count() == element_nodes);
	get_map(entity_type::NODE).map_data(data, field, num_to_get*element_nodes);
#endif
      }
      else if (field.get_name() == "ids" || field.get_name() == "implicit_ids") {
        // Map the local ids in this node block
        // (1...node_count) to global node ids.
        get_map(entity_type::ELEM).map_implicit_data(data, field, num_to_get, eb->get_offset());
      }
      else {
        num_to_get = Ioss::Utils::field_warning(eb, field, "input");
      }
    }
    else {
      num_to_get = Ioss::Utils::field_warning(eb, field, "unknown");
    }
    return num_to_get;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::NodeSet * /* ns */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::EdgeSet * /* ns */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::FaceSet * /* ns */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::ElementSet * /* ns */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::SideBlock *sb,
                                                 const Ioss::Field &field, void *data,
                                                 size_t data_size) const
  {
    int  id   = sb->get_property("id").get_int();
    auto sset = decomp->m_sideSets[id];

    ssize_t num_to_get = field.verify(data_size);
    if (num_to_get > 0) {
      int64_t entity_count = sb->get_property("entity_count").get_int();
      if (num_to_get != entity_count) {
        std::ostringstream errmsg;
        errmsg << "ERROR: Partial field input not yet implemented for side blocks";
        IOSS_ERROR(errmsg);
      }
    }

    Ioss::Field::RoleType role = field.get_role();
    if (role == Ioss::Field::MESH) {
      if (field.get_name() == "element_side_raw" || field.get_name() == "element_side") {

        decomp->get_sideset_element_side(cgnsFilePtr, sset, data);

        if (field.get_type() == Ioss::Field::INT32) {
          int *idata = (int *)data;
          Utils::map_cgns_face_to_ioss(sb->parent_element_topology(), num_to_get, idata);
        }
        else {
          int64_t *idata = (int64_t *)data;
          Utils::map_cgns_face_to_ioss(sb->parent_element_topology(), num_to_get, idata);
        }
        return num_to_get;
      }
    }
    num_to_get = Ioss::Utils::field_warning(sb, field, "input");
    return num_to_get;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::SideSet * /* fs */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::CommSet *cs, const Ioss::Field &field,
                                                 void *data, size_t data_size) const
  {
    size_t num_to_get = field.verify(data_size);

    // Return the <entity (node or side), processor> pair
    if (field.get_name() == "entity_processor" || field.get_name() == "entity_processor_raw") {

      // Check type -- node or side
      std::string type = cs->get_property("entity_type").get_string();

      if (type == "node") {

        bool do_map = field.get_name() == "entity_processor";
        // Convert local node id to global node id and store in 'data'
        const Ioss::MapContainer &map = get_map(entity_type::NODE).map;
        if (int_byte_size_api() == 4) {
          decomp->get_node_entity_proc_data(static_cast<int *>(data), map, do_map);
        }
        else {
          decomp->get_node_entity_proc_data(static_cast<int64_t *>(data), map, do_map);
        }
      }
      else {
        std::ostringstream errmsg;
        errmsg << "ERROR: Invalid commset type " << type;
        IOSS_ERROR(errmsg);
      }
    }
    else if (field.get_name() == "ids") {
      // Do nothing, just handles an idiosyncracy of the GroupingEntity
    }
    else {
      num_to_get = Ioss::Utils::field_warning(cs, field, "input");
    }
    return num_to_get;
  }

  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::Region *region,
                                                 const Ioss::Field &field, void *data,
                                                 size_t data_size) const
  {
    return -1;
  }

  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::ElementBlock * /* eb */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::StructuredBlock * /* sb */,
                                         const Ioss::Field & /* field */, void * /* data */,
                                         size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::FaceBlock * /* nb */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::EdgeBlock * /* nb */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::NodeBlock * /* nb */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }

  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::NodeSet * /* ns */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::EdgeSet * /* ns */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::FaceSet * /* ns */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::ElementSet * /* ns */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::SideBlock * /* fb */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::SideSet * /* fs */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::CommSet * /* cs */,
                                                 const Ioss::Field & /* field */, void * /* data */,
                                                 size_t /* data_size */) const
  {
    return -1;
  }

  unsigned ParallelDatabaseIO::entity_field_support() const { return Ioss::REGION; }
}
