// CGNS Assumptions:
// * All boundary conditions are listed as Family nodes at the "top" level.
// * Unstructured mesh only
// * Single Base.
// * ZoneGridConnectivity is 1to1 with point lists

// Copyright(C) 2015, 2016
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
#include <fstream>
#include <iostream>
#include <numeric>
#include <stddef.h>
#include <string>
#include <sys/select.h>
#include <time.h>
#include <vector>

#include <cgnsconfig.h>
#if CG_BUILD_PARALLEL
#include <pcgnslib.h>
#else
#include <cgnslib.h>
#endif

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
    int base      = 1;
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

    if (myProcessor == 0) {
      std::cout << "CGNS ParallelDatabaseIO using " << CG_SIZEOF_SIZE << "-bit integers.\n"
#if CG_BUILD_PARALLEL
                << "                        using the parallel CGNS library and API.\n";
#else
                << "                        using the serial CGNS library and API.\n";
#endif
    }
    if (CG_SIZEOF_SIZE == 64) {
      set_int_byte_size_api(Ioss::USE_INT64_API);
    }

    openDatabase();
  }

  ParallelDatabaseIO::~ParallelDatabaseIO() = default;
  void ParallelDatabaseIO::openDatabase() const
  {
    if (cgnsFilePtr < 0) {
      int mode = is_input() ? CG_MODE_READ : CG_MODE_WRITE;
#if CG_BUILD_PARALLEL
      cgp_mpi_comm(util().communicator());
      cgp_pio_mode(CGP_COLLECTIVE);
      int ierr = cgp_open(get_filename().c_str(), mode, &cgnsFilePtr);
#else
      int ierr = cg_open(get_filename().c_str(), mode, &cgnsFilePtr);
#endif
      if (ierr != CG_OK) {
        // NOTE: Code will not continue past this call...
        std::ostringstream errmsg;
        errmsg << "ERROR: Problem opening file '" << get_filename() << "' for read access. "
               << "CGNS Error: '" << cg_get_error() << "'";
        IOSS_ERROR(errmsg);
      }
    }
    assert(cgnsFilePtr >= 0);
  }

  void ParallelDatabaseIO::closeDatabase() const
  {
    if (cgnsFilePtr != -1) {
#if CG_BUILD_PARALLEL
      cgp_close(cgnsFilePtr);
#else
      cg_close(cgnsFilePtr);
#endif
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

    // ========================================================================
    // Get the number of families in the mesh...
    // Will treat these as sidesets if they are of the type "FamilyBC_t"
    Utils::add_sidesets(cgnsFilePtr, this);

    // ========================================================================
    // Get the number of zones (element blocks) in the mesh...
    int base = 1;
    int i    = 0;
    for (auto &block : decomp->m_elementBlocks) {
      std::string element_topo = block.topologyType;
#if defined(IOSS_DEBUG_OUTPUT)
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
#if defined(IOSS_DEBUG_OUTPUT)
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

  // TODO: See if code can be used for parallel node resolution...
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

    size_t num_nodes = Utils::resolve_nodes(*get_region(), myProcessor);
    return num_nodes;
  }

  void ParallelDatabaseIO::handle_structured_blocks()
  {
    int base = 1;

    Utils::add_sidesets(cgnsFilePtr, this);

    char     basename[33];
    cgsize_t cell_dimension = 0;
    cgsize_t phys_dimension = 0;
    cg_base_read(cgnsFilePtr, base, basename, &cell_dimension, &phys_dimension);

    // Iterate all structured blocks and set the intervals to zero
    // if the m_proc field does not match current processor...
    const auto &zones = decomp->m_structuredZones;

    size_t node_offset        = 0;
    size_t cell_offset        = 0;
    size_t global_node_offset = 0;
    size_t global_cell_offset = 0;

    for (auto &zone : zones) {
      if (zone->m_adam == zone) {
        // This is a "root" zone from the undecomposed mesh...
        // Now see if there are any non-empty blocks with
        // this m_adam on this processor.  If exists, then create
        // a StructuredBlock; otherwise, create an empty block.
        auto block_name = zone->m_name;

        Ioss::StructuredBlock *block = nullptr;
        Ioss::IJK_t            zeros{{0, 0, 0}};
        for (auto &pzone : zones) {
          if (pzone->m_proc == myProcessor && pzone->m_adam == zone) {
            // Create a non-empty structured block on this processor...
            block = new Ioss::StructuredBlock(this, block_name, phys_dimension, pzone->m_ordinal,
                                              pzone->m_offset, pzone->m_adam->m_ordinal);

            for (auto &zgc : pzone->m_zoneConnectivity) {
              block->m_zoneConnectivity.push_back(zgc);
            }
            break;
          }
        }
        if (block == nullptr) {
          // There is no block on this processor corresponding to the m_adam
          // block.  Create an empty block...
          block = new Ioss::StructuredBlock(this, block_name, phys_dimension, zeros, zeros,
                                            zone->m_adam->m_ordinal);
          for (auto zgc : zone->m_zoneConnectivity) {
            zgc.m_isActive = false;
            block->m_zoneConnectivity.push_back(zgc);
          }
        }
        assert(block != nullptr);
        get_region()->add(block);

        block->property_add(Ioss::Property("base", base));
        block->property_add(Ioss::Property("zone", zone->m_adam->m_zone));

        block->set_node_offset(node_offset);
        block->set_cell_offset(cell_offset);
        node_offset += block->get_property("node_count").get_int();
        cell_offset += block->get_property("cell_count").get_int();

        block->set_node_global_offset(global_node_offset);
        block->set_cell_global_offset(global_cell_offset);
        global_node_offset += block->get_property("global_node_count").get_int();
        global_cell_offset += block->get_property("global_cell_count").get_int();
      }
    }

    // ========================================================================
    // Iterate each StructuredBlock, get its zone. For that zone, get the number of
    // boundary conditions and then iterate those and create sideblocks in the
    // corresponding sideset.
    const auto &sbs = get_region()->get_structured_blocks();
    for (const auto &block : sbs) {
      // Handle boundary conditions...
      Utils::add_structured_boundary_conditions(cgnsFilePtr, block);
    }

    size_t node_count = finalize_structured_blocks();
    auto * nblock     = new Ioss::NodeBlock(this, "nodeblock_1", node_count, phys_dimension);
    nblock->property_add(Ioss::Property("base", base));
    get_region()->add(nblock);
  }

  void ParallelDatabaseIO::write_meta_data()
  {
    // Make sure mesh is not hybrid...
    if (get_region()->mesh_type() == Ioss::MeshType::HYBRID) {
      std::ostringstream errmsg;
      errmsg << "ERROR: CGNS: The mesh on region " << get_region()->name()
             << " is of type 'hybrid'."
             << " This is currently not allowed or supported.";
      IOSS_ERROR(errmsg);
    }

    int base           = 0;
    int phys_dimension = get_region()->get_property("spatial_dimension").get_int();
    int ierr           = cg_base_write(cgnsFilePtr, "Base", phys_dimension, phys_dimension, &base);
    if (ierr != CG_OK) {
      // NOTE: Code will not continue past this call...
      std::ostringstream errmsg;
      errmsg << "ERROR: Problem in call to cg_base_write. CGNS Error: '" << cg_get_error() << "'";
      IOSS_ERROR(errmsg);
    }
    ierr = cg_goto(cgnsFilePtr, base, "end");
    if (ierr != CG_OK) {
      // NOTE: Code will not continue past this call...
      std::ostringstream errmsg;
      errmsg << "ERROR: Problem in call to cg_goto. CGNS Error: '" << cg_get_error() << "'";
      IOSS_ERROR(errmsg);
    }
    cg_descriptor_write("Information", "IOSS: CGNS Writer version -1");

    // Output the sidesets as Family_t nodes

    const auto &sidesets = get_region()->get_sidesets();
    for (const auto &ss : sidesets) {
      int fam = 0;
      cg_family_write(cgnsFilePtr, base, ss->name().c_str(), &fam);
      int         bc_index = 0;
      CG_BCType_t bocotype = CG_BCTypeNull;
      if (ss->property_exists("bc_type")) {
        bocotype = (CG_BCType_t)ss->get_property("bc_type").get_int();
      }

      int64_t id = fam;
      if (ss->property_exists("id")) {
        id = ss->get_property("id").get_int();
      }

      cg_fambc_write(cgnsFilePtr, base, fam, "FamBC", bocotype, &bc_index);

      ierr = cg_goto(cgnsFilePtr, base, "Family_t", fam, NULL);
      if (ierr != CG_OK) {
        // NOTE: Code will not continue past this call...
        std::ostringstream errmsg;
        errmsg << "ERROR: Problem call cg_goto for node " << fam << ", CGNS Error: '"
               << cg_get_error() << "'";
        IOSS_ERROR(errmsg);
      }
      cg_descriptor_write("FamBC_TypeId", Ioss::Utils::to_string(bocotype).c_str());
      cg_descriptor_write("FamBC_TypeName", BCTypeName[bocotype]);
      cg_descriptor_write("FamBC_UserId", Ioss::Utils::to_string(id).c_str());
      cg_descriptor_write("FamBC_UserName", ss->name().c_str());
    }

    const auto &element_blocks = get_region()->get_element_blocks();
    for (const auto &eb : element_blocks) {
      int      zone    = 0;
      cgsize_t size[3] = {1, 0, 0};
      size[1]          = eb->get_property("entity_count").get_int();
      ierr = cg_zone_write(cgnsFilePtr, base, eb->name().c_str(), size, CG_Unstructured, &zone);
      if (ierr != CG_OK) {
        // NOTE: Code will not continue past this call...
        std::ostringstream errmsg;
        errmsg << "ERROR: Problem call cg_zone_write for node " << zone << ", CGNS Error: '"
               << cg_get_error() << "'";
        IOSS_ERROR(errmsg);
      }
    }

    const auto &structured_blocks = get_region()->get_structured_blocks();
    for (const auto &sb : structured_blocks) {
      int      zone    = 0;
      cgsize_t size[9] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
      size[3]          = sb->get_property("ni_global").get_int();
      size[4]          = sb->get_property("nj_global").get_int();
      size[5]          = sb->get_property("nk_global").get_int();

      size[0] = size[3] + 1;
      size[1] = size[4] + 1;
      size[2] = size[5] + 1;

      ierr = cg_zone_write(cgnsFilePtr, base, sb->name().c_str(), size, CG_Structured, &zone);
      if (ierr != CG_OK) {
        // NOTE: Code will not continue past this call...
        std::ostringstream errmsg;
        errmsg << "ERROR: Problem in call to cg_zone_write() for zone " << sb->name()
               << ", CGNS Error: '" << cg_get_error() << "'";
        IOSS_ERROR(errmsg);
      }

      // Add GridCoordinates Node...
      int grid_idx = 0;
      cg_grid_write(cgnsFilePtr, base, zone, "GridCoordinates", &grid_idx);

      // Transfer boundary condition nodes...
      for (const auto &bc : sb->m_boundaryConditions) {
        cgsize_t bc_idx   = 0;
        cgsize_t range[6] = {bc.m_rangeBeg[0], bc.m_rangeBeg[1], bc.m_rangeBeg[2],
                             bc.m_rangeEnd[0], bc.m_rangeEnd[1], bc.m_rangeEnd[2]};
        cg_boco_write(cgnsFilePtr, base, zone, bc.m_bcName.c_str(), CG_FamilySpecified,
                      CG_PointRange, 2, range, &bc_idx);

        ierr = cg_goto(cgnsFilePtr, base, sb->name().c_str(), 0, "ZoneBC_t", 1, bc.m_bcName.c_str(),
                       0, "end");
        if (ierr != CG_OK) {
          std::ostringstream errmsg;
          errmsg << "ERROR: Problem call cg_goto for BC node " << bc.m_bcName << ", CGNS Error: '"
                 << cg_get_error() << "'";
          IOSS_ERROR(errmsg);
        }
        cg_famname_write(bc.m_bcName.c_str());

        cg_boco_gridlocation_write(cgnsFilePtr, base, zone, bc_idx, CG_Vertex);
      }

      // Transfer Zone Grid Connectivity...
      for (const auto &zgc : sb->m_zoneConnectivity) {
        if (zgc.m_intraBlock) {
          continue;
        }
        cgsize_t zgc_idx = 0;
        cg_1to1_write(cgnsFilePtr, base, zone, zgc.m_connectionName.c_str(),
                      zgc.m_donorName.c_str(), &zgc.m_ownerRange[0], &zgc.m_donorRange[0],
                      &zgc.m_transform[0], &zgc_idx);
      }
    }
  }

  bool ParallelDatabaseIO::begin(Ioss::State /* state */) { return true; }

  bool ParallelDatabaseIO::end(Ioss::State state)
  {
    // Transitioning out of state 'state'
    switch (state) {
    case Ioss::STATE_DEFINE_MODEL:
      if (!is_input() && open_create_behavior() != Ioss::DB_APPEND) {
        write_meta_data();
      }
      break;
    default: // ignore everything else...
      break;
    }
    return true;
  }

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
      }
    }
    else {
      assert(1 == 0);
    }
    std::ostringstream errmsg;
    errmsg << "INTERNAL ERROR: Invalid map type. "
           << "Something is wrong in the Iocgns::ParallelDatabaseIO::get_map() function. "
           << "Please report.\n";
    IOSS_ERROR(errmsg);
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

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::StructuredBlock *sb,
                                                 const Ioss::Field &field, void *data,
                                                 size_t data_size) const
  {
    cgsize_t num_to_get = field.verify(data_size);

    Ioss::Field::RoleType role = field.get_role();
    cgsize_t              base = sb->get_property("base").get_int();
    cgsize_t              zone = sb->get_property("zone").get_int();

    cgsize_t rmin[3] = {0, 0, 0};
    cgsize_t rmax[3] = {0, 0, 0};

    if (role == Ioss::Field::MESH) {
      bool cell_field = true;
      if (field.get_name() == "mesh_model_coordinates" ||
          field.get_name() == "mesh_model_coordinates_x" ||
          field.get_name() == "mesh_model_coordinates_y" ||
          field.get_name() == "mesh_model_coordinates_z" || field.get_name() == "cell_node_ids") {
        cell_field = false;
      }

      if (cell_field) {
        assert(num_to_get == sb->get_property("cell_count").get_int());
        if (num_to_get > 0) {
          rmin[0] = sb->get_property("offset_i").get_int() + 1;
          rmin[1] = sb->get_property("offset_j").get_int() + 1;
          rmin[2] = sb->get_property("offset_k").get_int() + 1;

          rmax[0] = rmin[0] + sb->get_property("ni").get_int() - 1;
          rmax[1] = rmin[1] + sb->get_property("nj").get_int() - 1;
          rmax[2] = rmin[2] + sb->get_property("nk").get_int() - 1;
        }
      }
      else {
        // cell nodal field.
        assert(num_to_get == sb->get_property("node_count").get_int());
        if (num_to_get > 0) {
          rmin[0] = sb->get_property("offset_i").get_int() + 1;
          rmin[1] = sb->get_property("offset_j").get_int() + 1;
          rmin[2] = sb->get_property("offset_k").get_int() + 1;

          rmax[0] = rmin[0] + sb->get_property("ni").get_int();
          rmax[1] = rmin[1] + sb->get_property("nj").get_int();
          rmax[2] = rmin[2] + sb->get_property("nk").get_int();
        }
      }

      assert(num_to_get == 0 ||
             num_to_get ==
                 (rmax[0] - rmin[0] + 1) * (rmax[1] - rmin[1] + 1) * (rmax[2] - rmin[2] + 1));
      double *rdata = static_cast<double *>(data);

      if (field.get_name() == "mesh_model_coordinates_x") {
        int ierr =
#if CG_BUILD_PARALLEL
            cgp_coord_read_data(cgnsFilePtr, base, zone, 1, rmin, rmax, rdata);
#else
            cg_coord_read(cgnsFilePtr, base, zone, "CoordinateX", CG_RealDouble, rmin, rmax, rdata);
#endif
        if (ierr < 0) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }
      }

      else if (field.get_name() == "mesh_model_coordinates_y") {
        int ierr =
#if CG_BUILD_PARALLEL
            cgp_coord_read_data(cgnsFilePtr, base, zone, 2, rmin, rmax, rdata);
#else
            cg_coord_read(cgnsFilePtr, base, zone, "CoordinateY", CG_RealDouble, rmin, rmax, rdata);
#endif
        if (ierr < 0) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }
      }

      else if (field.get_name() == "mesh_model_coordinates_z") {
        int ierr =
#if CG_BUILD_PARALLEL
            cgp_coord_read_data(cgnsFilePtr, base, zone, 3, rmin, rmax, rdata);
#else
            cg_coord_read(cgnsFilePtr, base, zone, "CoordinateZ", CG_RealDouble, rmin, rmax, rdata);
#endif
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
#if CG_BUILD_PARALLEL
        int ierr = cgp_coord_read_data(cgnsFilePtr, base, zone, 1, rmin, rmax, TOPTR(coord));
#else
        int ierr = cg_coord_read(cgnsFilePtr, base, zone, "CoordinateX", CG_RealDouble, rmin, rmax,
                                 TOPTR(coord));
#endif
        if (ierr < 0) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }

        // Map to global coordinate position...
        for (cgsize_t i = 0; i < num_to_get; i++) {
          rdata[phys_dimension * i + 0] = coord[i];
        }

        if (phys_dimension >= 2) {
#if CG_BUILD_PARALLEL
          ierr = cgp_coord_read_data(cgnsFilePtr, base, zone, 2, rmin, rmax, TOPTR(coord));
#else
          ierr = cg_coord_read(cgnsFilePtr, base, zone, "CoordinateY", CG_RealDouble, rmin, rmax,
                               TOPTR(coord));
#endif
          if (ierr < 0) {
            Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
          }

          // Map to global coordinate position...
          for (cgsize_t i = 0; i < num_to_get; i++) {
            rdata[phys_dimension * i + 1] = coord[i];
          }
        }

        if (phys_dimension == 3) {
#if CG_BUILD_PARALLEL
          ierr = cgp_coord_read_data(cgnsFilePtr, base, zone, 3, rmin, rmax, TOPTR(coord));
#else
          ierr = cg_coord_read(cgnsFilePtr, base, zone, "CoordinateZ", CG_RealDouble, rmin, rmax,
                               TOPTR(coord));
#endif
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
        if (field.get_type() == Ioss::Field::INT64) {
          int64_t *idata = static_cast<int64_t *>(data);
          sb->get_cell_node_ids(idata, true);
        }
        else {
          assert(field.get_type() == Ioss::Field::INT32);
          int *idata = static_cast<int *>(data);
          sb->get_cell_node_ids(idata, true);
        }
      }
      else if (field.get_name() == "cell_ids") {
        if (field.get_type() == Ioss::Field::INT64) {
          int64_t *idata = static_cast<int64_t *>(data);
          sb->get_cell_ids(idata, true);
        }
        else {
          assert(field.get_type() == Ioss::Field::INT32);
          int *idata = static_cast<int *>(data);
          sb->get_cell_ids(idata, true);
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
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::StructuredBlock *sb,
                                                 const Ioss::Field &field, void *data,
                                                 size_t data_size) const
  {
    Ioss::Field::RoleType role = field.get_role();
    cgsize_t              base = sb->get_property("base").get_int();
    cgsize_t              zone = sb->get_property("zone").get_int();

    cgsize_t num_to_get = field.verify(data_size);

    cgsize_t rmin[3] = {0, 0, 0};
    cgsize_t rmax[3] = {0, 0, 0};

    if (role == Ioss::Field::MESH) {
      bool cell_field = true;
      if (field.get_name() == "mesh_model_coordinates" ||
          field.get_name() == "mesh_model_coordinates_x" ||
          field.get_name() == "mesh_model_coordinates_y" ||
          field.get_name() == "mesh_model_coordinates_z" || field.get_name() == "cell_node_ids") {
        cell_field = false;
      }

      if (cell_field) {
        assert(num_to_get == sb->get_property("cell_count").get_int());
        if (num_to_get > 0) {
          rmin[0] = sb->get_property("offset_i").get_int() + 1;
          rmin[1] = sb->get_property("offset_j").get_int() + 1;
          rmin[2] = sb->get_property("offset_k").get_int() + 1;

          rmax[0] = rmin[0] + sb->get_property("ni").get_int() - 1;
          rmax[1] = rmin[1] + sb->get_property("nj").get_int() - 1;
          rmax[2] = rmin[2] + sb->get_property("nk").get_int() - 1;
        }
      }
      else {
        // cell nodal field.
        assert(num_to_get == sb->get_property("node_count").get_int());
        if (num_to_get > 0) {
          rmin[0] = sb->get_property("offset_i").get_int() + 1;
          rmin[1] = sb->get_property("offset_j").get_int() + 1;
          rmin[2] = sb->get_property("offset_k").get_int() + 1;

          rmax[0] = rmin[0] + sb->get_property("ni").get_int();
          rmax[1] = rmin[1] + sb->get_property("nj").get_int();
          rmax[2] = rmin[2] + sb->get_property("nk").get_int();
        }
      }

      assert(num_to_get == 0 ||
             num_to_get ==
                 (rmax[0] - rmin[0] + 1) * (rmax[1] - rmin[1] + 1) * (rmax[2] - rmin[2] + 1));
      double *rdata = num_to_get > 0 ? static_cast<double *>(data) : nullptr;

      int crd_idx = 0;
      if (field.get_name() == "mesh_model_coordinates_x") {
        int ierr =
#if CG_BUILD_PARALLEL
            cgp_coord_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateX", &crd_idx);
        cgp_coord_write_data(cgnsFilePtr, base, zone, crd_idx, rmin, rmax, rdata);
#else
            cg_coord_partial_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateX", rmin,
                                   rmax, rdata, &crd_idx);
#endif
        if (ierr != CG_OK) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }
      }

      else if (field.get_name() == "mesh_model_coordinates_y") {
        int ierr =
#if CG_BUILD_PARALLEL
            cgp_coord_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateY", &crd_idx);
        cgp_coord_write_data(cgnsFilePtr, base, zone, crd_idx, rmin, rmax, rdata);
#else
            cg_coord_partial_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateY", rmin,
                                   rmax, rdata, &crd_idx);
#endif
        if (ierr != CG_OK) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }
      }

      else if (field.get_name() == "mesh_model_coordinates_z") {
        int ierr =
#if CG_BUILD_PARALLEL
            cgp_coord_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateZ", &crd_idx);
        cgp_coord_write_data(cgnsFilePtr, base, zone, crd_idx, rmin, rmax, rdata);
#else
            cg_coord_partial_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateZ", rmin,
                                   rmax, rdata, &crd_idx);
#endif
        if (ierr != CG_OK) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }
      }

      else if (field.get_name() == "mesh_model_coordinates") {
        int phys_dimension = get_region()->get_property("spatial_dimension").get_int();

        // Data required by upper classes store x0, y0, z0, ... xn,
        // yn, zn. Data stored in cgns file is x0, ..., xn, y0,
        // ..., yn, z0, ..., zn so we have to allocate some scratch
        // memory to read in the data and then map into supplied
        // 'data'
        std::vector<double> coord(num_to_get);
        // Map to global coordinate position...
        for (cgsize_t i = 0; i < num_to_get; i++) {
          coord[i] = rdata[phys_dimension * i + 0];
        }

        int ierr;
#if CG_BUILD_PARALLEL
        cgp_coord_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateX", &crd_idx);
        ierr = cgp_coord_write_data(cgnsFilePtr, base, zone, crd_idx, rmin, rmax, TOPTR(coord));
#else
        ierr = cg_coord_partial_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateX", rmin,
                                      rmax, rdata, &crd_idx);
#endif
        if (ierr != CG_OK) {
          Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
        }

        if (phys_dimension >= 2) {
          // Map to global coordinate position...
          for (cgsize_t i = 0; i < num_to_get; i++) {
            coord[i] = rdata[phys_dimension * i + 1];
          }
#if CG_BUILD_PARALLEL
          cgp_coord_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateY", &crd_idx);
          ierr = cgp_coord_write_data(cgnsFilePtr, base, zone, crd_idx, rmin, rmax, TOPTR(coord));
#else
          ierr = cg_coord_partial_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateY", rmin,
                                        rmax, rdata, &crd_idx);
#endif
          if (ierr != CG_OK) {
            Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
          }
        }

        if (phys_dimension == 3) {
          // Map to global coordinate position...
          for (cgsize_t i = 0; i < num_to_get; i++) {
            coord[i] = rdata[phys_dimension * i + 2];
          }

#if CG_BUILD_PARALLEL
          cgp_coord_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateZ", &crd_idx);
          ierr = cgp_coord_write_data(cgnsFilePtr, base, zone, crd_idx, rmin, rmax, TOPTR(coord));
#else
          ierr = cg_coord_partial_write(cgnsFilePtr, base, zone, CG_RealDouble, "CoordinateZ", rmin,
                                        rmax, rdata, &crd_idx);
#endif
          if (ierr != CG_OK) {
            Utils::cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, myProcessor);
          }
        }
      }
    }
    return num_to_get;
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
