// Copyright(C) 2016
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

#include <Iocgns_StructuredZoneData.h>
#include <Ionit_Initializer.h>
#include <Ioss_CodeTypes.h>
#include <Ioss_FileInfo.h>
#include <Ioss_ParallelUtils.h>
#include <Ioss_SubSystem.h>
#include <Ioss_SurfaceSplit.h>
#include <Ioss_TerminalColor.h>
#include <Ioss_Utils.h>

#include <algorithm>
#include <cassert>
#include <chrono>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <stddef.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <vector>

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#define OUTPUT                                                                                     \
  if (rank == 0)                                                                                   \
  std::cerr

// ========================================================================

namespace {

  double timer()
  {
#ifdef HAVE_MPI
    return MPI_Wtime();
#else
    static auto begin = std::chrono::high_resolution_clock::now();

    auto now = std::chrono::high_resolution_clock::now();
    return std::chrono::duration<double>(now - begin).count();
#endif
  }

  int rank = 0;

  void transfer_nodal(Ioss::Region &region, Ioss::Region &output_region);
  void transfer_connectivity(Ioss::Region &region, Ioss::Region &output_region);
  void output_sidesets(Ioss::Region &region, Ioss::Region &output_region);
  
  void transfer_nodeblock(Ioss::Region &region, Ioss::Region &output_region);
  void transfer_elementblocks(Ioss::Region &region, Ioss::Region &output_region);
  void transfer_sidesets(Ioss::Region &region, Ioss::Region &output_region);
  void create_unstructured(const std::string &input, const std::string &output);

  size_t transfer_coord(std::vector<double> &to, std::vector<double> &from,
                        std::vector<size_t> &node_id_list, size_t offset)
  {
    if (!node_id_list.empty()) {
      for (size_t i = 0; i < node_id_list.size(); i++) {
        size_t node = node_id_list[i];
        to[node]    = from[i];
      }
    }
    else {
      for (auto x : from) {
        to[offset++] = x;
      }
    }
    return offset;
  }

  size_t generate_shared_nodes(std::map<const std::string&, std::vector<size_t>> &node_list,
			       const Ioss::Region &region)
  {
#if 0
    // First step in generating the map from "cell-node" to global node position
    // in the model with all duplicate nodes equived out.
    int my_processor = region.get_database()->parallel_rank();

    size_t num_duplicate = 0; // Number of contiguous nodes.

    // Iterate through all zoneConnectivity (zgc) instances.  For each
    // zgc containing non-owned nodes, set the value of
    // m_blockLocalNodeIndex to point to the owning nodes
    // global_node_offset.  If a node in this block already has a
    // value, then that node is shared multiple times (at a corner of
    // three blocks) and need to resolve which of the nodes it points
    // to is the owner...
    for (const auto &zgc : m_zoneConnectivity) {
      if (!zgc.m_intraBlock) { // Not due to processor decomposition.
	std::vector<int> i_range = zgc.get_range(1);
	std::vector<int> j_range = zgc.get_range(2);
	std::vector<int> k_range = zgc.get_range(3);

	// NOTE: In parallel, the owner block should exist, but may not have
	// any cells on this processor.  We can access its global i,j,k, but
	// don't store or access any "bulk" data on it.
	auto owner_block = region.get_structured_block(zgc.m_donorName);
	assert(owner_block != nullptr);

	const std::array<int, 9> t_matrix = zgc.transform_matrix();
	for (auto &k : k_range) {
	  for (auto &j : j_range) {
	    for (auto &i : i_range) {
	      std::array<int, 3> index{{i, j, k}};
	      std::array<int, 3> owner = zgc.transform(t_matrix, index);
	      
	      // block-local location of the node in this block; 0-based
	      size_t block_local_offset = get_block_local_node_offset(index[0], index[1], index[2]);

	      size_t block_global_offset = get_global_node_offset(index[0], index[1], index[2])+1;
	      size_t owner_block_global_offset = owner_block->get_global_node_offset(owner[0], owner[1], owner[2])+1;

	      if (block_global_offset != owner_block_global_offset) {
		std::cerr << "Node at offset " << block_local_offset << " in block " << name() << " maps to global offsets "
			  << block_global_offset << " and " << owner_block_global_offset << "\n";
		size_t new_id = std::min(block_global_offset, owner_block_global_offset);
		if (new_id < m_blockLocalNodeIndex[block_local_offset]) {
		  m_blockLocalNodeIndex[block_local_offset] = new_id;
		  num_duplicate++;
		}
	      }
	    }
	  }
	}
      }
      else {
	// This zgc is the result of processor decomposition.
	// The adam zones of owner and donor should be the same.
	assert(1==0 && "TODO: Implement parallel decomposition node id mapping");
      }
    }
#if 0
      if (zgc.m_donorProcessor == my_processor) {
	if (!zgc.owns_shared_nodes()) {
	  // Iterate over the range of nodes on the interface...
	  std::vector<int> i_range = zgc.get_range(1);
	  std::vector<int> j_range = zgc.get_range(2);
	  std::vector<int> k_range = zgc.get_range(3);

	  auto owner_block = region.get_structured_block(zgc.m_donorName);
	  assert(owner_block != nullptr);
	  assert(!owner_block->m_blockLocalNodeIndex.empty());

	  const std::array<int, 9> t_matrix = zgc.transform_matrix();
	  for (auto &k : k_range) {
	    for (auto &j : j_range) {
	      for (auto &i : i_range) {
		std::array<int, 3> index{{i, j, k}};
		std::array<int, 3> owner = zgc.transform(t_matrix, index);

		if (zgc.m_ownerZone != zgc.m_donorZone) {
		  // Convert main and owner i,j,k triplets into model-local m_blockLocalNodeIndex
		  size_t block_local_offset =
                    get_block_local_node_offset(index[0], index[1], index[2]);
		  size_t local_offset =
                    owner_block->get_local_node_offset(owner[0], owner[1], owner[2]);

		  if (m_blockLocalNodeIndex[block_local_offset] != ss_max) {
		    // This node maps to two different nodes -- probably at a 3-way corner
		    // Need to adjust the node in 'owner_block' with id 'local_offset'
		    // to instead point to 'm_blockLocalNodeIndex[block_local_offset]'
		    size_t owner_offset =
                      owner_block->get_block_local_node_offset(owner[0], owner[1], owner[2]);
		    owner_block->m_blockLocalNodeIndex[owner_offset] =
                      m_blockLocalNodeIndex[block_local_offset];
		  }
		  else {
		    m_blockLocalNodeIndex[block_local_offset] = local_offset;
		  }
		}
		else {
		  // When mapping WITHIN a zone, need to avoid circular A->B and B->A.
		  // The GridConnectivity object will appear twice; once with each surface
		  // being the "owner"...
		  // Convert main and owner i,j,k triplets into zone-local offsets
		  size_t local_node = get_block_local_node_offset(index[0], index[1], index[2]);
		  size_t owner_node = get_block_local_node_offset(owner[0], owner[1], owner[2]);
		  if (owner_node < local_node) {
		    size_t local_offset = get_local_node_offset(owner[0], owner[1], owner[2]);
		    m_blockLocalNodeIndex[local_node] = local_offset;
		  }
		}
	      }
	    }
	  }
	}
      }
      else {
	if (zgc.m_donorName != name()) {
	// This zgc has a donorZone which is on a different processor...
	// Iterate over the range of nodes on the interface...
	std::vector<int> i_range = zgc.get_range(1);
	std::vector<int> j_range = zgc.get_range(2);
	std::vector<int> k_range = zgc.get_range(3);

	auto owner_block = region.get_structured_block(zgc.m_donorName);
	assert(owner_block != nullptr);

	const std::array<int, 9> t_matrix = zgc.transform_matrix();
	for (auto &k : k_range) {
	  for (auto &j : j_range) {
	    for (auto &i : i_range) {
	      std::array<int, 3> index{{i, j, k}};
	      std::array<int, 3> owner = zgc.transform(t_matrix, index);
	      
	      // block-local location of the node in this block; 0-based
	      size_t block_local_offset = get_block_local_node_offset(index[0], index[1], index[2]);

	      size_t block_global_offset = get_global_node_offset(index[0], index[1], index[2]);
	      size_t owner_block_global_offset = owner_block->get_global_node_offset(owner[0], owner[1], owner[2]);

	      if (block_global_offset != owner_block_global_offset) {
		std::cerr << "Node at offset " << block_local_offset << " in block " << name() << " maps to global offsets "
			  << block_global_offset << " and " << owner_block_global_offset << "\n";
		m_globalNodeIdList.emplace_back(block_local_offset, std::min(block_global_offset, owner_block_global_offset)+1);
	      }
	    }
	  }
	}
	}
      }
    }
#endif
    // At this point, the vector contains either "owned nodes" which have an entry
    // of 'ss_max', or shared nodes that it doesn't own which point to the global cell-node
    // offset of the owning node.
  return num_duplicate;
#endif
  return 0;
  }
    
} // namespace
// ========================================================================

namespace {
  std::string codename;
  std::string version = "4.7";
} // namespace

int main(int argc, char *argv[])
{
#ifdef HAVE_MPI
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#endif

  Ioss::Init::Initializer io;
  std::string             in_file = argv[1];
  std::string             out_file;
  if (argc > 2) {
    out_file = argv[2];
  }
  else {
    OUTPUT << "ERROR: Syntax is " << argv[0] << " {structured_input} {unstructured_output}\n";
    return EXIT_FAILURE;
  }

  OUTPUT << "Structured Input:    '" << in_file << "'\n";
  OUTPUT << "Unstructured Output: '" << out_file << "'\n";
  OUTPUT << '\n';

  double begin = timer();
  create_unstructured(in_file, out_file);
  double end = timer();

  OUTPUT << "\n\tElapsed time = " << end - begin << " seconds.\n";

  OUTPUT << "\n" << codename << " execution successful.\n";
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
  return EXIT_SUCCESS;
}

namespace {
  void create_unstructured(const std::string &inpfile, const std::string &outfile)
  {
    Ioss::PropertyManager properties;
    Ioss::DatabaseIO *    dbi = Ioss::IOFactory::create("cgns", inpfile, Ioss::READ_MODEL,
							(MPI_Comm)MPI_COMM_WORLD, properties);
    if (dbi == nullptr || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    // NOTE: 'region' owns 'db' pointer at this time...
    Ioss::Region region(dbi, "region_1");

    //========================================================================
    // OUTPUT ...
    //========================================================================
    Ioss::DatabaseIO *dbo = Ioss::IOFactory::create("exodus", outfile, Ioss::WRITE_RESTART,
                                                    (MPI_Comm)MPI_COMM_WORLD, properties);
    if (dbo == nullptr || !dbo->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    // NOTE: 'output_region' owns 'dbo' pointer at this time
    Ioss::Region output_region(dbo, "region_2");
    // Set the qa information...
    output_region.property_add(Ioss::Property(std::string("code_name"), codename));
    output_region.property_add(Ioss::Property(std::string("code_version"), version));

    if (!output_region.begin_mode(Ioss::STATE_DEFINE_MODEL)) {
      OUTPUT << "ERROR: Could not put output region into define model state\n";
      std::exit(EXIT_FAILURE);
    }

    transfer_nodeblock(region, output_region);
    transfer_elementblocks(region, output_region);
    transfer_sidesets(region, output_region);

    output_region.end_mode(Ioss::STATE_DEFINE_MODEL);

    // Model defined, now fill in the model data...
    output_region.begin_mode(Ioss::STATE_MODEL);

    transfer_nodal(region, output_region);
    transfer_connectivity(region, output_region);
    output_sidesets(region, output_region);
    
    output_region.end_mode(Ioss::STATE_MODEL);
  }

  void transfer_nodal(Ioss::Region &region, Ioss::Region &output_region)
  {
    auto nb = output_region.get_node_blocks()[0];
    size_t node_count = region.get_node_blocks()[0]->get_property("entity_count").get_int();
    {
      std::vector<int> ids(node_count); // To hold the global node id map.
      auto &           blocks = region.get_structured_blocks();
      for (auto &block : blocks) {
        std::vector<int> cell_id;
        block->get_field_data("cell_node_ids", cell_id);

	for (auto idx_id : block->m_globalIdMap) {
	  cell_id[idx_id.first] = idx_id.second;
	}

	for (size_t i=0; i < cell_id.size(); i++) {
	  size_t idx = block->m_blockLocalNodeIndex[i];
	  if (ids[idx] == 0) {
	    ids[idx] = cell_id[i];
	  }
	}

      }
      assert(nb != nullptr);
      nb->put_field_data("ids", ids);
    }

    std::vector<double> coordinate_x(node_count);
    std::vector<double> coordinate_y(node_count);
    std::vector<double> coordinate_z(node_count);

    size_t offset = 0; // Used only until parallel shared nodes figured out.
    auto & blocks = region.get_structured_blocks();
    for (auto &block : blocks) {
      std::vector<double> coord_tmp;
      block->get_field_data("mesh_model_coordinates_x", coord_tmp);
      transfer_coord(coordinate_x, coord_tmp, block->m_blockLocalNodeIndex, offset);

      block->get_field_data("mesh_model_coordinates_y", coord_tmp);
      transfer_coord(coordinate_y, coord_tmp, block->m_blockLocalNodeIndex, offset);

      block->get_field_data("mesh_model_coordinates_z", coord_tmp);
      offset = transfer_coord(coordinate_z, coord_tmp, block->m_blockLocalNodeIndex, offset);
    }
    nb->put_field_data("mesh_model_coordinates_x", coordinate_x);
    nb->put_field_data("mesh_model_coordinates_y", coordinate_y);
    nb->put_field_data("mesh_model_coordinates_z", coordinate_z);
  }

  void transfer_connectivity(Ioss::Region &region, Ioss::Region &output_region)
  {
    auto &blocks = region.get_structured_blocks();
    for (auto &block : blocks) {
      // We have a structured block of size ni x nj x nk.
      // Need to convert that to element connectivity
      // Node numbers are zero-based offset into this structured block
      // After generated, then map zero-based block-local into one-based global.
      // Since we are outputting "connectivity_raw", there is no difference
      // in parallel or serial.
      
      size_t ni = block->get_property("ni").get_int();
      size_t nj = block->get_property("nj").get_int();
      size_t nk = block->get_property("nk").get_int();

      size_t xp1yp1 = (ni + 1) * (nj + 1);

      // Find matching element block in output region...
      const auto &name   = block->name();
      auto *      output = output_region.get_element_block(name);
      assert(output != nullptr);

      {
        std::vector<int> connect;
	connect.reserve(ni*nj*nk*8);
        for (size_t k = 0; k < nk; k++) {
          for (size_t j = 0, m = 0; j < nj; j++) {
            for (size_t i = 0; i < ni; i++, m++) {
              size_t base = (k * xp1yp1) + m + j;

              connect.push_back(base);
              connect.push_back(base + 1);
              connect.push_back(base + ni + 2);
              connect.push_back(base + ni + 1);

              connect.push_back(xp1yp1 + base);
              connect.push_back(xp1yp1 + base + 1);
              connect.push_back(xp1yp1 + base + ni + 2);
              connect.push_back(xp1yp1 + base + ni + 1);
            }
          }
        }
        // 'connect' contains 0-based block-local node ids at this point
        // Now, map them to processor-global values...
	// NOTE: "processor-global" is 1..num_node_on_processor

        const auto &gnil = block->m_blockLocalNodeIndex;
	if (!gnil.empty()) {
	  for (size_t i = 0; i < connect.size(); i++) {
	    connect[i] = gnil[connect[i]] + 1;
	  }
	}
	else {
	  size_t node_offset = block->get_node_offset();
	  for (size_t i = 0; i < connect.size(); i++) {
	    connect[i] = connect[i] + node_offset + 1;
	  }
	}

        output->put_field_data("connectivity_raw", connect);
      }

      {
        std::vector<int> ids;
        block->get_field_data("cell_ids", ids);
	output->put_field_data("ids", ids);
      }
    }
    return;
  }

  void output_sidesets(Ioss::Region &region, Ioss::Region &output_region)
  {
    // Maps the 'parent_face'+3 returned from which_parent_face()
    // to the local 1-based face of the hex elements in that block.
    static int face_map[] = {5, 1, 4, 0, 2, 3, 6};

    const auto &ssets = region.get_sidesets();
    for (auto ss : ssets) {
      // Get corresponding sidset on output region...
      auto ofs = output_region.get_sideset(ss->name());
      assert(ofs != nullptr);
      
      const auto &fbs  = ss->get_side_blocks();
      size_t fb_index = 0;
      for (auto fb : fbs) {
	// Get corresponding sideblock on output sideset 'ofs'
	// Assumes sideblocks are ordered the same on input and output.
	auto ofb = ofs->get_block(fb_index++);
	assert(ofb != nullptr);
	
	// Get parent structured block for this side block...
	auto parent = fb->parent_block();
	assert(parent != nullptr);
	assert(parent->type() == Ioss::STRUCTUREDBLOCK);
	auto sb_parent = dynamic_cast<const Ioss::StructuredBlock*>(parent);
	assert(sb_parent != nullptr);

	// Find this sideblock on the parent block...
	auto &bc_name = fb->name();
	for (auto &bc : sb_parent->m_boundaryConditions) {
	  if (bc_name == bc.m_bcName) {
	    std::vector<int> elem_side;
	    if (bc.get_face_count() > 0) {
	    std::array<int, 3> range_beg = bc.m_rangeBeg;
	    std::array<int, 3> cell_range_end = bc.m_rangeEnd;

	    // The range_beg/end are current points and not cells.
	    // Need to convert cell_range_end to cells which is typically just point-1
	    // except if the ordinal is the plane of this surface. In this case,
	    // range_beg[ord] == cell_range_end[ord].  If this is the case,
	    // and we are at the top end of the range, then the beg and end must
	    // both be reduced by 1.  If at the bottom end of the range, then both
	    // are equal to 1.
	    // 
	    for (int i=0; i < 3; i++) {
	      if (cell_range_end[i] == range_beg[i]) {
		if (cell_range_end[i] != 1) {
		  cell_range_end[i]--;
		  range_beg[i]--;
		}
	      }
	      else {
		cell_range_end[i]--;
	      }
	    }

	    std::cerr << bc << "\n";
	    auto parent_face = face_map[bc.which_parent_face()+3];
	    elem_side.reserve(bc.get_face_count() * 2);
	    for (auto k=range_beg[2]; k <= cell_range_end[2]; k++) {
	      for (auto j=range_beg[1]; j <= cell_range_end[1]; j++) {
		for (auto i=range_beg[0]; i <= cell_range_end[0]; i++) {
		  auto cell_id = sb_parent->get_global_cell_id(i, j, k);
		  assert(cell_id > 0);
		  elem_side.push_back(cell_id);
		  elem_side.push_back(parent_face);
		}
	      }
	    }
	    }
	    ofb->put_field_data("element_side", elem_side);
	    break;
	  }
	}
      }
    }
  }

  void transfer_nodeblock(Ioss::Region &region, Ioss::Region &output_region)
  {
    const auto &nbs = region.get_node_blocks();
    assert(nbs.size() == 1);
    size_t degree    = nbs[0]->get_property("component_degree").get_int();
    size_t num_nodes = nbs[0]->get_property("entity_count").get_int();
    auto nb = new Ioss::NodeBlock(output_region.get_database(), nbs[0]->name(), num_nodes, degree);
    output_region.add(nb);

    std::cout << "P[" << rank << "] Number of coordinates per node ="
	      << std::setw(12) << degree << "\n";
    std::cout << "P[" << rank << "] Number of nodes                ="
	      << std::setw(12) << num_nodes << "\n";
  }

  void transfer_elementblocks(Ioss::Region &region, Ioss::Region &output_region)
  {
    const auto &blocks = region.get_structured_blocks();
    size_t total_entities = 0;
    for (auto iblock : blocks) {
      std::string name = iblock->name();
      std::string type  = "hex8";
      size_t      count = iblock->get_property("cell_count").get_int();
      auto block = new Ioss::ElementBlock(output_region.get_database(), name, type, count);
      output_region.add(block);
      std::cout << "P[" << rank << "] Created Element Block '" << name
		<< "' with " << count << " elements.\n";
      total_entities += count;
    }
    std::cout << "P[" << rank << "] Number of Element Blocks       ="
	      << std::setw(12) << blocks.size()
	      << ", Number of elements (cells) =" << std::setw(12) << total_entities << "\n";
  }

  void transfer_sidesets(Ioss::Region &region, Ioss::Region &output_region)
  {
    size_t total_sides = 0;
    const auto ssets = region.get_sidesets();
    for (auto ss : ssets) {
      std::string name = ss->name();
      
      int ss_sides = 0;
      auto                     surf = new Ioss::SideSet(output_region.get_database(), name);
      Ioss::SideBlockContainer fbs  = ss->get_side_blocks();
      for (auto fb : fbs) {
        std::string fbname = fb->name();
        std::string fbtype   = fb->get_property("topology_type").get_string();
        std::string partype  = fb->get_property("parent_topology_type").get_string();
        size_t      num_side = fb->get_property("entity_count").get_int();
        total_sides += num_side;

        auto block =
            new Ioss::SideBlock(output_region.get_database(), fbname, fbtype, partype, num_side);
        surf->add(block);
	block->property_add(Ioss::Property("set_offset", ss_sides));

	ss_sides += num_side;
      }
      output_region.add(surf);
    }
    std::cout << "P[" << rank << "] Number of SideSets             =" << std::setw(12) << ssets.size() << ", Number of cell faces       =" << std::setw(12) << total_sides << "\n";
  }

} // namespace
