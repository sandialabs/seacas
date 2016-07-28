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

  void transfer_nodeblock(Ioss::Region &region, Ioss::Region &output_region);
  void transfer_elementblocks(Ioss::Region &region, Ioss::Region &output_region);
  void create_unstructured(const std::string &input, const std::string &output);

  size_t transfer_coord(std::vector<double> &to, std::vector<double> &from,
                        std::vector<ssize_t> &node_id_list, size_t offset)
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

    output_region.end_mode(Ioss::STATE_DEFINE_MODEL);

    // Model defined, now fill in the model data...
    output_region.begin_mode(Ioss::STATE_MODEL);

    transfer_nodal(region, output_region);
    transfer_connectivity(region, output_region);

    output_region.end_mode(Ioss::STATE_MODEL);
  }

  void transfer_nodal(Ioss::Region &region, Ioss::Region &output_region)
  {
    auto nb = output_region.get_node_blocks()[0];
    size_t node_count = region.get_node_blocks()[0]->get_property("entity_count").get_int();
    {
      size_t           nod_ind = 0;
      std::vector<int> ids(node_count); // To hold the global node id map.
      auto &           blocks = region.get_structured_blocks();
      for (auto &block : blocks) {
        std::vector<int> cell_id;
        block->get_field_data("cell_node_ids", cell_id);
        if (!block->m_globalNodeIdList.empty()) {
          for (size_t i = 0; i < block->m_globalNodeIdList.size(); i++) {
            size_t node = block->m_globalNodeIdList[i];
            assert(node >= 0 && node < node_count);
            if (ids[node] == 0) {
              ids[node] = cell_id[i];
            }
          }
        }
        else {
          for (auto node : cell_id) {
            ids[nod_ind++] = node;
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
      transfer_coord(coordinate_x, coord_tmp, block->m_globalNodeIdList, offset);

      block->get_field_data("mesh_model_coordinates_y", coord_tmp);
      transfer_coord(coordinate_y, coord_tmp, block->m_globalNodeIdList, offset);

      block->get_field_data("mesh_model_coordinates_z", coord_tmp);
      offset = transfer_coord(coordinate_z, coord_tmp, block->m_globalNodeIdList, offset);
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

        const auto &gnil = block->m_globalNodeIdList;
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
} // namespace
