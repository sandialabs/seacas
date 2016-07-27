// Copyright(C) 1999-2010
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

  void transfer_coordinates(Ioss::Region &region, Ioss::Region &output_region);
  void transfer_connectivity(Ioss::Region &region, Ioss::Region &output_region);

  void transfer_nodeblock(Ioss::Region &region, Ioss::Region &output_region, bool debug);
  void transfer_elementblocks(Ioss::Region &region, Ioss::Region &output_region, bool debug);
  void file_copy(const std::string &input, const std::string &output);

  template <typename INT>
  void set_owned_node_count(Ioss::Region &region, int my_processor, INT dummy);
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
  OUTPUT << "Structured Input:    '" << in_file << "'\n";
  OUTPUT << "Unstructured Output: '" << out_file << "'\n";
  OUTPUT << '\n';

  double begin = timer();
  file_copy(in_file, out_file);
  double end = timer();

  OUTPUT << "\n\tElapsed time = " << end - begin << " seconds.\n";

  OUTPUT << "\n" << codename << " execution successful.\n";
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
  return EXIT_SUCCESS;
}

namespace {
  void file_copy(const std::string &inpfile, const std::string &outfile)
  {
    Ioss::PropertyManager properties;
    Ioss::DatabaseIO *    dbi = Ioss::IOFactory::create("cgns", inpfile, Ioss::READ_MODEL,
                                                    (MPI_Comm)MPI_COMM_WORLD, properties);
    if (dbi == nullptr || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    // NOTE: 'region' owns 'db' pointer at this time...
    std::cerr << "Creating Region\n";
    Ioss::Region region(dbi, "region_1");
    std::cerr << "Region Created\n";

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

    OUTPUT << "DEFINING MODEL ... \n";
    if (!output_region.begin_mode(Ioss::STATE_DEFINE_MODEL)) {
      OUTPUT << "ERROR: Could not put output region into define model state\n";
      std::exit(EXIT_FAILURE);
    }

    transfer_nodeblock(region, output_region, true);

#ifdef HAVE_MPI
    // This also assumes that the node order and count is the same for input
    // and output regions... (This is checked during nodeset output)
    if (output_region.get_database()->needs_shared_node_information()) {
      MPI_Comm_rank(MPI_COMM_WORLD, &rank);

      set_owned_node_count(region, rank, (int)0);
    }
#endif

    transfer_elementblocks(region, output_region, true);

    OUTPUT << "END STATE_DEFINE_MODEL... " << '\n';

    output_region.end_mode(Ioss::STATE_DEFINE_MODEL);

    OUTPUT << "TRANSFERRING MESH FIELD DATA ... " << '\n';

    // Model defined, now fill in the model data...
    output_region.begin_mode(Ioss::STATE_MODEL);

    transfer_coordinates(region, output_region);
    transfer_connectivity(region, output_region);

    OUTPUT << "END STATE_MODEL... " << '\n';
    output_region.end_mode(Ioss::STATE_MODEL);
  }

  void transfer_coordinates(Ioss::Region &region, Ioss::Region &output_region)
  {
    size_t glob_node_count = region.get_node_blocks()[0]->get_property("entity_count").get_int();
    auto   nb              = output_region.get_node_blocks()[0];

    {
      std::vector<int> ids(glob_node_count); // To hold the global node id map.
      auto &           blocks = region.get_structured_blocks();
      for (auto &block : blocks) {
	std::vector<int> cell_id;
	block->get_field_data("cell_node_ids", cell_id);
	for (size_t i = 0; i < block->m_globalNodeIdList.size(); i++) {
	  size_t node = block->m_globalNodeIdList[i];
	  assert(node >= 0 && node < glob_node_count);
	  if (ids[node] == 0) {
	    ids[node] = cell_id[i];
	  }
	}
      }
      nb->put_field_data("ids", ids);
    }

    std::vector<double> coordinate_x(glob_node_count, -100000);
    std::vector<double> coordinate_y(glob_node_count, -100000);
    std::vector<double> coordinate_z(glob_node_count, -100000);

    auto &blocks = region.get_structured_blocks();
    for (auto &block : blocks) {
      std::vector<double> coord_tmp;
      block->get_field_data("mesh_model_coordinates_x", coord_tmp);
      for (size_t i = 0; i < block->m_globalNodeIdList.size(); i++) {
        size_t node = block->m_globalNodeIdList[i];
        assert(node >= 0 && node < glob_node_count);
        coordinate_x[node] = coord_tmp[i];
      }
      block->get_field_data("mesh_model_coordinates_y", coord_tmp);
      for (size_t i = 0; i < block->m_globalNodeIdList.size(); i++) {
        size_t node = block->m_globalNodeIdList[i];
        assert(node >= 0 && node < glob_node_count);
        coordinate_y[node] = coord_tmp[i];
      }
      block->get_field_data("mesh_model_coordinates_z", coord_tmp);
      for (size_t i = 0; i < block->m_globalNodeIdList.size(); i++) {
        size_t node = block->m_globalNodeIdList[i];
        assert(node >= 0 && node < glob_node_count);
        coordinate_z[node] = coord_tmp[i];
      }
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
        for (size_t m = 0; m < nk; m++) {
          for (size_t i = 0, k = 0; i < nj; i++) {
            for (size_t j = 0; j < ni; j++, k++) {
              size_t base = (m * xp1yp1) + k + i;

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

        const auto &gnil = block->m_globalNodeIdList;

        for (size_t i = 0; i < connect.size(); i++) {
          connect[i] = gnil[connect[i]] + 1;
        }

        output->put_field_data("connectivity_raw", connect);
      }

      {
        size_t           cell_count = block->get_property("cell_count").get_int();
        std::vector<int> ids(cell_count);
        std::iota(ids.begin(), ids.end(), block->get_cell_offset() + 1);
        output->put_field_data("ids", ids);
      }
    }
    return;
  }

  void transfer_nodeblock(Ioss::Region &region, Ioss::Region &output_region, bool debug)
  {
    Ioss::NodeBlockContainer nbs = region.get_node_blocks();
    size_t                   id  = 1;
    for (auto inb : nbs) {
      auto & name      = inb->name();
      size_t num_nodes = inb->get_property("entity_count").get_int();
      size_t degree    = inb->get_property("component_degree").get_int();
      OUTPUT << " Number of coordinates per node       =" << std::setw(12) << degree << "\n";
      OUTPUT << " Number of nodes                      =" << std::setw(12) << num_nodes << "\n";

      auto nb = new Ioss::NodeBlock(output_region.get_database(), name, num_nodes, degree);
      output_region.add(nb);
      ++id;
    }
    if (debug) {
      OUTPUT << '\n';
    }
  }

  void transfer_elementblocks(Ioss::Region &region, Ioss::Region &output_region, bool debug)
  {
    auto &blocks = region.get_structured_blocks();
    if (!blocks.empty()) {
      size_t total_entities = 0;
      for (auto iblock : blocks) {
        std::string name = iblock->name();
        if (debug) {
          OUTPUT << name << ", ";
        }
        std::string type  = "hex8";
        size_t      count = iblock->get_property("cell_count").get_int();
        total_entities += count;

        auto block = new Ioss::ElementBlock(output_region.get_database(), name, type, count);
        output_region.add(block);
      }
      if (!debug) {
        OUTPUT << " Number of " << std::setw(14) << (*blocks.begin())->type_string()
               << "s            =" << std::setw(12) << blocks.size() << "\t"
               << "Length of entity list   =" << std::setw(12) << total_entities << "\n";
      }
      else {
        OUTPUT << '\n';
      }
    }
  }

  template <typename INT>
  void set_owned_node_count(Ioss::Region &region, int my_processor, INT /*dummy*/)
  {
    Ioss::NodeBlock *nb = region.get_node_block("nodeblock_1");
    if (nb->field_exists("owning_processor")) {
      std::vector<INT> my_data;
      nb->get_field_data("owning_processor", my_data);

      INT owned = std::count(my_data.begin(), my_data.end(), my_processor);
      nb->property_add(Ioss::Property("locally_owned_count", owned));

      // Set locally_owned_count property on all nodesets...
      Ioss::NodeSetContainer nss = region.get_nodesets();
      for (auto ns : nss) {

        std::vector<INT> ids;
        ns->get_field_data("ids_raw", ids);
        owned = 0;
        for (size_t n = 0; n < ids.size(); n++) {
          INT id = ids[n];
          if (my_data[id - 1] == my_processor) {
            owned++;
          }
        }
        ns->property_add(Ioss::Property("locally_owned_count", owned));
      }
    }
  }
} // namespace
