// Copyright(C) 1999-2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <functional>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

#include "Ionit_Initializer.h"
#include "Ioss_CodeTypes.h"
#include "Ioss_DBUsage.h"
#include "Ioss_DatabaseIO.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_FaceGenerator.h"
#include "Ioss_FileInfo.h"
#include "Ioss_IOFactory.h"
#include "Ioss_NodeBlock.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"
#include "Ioss_Region.h"
#include "Ioss_ScopeGuard.h"
#include "Ioss_SideBlock.h"
#include "Ioss_SideSet.h"
#include "Ioss_Utils.h"

#include <cassert>

#include "line_decomp_interface.h"

// ========================================================================

template <typename INT> using front_t = std::vector<std::pair<INT, int>>;
using connectivity_t                  = std::vector<std::array<const Ioss::Face *, 6>>;

template <typename INT> struct chain_entry_t
{
  bool    operator==(const chain_entry_t<INT> &other) { return (element == other.element); }
  int64_t element{}; // Element at root of chain
  int     link{};    // How far is this element in the chain (1-based)
};

template <typename INT> using chain_t = std::vector<chain_entry_t<INT>>;

namespace {
  template <typename INT> void line_decomp(Line_Decomp::Interface &interFace, INT /*dummy*/);
  std::string                  codename;
  std::string                  version = "0.99";

  // 0-based face
  int hex_opposite_side(int side)
  {
    switch (side) {
    case 0: return 2;
    case 1: return 3;
    case 2: return 0;
    case 3: return 1;
    case 4: return 5;
    case 5: return 6;
    }
    return -1;
  }

  std::vector<std::string> get_adjacent_blocks(Ioss::Region &region)
  {
    std::vector<std::string>      adjacent_blocks;
    const Ioss::SideSetContainer &fss = region.get_sidesets();
    for (auto &fs : fss) {
      // Save a list of all blocks that are adjacent to the surfaces...
      std::vector<std::string> blocks;
      fs->block_membership(blocks);
      for (const auto &block : blocks) {
        adjacent_blocks.push_back(block); // May have duplicates at this point.
      }
    }
    Ioss::Utils::uniquify(adjacent_blocks);
    return adjacent_blocks;
  }

  template <typename INT>
  front_t<INT> get_line_front(Ioss::Region &region, const std::string &adj_block,
                              chain_t<INT> &element_chains, INT /*dummy*/)
  {
    front_t<INT> front;

    // Since lines can not cross element blocks, we can process everything a block at a time.
    const auto *block = region.get_element_block(adj_block);
    assert(block != nullptr);
    if (block->topology()->shape() != Ioss::ElementShape::HEX) {
      fmt::print("Skipping Element Block {}; it does not contain HEX elements.\n", adj_block);
      return front;
    }

    fmt::print("Processing Element Block {}\n", adj_block);

    // Get the offset into the element_chains vector...
    auto offset = block->get_offset();

    // Now find the facesets that have faces on this block...
    const Ioss::SideSetContainer &fss = region.get_sidesets();
    for (auto &fs : fss) {
      std::vector<std::string> blocks;
      fs->block_membership(blocks);
      for (const auto &fs_block : blocks) {
        if (fs_block == adj_block) {
          // This faceset has some elements that are in `adj_block` -- put those in the `front`
          // list. Get list of "sides" in this faceset...
          std::vector<INT> element_side;
          assert(fs->side_block_count() == 1);
          const auto *fb = fs->get_block(0);
          fb->get_field_data("element_side_raw", element_side);

          // Mark each element so we know it is on the sideset(s)
          for (size_t i = 0; i < element_side.size(); i += 2) {
            auto element = element_side[i];
            if (block->contains(element)) {
              if (element_chains[element - offset] == chain_entry_t<INT>()) {
                int side                         = element_side[i + 1]; // 1-based sides
                element_chains[element - offset] = chain_entry_t<INT>{element, 0};
                front.push_back(std::make_pair(element, side));
                fmt::print("Putting element {}, side {} in front.\n", element, side);
              }
            }
          }
        }
      }
    }
    return front;
  }

  void generate_face_connectivity(const Ioss::FaceUnorderedSet &faces, int offset,
                                  connectivity_t &face_connectivity)
  {
    for (const auto &face : faces) {
      for (int i = 0; i < face.elementCount_; i++) {
        auto element                     = face.element[i] / 10 - offset;
        auto side                        = face.element[i] % 10; // 0-based side
        face_connectivity[element][side] = &face;
      }
    }

    fmt::print("\n-----------------------------\n");
    int l = 1;
    for (size_t i = 1; i < face_connectivity.size(); i++) {
      for (size_t j = 0; j < 6; j++) {
        const auto *face = face_connectivity[i][j];
        assert(face != nullptr);
        int  k       = (face->elementCount_ > 1 && face->element[0] / 10 - offset != i) ? 1 : 0;
        auto element = face->element[k] / 10;
        auto side    = face->element[k] % 10;
        assert(side == j);
        if (face->elementCount_ > 1) {
          fmt::print(
              "[{:3}] Element {}, Side {}/{} is Face {}.\tAdjacent to Element {}, Side {}.\n", l++,
              element, side, j, face->hashId_, face->element[1 - k] / 10,
              face->element[1 - k] % 10);
        }
        else {
          fmt::print("[{:3}] Element {}, Side {}/{} is Face {}.\n", l++, element, side, j,
                     face->hashId_);
        }
      }
    }
  }
} // namespace

int main(int argc, char *argv[])
{
#ifdef SEACAS_HAVE_MPI
  MPI_Init(&argc, &argv);
  ON_BLOCK_EXIT(MPI_Finalize);
#endif
  Ioss::ParallelUtils pu{};
  int                 my_rank = pu.parallel_rank();

  codename = Ioss::FileInfo(argv[0]).basename();

  Line_Decomp::Interface interFace;
  bool                   success = interFace.parse_options(argc, argv);
  if (!success) {
    return EXIT_FAILURE;
  }

  Ioss::Init::Initializer io;

  if (my_rank == 0) {
    fmt::print("\nInput:    '{}', Type: {}\n", interFace.input_filename(), interFace.input_type());
    if (!interFace.no_output()) {
      fmt::print("Output:   '{}', Type: {}\n", interFace.output_filename(),
                 interFace.output_type());
    }
  }

  double begin = Ioss::Utils::timer();
  try {
    if (interFace.ints_64_bit()) {
      line_decomp(interFace, static_cast<int64_t>(0));
    }
    else {
      line_decomp(interFace, 0);
    }
  }
  catch (std::exception &e) {
    fmt::print(stderr, "\n{}\n\nline_decomp terminated due to exception\n", e.what());
    exit(EXIT_FAILURE);
  }
  pu.barrier();
  double end = Ioss::Utils::timer();

  if (my_rank == 0) {
    fmt::print("\n\tTotal Execution Time = {:.4} seconds\n", end - begin);
    fmt::print("\n{} execution successful.\n\n", codename);
  }
  return EXIT_SUCCESS;
}

namespace {
  Ioss::PropertyManager set_properties(Line_Decomp::Interface &interFace);

  template <typename INT> void line_decomp(Line_Decomp::Interface &interFace, INT /*dummy*/)
  {
    std::string inpfile    = interFace.input_filename();
    std::string input_type = interFace.input_type();

    Ioss::PropertyManager properties = set_properties(interFace);

    //========================================================================
    // INPUT ...
    // NOTE: The "READ_RESTART" mode ensures that the node and element ids will be mapped.
    //========================================================================
    Ioss::DatabaseIO *dbi = Ioss::IOFactory::create(input_type, inpfile, Ioss::READ_RESTART,
                                                    Ioss::ParallelUtils::comm_world(), properties);
    if (dbi == nullptr || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    // Use 64-bit integers here...
    //    dbi->set_int_byte_size_api(Ioss::USE_INT64_API);

    // Do not decompose sidesets into sideblocks...
    dbi->set_surface_split_type(Ioss::SPLIT_BY_DONT_SPLIT);

    // NOTE: 'region' owns 'db' pointer at this time...
    Ioss::Region region(dbi, "region_1");
    int          my_rank = region.get_database()->util().parallel_rank();

    if (my_rank == 0) {
      region.output_summary(std::cerr, false);
    }

    // Generate the faces...
    Ioss::FaceGenerator face_generator(region);
    face_generator.generate_faces((INT)0, true, true);

    // Determine which element block(s) are adjacent to the faceset specifying "lines"
    // The `adjacent_blocks` contains the names of all element blocks that are adjacent to the
    // surface(s) that specify the faces at the 'root' of the lines...
    std::vector<std::string> adjacent_blocks = get_adjacent_blocks(region);

    for (const auto &adj_block : adjacent_blocks) {
      // Get the offset into the element_chains vector...
      const auto *block  = region.get_element_block(adj_block);
      auto        offset = block->get_offset();
      auto        count  = block->entity_count();

      chain_t<INT> element_chains(count + 1);
      auto         front = get_line_front(region, adj_block, element_chains, (INT)0);
      if (front.empty()) {
        continue;
      }

      // We want a vector giving us the Face for each face of each element in the block...
      connectivity_t face_connectivity(count + 1);
      generate_face_connectivity(face_generator.faces(adj_block), offset, face_connectivity);

      // For each face on the "front" (at the beginning the boundary sideset faces)
      // Set `element_chains` to the `face` "ID"
      // We are only working on the elements that are in the curent block...
      // Get the offset into the element_chains vector...
      front_t<INT> next_front;
      while (!front.empty()) {
        fmt::print("\n----------------------\n");
        next_front.reserve(front.size());
        for (auto &element_side : front) {
          auto element = element_side.first;
          auto side    = element_side.second - 1;

          auto  opp_side = hex_opposite_side(side);
          auto *opp_face = face_connectivity[element - offset][opp_side];
          // See if there is an element attached to the `opp_side`
          if (opp_face->elementCount_ > 1) {
            // Determine which is current element and which is adjacent element...
            int  index       = (opp_face->element[0] / 10 == element) ? 1 : 0;
            auto nxt_element = opp_face->element[index] / 10;
            auto nxt_side    = opp_face->element[index] % 10;
            if (element_chains[nxt_element - offset] == chain_entry_t<INT>()) {
              element_chains[nxt_element - offset] = element_chains[element - offset];
              element_chains[nxt_element - offset].link++;
              fmt::print("At element {}, side {} -- Next in chain is element {}, side {}\n",
                         element, side, nxt_element, nxt_side);
              next_front.push_back(std::make_pair(nxt_element, nxt_side + 1));
            }
            else {
              fmt::print("At element {}, side {} -- Termination of chain {} of size {}.\n", element,
                         side, element_chains[element - offset].element,
                         element_chains[element - offset].link + 1);
            }
          }
          else {
            fmt::print("At element {}, side {} -- Termination of chain {} of size {}.\n", element,
                       side, element_chains[element - offset].element,
                       element_chains[element - offset].link + 1);
          }
        }
        std::swap(front, next_front);
        next_front.clear();
      }
      int element = offset - 1;
      for (auto &chain : element_chains) {
        if (++element == 0) {
          continue;
        }
        fmt::print("[{}]: element {}, link {}\n", element, chain.element, chain.link);
      }
    } // End of block loop

    if (interFace.no_output()) {
      return;
    }
  }

  Ioss::PropertyManager set_properties(Line_Decomp::Interface &interFace)
  {
    Ioss::PropertyManager properties{};
    if (interFace.ints_64_bit()) {
      properties.add(Ioss::Property("INTEGER_SIZE_DB", 8));
      properties.add(Ioss::Property("INTEGER_SIZE_API", 8));
    }

    if (interFace.debug) {
      properties.add(Ioss::Property("LOGGING", 1));
    }

    if (!interFace.decomp_method.empty()) {
      properties.add(Ioss::Property("DECOMPOSITION_METHOD", interFace.decomp_method));
    }

    if (interFace.compression_level > 0 || interFace.shuffle) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf4"));
      properties.add(Ioss::Property("COMPRESSION_LEVEL", interFace.compression_level));
      properties.add(Ioss::Property("COMPRESSION_SHUFFLE", interFace.shuffle));
    }

    if (interFace.compose_output == "default") {
      properties.add(Ioss::Property("COMPOSE_RESULTS", "NO"));
      properties.add(Ioss::Property("COMPOSE_RESTART", "NO"));
    }
    else if (interFace.compose_output == "external") {
      properties.add(Ioss::Property("COMPOSE_RESULTS", "NO"));
      properties.add(Ioss::Property("COMPOSE_RESTART", "NO"));
    }
    else if (interFace.compose_output != "none") {
      properties.add(Ioss::Property("COMPOSE_RESULTS", "YES"));
      properties.add(Ioss::Property("COMPOSE_RESTART", "YES"));
    }

    if (interFace.netcdf4_) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf4"));
    }

    return properties;
  }
} // namespace
