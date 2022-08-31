// TODO:
// * Output the chains to an element map(s) instead of field.
// * Make the chain generation a library function (need to watch the sideset/sideblock splitting)
// * Parallelize
// * Auto-Decomp (Can we generate the chains in the ioss decomp lower level...)
// * Add to slice...

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
#include "Ioss_ChainGenerator.h"
#include "Ioss_CodeTypes.h"
#include "Ioss_CopyDatabase.h"
#include "Ioss_DBUsage.h"
#include "Ioss_DatabaseIO.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_FaceGenerator.h"
#include "Ioss_FileInfo.h"
#include "Ioss_IOFactory.h"
#include "Ioss_MeshCopyOptions.h"
#include "Ioss_NodeBlock.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"
#include "Ioss_Region.h"
#include "Ioss_SideBlock.h"
#include "Ioss_SideSet.h"
#include "Ioss_Utils.h"
#include "tokenize.h"

#include <cassert>

#include "line_decomp_interface.h"

// ========================================================================

namespace {
  int debug = 0;

  void add_chain_fields(Ioss::Region &region, const std::vector<std::string> &adj_blocks);

  template <typename INT>
  void output_chain_fields(Ioss::Region &region, const Ioss::ElementBlock *eb,
                           const Ioss::chain_t<INT> &chains);

  template <typename INT> void line_decomp(Line_Decomp::Interface &interFace, INT /*dummy*/);
  std::string                  codename;
  std::string                  version = "0.1";
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
  debug = interFace.debug();

  Ioss::Init::Initializer io;

  if (my_rank == 0) {
    fmt::print("\nInput:    '{}', Type: {}\n", interFace.input_filename(), interFace.input_type());
    fmt::print("Output:   '{}', Type: {}\n", interFace.output_filename(), interFace.output_type());
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
    fmt::print(stderr, "\n{}\n\n{} terminated due to exception\n", codename, e.what());
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
  Ioss::PropertyManager        set_properties(Line_Decomp::Interface &interFace);
  template <typename INT> void line_decomp(Line_Decomp::Interface &interFace, INT dummy)
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

    // Do not decompose sidesets into sideblocks...
    dbi->set_surface_split_type(Ioss::SPLIT_BY_DONT_SPLIT);

    // NOTE: 'region' owns 'db' pointer at this time...
    Ioss::Region region(dbi, "region_1");

    //=======================================================================
    // Function which generates chains...
    //=======================================================================
    auto element_chains = Ioss::generate_element_chains(region, interFace.surfaceList, dummy);

    if (debug & 8) {
      for (size_t i = 0; i < element_chains.size(); i++) {
        auto &chain_entry = element_chains[i];
        fmt::print("[{}]: element {}, link {}\n", i + 1, chain_entry.element, chain_entry.link);
      }
    }

    // Output File...
    Ioss::DatabaseIO *dbo =
        Ioss::IOFactory::create(interFace.output_type(), interFace.output_filename(),
                                Ioss::WRITE_RESTART, Ioss::ParallelUtils::comm_world(), properties);
    if (dbo == nullptr || !dbo->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    // NOTE: 'output_region' owns 'dbo' pointer at this time
    Ioss::Region output_region(dbo, "region_2");

    // Set the qa information...
    output_region.property_add(Ioss::Property(std::string("code_name"), codename));
    output_region.property_add(Ioss::Property(std::string("code_version"), version));

    Ioss::MeshCopyOptions options{};
    options.ints_64_bit       = sizeof(INT) == 64;
    options.delete_timesteps  = true;
    options.data_storage_type = 2;
    options.verbose           = true;

    // Copy mesh portion of input region to the output region
    Ioss::copy_database(region, output_region, options);
  }

  void add_chain_fields(Ioss::Region &region, const std::vector<std::string> &adj_blocks)
  {
    region.begin_mode(Ioss::STATE_DEFINE_TRANSIENT);
    for (auto &blk_name : adj_blocks) {
      auto *eb = region.get_element_block(blk_name);
      assert(eb != nullptr);
      if (eb->topology()->shape() == Ioss::ElementShape::HEX) {
        eb->field_add(Ioss::Field("chain", Ioss::Field::REAL, "scalar", Ioss::Field::TRANSIENT));
      }
    }
    region.end_mode(Ioss::STATE_DEFINE_TRANSIENT);

    region.begin_mode(Ioss::STATE_TRANSIENT);

    auto step = region.add_state(0.0);
    region.begin_state(step);
  }

  template <typename INT>
  void output_chain_fields(Ioss::Region &region, const Ioss::ElementBlock *eb,
                           const Ioss::chain_t<INT> &chains)
  {
    Ioss::ElementBlock *oeb = region.get_element_block(eb->name());
    assert(oeb != nullptr);
    std::vector<double> chain(oeb->entity_count());
    assert(chain.size() == chains.size());

    for (size_t i = 0; i < chain.size(); i++) {
      auto &chain_entry = chains[i];
      chain[i]          = chain_entry.element;
      if (debug & 8) {
        fmt::print("[{}]: element {}, link {}\n", i + 1, chain_entry.element, chain_entry.link);
      }
    }

    oeb->put_field_data("chain", chain);
  }

  Ioss::PropertyManager set_properties(Line_Decomp::Interface &interFace)
  {
    Ioss::PropertyManager properties{};
    if (interFace.ints_64_bit()) {
      properties.add(Ioss::Property("INTEGER_SIZE_DB", 8));
      properties.add(Ioss::Property("INTEGER_SIZE_API", 8));
    }

    if (interFace.debug() & 1) {
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
