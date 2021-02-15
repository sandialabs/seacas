// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "ZE_SystemInterface.h"
#include "ZE_Version.h" // for qainfo

#include <Ioss_ParallelUtils.h>
#include <copyright.h>
#include <cstddef> // for size_t
#include <cstdlib> // for exit, strtod, strtoul, abs, etc
#include <fmt/color.h>
#include <fmt/format.h>
#include <iosfwd> // for ostream

SystemInterface::SystemInterface() { enroll_options(); }

SystemInterface::~SystemInterface() = default;

void SystemInterface::enroll_options()
{
  options_.usage("[options] -lattice <lattice_definition_file>");

  options_.enroll("help", GetLongOption::NoValue, "Print this summary and exit", nullptr);

  options_.enroll("version", GetLongOption::NoValue, "Print version and exit", nullptr);

  options_.enroll("lattice", GetLongOption::MandatoryValue,
                  "Name of file to read lattice definition from.", "");

  options_.enroll("output", GetLongOption::MandatoryValue,
                  "Name of output file to create. Default is `zellij-out.e`", "zellij-out.e");

  options_.enroll("netcdf3", GetLongOption::NoValue,
                  "Output database will be a netcdf3 "
                  "native classical netcdf file format (32-bit only)",
                  nullptr);

  options_.enroll("netcdf4", GetLongOption::NoValue,
                  "Output database will be a netcdf4 "
                  "hdf5-based file instead of the "
                  "classical netcdf file format (default)",
                  nullptr);

  options_.enroll("netcdf5", GetLongOption::NoValue,
                  "Output database will be a netcdf5 (CDF5) "
                  "file instead of the classical netcdf file format",
                  nullptr);

  options_.enroll("32-bit", GetLongOption::NoValue,
                  "True if forcing the use of 32-bit integers for the output file", nullptr);

  options_.enroll("64-bit", GetLongOption::NoValue,
                  "True if forcing the use of 64-bit integers for the output file (default)",
                  nullptr);

  options_.enroll(
      "zlib", GetLongOption::NoValue,
      "Use the Zlib / libz compression method if compression is enabled (default) [exodus only].",
      nullptr);

  options_.enroll("szip", GetLongOption::NoValue,
                  "Use SZip compression. [exodus only, enables netcdf-4]", nullptr);

  options_.enroll("compress", GetLongOption::MandatoryValue,
                  "Specify the hdf5 zlib compression level [0..9] or szip [even, 4..32] to be used "
                  "on the output file.",
                  nullptr);

  options_.enroll(
      "rcb", GetLongOption::NoValue,
      "Use recursive coordinate bisection method to decompose the input mesh in a parallel run.",
      nullptr);
  options_.enroll(
      "rib", GetLongOption::NoValue,
      "Use recursive inertial bisection method to decompose the input mesh in a parallel run.",
      nullptr);

  options_.enroll("hsfc", GetLongOption::NoValue,
                  "Use hilbert space-filling curve method to decompose the input mesh in a "
                  "parallel run. [default]",
                  nullptr);

  options_.enroll("linear", GetLongOption::NoValue,
                  "Use the linear method to decompose the input mesh in a parallel run.\n"
                  "\t\tElements in order first n/p to proc 0, next to proc 1.",
                  nullptr);

  options_.enroll("cyclic", GetLongOption::NoValue,
                  "Use the cyclic method to decompose the input mesh in a parallel run.\n"
                  "\t\tElements handed out to id % proc_count",
                  nullptr);

  options_.enroll("random", GetLongOption::NoValue,
                  "Use the random method to decompose the input mesh in a parallel run.\n"
                  "\t\tElements assigned randomly to processors in a way that preserves balance\n"
                  "\t\t(do *not* use for a real run)",
                  nullptr);

  options_.enroll("ranks", GetLongOption::MandatoryValue,
                  "Number of ranks to decompose mesh across", "1");

  options_.enroll("start_rank", GetLongOption::MandatoryValue,
                  "In partial output mode, start outputting decomposed files at this rank", "0");

  options_.enroll("rank_count", GetLongOption::MandatoryValue,
                  "In partial output mode, output this number of ranks", "0");

  options_.enroll("separate_cells", GetLongOption::NoValue,
                  "Do not equivalence the nodes between adjacent unit cells.", nullptr);

  options_.enroll("subcycle", GetLongOption::NoValue,
                  "Process cells in groups of '-rank_count'.  Helps minimize open files,\n"
                  "\t\tbut is faster than only having a single file open.",
                  nullptr);

  options_.enroll("generate_sidesets", GetLongOption::MandatoryValue,
                  "Which surfaces on the output mesh should have sidesets generated,\n"
                  "\t\t Valid options are:\n"
                  "\t\t 'x' or 'i' for surface on minimum X coordinate,\n"
                  "\t\t 'y' or 'j' for surface on minimum Y coordinate,\n"
                  "\t\t 'z' or 'k' for surface on minimum Z coordinate,\n"
                  "\t\t 'Z' or 'I' for surface on maximum X coordinate,\n"
                  "\t\t 'Y' or 'J' for surface on maximum Y coordinate,\n"
                  "\t\t 'Z' or 'K' for surface on maximum Z coordinate,\n"
                  "\t\t For example `xyXY` would generate sideset on min/max X and Y surfaces.",
                  "");

  options_.enroll(
      "minimize_open_files", GetLongOption::OptionalValue,
      "Close files after accessing them to avoid issues with too many open files.\n"
      "\t\tIf argument is 'output' then close output, if 'unit' then close unit cells;\n"
      "\t\tif 'all' or no argument close all.\n"
      "\t\tShould not need to use this option unless you get an error message "
      "indicating this issue.",
      nullptr, "all");

  options_.enroll("debug", GetLongOption::MandatoryValue,
                  "debug level (values are or'd)\n"
                  "\t\t   1 = Time stamp information.\n"
                  "\t\t   2 = Memory information.\n"
                  "\t\t   4 = Verbose Unit Cell information.\n"
                  "\t\t   8 = Verbose output of Grid finalization calculations.\n"
                  "\t\t  16 = Put exodus library into verbose mode.\n"
                  "\t\t  32 = Verbose decomposition information.\n"
                  "\t\t  64 = Verbose output database summary information.",
                  "\t\t 128 = Verbose sideset generation information.", "0");

  options_.enroll("copyright", GetLongOption::NoValue, "Show copyright and license data.", nullptr);
}

bool SystemInterface::parse_options(int argc, char **argv)
{
  int option_index = options_.parse(argc, argv);
  if (option_index < 1) {
    return false;
  }

  if (options_.retrieve("help") != nullptr) {
    options_.usage();
    fmt::print(stderr, "\n\tCan also set options via ZELLIJ_OPTIONS environment variable.\n"
                       "\n\t->->-> Send email to gdsjaar@sandia.gov for zellij support.<-<-<-\n");
    exit(EXIT_SUCCESS);
  }

  if (options_.retrieve("version") != nullptr) {
    // Version is printed up front, just exit...
    exit(0);
  }

  if (options_.retrieve("rcb") != nullptr) {
    decompMethod_ = "RCB";
  }

  if (options_.retrieve("rib") != nullptr) {
    decompMethod_ = "RIB";
  }

  if (options_.retrieve("hsfc") != nullptr) {
    decompMethod_ = "HSFC";
  }

  if (options_.retrieve("linear") != nullptr) {
    decompMethod_ = "LINEAR";
  }

  if (options_.retrieve("cyclic") != nullptr) {
    decompMethod_ = "CYCLIC";
  }

  if (options_.retrieve("random") != nullptr) {
    decompMethod_ = "RANDOM";
  }

  subcycle_ = (options_.retrieve("subcycle") != nullptr);

  equivalenceNodes_ = options_.retrieve("separate_cells") == nullptr;
  {
    const char *temp = options_.retrieve("minimize_open_files");
    if (temp != nullptr) {
      auto mode = Ioss::Utils::lowercase(temp);
      if (mode == "all") {
        minimizeOpenFiles_ = Minimize::ALL;
      }
      else if (mode == "unit") {
        minimizeOpenFiles_ = Minimize::UNIT;
      }
      else if (mode == "output") {
        minimizeOpenFiles_ = Minimize::OUTPUT;
      }
      else if (mode == "none") {
        minimizeOpenFiles_ = Minimize::NONE;
      }
    }
  }

  {
    const char *temp = options_.retrieve("ranks");
    if (temp != nullptr) {
      ranks_ = strtol(temp, nullptr, 10);
    }
  }

  {
    const char *temp = options_.retrieve("start_rank");
    if (temp != nullptr) {
      startRank_ = strtol(temp, nullptr, 10);
    }
  }

  {
    const char *temp = options_.retrieve("rank_count");
    if (temp != nullptr) {
      rankCount_ = strtol(temp, nullptr, 10);
    }
  }

  {
    const char *temp = options_.retrieve("debug");
    if (temp != nullptr) {
      debugLevel_ = strtol(temp, nullptr, 10);
    }
  }

  if (options_.retrieve("copyright") != nullptr) {
    fmt::print("{}", copyright("2021"));
    exit(EXIT_SUCCESS);
  }

  // Get options from environment variable also...
  char *options = getenv("ZELLIJ_OPTIONS");
  if (options != nullptr) {
    fmt::print(
        stderr,
        "\nThe following options were specified via the ZELLIJ_OPTIONS environment variable:\n"
        "\t{}\n\n",
        options);
    options_.parse(options, options_.basename(*argv));
  }

  {
    const char *temp = options_.retrieve("output");
    if (temp != nullptr) {
      outputName_ = temp;
    }
  }

  {
    const char *temp = options_.retrieve("lattice");
    if (temp != nullptr) {
      lattice_ = temp;
    }
  }

  {
    const char *temp = options_.retrieve("generate_sidesets");
    if (temp != nullptr) {
      sidesetSurfaces_ = temp;
    }
  }

  // Default to 64...
  ints32bit_ = options_.retrieve("32-bit") != nullptr;
  if (options_.retrieve("64-bit") != nullptr) {
    ints32bit_ = false;
  }

  if (options_.retrieve("netcdf3") != nullptr) {
    ints32bit_  = true;
    useNetcdf4_ = false;
    useNetcdf5_ = false;
  }

  if (options_.retrieve("netcdf4") != nullptr) {
    useNetcdf4_ = true;
    useNetcdf5_ = false;
  }

  if (options_.retrieve("netcdf5") != nullptr) {
    useNetcdf4_ = false;
    useNetcdf5_ = true;
  }

  if (options_.retrieve("szip") != nullptr) {
    szip_ = true;
    zlib_ = false;
  }
  zlib_ = (options_.retrieve("zlib") != nullptr);

  if (szip_ && zlib_) {
    fmt::print(stderr, fmt::fg(fmt::color::red),
               "\nERROR: Only one of 'szip' or 'zlib' can be specified.\n");
  }

  {
    const char *temp = options_.retrieve("compress");
    if (temp != nullptr) {
      compressionLevel_ = std::strtol(temp, nullptr, 10);
    }
  }

  // Adjust start_rank and rank_count if running in parallel...
  Ioss::ParallelUtils pu{MPI_COMM_WORLD};
  if (pu.parallel_size() > 1) {
    auto size          = pu.parallel_size();
    auto ranks_per_mpi = ranks_ / size;
    auto extra         = ranks_ % size;

    auto my_rank = pu.parallel_rank();
    if (my_rank < extra) {
      startRank_ = (ranks_per_mpi + 1) * my_rank;
    }
    else {
      startRank_ = (ranks_per_mpi + 1) * extra + ranks_per_mpi * (my_rank - extra);
    }
    rankCount_ = ranks_per_mpi + (my_rank < extra ? 1 : 0);
  }
  if ((rankCount_ == 0) || (startRank_ + rankCount_ > ranks_)) {
    rankCount_ = ranks_ - startRank_;
  }

  if (lattice().empty()) {
    fmt::print(stderr, fmt::fg(fmt::color::red),
               "\nERROR: Missing specification of lattice file.\n");
    options_.usage();
    return false;
  }
  else {
    return true;
  }
}

void SystemInterface::show_version()
{
  fmt::print(fmt::fg(fmt::color::cyan),
             "Zellij\n"
             "\t(A code for tiling 1 or more template databases into a single output database.)\n"
             "\t(Version: {}) Modified: {}\n",
             qainfo[2], qainfo[1]);
}
