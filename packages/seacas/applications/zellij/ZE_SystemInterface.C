// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "ZE_SystemInterface.h"
#include "ZE_Version.h" // for qainfo

#include <copyright.h>
#include <cstddef> // for size_t
#include <cstdlib> // for exit, strtod, strtoul, abs, etc
#include <fmt/format.h>
#include <iosfwd> // for ostream

SystemInterface::SystemInterface() { enroll_options(); }

SystemInterface::~SystemInterface() = default;

void SystemInterface::enroll_options()
{
  options_.usage("[options] list_of_files_to_join");

  options_.enroll("help", GetLongOption::NoValue, "Print this summary and exit", nullptr);

  options_.enroll("version", GetLongOption::NoValue, "Print version and exit", nullptr);

  options_.enroll("lattice", GetLongOption::MandatoryValue,
                  "Name of file to read lattice definition from.", "");

  options_.enroll("output", GetLongOption::MandatoryValue, "Name of output file to create",
                  "zellij-out.e");

  options_.enroll("netcdf4", GetLongOption::NoValue,
                  "Create output database using the HDF5-based "
                  "netcdf which allows for up to 2.1 GB "
                  "nodes and elements",
                  nullptr);

  options_.enroll("64-bit", GetLongOption::NoValue,
                  "True if forcing the use of 64-bit integers for the output file", nullptr);

  options_.enroll(
      "zlib", GetLongOption::NoValue,
      "Use the Zlib / libz compression method if compression is enabled (default) [exodus only].",
      nullptr);

  options_.enroll("szip", GetLongOption::NoValue,
                  "Use SZip compression. [exodus only, enables netcdf-4]", nullptr);

  options_.enroll(
      "compress", GetLongOption::MandatoryValue,
      "Specify the hdf5 (netcdf4) compression level [0..9] to be used on the output file.",
      nullptr);

  options_.enroll("debug", GetLongOption::MandatoryValue,
                  "debug level (values are or'd)\n"
                  "\t\t  1 = timing information.\n"
                  "\t\t  4 = Verbose Element block information.\n"
                  "\t\t  8 = Check consistent nodal coordinates between parts.\n"
                  "\t\t 16 = Verbose Sideset information.\n"
                  "\t\t 32 = Verbose Nodeset information.\n"
                  "\t\t 64 = put exodus library into verbose mode.\n"
                  "\t\t128 = Check consistent global field values between parts.",
                  "0");

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

  {
    const char *temp = options_.retrieve("debug");
    if (temp != nullptr) {
      debugLevel_ = strtol(temp, nullptr, 10);
    }
  }

  if (options_.retrieve("copyright") != nullptr) {
    fmt::print("{}", copyright("2010-2019"));
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

  useNetcdf4_ = options_.retrieve("netcdf4") != nullptr;
  ints64bit_  = options_.retrieve("64-bit") != nullptr;

  if (options_.retrieve("szip") != nullptr) {
    szip_ = true;
    zlib_ = false;
  }
  zlib_ = (options_.retrieve("zlib") != nullptr);

  if (szip_ && zlib_) {
    fmt::print(stderr, "ERROR: Only one of 'szip' or 'zlib' can be specified.\n");
  }

  {
    const char *temp = options_.retrieve("compress");
    if (temp != nullptr) {
      compressionLevel_ = std::strtol(temp, nullptr, 10);
    }
  }

  return true;
}

void SystemInterface::show_version()
{
  fmt::print("Zellij\n"
             "\t(A code for tiling 1 or more template databases into a single output database.)\n"
             "\t(Version: {}) Modified: {}\n",
             qainfo[2], qainfo[1]);
}
