// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#include <cstdlib>
#include <exception>
#include <fstream>
#include <iterator>
#include <limits>
#include <memory>
#include <numeric>
#include <string>
#ifndef _MSC_VER
#include <sys/times.h>
#include <sys/utsname.h>
#endif
#include <unistd.h>
#include <vector>

#include "add_to_log.h"
#include "fmt/ostream.h"
#include "tokenize.h"

#include <exodusII.h>

#include <Ionit_Initializer.h>
#include <Ioss_SmartAssert.h>
#include <Ioss_SubSystem.h>
#include <Ioss_Transform.h>

#include "Grid.h"
#include "GridEntry.h"
#include "UnitCell.h"
#include "ZE_SystemInterface.h"
#include "ZE_Version.h"

#ifdef SEACAS_HAVE_MPI
#include <mpi.h>
#endif

namespace {
  Grid define_lattice(UnitCellMap &unit_cells, SystemInterface &interFace);

  Ioss::PropertyManager parse_properties(SystemInterface &interFace, int int_size)
  {
    Ioss::PropertyManager properties;
    if (int_size == 8) {
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
  ex_opts(EX_VERBOSE | EX_DEBUG);

  if (debug_level & 1) {
    fmt::print(stderr, "{}", time_stamp(tsFormat));
  }

  UnitCellMap unit_cells;
  auto        grid = define_lattice(unit_cells, interFace);

  if (debug_level & 1) {
    fmt::print(stderr, "{}", time_stamp(tsFormat));
  }

  // All unit cells have been mapped into the IxJ grid, now calculate all node / element offsets
  // and the global node and element counts... (TODO: Parallel decomposition)
  //
  // Iterate through the grid starting with (0,0) and accumulate node and element counts...
  if (debug_level & 1) {
    fmt::print(stderr, "{} Finalize\n", time_stamp(tsFormat));
  }

  grid.finalize();

  if (debug_level & 1) {
    fmt::print(stderr, "{} Output Model\n", time_stamp(tsFormat));
  }
  grid.output_model();

  if (debug_level & 1) {
    fmt::print(stderr, "{} Done\n", time_stamp(tsFormat));
  }

  /*************************************************************************/
  // EXIT program
  if (debug_level & 1) {
    fmt::print(stderr, "{}", time_stamp(tsFormat));
  }

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

  std::shared_ptr<Ioss::Region> create_input_region(const std::string &key, std::string filename)
  {
    // Check that 'filename' does not contain a starting/ending double quote...
    filename.erase(remove(filename.begin(), filename.end(), '\"'), filename.end());
    Ioss::DatabaseIO *dbi =
        Ioss::IOFactory::create("exodus", filename, Ioss::READ_RESTART, (MPI_Comm)MPI_COMM_WORLD);
    if (dbi == nullptr || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    dbi->set_surface_split_type(Ioss::SPLIT_BY_DONT_SPLIT);
    dbi->set_int_byte_size_api(Ioss::USE_INT64_API);

    // Generate a name for the region based on the key...
    std::string name = "Region_" + key;
    // NOTE: region owns database pointer at this time...
    return std::make_shared<Ioss::Region>(dbi, name);
  }

  std::unique_ptr<Ioss::Region> create_output_region(SystemInterface &interFace)
  {
    // At this point, can begin to define the output database...
    Ioss::PropertyManager properties = parse_properties(interFace, 8);
    properties.add(Ioss::Property("OMIT_NUM_MAPS", 1));

    Ioss::DatabaseIO *dbo = Ioss::IOFactory::create(
        "exodus", interFace.outputName_, Ioss::WRITE_RESTART, (MPI_Comm)MPI_COMM_WORLD, properties);
    if (dbo == nullptr || !dbo->ok(true)) {
      std::exit(EXIT_FAILURE);
    }
    std::unique_ptr<Ioss::Region> output_region(new Ioss::Region(dbo, "zellij_output_region"));
    output_region->begin_mode(Ioss::STATE_DEFINE_MODEL);
    output_region->property_add(Ioss::Property("code_name", qainfo[0]));
    output_region->property_add(Ioss::Property("code_version", qainfo[2]));
    return output_region;
  }

  Grid define_lattice(UnitCellMap &unit_cells, SystemInterface &interFace)
  {
    std::string filename = interFace.lattice();

    std::ifstream input(filename, std::ios::in);
    bool          in_dictionary{false};
    bool          in_lattice{false};

    std::string line;
    while (getline(input, line)) {
      if (line.empty()) {
        continue;
      }

      auto tokens = Ioss::tokenize(line, " ");
      if (tokens[0] == "BEGIN_DICTIONARY") {
        assert(!in_lattice && !in_dictionary);
        in_dictionary = true;
      }
      else if (tokens[0] == "END_DICTIONARY") {
        assert(!in_lattice && in_dictionary);
        in_dictionary = false;
      }
      else if (in_dictionary) {
        SMART_ASSERT(tokens.size() == 2)(tokens.size())(line);
        if (unit_cells.find(tokens[0]) != unit_cells.end()) {
          fmt::print(
              "\nERROR: There is a duplicate `unit cell` ({}) in the lattice dictionary.\n\n",
              tokens[0]);
          exit(EXIT_FAILURE);
        }

        auto region = create_input_region(tokens[0], tokens[1]);
        SMART_ASSERT(region != nullptr)(tokens[0])(tokens[1]);
        unit_cells.emplace(tokens[0], std::make_shared<UnitCell>(region));
      }
      else if (tokens[0] == "BEGIN_LATTICE") {
        assert(!in_lattice && !in_dictionary);
        in_lattice = true;
        break;
      }
    }

    if (!in_lattice) {
      // ERROR -- file ended before lattice definition...
    }

    // Tokenize line to get I J K size of lattice
    auto tokens = Ioss::tokenize(line, " ");
    SMART_ASSERT(tokens[0] == "BEGIN_LATTICE")(tokens[0])(line);
    SMART_ASSERT(tokens.size() == 4)(tokens.size())(line);
    int II = std::stoi(tokens[1]);
    int JJ = std::stoi(tokens[2]);
    int KK = std::stoi(tokens[3]);
    assert(KK == 1);

    auto output_region = create_output_region(interFace);
    Grid grid(output_region, II, JJ);

    size_t row{0};
    while (getline(input, line)) {
      if (line.empty()) {
        continue;
      }

      auto tokens = Ioss::tokenize(line, " ");
      if (tokens[0] == "END_LATTICE") {
        assert(in_lattice && !in_dictionary);
        in_lattice = false;

        // Check row count to make sure matches 'I' size of lattice
        if (row != grid.JJ()) {
          fmt::print("\nERROR: Only {} rows of the {} x {} lattice were defined.\n\n", row, II, JJ);
          exit(EXIT_FAILURE);
        }
        break;
      }
      else if (in_lattice) {
        // TODO: Currently assumes that each row in the lattice is defined on a single row;
        //       This will need to be relaxed since a lattice of 5000x5000 would result in
        //       lines that are too long and would be easier to split a row over multiple lines...
        if (tokens.size() != grid.II()) {
          fmt::print(
              "\nERROR: Line {} of the lattice definition has {} entries.  It should have {}.\n\n",
              row + 1, tokens.size(), grid.II());
          exit(EXIT_FAILURE);
        }

        if (row >= grid.JJ()) {
          fmt::print("\nERROR: There are too many rows in the lattice definition. The lattice is "
                     "{} x {}.\n\n",
                     grid.II(), grid.JJ());
          exit(EXIT_FAILURE);
        }

        SMART_ASSERT(tokens.size() == grid.II())(tokens.size())(grid.II());

        size_t col = 0;
        for (auto &key : tokens) {
          if (unit_cells.find(key) == unit_cells.end()) {
            fmt::print("\nERROR: In row {}, column {}, the lattice specifies a unit cell ({}) that "
                       "has not been defined.\n\n",
                       row + 1, col + 1, key);
            exit(EXIT_FAILURE);
          }

          auto &unit_cell = unit_cells[key];
          SMART_ASSERT(unit_cell->m_region != nullptr)(row)(col)(key);
          grid.initialize(col++, row, unit_cell);
        }
        row++;
      }
    }
    return grid;
  }
} // namespace
