#include <Ionit_Initializer.h>
#include <Ioss_CodeTypes.h>
#include <Ioss_Utils.h>
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <numeric>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#undef NDEBUG
#include "Ioss_DatabaseIO.h"
#include "Ioss_IOFactory.h"
#include "Ioss_Property.h"
#include "Ioss_Region.h"
#include "Ioss_SmartAssert.h"
#include "Ioss_ZoneConnectivity.h"

#include <cgns/Iocgns_StructuredZoneData.h>
#include <cgns/Iocgns_Utils.h>

#include "fmt/format.h"
#include "vector3d.h"

namespace {
  std::string codename;
  std::string version = "0.9";

  void cleanup(std::vector<Iocgns::StructuredZoneData *> &zones)
  {
    for (auto &zone : zones) {
      delete zone;
      zone = nullptr;
    }
  }

  int64_t generate_guid(size_t id, int rank, int proc_count)
  {
    static size_t lpow2 = 0;
    static int    procs = -1;
    if (procs != proc_count) {
      lpow2 = Ioss::Utils::log_power_2(proc_count);
      procs = proc_count;
    }
    assert(rank >= 0);
    return (id << lpow2) + rank;
  }

  void update_zgc_data(std::vector<Iocgns::StructuredZoneData *> &zones, int proc_count)
  {
    for (auto &zone : zones) {
      if (zone->is_active()) {
        zone->resolve_zgc_split_donor(zones);
      }
    }

    for (auto &zone : zones) {
      if (zone->is_active()) {
        zone->update_zgc_processor(zones);
      }
    }

    // Set GUID on all zgc instances...
    for (auto &zone : zones) {
      if (zone->is_active()) {
        for (auto &zgc : zone->m_zoneConnectivity) {
          zgc.m_ownerGUID = generate_guid(zgc.m_ownerZone, zgc.m_ownerProcessor, proc_count);
          zgc.m_donorGUID = generate_guid(zgc.m_donorZone, zgc.m_donorProcessor, proc_count);
        }
      }
    }
  }

  void check_split_assign(std::vector<Iocgns::StructuredZoneData *> &zones,
                          double load_balance_tolerance, size_t proc_count, double min_toler = 0.9,
                          double max_toler = 1.0)
  {
    double total_work =
        std::accumulate(zones.begin(), zones.end(), 0.0,
                        [](double a, Iocgns::StructuredZoneData *b) { return a + b->work(); });

    double avg_work = total_work / (double)proc_count;
    {
      size_t zcount = zones.size();
      double max_work = avg_work * load_balance_tolerance * max_toler;

      Iocgns::Utils::pre_split(zones, avg_work, load_balance_tolerance, 0, proc_count);

      for (const auto zone : zones) {
        if (zone->is_active()) {
          SMART_ASSERT(zone->work() <= max_work)(zone->work())(max_work);
        }
      }

      for (size_t i = 0; i < zones.size(); i++) {
        SMART_ASSERT(zones[i]->m_zone == i + 1)(zones[i]->m_zone)(i+1);
      }

      {
        std::vector<size_t> work_vector(proc_count);
        Iocgns::Utils::assign_zones_to_procs(zones, work_vector);

	// Print work/processor map...
	std::vector<size_t> proc_work(proc_count);

	fmt::print("\nDecomposition for {} zones over {} processors; Total work = {:n}, Average = {:.2f}\n",
		   zcount, proc_count, (size_t)total_work, avg_work);

	for (const auto zone : zones) {
	  if (zone->is_active()) {
	    fmt::print("\tZone {},\tProc: {:4}\tOrdinal: {}x{}x{}\tWork: {:n}\n",
		       zone->m_name, zone->m_proc, zone->m_ordinal[0], zone->m_ordinal[1],
		       zone->m_ordinal[2], zone->work());
	    proc_work[zone->m_proc] += zone->work();
	  }
	}

	auto v1 = *std::min_element(proc_work.begin(), proc_work.end());
	auto v2 = *std::max_element(proc_work.begin(), proc_work.end());

	if (v1 == v2) {
	  fmt::print("\nWork on all processors is {:n}\n\n", v1);
	}
	else {
	  fmt::print("\nWork per processor. Minimum = {:n}, Maximum = {:n}, Ratio = {}\n\n", v1, v2, (double)(v2)/v1);
	  for (size_t i=0; i < proc_work.size(); i++) {
	    fmt::print("\tProcessor {}, work = {:n}\n", i, proc_work[i]);
	  }
	}

        // Each active zone must be on a processor
        for (const auto zone : zones) {
          if (zone->is_active()) {
            SMART_ASSERT(zone->m_proc >= 0)(zone->m_proc);
          }
        }

        // Work must be min_work <= work <= max_work
        double min_work = avg_work / load_balance_tolerance * min_toler;
        for (auto work : work_vector) {
          SMART_ASSERT(work >= min_work)(work)(min_work);
          SMART_ASSERT(work <= max_work * max_toler)(work)(max_work*max_toler);
        }

        // A processor cannot have more than one zone with the same adam zone
        std::set<std::pair<int, int>> proc_adam_map;
        for (const auto zone : zones) {
          if (zone->is_active()) {
            auto success = proc_adam_map.insert(std::make_pair(zone->m_adam->m_zone, zone->m_proc));
            SMART_ASSERT(success.second);
          }
        }

        // Zone Grid Connectivity Checks:
        update_zgc_data(zones, proc_count);

        // Zone Grid Connectivity instances can't connect to themselves...
        for (auto &zone : zones) {
          if (zone->is_active()) {
            for (const auto &zgc : zone->m_zoneConnectivity) {
              if (zgc.is_active()) {
                SMART_ASSERT(zgc.m_ownerZone != zgc.m_donorZone)(zgc.m_ownerZone)(zgc.m_donorZone);
                SMART_ASSERT(zgc.m_ownerGUID != zgc.m_donorGUID)(zgc.m_ownerGUID)(zgc.m_donorGUID);
              }
            }
          }
        }

        // In Iocgns::Utils::common_write_meta_data, there is code to make
        // sure that the zgc.m_connectionName  is unique for all zgc instances on
        // a zone / processor pair (if !parallel_io which is file-per-processor)
        // The uniquification appends a letter from 'A' to 'Z' to the name
        // If the name is still not unique, repeats process with 'AA' to 'ZZ'
        // Make sure that there are not more than 26 + 26*26 + 1 instances of the same
        // name on a zone to ensure that this works...
        for (auto &zone : zones) {
          if (zone->is_active()) {
            std::map<std::string, int> zgc_map;
            for (const auto &zgc : zone->m_zoneConnectivity) {
              if (zgc.is_active() && !zgc.is_from_decomp()) {
                zgc_map[zgc.m_connectionName]++;
              }
            }
            for (const auto &kk : zgc_map) {
              SMART_ASSERT(kk.second < 26 * 26 + 26 + 1)(kk.second);
            }
          }
        } //

        // Zone Grid Connectivity from_decomp instances must be symmetric...
        // The GUID encodes the id and the processor,
        std::map<std::pair<size_t, size_t>, int> is_symm;
        for (auto &zone : zones) {
          if (zone->is_active()) {
            for (const auto &zgc : zone->m_zoneConnectivity) {
              if (zgc.is_active() && zgc.is_from_decomp()) {
                is_symm[std::make_pair(std::min(zgc.m_ownerGUID, zgc.m_donorGUID),
                                       std::max(zgc.m_ownerGUID, zgc.m_donorGUID))]++;
              }
            }
          }
        }
        // Iterate `is_symm` and make sure all entries == 2
        for (const auto &item : is_symm) {
          SMART_ASSERT(item.second == 2);
        }
      }
    }
  }
} // namespace

int main(int argc, char *argv[])
{
#ifdef SEACAS_HAVE_MPI
  MPI_Init(&argc, &argv);
#endif

  std::string in_type  = "cgns";

  codename   = argv[0];
  size_t ind = codename.find_last_of('/', codename.size());
  if (ind != std::string::npos) {
    codename = codename.substr(ind + 1, codename.size());
  }

  Ioss::Init::Initializer io;
  std::string input_file;
  int ordinal = -1;
  int proc_count = 0;
  double load_balance = 1.4;

  // Skip past any options...
  int i = 1;

  while (i < argc && argv[i][0] == '-') {
    if (std::strcmp("--ordinal", argv[i]) == 0) {
      i++;
      ordinal = std::stoi(argv[i++]);
    }
    else if (std::strcmp("--processors", argv[i]) == 0) {
      i++;
      proc_count = std::stoi(argv[i++]);
    }
    else if (std::strcmp("--load_balance", argv[i]) == 0) {
      i++;
      load_balance = std::stod(argv[i++]);
    }

    // Found an option.  See if it has an argument...
    else if (i + 1 < argc && argv[i + 1][0] == '-') {
      // No argument, another option
      i++;
    }
    else {
      // Skip the argument...
      i += 2;
    }
  }

  std::string cgns_file   = Ioss::Utils::local_filename(argv[i++], in_type, "");
  Ioss::PropertyManager properties;
  Ioss::DatabaseIO *dbi =
    Ioss::IOFactory::create(in_type, cgns_file, Ioss::READ_RESTART, (MPI_Comm)MPI_COMM_WORLD, properties);
  if (dbi == nullptr || !dbi->ok()) {
    std::cerr << "ERROR: Could not open database '" << cgns_file << "' of type '" << in_type
	      << "'\n";
    std::exit(EXIT_FAILURE);
  }

  // NOTE: 'region' owns 'db' pointer at this time...
  Ioss::Region region(dbi, "region_1");

  // Get the structured blocks...
  std::vector<Iocgns::StructuredZoneData *> zones;
  int zone = 1;

  const auto &blocks         = region.get_structured_blocks();
  if (blocks.empty()) {
    fmt::print("ERROR: There are no structured blocks on the mesh.\n");
    return EXIT_FAILURE;
  }

  for (auto iblock : blocks) {
    size_t ni = iblock->get_property("ni").get_int();
    size_t nj = iblock->get_property("nj").get_int();
    size_t nk = iblock->get_property("nk").get_int();
    std::string format = fmt::format("{}x{}x{}", ni, nj, nk);
    fmt::print("Adding zone with {}\n", format);
    zones.push_back(new Iocgns::StructuredZoneData(zone++, format));
    if (ordinal >= 0) {
      zones.back()->m_lineOrdinal = ordinal;
    }
  }

  region.output_summary(std::cout, false);
  check_split_assign(zones, load_balance, proc_count);
  cleanup(zones);
}
