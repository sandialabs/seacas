#define CATCH_CONFIG_MAIN
#include <catch.hpp>

#include <cgns/Iocgns_StructuredZoneData.h>
#include <cgns/Iocgns_Utils.h>
#include <exception>
#include <map>
#include <numeric>
#include <vector>


// Helper function that drives all tests...
void check_split_assign(std::vector<Iocgns::StructuredZoneData *> &zones, double load_balance_tolerance, size_t proc_count,
			double min_toler = 0.9)
{
  double total_work =
      std::accumulate(zones.begin(), zones.end(), 0.0,
                      [](double a, Iocgns::StructuredZoneData *b) { return a + b->work(); });

  double avg_work = total_work / (double)proc_count;
  SECTION("split_zones")
    {
      Iocgns::Utils::pre_split(zones, avg_work, load_balance_tolerance, 0, proc_count);
      
      double max_work = avg_work * load_balance_tolerance;
      for (const auto zone : zones) {
	if (zone->is_active()) {
	  CHECK(zone->work() <= max_work);
	}
      }
      
      SECTION("assign_to_procs")
        {
          std::vector<size_t> work_vector(proc_count);
          Iocgns::Utils::assign_zones_to_procs(zones, work_vector);
	  
#if 0
	  std::cerr << "\nDecomposition for " << proc_count << " processors; Total work = " << total_work << " Average = " << avg_work << "\n";
	  for (const auto zone : zones) {
	    std::cerr << "Zone " << zone->m_name << "\tProc: " << zone->m_proc
		      << "\tOrdinal: " << zone->m_ordinal[0] << "x" << zone->m_ordinal[1] << "x" << zone->m_ordinal[2] 
		      << " \tWork: " << zone->work() << "\n";
	  }
#endif
          // Each active zone must be on a processor
          for (const auto zone : zones) {
            if (zone->is_active()) {
              CHECK(zone->m_proc >= 0);
            }
          }
	  
          // Work must be min_work <= work <= max_work
	  double min_work = avg_work / load_balance_tolerance * min_toler;
          for (auto work : work_vector) {
	    CHECK(work >= min_work);
            CHECK(work <= max_work);
          }
	  
          // A processor cannot have more than one zone with the same adam zone
          std::set<std::pair<int, int>> proc_adam_map;
          for (const auto zone : zones) {
            if (zone->is_active()) {
              auto success =
		proc_adam_map.insert(std::make_pair(zone->m_adam->m_zone, zone->m_proc));
              CHECK(success.second);
            }
          }
        }
    }
}

TEST_CASE("single block", "[single_block]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;
  zones.push_back(new Iocgns::StructuredZoneData(1, "4x4x1"));

  int    proc_count = 2;
  double load_balance_tolerance = 1.2;

  check_split_assign(zones, load_balance_tolerance, proc_count);
}

TEST_CASE("single block line", "[single_block_line]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;
  zones.push_back(new Iocgns::StructuredZoneData(1, "4x4x1"));
  zones.back()->m_lineOrdinal = 0;

  int    proc_count = 4;
  double load_balance_tolerance = 1.05;

  check_split_assign(zones, load_balance_tolerance, proc_count);
}

TEST_CASE("prime sides", "[prime_sides]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;
  zones.push_back(new Iocgns::StructuredZoneData(1, "3x5x7"));

  double load_balance_tolerance = 1.25;

  for (size_t proc_count = 2; proc_count < 8; proc_count++) {
    std::string name = "Prime_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      check_split_assign(zones, load_balance_tolerance, proc_count, 0.8);
    }
  }
}

TEST_CASE("farmer plenum", "[farmer_plenum]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;

  auto *zone1 = new Iocgns::StructuredZoneData(1, "56x128x48");
  zones.push_back(zone1);

  auto *zone2 = new Iocgns::StructuredZoneData(2, "32x64x48");
  zones.push_back(zone2);

  double load_balance_tolerance = 1.1;

  for (size_t proc_count = 2; proc_count < 20; proc_count++) {
    std::string name = "Plenum_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      check_split_assign(zones, load_balance_tolerance, proc_count);
    }
  }
}

TEST_CASE("grv", "[grv]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;

  int zone = 1;
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x1x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x1x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x4x4"));

  for (size_t proc_count = 2; proc_count < 16; proc_count++) {
    std::string name = "GRV_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      double load_balance_tolerance = 1.3;
      check_split_assign(zones, load_balance_tolerance, proc_count, .7);
    }
  }
}

TEST_CASE("mk21", "[mk21]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;

  int zone = 1;
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x2x8"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x4x4"));

  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x8x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "6x8x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x4"));

  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x4x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x4x2"));

  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "4x2x2"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "16x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "16x4x4"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "16x4x4"));

  double load_balance_tolerance = 1.2;

  for (size_t proc_count = 2; proc_count < 17; proc_count++) {
    std::string name = "MK21_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      check_split_assign(zones, load_balance_tolerance, proc_count);
    }
  }
}

TEST_CASE("farmer_h1_nozzle", "[h1_nozzle]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;

  // tried a nozzle only case:
  // farmer_h1_nozzle_3dh_2M-hex-join-domains.cgns. This
  // case runs on 320 processes, but on 384 processes, it results in
  // asymmetric communication, which appears to be coming from
  // IOSS.
  //
  // For example, on process 067, there are two interfaces
  // connected to 309:
  // current surf 189 1to1ConnectionB7 ranks: 67 309 gids: 2115 1845 owner ranges: 1 1 2 33 80 81
  // current surf 209 1to1ConnectionB7 ranks: 67 309 gids: 2115 1845 owner ranges: 1 1 2 33 70 71
  //
  // But on process 309, there is only one surface connection to 67:
  // current surf 4 1to1ConnectionA7 ranks: 309 67 gids: 1845 2115 owner ranges: 57 57 59 60 34 65

  int zone = 1;
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "56x128x48"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "32x64x48"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "56x192x128"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "32x64x128"));

  double load_balance_tolerance = 1.33;

  for (size_t proc_count = 3; proc_count <= 384; proc_count *= 2) {
    std::string name = "NOZ_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      check_split_assign(zones, load_balance_tolerance, proc_count);
    }
  }
}

TEST_CASE("farmer_h1_mk21", "[h1_mk21]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;

  // We are still having some issues with IOSS auto decomp on the full
  // grid.
  // When trying to run it on 320 processes, I get the error that the
  // block global ids of an active interface are equivalent on both
  // sides of the interface. Combing through the pointwise we didn’t
  // find any connection that has two blocks the same, so it seems to
  // come from decomposition.
  // The specific connection that fails 6_293--6_294 on proc 316. Global ids are 3388 3388.

  // StructuredBlock 'blk-01' 192x64x88     1081344 cells,      1116505 nodes
  // StructuredBlock 'blk-02' 32x88x64      180224 cells,       190905 nodes
  // StructuredBlock 'blk-03' 32x64x88      180224 cells,       190905 nodes
  // StructuredBlock 'blk-04' 56x128x48      344064 cells,       360297 nodes
  // StructuredBlock 'blk-05' 32x64x48       98304 cells,       105105 nodes
  // StructuredBlock 'blk-06' 56x192x128     1376256 cells,      1419129 nodes
  // StructuredBlock 'blk-07' 72x128x64      589824 cells,       612105 nodes
  // StructuredBlock 'blk-08' 56x80x32      143360 cells,       152361 nodes
  // StructuredBlock 'blk-09' 8x48x128       49152 cells,        56889 nodes
  // StructuredBlock 'blk-10' 128x16x16       32768 cells,        37281 nodes
  // StructuredBlock 'blk-11' 32x64x128      262144 cells,       276705 nodes
  // StructuredBlock 'blk-12' 56x8x64       28672 cells,        33345 nodes

  int zone = 1;
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "192x64x88"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "32x88x64"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "32x64x88"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "56x128x48"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "32x64x48"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "56x192x128"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "72x128x64"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "56x80x32"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "8x48x128"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "128x16x16"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "32x64x128"));
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "56x8x64"));

  double load_balance_tolerance = 1.2;

  for (size_t proc_count = 3; proc_count <= 384; proc_count *= 2) {
    std::string name = "H1_MK21_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      check_split_assign(zones, load_balance_tolerance, proc_count, 0.75);
    }
  }
}

TEST_CASE("bc-257x129x2", "[bc-257x129x2]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;

  // Failing for line decomposition on 84 processors; 72 works
  int zone = 1;
  zones.push_back(new Iocgns::StructuredZoneData(zone++, "257x129x2"));
  zones.back()->m_lineOrdinal = 1;

  double load_balance_tolerance = 1.2;

  for (size_t proc_count = 4; proc_count <= 84; proc_count += 4) {
    std::string name = "BC_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      check_split_assign(zones, load_balance_tolerance, proc_count);
    }
  }
}
