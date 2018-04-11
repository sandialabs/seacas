#define CATCH_CONFIG_MAIN
#include <catch.hpp>

#include <cgns/Iocgns_StructuredZoneData.h>
#include <cgns/Iocgns_Utils.h>
#include <exception>
#include <map>
#include <vector>

TEST_CASE("test single block", "[single_block]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;
  auto *zone = new Iocgns::StructuredZoneData("zone1", 1, 4, 4, 1);
  zones.push_back(zone);

  int    proc_count             = 2;
  double avg_work               = zone->work() / (double)proc_count;
  double load_balance_tolerance = 1.2;

  Iocgns::Utils::pre_split(zones, avg_work, load_balance_tolerance, 0, proc_count);
}

TEST_CASE("test prime sides", "[prime_sides]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;
  auto *zone = new Iocgns::StructuredZoneData("zone1", 1, 3, 5, 7);
  zones.push_back(zone);

  double load_balance_tolerance = 1.1;
  for (size_t proc_count = 2; proc_count < 8; proc_count++) {
    std::string name = "Prime_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      double avg_work = zone->work() / (double)proc_count;

      Iocgns::Utils::pre_split(zones, avg_work, load_balance_tolerance, 0, proc_count);
    }
  }
}

TEST_CASE("test farmer plenum", "[farmer_plenum]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;

  auto *zone1 = new Iocgns::StructuredZoneData("zone1", 1, 56, 128, 48);
  zones.push_back(zone1);

  auto *zone2 = new Iocgns::StructuredZoneData("zone2", 2, 32, 64, 48);
  zones.push_back(zone2);

  double load_balance_tolerance = 1.1;

  for (size_t proc_count = 2; proc_count < 20; proc_count++) {
    std::string name = "Plenum_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      double avg_work = (zone1->work() + zone2->work()) / (double)proc_count;

      SECTION("split_zones")
      {
        Iocgns::Utils::pre_split(zones, avg_work, load_balance_tolerance, 0, proc_count);

        // double min_work = avg_work / load_balance_tolerance;
        double max_work = avg_work * load_balance_tolerance;
        for (const auto zone : zones) {
          if (zone->is_active()) {
            REQUIRE(zone->work() <= max_work);
          }
        }

        SECTION("assign_to_procs")
        {
          std::vector<size_t> work_vector(proc_count);
          Iocgns::Utils::assign_zones_to_procs(zones, work_vector);

          // Each active zone must be on a processor
          for (const auto zone : zones) {
            if (zone->is_active()) {
              REQUIRE(zone->m_proc >= 0);
            }
          }

          // Work must be min_work <= work <= max_work
          for (auto work : work_vector) {
            // REQUIRE(work >= min_work);
            REQUIRE(work <= max_work);
          }

          // A processor cannot have more than one zone with the same adam zone
          std::set<std::pair<int, int>> proc_adam_map;
          for (size_t i = 0; i < zones.size(); i++) {
            if (zones[i]->is_active()) {
              auto success =
                  proc_adam_map.insert(std::make_pair(zones[i]->m_adam->m_zone, zones[i]->m_proc));
              REQUIRE(success.second);
            }
          }
        }
      }
    }
  }
}

TEST_CASE("test grv", "[grv]")
{
  std::vector<Iocgns::StructuredZoneData *> zones;

  auto *zone1 = new Iocgns::StructuredZoneData("zone1", 1, 56, 128, 48);
  zones.push_back(zone1);

  auto *zone2 = new Iocgns::StructuredZoneData("zone2", 2, 32, 64, 48);
  zones.push_back(zone2);

  double load_balance_tolerance = 1.1;

  for (size_t proc_count = 2; proc_count < 20; proc_count++) {
    std::string name = "Plenum_ProcCount_" + std::to_string(proc_count);
    SECTION(name)
    {
      double avg_work = (zone1->work() + zone2->work()) / (double)proc_count;

      SECTION("split_zones")
      {
        Iocgns::Utils::pre_split(zones, avg_work, load_balance_tolerance, 0, proc_count);

        // double min_work = avg_work / load_balance_tolerance;
        double max_work = avg_work * load_balance_tolerance;
        for (const auto zone : zones) {
          if (zone->is_active()) {
            REQUIRE(zone->work() <= max_work);
          }
        }

        SECTION("assign_to_procs")
        {
          std::vector<size_t> work_vector(proc_count);
          Iocgns::Utils::assign_zones_to_procs(zones, work_vector);

          // Each active zone must be on a processor
          for (const auto zone : zones) {
            if (zone->is_active()) {
              REQUIRE(zone->m_proc >= 0);
            }
          }

          // Work must be min_work <= work <= max_work
          for (auto work : work_vector) {
            // REQUIRE(work >= min_work);
            REQUIRE(work <= max_work);
          }

          // A processor cannot have more than one zone with the same adam zone
          std::set<std::pair<int, int>> proc_adam_map;
          for (size_t i = 0; i < zones.size(); i++) {
            if (zones[i]->is_active()) {
              auto success =
                  proc_adam_map.insert(std::make_pair(zones[i]->m_adam->m_zone, zones[i]->m_proc));
              REQUIRE(success.second);
            }
          }
        }
      }
    }
  }
}
