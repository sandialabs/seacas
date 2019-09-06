#define CATCH_CONFIG_MAIN
#include <catch.hpp>

#include <Ioss_Utils.h>
#include <exception>
#include <fmt/ostream.h>
#include <vector>

TEST_CASE("distinct", "[distinct]")
{
  std::string ret = Ioss::Utils::format_id_list({1,3,5,7,9,11});
  REQUIRE(ret == std::string("1, 3, 5, 7, 9, 11"));
}

TEST_CASE("one-range", "[one-range]")
{
  std::string ret = Ioss::Utils::format_id_list({1,2,3,4,5,6,7,8,9,10,11}, "--");
  REQUIRE(ret == std::string("1--11"));
}

TEST_CASE("one value", "[one-value]")
{
  std::string ret = Ioss::Utils::format_id_list({11}, "--");
  REQUIRE(ret == std::string("11"));
}

TEST_CASE("two consecutive values", "[two-consec-values]")
{
  std::string ret = Ioss::Utils::format_id_list({10,11}, "--");
  REQUIRE(ret == std::string("10, 11"));
}

TEST_CASE("two separated values", "[two-sep-values]")
{
  std::string ret = Ioss::Utils::format_id_list({2,11}, "--");
  REQUIRE(ret == std::string("2, 11"));
}

TEST_CASE("two ranges", "[two-ranges]")
{
  std::string ret = Ioss::Utils::format_id_list({1,2,3,4, 6,7,8}, "--");
  REQUIRE(ret == std::string("1--4, 6--8"));
}

TEST_CASE("large range", "[large-range]")
{
  std::vector<size_t> range(100000);
  std::iota(range.begin(), range.end(), 42);
  std::string ret = Ioss::Utils::format_id_list(range, "--");
  REQUIRE(ret == std::string("42--100041"));
}

TEST_CASE("large range with singles", "[large-range-w-singles]")
{
  std::vector<size_t> range(100000);
  std::iota(range.begin(), range.end(), 42);
  range[0] = 1;
  range[range.size()-1]++;
  std::string ret = Ioss::Utils::format_id_list(range, "--");
  REQUIRE(ret == std::string("1, 43--100040, 100042"));
}

TEST_CASE("large range with singles with to", "[large-range-w-singles-w-to]")
{
  std::vector<size_t> range(100000);
  std::iota(range.begin(), range.end(), 42);
  range[0] = 1;
  range[range.size()-1]++;
  std::string ret = Ioss::Utils::format_id_list(range);
  REQUIRE(ret == std::string("1, 43 to 100040, 100042"));
}

TEST_CASE("large range with singles with colon", "[large-range-w-singles-w-colon]")
{
  std::vector<size_t> range(100000);
  std::iota(range.begin(), range.end(), 42);
  range[0] = 1;
  range[range.size()-1]++;
  std::string ret = Ioss::Utils::format_id_list(range, ":");
  REQUIRE(ret == std::string("1, 43:100040, 100042"));
}

TEST_CASE("detect unsorted two ids", "[detect-unsorted-two-ids]")
{
  CHECK_THROWS(Ioss::Utils::format_id_list({2,1}));
}

TEST_CASE("detect unsorted ids", "[detect-unsorted-ids]")
{
  CHECK_THROWS(Ioss::Utils::format_id_list({1,2,3,4,5,1}));
}

TEST_CASE("detect duplicate ids", "[detect-duplicate-ids]")
{
  CHECK_THROWS(Ioss::Utils::format_id_list({1,2,3,3,4,5,6}));
}
