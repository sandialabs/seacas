// Copyright(C) 1999-2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest.h>

#include <Ioss_CodeTypes.h>
#include <Ioss_Utils.h>
#include <exception>
#include <numeric>
#include <vector>

DOCTEST_TEST_CASE("number_width")
{
  DOCTEST_SUBCASE("single digit")
  {
    for (int i = 0; i < 10; i++) {
      REQUIRE(1 == Ioss::Utils::number_width(i));
      REQUIRE(1 == Ioss::Utils::number_width(i, true));
    }
  }

  DOCTEST_SUBCASE("double digit")
  {
    for (int i = 11; i < 100; i += 11) {
      REQUIRE(2 == Ioss::Utils::number_width(i));
      REQUIRE(2 == Ioss::Utils::number_width(i, true));
    }
  }

  DOCTEST_SUBCASE("triple digit")
  {
    for (int i = 111; i < 1'000; i += 111) {
      REQUIRE(3 == Ioss::Utils::number_width(i));
      REQUIRE(3 == Ioss::Utils::number_width(i, true));
    }
  }

  DOCTEST_SUBCASE("quad digit")
  {
    for (int i = 1111; i < 10'000; i += 1111) {
      REQUIRE(4 == Ioss::Utils::number_width(i));
      REQUIRE(5 == Ioss::Utils::number_width(i, true));
    }
  }

  DOCTEST_SUBCASE("larger")
  {
    REQUIRE(6 == Ioss::Utils::number_width(999'999));
    REQUIRE(7 == Ioss::Utils::number_width(999'999, true));
    REQUIRE(7 == Ioss::Utils::number_width(1'000'000));
    REQUIRE(9 == Ioss::Utils::number_width(1'000'000, true));
    REQUIRE(10 == Ioss::Utils::number_width(1'111'111'111));
    REQUIRE(13 == Ioss::Utils::number_width(1'111'111'111, true));
#if !defined(__IOSS_WINDOWS__)
    REQUIRE(15 == Ioss::Utils::number_width(111'111'111'111, true));
#endif
  }
}

#if !defined __NVCC__
DOCTEST_TEST_CASE("str_equal")
{
  REQUIRE(Ioss::Utils::str_equal("", ""));
  REQUIRE(!Ioss::Utils::str_equal("", "a"));
  REQUIRE(!Ioss::Utils::str_equal("a", ""));
  REQUIRE(Ioss::Utils::str_equal("a", "a"));
  REQUIRE(Ioss::Utils::str_equal("A", "a"));
  REQUIRE(Ioss::Utils::str_equal("a", "A"));

  REQUIRE(Ioss::Utils::str_equal("longer_than_single_character", "longer_than_single_character"));
  REQUIRE(Ioss::Utils::str_equal("longer_than_single_character", "LONGER_THAN_SINGLE_CHARACTER"));
  REQUIRE(Ioss::Utils::str_equal("LONGER_THAN_SINGLE_CHARACTER", "longer_than_single_character"));
  REQUIRE(Ioss::Utils::str_equal("LONGER_THAN_SINGLE_CHARACTER", "LONGER_THAN_SINGLE_CHARACTER"));
  REQUIRE(Ioss::Utils::str_equal("LoNgEr_ThAn_SiNgLe_ChArAcTeR", "lOnGeR_tHaN_sInGlE_cHaRaCtEr"));

  REQUIRE(!Ioss::Utils::str_equal("Almost_The_Same", "almost_the_sam"));
}

DOCTEST_TEST_CASE("substr_equal")
{
  REQUIRE(Ioss::Utils::substr_equal("", ""));
  REQUIRE(Ioss::Utils::substr_equal("", "a"));
  REQUIRE(!Ioss::Utils::substr_equal("a", ""));
  REQUIRE(Ioss::Utils::substr_equal("a", "a"));
  REQUIRE(Ioss::Utils::substr_equal("A", "a"));
  REQUIRE(Ioss::Utils::substr_equal("a", "A"));

  REQUIRE(!Ioss::Utils::substr_equal("prefix", "pref"));
  REQUIRE(Ioss::Utils::substr_equal("prefix", "PREFIX"));
  REQUIRE(Ioss::Utils::substr_equal("pre", "PREFIX"));
  REQUIRE(Ioss::Utils::substr_equal("pre", "prefix"));
  REQUIRE(Ioss::Utils::substr_equal("PRe", "prefix"));
  REQUIRE(Ioss::Utils::substr_equal("PRe", "PREFIX"));
}

DOCTEST_TEST_CASE("format_id_list")
{
  DOCTEST_SUBCASE("ids_empty")
  {
    std::string ret = Ioss::Utils::format_id_list({});
    REQUIRE(ret == std::string(""));
  }

  DOCTEST_SUBCASE("ids_distinct")
  {
    std::string ret = Ioss::Utils::format_id_list({1, 3, 5, 7, 9, 11});
    REQUIRE(ret == std::string("1, 3, 5, 7, 9, 11"));
  }

  DOCTEST_SUBCASE("ids two element ranges")
  {
    std::string ret = Ioss::Utils::format_id_list({1, 2, 4, 5, 7, 8, 10, 11});
    REQUIRE(ret == std::string("1, 2, 4, 5, 7, 8, 10, 11"));
  }

  DOCTEST_SUBCASE("ids one-range")
  {
    std::string ret = Ioss::Utils::format_id_list({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}, "--");
    REQUIRE(ret == std::string("1--11"));
  }

  DOCTEST_SUBCASE("ids one value")
  {
    std::string ret = Ioss::Utils::format_id_list({11}, "--");
    REQUIRE(ret == std::string("11"));
  }

  DOCTEST_SUBCASE("ids two consecutive values")
  {
    std::string ret = Ioss::Utils::format_id_list({10, 11}, "--");
    REQUIRE(ret == std::string("10, 11"));
  }

  DOCTEST_SUBCASE("ids two separated values")
  {
    std::string ret = Ioss::Utils::format_id_list({2, 11}, "--");
    REQUIRE(ret == std::string("2, 11"));
  }

  DOCTEST_SUBCASE("ids two separated values pipe sep")
  {
    std::string ret = Ioss::Utils::format_id_list({2, 11}, "--");
    REQUIRE(ret == std::string("2, 11"));
  }

  DOCTEST_SUBCASE("ids two ranges")
  {
    std::string ret = Ioss::Utils::format_id_list({1, 2, 3, 4, 6, 7, 8}, "--");
    REQUIRE(ret == std::string("1--4, 6--8"));
  }

  DOCTEST_SUBCASE("ids two ranges pipe separated")
  {
    std::string ret = Ioss::Utils::format_id_list({1, 2, 3, 4, 6, 7, 8}, "--", "|");
    REQUIRE(ret == std::string("1--4|6--8"));
  }

  DOCTEST_SUBCASE("ids large range")
  {
    std::vector<size_t> range(100'000);
    std::iota(range.begin(), range.end(), 42);
    std::string ret = Ioss::Utils::format_id_list(range, "--");
    REQUIRE(ret == std::string("42--100041"));
  }

  DOCTEST_SUBCASE("ids large range with singles")
  {
    std::vector<size_t> range(100'000);
    std::iota(range.begin(), range.end(), 42);
    range[0] = 1;
    range[range.size() - 1]++;
    std::string ret = Ioss::Utils::format_id_list(range, "--");
    REQUIRE(ret == std::string("1, 43--100040, 100042"));
  }

  DOCTEST_SUBCASE("ids large range with singles with to")
  {
    std::vector<size_t> range(100'000);
    std::iota(range.begin(), range.end(), 42);
    range[0] = 1;
    range[range.size() - 1]++;
    std::string ret = Ioss::Utils::format_id_list(range);
    REQUIRE(ret == std::string("1, 43 to 100040, 100042"));
  }

  DOCTEST_SUBCASE("ids large range with singles with colon")
  {
    std::vector<size_t> range(100'000);
    std::iota(range.begin(), range.end(), 42);
    range[0] = 1;
    range[range.size() - 1]++;
    std::string ret = Ioss::Utils::format_id_list(range, ":");
    REQUIRE(ret == std::string("1, 43:100040, 100042"));
  }

  DOCTEST_SUBCASE("ids large range with singles with words")
  {
    std::vector<size_t> range(100'000);
    std::iota(range.begin(), range.end(), 42);
    range[0] = 1;
    range[range.size() - 1]++;
    std::string ret = Ioss::Utils::format_id_list(range, " to ", " and ");
    REQUIRE(ret == std::string("1 and 43 to 100040 and 100042"));
  }

  DOCTEST_SUBCASE("detect unsorted two ids") { CHECK_THROWS(Ioss::Utils::format_id_list({2, 1})); }

  DOCTEST_SUBCASE("detect unsorted ids")
  {
    CHECK_THROWS(Ioss::Utils::format_id_list({1, 2, 3, 4, 5, 1}));
  }

  DOCTEST_SUBCASE("detect duplicate ids")
  {
    CHECK_THROWS(Ioss::Utils::format_id_list({1, 2, 3, 3, 4, 5, 6}));
  }
}
#endif
