#define CATCH_CONFIG_MAIN 
#include <catch.hpp>
#include <Ioss_Map.h>
#include <algorithm>
#include <exception>
#include <numeric>
#include <vector>

void verify_global_to_local(const Ioss::Map &my_map, const std::vector<int> &init)
{
  size_t count = my_map.map().size() - 1;
  REQUIRE(count == init.size());

  for (size_t i = 0; i < count; i++) {
    int global = init[i];
    REQUIRE((size_t)my_map.global_to_local(global) == i + 1);
  }
}

void test_reorder(Ioss::Map &my_map, std::vector<int> &init, size_t offset)
{
  std::random_shuffle(init.begin(), init.end());
  my_map.set_map(init.data(), init.size(), 0, false);
  REQUIRE(!my_map.is_sequential());
  
  // Check that we get the *original* ordering back from global_to_local.
  size_t count = init.size();
  for (size_t i = 0; i < count; i++) {
    REQUIRE(my_map.global_to_local(init[i]) == int(init[i] - offset));
  }

  // Check that we get the the `reordered` vector has been put into `db` order.
  std::vector<double> reordered(count);
  my_map.map_field_to_db_scalar_order(init.data(), reordered, 0, count, 1, 0);
  for (size_t i = 0; i < count; i++) {
    REQUIRE(reordered[i] == offset + i + 1);
  }
}

TEST_CASE("test random ids", "[random_ids]") 
{
  size_t count = 128;
  Ioss::Map my_map;
  my_map.set_size(count);

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), 2511);
  std::random_shuffle(init.begin(), init.end());
  for (auto &e : init) {
    e = 11 * e;
  }

  my_map.set_map(init.data(), init.size(), 0, true);

  REQUIRE(!my_map.is_sequential());
  REQUIRE(!my_map.is_sequential(true));
  REQUIRE_NOTHROW(verify_global_to_local(my_map, init));
}

TEST_CASE("test sequential map with offset", "[sequential offset]")
{
  size_t count = 128;
  Ioss::Map my_map;
  my_map.set_size(count);

  std::vector<int> init(count);
  std::vector<size_t> offsets{0, 123};
  std::vector<std::string> sections{"offset0", "offset123"};

  for (size_t i=0; i < offsets.size(); i++) {
    SECTION(sections[i]) {
      std::size_t offset = offsets[i];
      std::iota(init.begin(), init.end(), offset + 1);

      my_map.set_map(init.data(), init.size(), 0, true);

      REQUIRE(my_map.is_sequential());
      REQUIRE(my_map.is_sequential(true));
      REQUIRE_NOTHROW(verify_global_to_local(my_map, init));

      SECTION("Reorder-1") {
	test_reorder(my_map, init, offset);

	SECTION("Reorder-2") {
	  test_reorder(my_map, init, offset);
	}
      }
    }
  }
}

TEST_CASE("test segmented map creation", "[segment]")
{
  Ioss::Map my_map;
  size_t segments = 4;

  size_t count    = 128;
  my_map.set_size(count);

  size_t seg_size = count / segments;
  CHECK(count % segments == 0);

  std::vector<int> init(count);

  std::vector<size_t> offsets{0, 123};
  std::vector<std::string> sections{"offset0", "offset123"};

  for (size_t i=0; i < offsets.size(); i++) {
    SECTION(sections[i]) {
      std::size_t offset = offsets[i];
      std::iota(init.begin(), init.end(), offset + 1);

      for (size_t i = 0; i < segments; i++) {
	my_map.set_map(&init[i * seg_size], seg_size, i * seg_size, true);
	my_map.set_map(&init[i * seg_size], 0, i * seg_size, true); // make sure handles empty segments
      }

      REQUIRE(my_map.is_sequential()); // Based on m_map[0] setting.
      REQUIRE(my_map.is_sequential(true)); // Based on checking all values.
      REQUIRE_NOTHROW(verify_global_to_local(my_map, init));
    }
  }
}

TEST_CASE("test reverse segmented map creation", "[reverse segment]")
{
  Ioss::Map my_map;
  size_t segments = 4;

  size_t count    = 128;
  my_map.set_size(count);

  size_t seg_size = count / segments;
  CHECK(count % segments == 0);

  std::vector<int> init(count);

  std::vector<size_t> offsets{0, 123};
  std::vector<std::string> sections{"offset0", "offset123"};

  for (size_t i=0; i < offsets.size(); i++) {
    SECTION(sections[i]) {
      std::size_t offset = offsets[i];
      std::iota(init.begin(), init.end(), offset + 1);

      for (size_t j = 0; j < segments; j++) {
	size_t i = segments - j - 1;
	my_map.set_map(&init[i * seg_size], seg_size, i * seg_size, true);
	my_map.set_map(&init[i * seg_size], 0, i * seg_size, true); // make sure handles empty segments
      }

      REQUIRE(my_map.is_sequential());
      REQUIRE(my_map.is_sequential(true));
      REQUIRE_NOTHROW(verify_global_to_local(my_map, init));
    }
  }
}

TEST_CASE("test segment gap", "[segment gap]")
{
  size_t segments = 4;
  size_t count    = 128;
  size_t seg_size = count / segments;
  CHECK(count % segments == 0);

  Ioss::Map my_map;
  my_map.set_size(count);

  std::vector<int> init(count);

  std::vector<size_t> offsets{0, 123};
  std::vector<std::string> sections{"offset0", "offset123"};

  for (size_t i=0; i < offsets.size(); i++) {
    SECTION(sections[i]) {
      std::size_t offset = offsets[i];
      for (size_t j = 0; j < segments; j++) {
	size_t seg_begin = j * seg_size;
	size_t seg_end   = seg_begin + seg_size;

	for (size_t i = seg_begin; i < seg_end; i++) {
	  init[i] = i + j + offset + 1;
	}
      }

      for (size_t j = 0; j < segments; j++) {
	size_t i = segments - j - 1;
	my_map.set_map(&init[i * seg_size], seg_size, i * seg_size, true);
      }
      REQUIRE(!my_map.is_sequential());
      REQUIRE(!my_map.is_sequential(true));
      REQUIRE_NOTHROW(verify_global_to_local(my_map, init));
    }
  }
}

TEST_CASE("test small reverse", "[small reverse]")
{
  size_t count    = 2;
  Ioss::Map my_map;
  my_map.set_size(count);

  std::vector<int> init(2);

  init[0] = 1;
  init[1] = 3;
  my_map.set_map(&init[1], 1, 1, true);
  my_map.set_map(&init[0], 1, 0, true);

  REQUIRE(!my_map.is_sequential());
  REQUIRE(!my_map.is_sequential(true));
  REQUIRE_NOTHROW(verify_global_to_local(my_map, init));
}
