#include <Ioss_Map.h>
#include <Ioss_SmartAssert.h>
#include <algorithm>
#include <numeric>
#include <vector>

void test_one2one(Ioss::Map &my_map, size_t offset)
{
  size_t count = 128;
  my_map.map().resize(count + 1);
  my_map.map()[0] = -1;

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), offset + 1);

  my_map.set_map(init.data(), init.size(), 0, true);

  auto mmap = my_map.map();

  SMART_ASSERT(mmap[0] == -1);

  for (size_t i = 0; i < count; i++) {
    SMART_ASSERT(my_map.global_to_local(init[i]) == i + 1);
  }
}

void test_reorder(Ioss::Map &my_map, size_t offset)
{
  size_t count = my_map.map().size() - 1;

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), offset + 1);
  std::random_shuffle(init.begin(), init.end());

  my_map.set_map(init.data(), init.size(), 0, false);

  auto mmap = my_map.map();

  SMART_ASSERT(mmap[0] == 1);

  // Check that we get the *original* ordering back from global_to_local.
  for (size_t i = 0; i < count; i++) {
    SMART_ASSERT(my_map.global_to_local(init[i]) == init[i] - offset)(i)(init[i]);
  }

  // Check that we get the the `reordered` vector has been put into `db` order.
  std::vector<double> reordered(count);
  my_map.map_field_to_db_scalar_order(init.data(), reordered, 0, count, 1, 0);
  for (size_t i = 0; i < count; i++) {
    SMART_ASSERT(reordered[i] == offset + i + 1)(offset)(i + 1)(reordered[i]);
  }
}

void test_segment(Ioss::Map &my_map, size_t segments, size_t offset)
{
  size_t count    = 128;
  size_t seg_size = count / segments;
  SMART_ASSERT(count % segments == 0)(count)(segments);

  my_map.map().resize(count + 1);
  my_map.map()[0] = -1;

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), offset + 1);

  for (size_t i = 0; i < segments; i++) {
    my_map.set_map(&init[i * seg_size], seg_size, i * seg_size, true);
  }

  auto mmap = my_map.map();

  SMART_ASSERT(mmap[0] == -1);

  for (size_t i = 0; i < count; i++) {
    SMART_ASSERT(my_map.global_to_local(init[i]) == i + 1);
  }
}

int main()
{
  {
    // Simple case -- ids 1..count
    Ioss::Map my_map;
    test_one2one(my_map, 0);
    test_reorder(my_map, 0);
    test_reorder(my_map, 0);
  }

  {
    // Simple case with offset -- ids 1+offset..count+offset
    Ioss::Map my_map;
    test_one2one(my_map, 100);
    test_reorder(my_map, 100);
    test_reorder(my_map, 100);
  }

  {
    // Build incrementally -- ids 1..count in 4 segments
    Ioss::Map my_map;
    test_segment(my_map, 4, 0);
  }

  {
    // Build incrementally -- ids 1+offset..count+offset in 4 segments
    Ioss::Map my_map;
    test_segment(my_map, 4, 200);
  }
}
