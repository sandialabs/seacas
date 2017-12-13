#include <Ioss_Map.h>
#ifdef NDEBUG
#undef NDEBUG
#endif
#include <Ioss_SmartAssert.h>
#include <algorithm>
#include <numeric>
#include <vector>

void verify_global_to_local(const Ioss::Map &my_map, const std::vector<int> &init)
{
  size_t count = my_map.map().size() - 1;
  SMART_ASSERT(count == init.size());

  for (size_t i = 0; i < count; i++) {
    int global = init[i];
    SMART_ASSERT((size_t)my_map.global_to_local(global) == i + 1);
  }
}

void test_random()
{
  size_t count = 128;

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), 2511);
  std::random_shuffle(init.begin(), init.end());
  for (auto &e : init) {
    e = 11 * e;
  }

  Ioss::Map my_map;
  my_map.set_size(count);
  my_map.set_map(init.data(), init.size(), 0, true);

  SMART_ASSERT(!my_map.is_sequential());
  verify_global_to_local(my_map, init);
}

void test_one2one(Ioss::Map &my_map, size_t offset)
{
  size_t count = 128;
  my_map.set_size(count);

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), offset + 1);

  my_map.set_map(init.data(), init.size(), 0, true);

  SMART_ASSERT(my_map.is_sequential());
  verify_global_to_local(my_map, init);
}

void test_reorder(Ioss::Map &my_map, size_t offset)
{
  size_t count = my_map.map().size() - 1;

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), offset + 1);
  std::random_shuffle(init.begin(), init.end());

  my_map.set_map(init.data(), init.size(), 0, false);

  SMART_ASSERT(!my_map.is_sequential());

  // Check that we get the *original* ordering back from global_to_local.
  for (size_t i = 0; i < count; i++) {
    SMART_ASSERT(my_map.global_to_local(init[i]) == int(init[i] - offset))(i)(init[i]);
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

  my_map.set_size(count);

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), offset + 1);

  for (size_t i = 0; i < segments; i++) {
    my_map.set_map(&init[i * seg_size], seg_size, i * seg_size, true);
  }

  SMART_ASSERT(my_map.is_sequential());
  my_map.map()[0] = 0;
  SMART_ASSERT(my_map.is_sequential());

  verify_global_to_local(my_map, init);
}

void test_reverse_segment(Ioss::Map &my_map, size_t segments, size_t offset)
{
  size_t count    = 128;
  size_t seg_size = count / segments;
  SMART_ASSERT(count % segments == 0)(count)(segments);

  my_map.set_size(count);

  std::vector<int> init(count);
  std::iota(init.begin(), init.end(), offset + 1);

  for (size_t j = 0; j < segments; j++) {
    size_t i = segments - j - 1;
    my_map.set_map(&init[i * seg_size], seg_size, i * seg_size, true);
  }

  SMART_ASSERT(my_map.is_sequential());
  verify_global_to_local(my_map, init);
}

void test_segment_gap(size_t offset)
{
  std::cerr << "testing segment gap with offset = " << offset << "\n";
  size_t segments = 4;
  size_t count    = 128;
  size_t seg_size = count / segments;
  SMART_ASSERT(count % segments == 0)(count)(segments);

  Ioss::Map my_map;
  my_map.set_size(count);

  std::vector<int> init(count);
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

  SMART_ASSERT(!my_map.is_sequential());
  verify_global_to_local(my_map, init);
}

void test_small_reverse()
{
  size_t count    = 2;

  Ioss::Map my_map;
  my_map.set_size(count);

  std::vector<int> init(2);

  init[0] = 1;
  init[1] = 3;
  my_map.set_map(&init[1], 1, 1, true);
  my_map.set_map(&init[0], 1, 0, true);

  SMART_ASSERT(!my_map.is_sequential());
  verify_global_to_local(my_map, init);
}

int main()
{
  // Non-sequential, random
  test_random();

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

  {
    // Build incrementally -- ids 1+offset..count+offset in 4 segments
    // Do segment order 3, 2, 1, 0
    Ioss::Map my_map;
    test_reverse_segment(my_map, 4, 200);
  }

  // Each segment is one-to-one, but gap between segments...
  test_segment_gap(123);
  test_segment_gap(0);

  // Test small reverse build..
  test_small_reverse();
}
