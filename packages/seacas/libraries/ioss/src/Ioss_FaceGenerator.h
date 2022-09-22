// Copyright(C) 1999-2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include <Ioss_Face.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <map>

#define FG_USE_ROBIN
#if defined FG_USE_STD
#include <unordered_set>
#elif defined FG_USE_HOPSCOTCH
#include <hopscotch_set.h>
#elif defined FG_USE_ROBIN
#include <robin_set.h>
#endif

#include <utility>

namespace Ioss {
  class Region;

  struct FaceHash
  {
    size_t operator()(const Face &face) const { return face.hashId_; }
  };

  struct FaceEqual
  {
    bool operator()(const Face &left, const Face &right) const
    {
      if (left.hashId_ != right.hashId_) {
        return false;
      }
      // Hash (hashId_) is equal
      // Check whether same vertices (can be in different order)
      // Most (All?) of the time, there are no hashId_ collisions, so this test will not
      // find a difference and the function will return 'true'
      // However, for some reason, removing this check does not change the execution time
      // appreiciably...
      for (auto lvert : left.connectivity_) {
        if (std::find(right.connectivity_.cbegin(), right.connectivity_.cend(), lvert) ==
            right.connectivity_.cend()) {
          // Not found, therefore not the same.
          return false;
        }
      }
      return true;
    }
  };

#if defined FG_USE_STD
  using FaceUnorderedSet = std::unordered_set<Face, FaceHash, FaceEqual>;
#elif defined FG_USE_HOPSCOTCH
  using FaceUnorderedSet = tsl::hopscotch_set<Face, FaceHash, FaceEqual>;
  // using FaceUnorderedSet = tsl::hopscotch_pg_set<Face, FaceHash, FaceEqual>;
#elif defined FG_USE_ROBIN
  using FaceUnorderedSet = tsl::robin_set<Face, FaceHash, FaceEqual>;
  // using FaceUnorderedSet = tsl::robin_pg_set<Face, FaceHash, FaceEqual>;
#endif
  class FaceGenerator
  {
  public:
    explicit FaceGenerator(Ioss::Region &region);
    ~FaceGenerator() = default;

    template <typename INT>
    void generate_faces(INT /*dummy*/, bool block_by_block = false, bool local_ids = false);
    FaceUnorderedSet &faces(const std::string &name = "ALL") { return faces_[name]; }

    //! Given a local node id (0-based), return the hashed value.
    size_t node_id_hash(size_t local_node_id) const { return hashIds_[local_node_id]; }

  private:
    template <typename INT> void hash_node_ids(const std::vector<INT> &node_ids);

    Ioss::Region                           &region_;
    std::map<std::string, FaceUnorderedSet> faces_;
    std::vector<size_t>                     hashIds_;
  };

} // namespace Ioss
