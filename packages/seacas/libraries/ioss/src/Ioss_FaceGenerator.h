// Copyright(C) 1999-2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include <Ioss_Face.h>
#include <Ioss_FaceGeneratorUtils.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <map>

#include <utility>

namespace Ioss {
  class Region;

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
