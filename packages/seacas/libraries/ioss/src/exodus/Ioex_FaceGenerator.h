// Copyright(C) 1999-2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include <Ioss_FaceGeneratorUtils.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <map>

#include <utility>

namespace Ioex {
  class FaceGenerator
  {
  public:
    explicit FaceGenerator(int exoid) : exodusFilePtr_(exoid) {}
    ~FaceGenerator() = default;

    template <typename INT>
    void generate_faces(MPI_Comm communicator, INT /*dummy*/, bool block_by_block,
                        bool use_local_ids);
    Ioss::FaceUnorderedSet &faces(int64_t id) { return faces_[id]; }

    //! Given a local node id (0-based), return the hashed value.
    size_t node_id_hash(size_t local_node_id) const { return hashIds_[local_node_id]; }

  private:
    template <typename INT> void hash_node_ids(const std::vector<INT> &node_ids);

    int                                       exodusFilePtr_{};
    std::map<int64_t, Ioss::FaceUnorderedSet> faces_;
    std::vector<size_t>                       hashIds_;
  };

} // namespace Ioex
