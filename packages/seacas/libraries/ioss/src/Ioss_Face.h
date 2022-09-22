// Copyright(C) 2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include <Ioss_Utils.h>

#include <array>
#include <cstddef>
#include <fmt/ostream.h>

namespace Ioss {
  class Face
  {
  public:
    Face() = default;
    Face(size_t id, std::array<size_t, 4> conn) : connectivity_(std::move(conn)), hashId_(id) {}
    explicit Face(std::array<size_t, 4> conn);

    void add_element(size_t element_id) const
    {
      if (elementCount_ < 2) {
        element[elementCount_++] = element_id;
      }
      else {
        face_element_error(element_id);
      }
    }

    void add_element(size_t element_id, size_t face_ordinal) const
    {
      add_element(element_id * 10 + face_ordinal);
    }

    void face_element_error(size_t element_id) const;

    // NOTE: `element` is not used at all by `Face` or `FaceGenerator`
    // class, but is used by skinner to give a consistent element id
    // in cases where there is a hash collision (face.id).

    // NOTE: For interior faces, `element` will not be the same value
    // for each face where the `hashId_` *will* be consistent for
    // interior faces.  Should only use this as an id if
    // `elementCount_` is 1.

    // NOTE: This could be used to do parallel or block boundary
    // collision since it is calculated as 10*element_id + local_face,
    // you could recover element_id and local_face and then set up
    // parallel communication maps.  May need to save the proc it is
    // shared with also (code to do this is available in git history)
    mutable std::array<size_t, 2> element{};
    std::array<size_t, 4>         connectivity_{};

    size_t      hashId_{0};
    mutable int elementCount_{0}; // Should be max of 2 solid elements...
  };

  inline Face::Face(std::array<size_t, 4> conn) : connectivity_(std::move(conn))
  {
    for (const auto &node : connectivity_) {
      hashId_ += Ioss::Utils::id_hash(node);
    }
  }

  inline void Face::face_element_error(size_t element_id) const
  {
    std::ostringstream errmsg;
    fmt::print(errmsg,
               "ERROR: Face {} has more than two elements using it.\n"
               "       The element/local_face are: {}:{}, {}:{}, and {}:{}.\n"
               "       The face connectivity is {} {} {} {}.\n",
               hashId_, element[0] / 10, element[0] % 10, element[1] / 10, element[1] % 10,
               element_id / 10, element_id % 10, connectivity_[0], connectivity_[1],
               connectivity_[2], connectivity_[3]);
    IOSS_ERROR(errmsg);
  }
} // namespace Ioss
