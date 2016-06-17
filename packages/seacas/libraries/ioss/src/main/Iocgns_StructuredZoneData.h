/*
 * Copyright (c) 2015, 2016 Sandia Corporation.
 * Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 * the U.S. Government retains certain rights in this software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of Sandia Corporation nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
#ifndef IOCGNS_STRUCTUREDZONEDATA_H
#define IOCGNS_STRUCTUREDZONEDATA_H

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>

namespace Iocgns {

  class StructuredZoneData
  {
  public:
    StructuredZoneData()
        : m_ordinal{{0, 0, 0}}, m_offset{{0, 0, 0}}, m_zone(0), m_adam(nullptr), m_parent(nullptr),
          m_proc(-1), m_splitOrdinal(0), m_child1(nullptr), m_child2(nullptr), m_sibling(nullptr)
    {
    }

    StructuredZoneData(int zone, int ni, int nj, int nk)
        : m_ordinal{{ni, nj, nk}}, m_offset{{0, 0, 0}}, m_zone(zone), m_adam(nullptr),
          m_parent(nullptr), m_proc(-1), m_splitOrdinal(0), m_child1(nullptr), m_child2(nullptr),
          m_sibling(nullptr)
    {
    }

    std::array<int, 3> m_ordinal;

    // Offset of this block relative to its
    // adam block. ijk_adam = ijk_me + m_offset[ijk];
    std::array<int, 3> m_offset;

    int m_zone;

    // The zone in the undecomposed model that this zone is a
    // descendant of.  If not decomposed, then m_zone == m_adam;
    StructuredZoneData *m_adam;

    // If this zone was the result of splitting another zone, then
    // what is the zone number of that zone.  Zones are kept in a
    // vector and the zone number is its position in that vector+1
    // to make it 1-based and match numbering on file.
    StructuredZoneData *m_parent;

    int m_proc; // The processor this block might be run on...

    // Which ordinal of the parent was split to generate this zone and its sibling.
    int m_splitOrdinal;

    // The two zones that were split off from this zone.
    // Might be reasonable to do a 3-way or n-way split, but for now
    // just do a 2-way.
    StructuredZoneData *m_child1;
    StructuredZoneData *m_child2;

    StructuredZoneData *m_sibling;

    bool is_active() const
    {
      // Zone is active if it hasn't been split.
      return m_child1 == nullptr && m_child2 == nullptr;
    }

    // Assume the "work" or computational effort required for a
    // block is proportional to the number of nodes.
    size_t work() const
    {
      //     return (m_ordinal[0]+1) * (m_ordinal[1]+1) * (m_ordinal[2]+1);
      return m_ordinal[0] * m_ordinal[1] * m_ordinal[2];
    }

    // Split this StructuredZone along the largest ordinal
    // into two children and return the created zones.
    std::pair<StructuredZoneData *, StructuredZoneData *> split(int zone_id)
    {
      assert(is_active());

      // Find ordinal with largest value... Split along that ordinal
      int ordinal = 0;
      if (m_ordinal[1] > m_ordinal[ordinal]) {
        ordinal = 1;
      }
      if (m_ordinal[2] > m_ordinal[ordinal]) {
        ordinal = 2;
      }

      if (m_ordinal[ordinal] <= 1) {
        return std::make_pair(nullptr, nullptr);
      }

      StructuredZoneData *child1 = new StructuredZoneData;
      StructuredZoneData *child2 = new StructuredZoneData;

      child1->m_ordinal          = m_ordinal;
      child1->m_ordinal[ordinal] = child1->m_ordinal[ordinal] / 2;
      child1->m_offset           = m_offset; // Child1 offsets the same as parent;

      child1->m_zone         = zone_id++;
      child1->m_adam         = m_adam;
      child1->m_parent       = this;
      child1->m_splitOrdinal = ordinal;
      child1->m_sibling      = child2;

      child2->m_ordinal          = m_ordinal;
      child2->m_ordinal[ordinal] = m_ordinal[ordinal] - child1->m_ordinal[ordinal];
      child2->m_offset           = m_offset;
      child2->m_offset[ordinal] += child1->m_ordinal[ordinal];

      child2->m_zone         = zone_id++;
      child2->m_adam         = m_adam;
      child2->m_parent       = this;
      child2->m_splitOrdinal = ordinal;
      child2->m_sibling      = child1;

      m_child1 = child1;
      m_child2 = child2;

      std::cerr << "Zone " << m_zone << "(" << m_adam->m_zone << ") with intervals " << m_ordinal[0]
                << " " << m_ordinal[1] << " " << m_ordinal[2] << " work = " << work()
                << " with offset " << m_offset[0] << " " << m_offset[1] << " " << m_offset[2]
                << " split along ordinal " << ordinal << "\n"
                << "\tChild 1: Zone " << child1->m_zone << "(" << child1->m_adam->m_zone
                << ") with intervals " << child1->m_ordinal[0] << " " << child1->m_ordinal[1] << " "
                << child1->m_ordinal[2] << " work = " << child1->work() << " with offset "
                << child1->m_offset[0] << " " << child1->m_offset[1] << " " << child1->m_offset[2]
                << "\n"
                << "\tChild 2: Zone " << child2->m_zone << "(" << child1->m_adam->m_zone
                << ") with intervals " << child2->m_ordinal[0] << " " << child2->m_ordinal[1] << " "
                << child2->m_ordinal[2] << " work = " << child2->work() << " with offset "
                << child2->m_offset[0] << " " << child2->m_offset[1] << " " << child2->m_offset[2]
                << "\n";

      return std::make_pair(child1, child2);
    }
  };
}
#endif
