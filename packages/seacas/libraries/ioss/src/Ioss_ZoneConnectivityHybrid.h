// Copyright(C) 2017 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//
//     * Neither the name of NTESS nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#ifndef IOSS_Ioss_ZoneConnectivityHybrid_h
#define IOSS_Ioss_ZoneConnectivityHybrid_h

#include <Ioss_CodeTypes.h>
#include <array>
#include <cassert>
#include <string>

#if defined(SEACAS_HAVE_CGNS) && !defined(BUILT_IN_SIERRA)
#include <cgnstypes.h>
using INT = cgsize_t;
#else
// If this is not being built with CGNS, then default to using 32-bit integers.
// Currently there is no way to input/output a structured mesh without CGNS,
// so this block is simply to get things to compile and probably has no use.
using INT = int;
#endif

namespace Ioss {
  class Region;

  struct ZoneConnectivityHybrid
  {
    ZoneConnectivityHybrid(const std::string name, int owner_zone, const std::string donor_name,
                           int donor_zone)
        : m_connectionName(std::move(name)), m_donorName(std::move(donor_name)),
          m_ownerZone(owner_zone), m_donorZone(donor_zone)
    {
      assert(is_valid());
      m_isActive = has_faces();
    }

    ZoneConnectivityHybrid(const std::string name, int owner_zone, const std::string donor_name,
                           int donor_zone, bool from_decomp)
        : m_connectionName(std::move(name)), m_donorName(std::move(donor_name)),
          m_ownerZone(owner_zone), m_donorZone(donor_zone), m_fromDecomp(from_decomp)
    {
      // This constructor typically called from decomposition process.
      assert(is_valid());
      m_isActive = has_faces();
    }

    ZoneConnectivityHybrid(const ZoneConnectivityHybrid &copy_from) = default;

    // Return number of nodes in the connection shared with the donor zone.
    size_t get_shared_node_count() const { return m_structuredFaces.size(); }

    // Validate zgc -- if is_active(), then must have non-zero entries for all ranges.
    // transform must have valid entries.
    bool is_valid() const
    {
      return (m_structuredFaces.size() == m_unstructuredFaces.size()) &&
             (m_isActive != m_structuredFaces.empty());
    }
    bool has_faces() const { return !m_structuredFaces.empty() && !m_unstructuredFaces.empty(); }

    friend std::ostream &operator<<(std::ostream &os, const ZoneConnectivityHybrid &zgc);

    bool is_from_decomp() const { return m_fromDecomp; }
    bool is_active() const { return m_isActive && has_faces(); }

    std::string m_connectionName; // Name of the connection; either generated or from file
    std::string m_donorName; // Name of the zone (m_donorZone) to which this zone is connected via
                             // this connection.
    // NOTE: Shared nodes are "owned" by the zone with the lowest zone id.
    int    m_ownerZone{};        // "id" of zone that owns this connection
    int    m_donorZone{};        // "id" of zone that is donor (or other side) of this connection
    size_t m_ownerGUID{};        // globally-unique id of owner
    size_t m_donorGUID{};        // globally-unique id of donor
    int    m_ownerProcessor{-1}; // processor that owns the owner zone
    int    m_donorProcessor{-1}; // processor that owns the donor zone
    bool   m_sameRange{false};   // True if owner and donor range should always match...(special use
                                 // during decomp)
    // This class only valid for hybrid connection, so either owner is structured and donor
    // unstructured or vice versa.
    bool m_ownerStructured;

    // TODO: Would be nice to share these lists with the symmetric
    // instance; although if split across processor, doesn't save any
    // space.)
    std::vector<Ioss::IJK_t>             m_structuredFaces;
    std::vector<std::pair<int64_t, int>> m_unstructuredFaces; // Element index + local face number.

    // True if this zc is created due to processor decompositions in a parallel run
    bool m_fromDecomp{false};

    bool m_isActive{true}; // True if non-zero range. That is, it has at least one face
  };
} // namespace Ioss
#endif
