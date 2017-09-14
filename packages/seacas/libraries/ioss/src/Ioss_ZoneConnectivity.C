// Copyright(C) 1999-2017 National Technology & Engineering Solutions
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

#include <Ioss_ZoneConnectivity.h>
#include <cstddef> // for size_t
#include <string> // for string
#include <vector> // for vector

namespace {
  int sign(int value) { return value < 0 ? -1 : 1; }

  int del(int v1, int v2) { return static_cast<int>(std::abs(v1) == std::abs(v2)); }

} // namespace

namespace Ioss {
  std::ostream &operator<<(std::ostream &os, const ZoneConnectivity &zgc)
  {
    std::array<std::string, 7> tf{{"-k", "-j", "-i", " ", "i", "j", "k"}};

    // 0 -3 -k
    // 1 -2 -j
    // 2 -1 -i
    // 3
    // 4  1  i
    // 5  2  j
    // 6  3  k
    std::string transform = "[i..";
    transform += tf[zgc.m_transform[0] + 3];
    transform += " j..";
    transform += tf[zgc.m_transform[1] + 3];
    transform += " k..";
    transform += tf[zgc.m_transform[2] + 3];
    transform += "] ";

    os << "\t\t" << zgc.m_donorName << "[P" << zgc.m_donorProcessor << "]:\tDZ " << zgc.m_donorZone
       << "\tName '" << zgc.m_connectionName << "' shares " << zgc.get_shared_node_count()
       << " nodes. (Owned = " << (zgc.owns_shared_nodes() ? "true" : "false") << ")."
       << "\n\t\t\t\tRange: [" << zgc.m_rangeBeg[0] << ".." << zgc.m_rangeEnd[0] << ", "
       << zgc.m_rangeBeg[1] << ".." << zgc.m_rangeEnd[1] << ", " << zgc.m_rangeBeg[2] << ".."
       << zgc.m_rangeEnd[2] << "]\tDonor Range: [" << zgc.m_donorRangeBeg[0] << ".."
       << zgc.m_donorRangeEnd[0] << ", " << zgc.m_donorRangeBeg[1] << ".." << zgc.m_donorRangeEnd[1]
       << ", " << zgc.m_donorRangeBeg[2] << ".." << zgc.m_donorRangeEnd[2] << "]";
    return os;
  }

  std::vector<int> ZoneConnectivity::get_range(int ordinal) const
  {
    // Return the integer values for the specified range for the specified ordinal (1,2,3) ->
    // (i,j,k)
    ordinal--;
    int size  = std::abs(m_rangeBeg[ordinal] - m_rangeEnd[ordinal]) + 1;
    int delta = sign(m_rangeEnd[ordinal] - m_rangeBeg[ordinal]);
    assert(delta == 1 || delta == -1);

    std::vector<int> range(size);
    for (int i = 0; i < size; i++) {
      range[i] = m_rangeBeg[ordinal] + i * delta;
    }
    return range;
  }

  std::array<INT, 9> ZoneConnectivity::transform_matrix() const
  {
    std::array<INT, 9> t_matrix{};
    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
        t_matrix[3 * i + j] = sign(m_transform[j]) * del(m_transform[j], i + 1);
      }
    }
    return t_matrix;
  }

  Ioss::IJK_t ZoneConnectivity::transform(const Ioss::IJK_t &index_1) const
  {
    auto t_matrix = transform_matrix();

    Ioss::IJK_t diff{};
    Ioss::IJK_t donor{};

    diff[0] = index_1[0] - m_rangeBeg[0];
    diff[1] = index_1[1] - m_rangeBeg[1];
    diff[2] = index_1[2] - m_rangeBeg[2];

    donor[0] =
        t_matrix[0] * diff[0] + t_matrix[1] * diff[1] + t_matrix[2] * diff[2] + m_donorRangeBeg[0];
    donor[1] =
        t_matrix[3] * diff[0] + t_matrix[4] * diff[1] + t_matrix[5] * diff[2] + m_donorRangeBeg[1];
    donor[2] =
        t_matrix[6] * diff[0] + t_matrix[7] * diff[1] + t_matrix[8] * diff[2] + m_donorRangeBeg[2];

    assert(std::fabs(donor[0] - m_donorRangeBeg[0]) <=
           std::fabs(m_donorRangeBeg[0] - m_donorRangeEnd[0]));
    assert(std::fabs(donor[1] - m_donorRangeBeg[1]) <=
           std::fabs(m_donorRangeBeg[1] - m_donorRangeEnd[1]));
    assert(std::fabs(donor[2] - m_donorRangeBeg[2]) <=
           std::fabs(m_donorRangeBeg[2] - m_donorRangeEnd[2]));
    return donor;
  }

  // ----------------------------------------------------------------------------

  Ioss::IJK_t ZoneConnectivity::inverse_transform(const Ioss::IJK_t &index_1) const
  {
    auto t_matrix = transform_matrix();

    Ioss::IJK_t diff{};
    Ioss::IJK_t index{};

    diff[0] = index_1[0] - m_donorRangeBeg[0];
    diff[1] = index_1[1] - m_donorRangeBeg[1];
    diff[2] = index_1[2] - m_donorRangeBeg[2];

    index[0] =
        t_matrix[0] * diff[0] + t_matrix[3] * diff[1] + t_matrix[6] * diff[2] + m_rangeBeg[0];
    index[1] =
        t_matrix[1] * diff[0] + t_matrix[4] * diff[1] + t_matrix[7] * diff[2] + m_rangeBeg[1];
    index[2] =
        t_matrix[2] * diff[0] + t_matrix[5] * diff[1] + t_matrix[8] * diff[2] + m_rangeBeg[2];

    return index;
  }

} // namespace Ioss
