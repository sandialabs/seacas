// Copyright(C) 1999-2010
// Sandia Corporation. Under the terms of Contract
// DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
// certain rights in this software.
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
//     * Neither the name of Sandia Corporation nor the names of its
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

#ifndef IOSS_Ioss_StructuredBlock_h
#define IOSS_Ioss_StructuredBlock_h

#include <Ioss_BoundingBox.h>
#include <Ioss_CodeTypes.h>
#include <Ioss_EntityBlock.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_Property.h>
#include <array>
#include <assert.h>
#include <string>

namespace Ioss {
  struct ZoneConnectivity
  {
    ZoneConnectivity(const std::string name, const std::string donor_name,
                     const std::array<int, 3> transform, const std::array<int, 6> range,
                     const std::array<int, 6> donor_range)
        : m_connectionName(std::move(name)), m_donorName(std::move(donor_name)),
          m_transform(std::move(transform)), m_range(std::move(range)),
          m_donorRange(std::move(donor_range))
    {
    }

    std::string m_connectionName;
    std::string m_donorName;
    std::array<int, 3> m_transform;
    std::array<int, 6> m_range;
    std::array<int, 6> m_donorRange;
  };

  class DatabaseIO;

  /** \brief A collection of structureds having the same topology.
   */
  class StructuredBlock : public GroupingEntity
  {
  public:
    StructuredBlock(DatabaseIO *io_database, const std::string &my_name, int index_dim, int ni,
                    int nj = 0, int nk = 0);

    ~StructuredBlock() override;

    std::string type_string() const override { return "StructuredBlock"; }
    std::string short_type_string() const override { return "structuredblock"; }
    EntityType  type() const override { return STRUCTUREDBLOCK; }

    const Ioss::NodeBlock &get_node_block() const { return m_nodeBlock; }

    // Handle implicit properties -- These are calcuated from data stored
    // in the grouping entity instead of having an explicit value assigned.
    // An example would be 'element_block_count' for a region.
    Property get_implicit_property(const std::string &my_name) const override;

    AxisAlignedBoundingBox get_bounding_box() const;

  protected:
    int64_t internal_get_field_data(const Field &field, void *data,
                                    size_t data_size) const override;

    int64_t internal_put_field_data(const Field &field, void *data,
                                    size_t data_size) const override;

  private:
    int m_ni;
    int m_nj;
    int m_nk;

    Ioss::NodeBlock m_nodeBlock;

  public:
    std::vector<ZoneConnectivity> m_zoneConnectivity;
  };
}
#endif
