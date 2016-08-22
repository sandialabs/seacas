// Copyright(C) 2015, 2016
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
  class Region;

  struct ZoneConnectivity
  {
    ZoneConnectivity(const std::string name, int owner_zone, const std::string donor_name,
                     int donor_zone, const std::array<int, 3> p_transform,
                     const std::array<int, 3> range_beg, const std::array<int, 3> range_end,
                     const std::array<int, 3> donor_beg, const std::array<int, 3> donor_end)
        : m_connectionName(std::move(name)), m_donorName(std::move(donor_name)),
          m_transform(std::move(p_transform)), m_rangeBeg(std::move(range_beg)),
          m_rangeEnd(std::move(range_end)), m_donorRangeBeg(std::move(donor_beg)),
          m_donorRangeEnd(std::move(donor_end)), m_ownerZone(owner_zone), m_donorZone(donor_zone),
          m_donorProcessor(-1), m_sameRange(false)
    {
    }

    ZoneConnectivity(const ZoneConnectivity &copy_from) = default;

    // Return number of nodes in the connection shared with the donor zone.
    size_t get_shared_node_count() const
    {
      size_t snc = 1;
      for (int i = 0; i < 3; i++) {
        snc *= (std::abs(m_rangeEnd[i] - m_rangeBeg[i]) + 1);
      }
      return snc;
    }

    bool owns_shared_nodes() const { return m_donorZone == -1 || m_ownerZone < m_donorZone; }

    std::array<int, 9> transform_matrix() const;
    std::array<int, 3> transform(const std::array<int, 9> &t_matrix,
                                 const std::array<int, 3> &index_1) const;
    std::array<int, 3> inverse_transform(const std::array<int, 9> &t_matrix,
                                         const std::array<int, 3> &index_1) const;

    std::vector<int> get_range(int ordinal) const;

    std::string m_connectionName;
    std::string m_donorName;
    std::array<int, 3> m_transform;
    std::array<int, 3> m_rangeBeg;
    std::array<int, 3> m_rangeEnd;
    std::array<int, 3> m_donorRangeBeg;
    std::array<int, 3> m_donorRangeEnd;

    friend std::ostream &operator<<(std::ostream &os, const ZoneConnectivity &zgc);

    // NOTE: Shared nodes are "owned" by the zone with the lowest zone id.
    int  m_ownerZone;      // "id" of zone that owns this connection
    int  m_donorZone;      // "id" of zone that is donor of this connection
    int  m_donorProcessor; // processor that owns the donor zone
    bool m_sameRange; // True if owner and donor range should always match...(special use during
                      // decomp)
  };

  struct BoundaryCondition
  {
    BoundaryCondition(const std::string name, const std::array<int, 3> range_beg,
                      const std::array<int, 3> range_end)
        : m_bcName(std::move(name)), m_rangeBeg(std::move(range_beg)),
          m_rangeEnd(std::move(range_end))
    {
    }

    BoundaryCondition(const BoundaryCondition &copy_from) = default;

    // Return number of cell faces in the BC
    size_t get_face_count() const
    {
      size_t cell_count = 1;
      for (int i = 0; i < 3; i++) {
        auto diff = std::abs(m_rangeEnd[i] - m_rangeBeg[i]);
        cell_count *= ((diff == 0) ? 1 : diff);
      }
      return cell_count;
    }

    std::vector<int> get_range(int ordinal) const;

    std::string m_bcName;
    std::array<int, 3> m_rangeBeg;
    std::array<int, 3> m_rangeEnd;

    friend std::ostream &operator<<(std::ostream &os, const BoundaryCondition &zgc);
  };

  class DatabaseIO;

  /** \brief A collection of structureds having the same topology.
   */
  class StructuredBlock : public EntityBlock
  {
  public:
    StructuredBlock(DatabaseIO *io_database, const std::string &my_name, int index_dim, int ni,
                    int nj = 0, int nk = 0, int off_i = 0, int off_j = 0, int off_k = 0);
    StructuredBlock(DatabaseIO *io_database, const std::string &my_name, int index_dim,
                    std::array<int, 3> &ordinal, std::array<int, 3> &offset,
                    std::array<int, 3> &global_ordinal);

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

    /** \brief Set the 'offset' for the block.
     *
     *  The 'offset' is used to map a cell or node location within a
     *  structured block to the model implicit cell or node location
     *  on a single processor.  zero-based.
     *
     *  The 'global' offsets do the same except for they apply over
     *  the entire model on all processors. zero-based.
     *
     *  For example, the file descriptor (1-based) of
     *  the 37th cell in the 4th block is calculated by:
     *
     *  file_descriptor = offset of block 4 + 37
     *
     *  This can also be used to determine which structured block
     *  a cell with a file_descriptor maps into. An particular
     *  structured block contains all cells in the range:
     *
     *  offset < file_descriptor <= offset+number_cells_per_block
     *
     *  Note that for nodes, the nodeOffset does not take into account
     *  the nodes that are shared between blocks.
     */
    void set_node_offset(size_t offset) { m_nodeOffset = offset; }
    void set_cell_offset(size_t offset) { m_cellOffset = offset; }
    void set_node_global_offset(size_t offset) { m_nodeGlobalOffset = offset; }
    void set_cell_global_offset(size_t offset) { m_cellGlobalOffset = offset; }

    size_t get_node_offset() const { return m_nodeOffset; }
    size_t get_cell_offset() const { return m_cellOffset; }
    size_t get_node_global_offset() const { return m_nodeGlobalOffset; }
    size_t get_cell_global_offset() const { return m_cellGlobalOffset; }

    // Get the local (relative to this block on this processor) node
    // id at the specified i,j,k location (1 <= i,j,k <= ni+1,nj+1,nk+1).  1-based.
    size_t get_local_node_id(size_t i, size_t j, size_t k) const
    {
      return (k - 1) * (m_ni + 1) * (m_nj + 1) + (j - 1) * (m_ni + 1) + i;
    }

    // Get the local (relative to this block on this processor) node id at the specified
    // i,j,k location (1 <= i,j,k <= ni+1,nj+1,nk+1).  0-based.
    size_t get_local_node_offset(size_t i, size_t j, size_t k) const
    {
      return (k - 1) * (m_ni + 1) * (m_nj + 1) + (j - 1) * (m_ni + 1) + i - 1;
    }

    // Get the global (on this processor) cell-node offset at the specified
    // i,j,k location (1 <= i,j,k <= ni+1,nj+1,nk+1).  0-based.
    size_t get_global_node_offset(size_t i, size_t j, size_t k) const

    {
      return get_local_node_offset(i, j, k) + m_nodeOffset;
    }

    // Get the global node id (on this processor) at the specified
    // i,j,k location (1 <= i,j,k <= ni+1,nj+1,nk+1).  0-based.  This
    // is the position in the global node block (on this processor) of
    // the node at i,j,k.
    size_t get_global_node_id(size_t i, size_t j, size_t k) const
    {
      assert(!m_globalNodeIdList.empty());
      size_t offset = get_local_node_offset(i, j, k);
      return m_globalNodeIdList[offset];
    }

    template <typename INT> size_t get_cell_node_ids(INT *idata, bool add_offset) const
    {
      // Fill 'idata' with the cell node ids which are the
      // 1-based location of each node in this zone
      // The location is based on the "model" (all processors) zone.
      // If this is a parallel decomposed model, then
      // this block may be a subset of the "model" zone
      //
      // if 'add_offset' is true, then add the m_cellGlobalOffset
      // which changes the location to be the location in the
      // entire "mesh" instead of within a "zone" (all processors)

      size_t index  = 0;
      size_t offset = add_offset ? m_nodeGlobalOffset : 0;

      if (m_nk == 0 && m_nj == 0 && m_ni == 0) {
        return index;
      }

      for (int kk = 0; kk < m_nk + 1; kk++) {
        int k = m_offsetK + kk;
        for (int jj = 0; jj < m_nj + 1; jj++) {
          int j = m_offsetJ + jj;
          for (int ii = 0; ii < m_ni + 1; ii++) {
            int i = m_offsetI + ii;

            size_t ind = k * (m_niGlobal + 1) * (m_njGlobal + 1) + j * (m_niGlobal + 1) + i;

            idata[index++] = ind + offset + 1;
          }
        }
      }
      return index;
    }

    template <typename INT> size_t get_cell_ids(INT *idata, bool add_offset) const
    {
      // Fill 'idata' with the cell ids which are the
      // 1-based location of each cell in this zone
      // The location is based on the "model" zone.
      // If this is a parallel decomposed model, then
      // this block may be a subset of the "model" zone
      //
      // if 'add_offset' is true, then add the m_cellGlobalOffset
      // which changes the location to be the location in the
      // entire "mesh" instead of within a "zone"

      size_t index  = 0;
      size_t offset = add_offset ? m_cellGlobalOffset : 0;

      if (m_nk == 0 && m_nj == 0 && m_ni == 0) {
        return index;
      }

      for (int kk = 0; kk < m_nk; kk++) {
        int k = m_offsetK + kk;
        for (int jj = 0; jj < m_nj; jj++) {
          int j = m_offsetJ + jj;
          for (int ii = 0; ii < m_ni; ii++) {
            int i = m_offsetI + ii;

            size_t ind = k * m_niGlobal * m_njGlobal + j * m_niGlobal + i;

            idata[index++] = ind + offset + 1;
          }
        }
      }
      return index;
    }

    void generate_shared_nodes(const Ioss::Region &region);

    bool contains(size_t global_offset) const
    {
      return (global_offset >= m_nodeOffset &&
              global_offset < m_nodeOffset + get_property("node_count").get_int());
    }

  protected:
    int64_t internal_get_field_data(const Field &field, void *data,
                                    size_t data_size) const override;

    int64_t internal_put_field_data(const Field &field, void *data,
                                    size_t data_size) const override;

  private:
    void add_properties_and_fields(int index_dim);

    int m_ni;
    int m_nj;
    int m_nk;

    int m_offsetI; // Valid 'i' ordinal runs from m_offsetI+1 to m_offsetI+m_ni
    int m_offsetJ;
    int m_offsetK;

    int m_niGlobal; // The ni,nj,nk of the master block this is a subset of.
    int m_njGlobal;
    int m_nkGlobal;

    size_t m_nodeOffset;
    size_t m_cellOffset;

    size_t m_nodeGlobalOffset;
    size_t m_cellGlobalOffset;

    Ioss::NodeBlock m_nodeBlock;

  public:
    std::vector<ZoneConnectivity>  m_zoneConnectivity;
    std::vector<BoundaryCondition> m_boundaryConditions;
    mutable std::vector<ssize_t>   m_globalNodeIdList;
  };
}
#endif
