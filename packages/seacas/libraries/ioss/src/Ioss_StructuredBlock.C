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

#include "Ioss_BoundingBox.h"  // for AxisAlignedBoundingBox
#include "Ioss_FieldManager.h" // for FieldManager
#include <Ioss_DatabaseIO.h>   // for DatabaseIO
#include <Ioss_Region.h>
#include <Ioss_Field.h>        // for Field, etc
#include <Ioss_Property.h>     // for Property
#include <Ioss_StructuredBlock.h>
#include <stddef.h> // for size_t
#include <string>   // for string
#include <vector>   // for vector
#include <numeric>

namespace {
  const std::string SCALAR() { return std::string("scalar"); }
  const std::string VECTOR_2D() { return std::string("vector_2d"); }
  const std::string VECTOR_3D() { return std::string("vector_3d"); }

  int sign(int value) {
    return value < 0 ? -1 : 1;
  }

  int del(int v1, int v2) {
    return (std::abs(v1) == std::abs(v2));
  }

} // namespace

namespace Ioss {
  class Field;

  /** \brief Create a structured block.
   *
   *  \param[in] io_database The database associated with the region containing the structured
   * block.
   *  \param[in] my_name The structured block's name.
   *  \param[in] index_dim The dimensionality of the block -- 1D, 2D, 3D
   *  \param[in] ni The number of intervals in the (i) direction.
   *  \param[in] nj The number of intervals in the (j) direction. Zero if 1D
   *  \param[in] nk The number of intervals in the (k) direction. Zero if 2D
   */
  StructuredBlock::StructuredBlock(DatabaseIO *io_database, const std::string &my_name, int index_dim,
				   int ni, int nj, int nk, int off_i, int off_j, int off_k)
    : GroupingEntity(io_database, my_name, ni * (nj > 0 ? nj : 1) * (nk > 0 ? nk : 1)),
      m_ni(ni), m_nj(nj), m_nk(nk),
      m_offsetI(off_i), m_offsetJ(off_j), m_offsetK(off_k),
      m_nodeOffset(0), m_cellOffset(0), 
      m_nodeBlock(io_database, my_name + "_nodes",
		  (m_ni + 1) * (m_nj + 1) * (m_nk + 1), index_dim)
  {
    assert(index_dim == 1 || index_dim == 2 || index_dim == 3);

    int64_t cell_count = (m_ni == 0 ? 1 : m_ni) * (m_nj == 0 ? 1 : m_nj) * (m_nk == 0 ? 1 : m_nk);
    int64_t node_count = (m_ni + 1) * (m_nj + 1) * (m_nk + 1);

    properties.add(Property("component_degree", index_dim));
    properties.add(Property("node_count", node_count));
    properties.add(Property("cell_count", cell_count));
    properties.add(Property("ni", m_ni));
    properties.add(Property("nj", m_nj));
    properties.add(Property("nk", m_nk));
    properties.add(Property("offset_i", m_offsetI));
    properties.add(Property("offset_j", m_offsetJ));
    properties.add(Property("offset_k", m_offsetK));

    std::string vector_name;
    if (index_dim == 1) {
      vector_name = SCALAR();
    }
    else if (index_dim == 2) {
      vector_name = VECTOR_2D();
    }
    else if (index_dim == 3) {
      vector_name = VECTOR_3D();
    }
    fields.add(Ioss::Field("cell_node_ids", Ioss::Field::INTEGER, SCALAR(),
                           Ioss::Field::MESH, node_count));

    fields.add(Ioss::Field("mesh_model_coordinates", Ioss::Field::REAL, vector_name,
                           Ioss::Field::MESH, node_count));

    // Permit access 1-coordinate at a time
    fields.add(Ioss::Field("mesh_model_coordinates_x", Ioss::Field::REAL, SCALAR(),
                           Ioss::Field::MESH, node_count));
    if (index_dim > 1) {
      fields.add(Ioss::Field("mesh_model_coordinates_y", Ioss::Field::REAL, SCALAR(),
                             Ioss::Field::MESH, node_count));
    }

    if (index_dim > 2) {
      fields.add(Ioss::Field("mesh_model_coordinates_z", Ioss::Field::REAL, SCALAR(),
                             Ioss::Field::MESH, node_count));
    }
  }

  StructuredBlock::~StructuredBlock() = default;

  Property StructuredBlock::get_implicit_property(const std::string &my_name) const
  {
    return GroupingEntity::get_implicit_property(my_name);
  }

  int64_t StructuredBlock::internal_get_field_data(const Field &field, void *data,
                                                   size_t data_size) const
  {
    return get_database()->get_field(this, field, data, data_size);
  }

  int64_t StructuredBlock::internal_put_field_data(const Field &field, void *data,
                                                   size_t data_size) const
  {
    return get_database()->put_field(this, field, data, data_size);
  }

  AxisAlignedBoundingBox StructuredBlock::get_bounding_box() const
  {
    return get_database()->get_bounding_box(this);
  }

  void StructuredBlock::generate_shared_nodes(const Ioss::Region &region)
  {
    // First step in generating the map from "cell-node" to global node position
    // in the model with all duplicate nodes equived out.
    assert(m_globalNodeIdList.empty());
    
    size_t node_count = get_property("node_count").get_int();
    ssize_t ss_max = std::numeric_limits<ssize_t>::max();
    m_globalNodeIdList.resize(node_count, ss_max);
      
    // Iterate through all zoneConnectivity instances.  For each one
    // containing non-owned nodes, set the value of m_globalNodeIdList
    // to point to the owning nodes global_node_offset.
    // If a node in this block already has a value, then that node
    // is shared multiple times (at a corner of three blocks) and need
    // to resolve which of the nodes it points to is the owner...
    for (const auto &zgc : m_zoneConnectivity) {
      if (!zgc.owns_shared_nodes()) {
	// Iterate over the range of nodes on the interface...
	std::vector<int> i_range = zgc.get_range(1);
	std::vector<int> j_range = zgc.get_range(2);
	std::vector<int> k_range = zgc.get_range(3);
	
	auto owner_block = region.get_structured_block(zgc.m_donorName);
	assert(owner_block != nullptr);
	assert(!owner_block->m_globalNodeIdList.empty());

	const std::array<int,9> t_matrix = zgc.transform_matrix();
	for (auto &k : k_range) {
	  for (auto &j : j_range) {
	    for (auto &i : i_range) {
	      std::array<int,3> index {{i, j, k}};
	      std::array<int,3> owner = zgc.transform(t_matrix, index);
	      
	      if (zgc.m_ownerZone != zgc.m_donorZone) {
		// Convert main and owner i,j,k triplets into model-local m_globalNodeIdList
		size_t local_offset = get_local_node_offset(index[0], index[1], index[2]);
		size_t global_offset = owner_block->get_global_node_offset(owner[0], owner[1], owner[2]);

		if (m_globalNodeIdList[local_offset] != ss_max) {
		  // This node maps to two different nodes -- probably at a 3-way corner
		  // Need to adjust the node in 'owner_block' with id 'global_offset'
		  // to instead point to 'm_globalNodeIdList[local_offset]'
		  size_t owner_offset = owner_block->get_local_node_offset(owner[0], owner[1], owner[2]);
		  owner_block->m_globalNodeIdList[owner_offset] = m_globalNodeIdList[local_offset];
		}
		else {
		  m_globalNodeIdList[local_offset] = global_offset;
		}
	      }
	      else {
		// When mapping WITHIN a zone, need to avoid circular A->B and B->A. 
		// The GridConnectivity object will appear twice; once with each surface
		// being the "owner"...
		// Want to map only if local < owner_local
		// Convert main and owner i,j,k triplets into zone-local offsets
		size_t local_node  = get_local_node_offset(index[0], index[1], index[2]);
		size_t owner_node = get_local_node_offset(owner[0], owner[1], owner[2]);
		if (owner_node < local_node) {
		  size_t global_offset = get_global_node_offset(owner[0], owner[1], owner[2]);
		  m_globalNodeIdList[local_node] = global_offset;
		}
	      }
	    }
	  }
	}
      }
    }
    // At this point, the vector contains either "owned nodes" which have an entry
    // of 'ss_max', or shared nodes that it doesn't own which point to the global cell-node
    // offset of the owning node.
  }

  std::vector<int> ZoneConnectivity::get_range(int ordinal) const
  {
    // Return the integer values for the specified range for the specified ordinal (1,2,3) -> (i,j,k)
    int size = std::abs(m_range[(ordinal-1)] - m_range[(ordinal-1)+3])+1;
    int delta = sign(m_range[(ordinal-1)+3] - m_range[(ordinal-1)]);
    assert(delta == 1 || delta == -1);
    
    std::vector<int> range(size);
    for (int i=0; i < size; i++) {
      range[i] = m_range[(ordinal-1)] + i * delta;
    }
    return range;
  }
    
  std::array<int,9> ZoneConnectivity::transform_matrix() const
  {
    std::array<int,9> t_matrix;
    for (int i=0; i < 3; i++) {
      for (int j=0; j < 3; j++) {
	t_matrix[3*i+j] = sign(m_transform[j]) * del(m_transform[j], i+1);
      }
    }
    return t_matrix;
  }

  std::array<int,3> ZoneConnectivity::transform(const std::array<int,9> &t_matrix,
						const std::array<int,3> &index_1) const
  {
    std::array<int,3> diff;
    std::array<int,3> donor;

    diff[0] = index_1[0] - m_range[0];
    diff[1] = index_1[1] - m_range[1];
    diff[2] = index_1[2] - m_range[2];

    donor[0] = t_matrix[0] * diff[0] + t_matrix[1] * diff[1] + t_matrix[2] * diff[2] + m_donorRange[0];
    donor[1] = t_matrix[3] * diff[0] + t_matrix[4] * diff[1] + t_matrix[5] * diff[2] + m_donorRange[1];
    donor[2] = t_matrix[6] * diff[0] + t_matrix[7] * diff[1] + t_matrix[8] * diff[2] + m_donorRange[2];
   
    assert(std::fabs(donor[0]-m_donorRange[0]) <= std::fabs(m_donorRange[0]-m_donorRange[3]));
    assert(std::fabs(donor[1]-m_donorRange[1]) <= std::fabs(m_donorRange[1]-m_donorRange[4]));
    assert(std::fabs(donor[2]-m_donorRange[2]) <= std::fabs(m_donorRange[2]-m_donorRange[5]));
    return donor;
  }

  // ----------------------------------------------------------------------------

  std::array<int,3> ZoneConnectivity::inverse_transform(const std::array<int,9> &t_matrix,
							const std::array<int,3> &index_1) const
  {
    std::array<int, 3> diff;
    std::array<int, 3> index;

    diff[0] = index_1[0] - m_donorRange[0];
    diff[1] = index_1[1] - m_donorRange[1];
    diff[2] = index_1[2] - m_donorRange[2];

    index[0] = t_matrix[0] * diff[0] + t_matrix[3] * diff[1] + t_matrix[6] * diff[2] + m_range[0];
    index[1] = t_matrix[1] * diff[0] + t_matrix[4] * diff[1] + t_matrix[7] * diff[2] + m_range[1];
    index[2] = t_matrix[2] * diff[0] + t_matrix[5] * diff[1] + t_matrix[8] * diff[2] + m_range[2];
    
    return index;
  }

} // namespace Ioss
