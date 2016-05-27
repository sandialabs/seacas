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

#include "Ioss_FieldManager.h" // for FieldManager
#include <Ioss_DatabaseIO.h>   // for DatabaseIO
#include <Ioss_StructuredBlock.h>
#include <Ioss_Field.h>    // for Field, etc
#include <Ioss_Property.h> // for Property
#include <stddef.h>        // for size_t
#include <string>          // for string
#include <vector>          // for vector

namespace Ioss {
  class Field;

  /** \brief Create a structured block.
   *
   *  \param[in] io_database The database associated with the region containing the structured block.
   *  \param[in] my_name The structured block's name.
   *  \param[in] ni The number of intervals in the (i) direction.
   *  \param[in] nj The number of intervals in the (j) direction. Zero if 1D
   *  \param[in] nk The number of intervals in the (k) direction. Zero if 2D
   */
  StructuredBlock::StructuredBlock(DatabaseIO *io_database, const std::string &my_name,
				   int ni, int nj, int nk)
      : GroupingEntity(io_database, my_name, 
		       ni * (nj > 0 ? nj : 1) * (nk > 0 ? nk : 1)),
	m_ni(ni), m_nj(nj), m_nk(nk)
  {
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

} // namespace Ioss
