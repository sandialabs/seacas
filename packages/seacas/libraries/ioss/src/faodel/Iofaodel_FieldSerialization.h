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

#ifndef Iofaodel_FieldSerialization_h
#define Iofaodel_FieldSerialization_h

#include "Iofaodel_Utils.h"
#include <Iofaodel_Serialize.h>
#include <Ioss_GroupingEntity.h>
#include <Ioss_Property.h>
#include <Ioss_Field.h>

#include <string>
#include <vector>


namespace Iofaodel {

  size_t data_size(const Ioss::Field & f);

// Caller should write their own version of this
// FieldFunction should return a function or a lamba that matches the
// signature below. The function it returns may or may not capture variables
// that are given to the user-defined function.
// Some examples are given in this file and are also useful
//
using FieldFunction = std::function<void(const Ioss::Region &,
    const Ioss::GroupingEntity &, const Ioss::Field &)>;


// Applies FieldFunction 'op' to all fields encountered in the
// Ioss::Region and it's various Ioss::GroupingEntities
void map_fields(const Ioss::Region & region, FieldFunction op);


// Applies FieldFunction 'op' to all fields encountered in the
// Ioss::GroupingEntity
void map_fields(const Ioss::Region & region,
    const Ioss::GroupingEntity & grouping_entity, FieldFunction op);



lunasa::DataObject pack_field(const Ioss::Region & region,
    const Ioss::GroupingEntity & entity, const Ioss::Field & field);

lunasa::DataObject pack_field(const Ioss::Region & r,
    const Ioss::GroupingEntity & e, const Ioss::Field & f, void *data,
    size_t data_size);

// Put this in the meta data section of the LDO
struct field_entry_t {
  Ioss::Field::BasicType basic_type;
  Ioss::Field::RoleType role_type;
  bool is_implicit;
  bool is_valid;
  size_t raw_count;

  // value_entry_t storage;
  value_entry_t name;
  value_entry_t value;
  value_entry_t storage;
  size_t data_size; // Total size of data stored in LDO data section

  char data[0];

  explicit field_entry_t(const Ioss::Field & field,
      const size_t start = 0);

};

} // namespace

#endif
