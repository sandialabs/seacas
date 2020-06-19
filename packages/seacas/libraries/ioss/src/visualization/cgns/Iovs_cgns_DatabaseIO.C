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

/*--------------------------------------------------------------------*/
/*    Copyright 2000-2010 NTESS.                         */
/*    Under the terms of Contract DE-AC04-94AL85000, there is a       */
/*    non-exclusive license for use of this work by or on behalf      */
/*    of the U.S. Government.  Export of this program may require     */
/*    a license from the United States Government.                    */
/*--------------------------------------------------------------------*/

#include <visualization/cgns/Iovs_cgns_DatabaseIO.h>
#include <visualization/utils/Iovs_Utils.h>
#include <ParaViewCatalystCGNSAdapter.h>

#include "Ioss_Property.h"
#include "Ioss_Region.h"
#include "Ioss_State.h"
#include "Ioss_StructuredBlock.h"
#include "Ioss_Utils.h"

namespace Iovs_cgns {

    DatabaseIO::DatabaseIO(Ioss::Region *region, const std::string &filename,
        Ioss::DatabaseUsage db_usage, MPI_Comm communicator,
            const Ioss::PropertyManager &props)
        : Ioss::DatabaseIO(region, filename, db_usage,
                           communicator, props)
    {
      dbState = Ioss::STATE_UNKNOWN;
      this->pvcca = nullptr;
    }

    DatabaseIO::~DatabaseIO()
    {
    }

    bool DatabaseIO::begin__(Ioss::State state)
    {
      return true;
    }

    bool DatabaseIO::end__(Ioss::State state)
    {
      switch (state) {
      case Ioss::STATE_DEFINE_MODEL:
      {
        write_meta_data();
        break;
      }
      default:
        break;
      }
      return true;
    }
    
    bool DatabaseIO::begin_state__(int state, double time)
    {
      if (this->pvcca != nullptr) {
        this->pvcca->SetTimeData(time, state - 1);
      }
      return true;
    }

    bool DatabaseIO::end_state__(int state, double time)
    {
      if (this->pvcca != nullptr) {
        this->pvcca->PerformCoProcessing();;
      }
      return true;
    }

    void DatabaseIO::read_meta_data__() {}

    void DatabaseIO::write_meta_data()
    {
      if (this->pvcca == nullptr) {
        this->pvcca = Iovs::Utils::getInstance()\
            .createParaViewCatalystCGNSAdapterInstance();

        std::string ps = Iovs::Utils::getInstance()\
            .getCatalystPythonDriverPath();

        this->pvcca->CreateNewPipeline(ps.c_str(), ps.c_str());
      }

      if(this->pvcca != nullptr) {
        this->pvcca->CreateBase(0, "Base");
      }

      const auto &structured_blocks = this->get_region()->get_structured_blocks();
      int base = 0;
      int zone = 0;
      for (const auto &sb : structured_blocks) {
        sb->property_update("zone", zone);
        sb->property_update("base", base);
        zone++;
      }
    }

    int64_t DatabaseIO::put_field_internal(const Ioss::StructuredBlock *sb,
        const Ioss::Field &field, void *data, size_t data_size) const {

      Ioss::Field::RoleType role = field.get_role();
      size_t base = sb->get_property("base").get_int();
      size_t zone = sb->get_property("zone").get_int();
      size_t node_count = sb->get_property("node_count").get_int();
      size_t num_to_get = field.verify(data_size);
/*
      size_t rmin[3] = {0, 0, 0};
      size_t rmax[3] = {0, 0, 0};
*/

      auto var_type               = field.transformed_storage();
      int  comp_count             = var_type->component_count();
      char field_suffix_separator = get_field_separator();

      bool is_cell_field = true;
      if(node_count == num_to_get) {
        is_cell_field = false;
      }

/*
      if(is_cell_field) {
        if (num_to_get > 0) {
          rmin[0] = sb->get_property("offset_i").get_int() + 1;
          rmin[1] = sb->get_property("offset_j").get_int() + 1;
          rmin[2] = sb->get_property("offset_k").get_int() + 1;

          rmax[0] = rmin[0] + sb->get_property("ni").get_int() - 1;
          rmax[1] = rmin[1] + sb->get_property("nj").get_int() - 1;
          rmax[2] = rmin[2] + sb->get_property("nk").get_int() - 1;
        }
      }
      else if(num_to_get == node_count) {
        if (num_to_get > 0) {
          rmin[0] = sb->get_property("offset_i").get_int() + 1;
          rmin[1] = sb->get_property("offset_j").get_int() + 1;
          rmin[2] = sb->get_property("offset_k").get_int() + 1;

          rmax[0] = rmin[0] + sb->get_property("ni").get_int();
          rmax[1] = rmin[1] + sb->get_property("nj").get_int();
          rmax[2] = rmin[2] + sb->get_property("nk").get_int();
        }
      }

      assert(num_to_get == 0 || num_to_get == (rmax[0] - rmin[0] + 1) * (rmax[1] - rmin[1] + 1) *
                                                  (rmax[2] - rmin[2] + 1));
*/

      double *rdata = num_to_get > 0 ? static_cast<double *>(data) : nullptr;

      if (role == Ioss::Field::MESH) {
        if (field.get_name() == "mesh_model_coordinates_x" ||
            field.get_name() == "mesh_model_coordinates_y" ||
            field.get_name() == "mesh_model_coordinates_z") {
          if(this->pvcca != nullptr) {
            this->pvcca->AddStructuredZoneData(base,
                                               zone,
                                               sb->name(),
                                               field.get_name(),
                                               sb->get_property("ni").get_int(),
                                               sb->get_property("nj").get_int(),
                                               sb->get_property("nk").get_int(),
                                               comp_count,
                                               is_cell_field,
                                               field_suffix_separator,
                                               rdata,
                                               num_to_get);
          }
        }
        else if(field.get_name() == "mesh_model_coordinates") {
          int phys_dimension = get_region()->get_property("spatial_dimension").get_int();

          std::vector<double> coord(num_to_get);

          // ========================================================================
          // Repetitive code for each coordinate direction; use a lambda to consolidate...
          auto coord_lambda = [=, &coord](const char *ordinate, int ordinal) {
            // Data required by upper classes store x0, y0, z0, ... xn,
            // yn, zn. Data stored in cgns file is x0, ..., xn, y0,
            // ..., yn, z0, ..., zn so we have to allocate some scratch
            // memory to read in the data and then map into supplied
            // 'data'
            // Map to global coordinate position...
            for (size_t i = 0; i < num_to_get; i++) {
              coord[i] = rdata[phys_dimension * i + ordinal];
            }

            if(this->pvcca != nullptr) {
              this->pvcca->AddStructuredZoneData(base,
                                                 zone,
                                                 sb->name(),
                                                 ordinate,
                                                 sb->get_property("ni").get_int(),
                                                 sb->get_property("nj").get_int(),
                                                 sb->get_property("nk").get_int(),
                                                 comp_count,
                                                 is_cell_field,
                                                 field_suffix_separator,
                                                 coord.data(),
                                                 num_to_get);
            }
          };
          // ========================================================================

          coord_lambda("mesh_model_coordinates_x", 0);

          if (phys_dimension >= 2) {
            coord_lambda("mesh_model_coordinates_y", 1);
          }

          if (phys_dimension == 3) {
            coord_lambda("mesh_model_coordinates_z", 2);
          }
        }
      }
      else if (role == Ioss::Field::TRANSIENT) {
        if(this->pvcca != nullptr) {
          this->pvcca->AddStructuredZoneData(base,
                                             zone,
                                             sb->name(),
                                             field.get_name(),
                                             sb->get_property("ni").get_int(),
                                             sb->get_property("nj").get_int(),
                                             sb->get_property("nk").get_int(),
                                             comp_count,
                                             is_cell_field,
                                             field_suffix_separator,
                                             rdata,
                                             num_to_get);
        }
      }
      return num_to_get;
    }

    int64_t DatabaseIO::put_field_internal(const Ioss::ElementBlock *eb,
        const Ioss::Field &field, void *data, size_t data_size) const {

        size_t num_to_get = field.verify(data_size);
        return num_to_get;
    }

} // namespace Iovs_catalyst
