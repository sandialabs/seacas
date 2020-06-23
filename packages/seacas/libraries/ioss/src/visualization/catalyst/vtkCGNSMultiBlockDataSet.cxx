// Copyright(C) 1999 - 2010-2017 National Technology &Engineering Solutions
// of Sandia, LLC(NTESS).Under the terms of Contract DE - NA0003525 with
// NTESS, the U.S.Government retains certain rights in this software.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following
// disclaimer in the documentation and / or other materials provided
// with the                                                 distribution.
//
// * Neither the name of NTESS nor the names of its
// contributors may be used to endorse or promote products derived
// from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "include/vtkCGNSMultiBlockDataSet.h"
#include "vtkObjectFactory.h"
#include "vtkInformation.h"
#include "vtkStructuredGrid.h"
#include "vtkPoints.h"
#include "vtkDoubleArray.h"
#include "vtkPointData.h"
#include "vtkCellData.h"
#include <sstream>

namespace {

const unsigned int BASES_BLOCK_ID   = 0;
const char *       BASES_BLOCK_NAME = "Bases";

const unsigned int ZONES_BLOCK_ID   = 0;
const char *       ZONES_BLOCK_NAME = "Zones";

}

vtkStandardNewMacro(vtkCGNSMultiBlockDataSet);

vtkCGNSMultiBlockDataSet::vtkCGNSMultiBlockDataSet()
{
  vtkMultiBlockDataSet* b = vtkMultiBlockDataSet::New();
  this->SetBlock(BASES_BLOCK_ID, b);
  this->GetMetaData(BASES_BLOCK_ID)->Set(vtkCompositeDataSet::NAME(), BASES_BLOCK_NAME);
  b->Delete();
}

vtkCGNSMultiBlockDataSet::~vtkCGNSMultiBlockDataSet()
{
}

void vtkCGNSMultiBlockDataSet::CreateBase(int base_id,
                                          const std::string& base_name)
{
  if(base_id_to_base_map.find(base_id) !=
     base_id_to_base_map.end()) {
    return;
  }

  vtkMultiBlockDataSet *b = vtkMultiBlockDataSet::SafeDownCast(this->GetBlock(BASES_BLOCK_ID));
  vtkMultiBlockDataSet *nb = vtkMultiBlockDataSet::New();
  vtkMultiBlockDataSet *zb = vtkMultiBlockDataSet::New();
  nb->SetBlock(ZONES_BLOCK_ID, zb);
  nb->GetMetaData(ZONES_BLOCK_ID)->Set(vtkCompositeDataSet::NAME(), ZONES_BLOCK_NAME);
  int location = b->GetNumberOfBlocks();
  b->SetBlock(location, nb);
  b->GetMetaData(location)->Set(vtkCompositeDataSet::NAME(), base_name);
  nb->Delete();
  zb->Delete();

  base bs;
  bs.base_location = location;

  base_id_to_base_map[base_id] = bs; 
}

void vtkCGNSMultiBlockDataSet::AddStructuredZoneData(int base_id,
                                                     int zone_id,
                                                     const std::string& zone_name,
                                                     const std::string& data_name,
                                                     int ni,
                                                     int nj,
                                                     int nk,
                                                     int comp_count,
                                                     bool is_cell_field,
                                                     char field_suffix_separator,
                                                     double* data,
                                                     int size)
{
  if(base_id_to_base_map.find(base_id) ==
     base_id_to_base_map.end()) {
    return;
  }

  base& bs = base_id_to_base_map[base_id];
  int dims[3] = {ni+1,nj+1,nk+1};
  vtkMultiBlockDataSet *bases = vtkMultiBlockDataSet::SafeDownCast(this->GetBlock(BASES_BLOCK_ID));
  vtkMultiBlockDataSet *base = vtkMultiBlockDataSet::SafeDownCast(bases->GetBlock(bs.base_location));
  vtkMultiBlockDataSet *zones = vtkMultiBlockDataSet::SafeDownCast(base->GetBlock(ZONES_BLOCK_ID));

  if(bs.zone_id_to_zone_location_map.find(zone_id) ==
     bs.zone_id_to_zone_location_map.end()) {
    int location = zones->GetNumberOfBlocks();
    vtkStructuredGrid* sg = vtkStructuredGrid::New();
    vtkPoints* pts = vtkPoints::New();
    sg->SetDimensions(dims);
    pts->Allocate(dims[0]*dims[1]*dims[2]);
    sg->SetPoints(pts);
    zones->SetBlock(location, sg);
    zones->GetMetaData(location)->Set(vtkCompositeDataSet::NAME(), zone_name);
    sg->Delete();
    pts->Delete();
    bs.zone_id_to_zone_location_map[zone_id] = location;
  }

  int zone_location = bs.zone_id_to_zone_location_map[zone_id];
  vtkStructuredGrid *sg = vtkStructuredGrid::SafeDownCast(zones->GetBlock(zone_location));

  int index = -1;
  if(data_name == "mesh_model_coordinates_x") {
    index = 0;
  }
  else if(data_name == "mesh_model_coordinates_y") {
    index = 1;
  }
  else if(data_name == "mesh_model_coordinates_z") {
    index = 2;
  }

  if(index >= 0) {
    vtkPoints* pts = sg->GetPoints();
    for(int i=0; i<size;i++) {
      double p[3];
      pts->GetPoint(i, p);
      p[index] = data[i];
      pts->InsertPoint(i, p);
    }
  }
  else {

    for(int i=0; i<comp_count; i++) {
      vtkDoubleArray* da = vtkDoubleArray::New();
      std::ostringstream oss;
      oss << i + 1;
      da->SetName((data_name + field_suffix_separator + oss.str()).c_str());
      da->SetNumberOfComponents(1);
      da->SetNumberOfTuples(size);
      for(int j=0; j<size;j++) {
        da->InsertValue(j, data[comp_count * j + i]);
      }
 
      if(is_cell_field) {
        sg->GetCellData()->AddArray(da);
      }
      else {
        sg->GetPointData()->AddArray(da);
      }
      da->Delete();
    }
  }
}

void vtkCGNSMultiBlockDataSet::PrintSelf(ostream &os, vtkIndent indent) {}
