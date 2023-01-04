// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "CatalystCGNSMesh.h"
#include "CatalystManager.h"
#include "vtkCellData.h"
#include "vtkDataAssembly.h"
#include "vtkDoubleArray.h"
#include "vtkInformation.h"
#include "vtkObjectFactory.h"
#include "vtkPartitionedDataSet.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkStructuredGrid.h"
#include <sstream>

namespace Iovs_cgns {

  CatalystCGNSMesh::CatalystCGNSMesh(Iovs::CatalystManager *cm,
                                     CatalystPipelineInfo  &catalystPipelineInfo)
  {

    vtkNew<vtkDataAssembly> assembly;
    assembly->SetRootNodeName("IOSS");
    auto id = assembly->AddNode("structured_blocks");
    this->vpdc->SetDataAssembly(assembly);

    this->catManager           = cm;
    this->catalystPipelineInfo = catalystPipelineInfo;
  }

  CatalystCGNSMesh::~CatalystCGNSMesh() {}

  vtkPartitionedDataSetCollection *CatalystCGNSMesh::getPartitionedDataSetCollection()
  {
    return this->vpdc.GetPointer();
  }

  void CatalystCGNSMesh::PerformCoProcessing(std::vector<int>         &error_and_warning_codes,
                                             std::vector<std::string> &error_and_warning_messages)
  {

    this->catManager->PerformCoProcessing(error_and_warning_codes, error_and_warning_messages,
                                          catalystPipelineInfo);
  }

  void CatalystCGNSMesh::SetTimeData(double currentTime, int timeStep)
  {
    this->catManager->SetTimeData(currentTime, timeStep, catalystPipelineInfo);
  }

  void CatalystCGNSMesh::ReleaseMemory() {}

  void CatalystCGNSMesh::logMemoryUsageAndTakeTimerReading()
  {
    this->catManager->logMemoryUsageAndTakeTimerReading(catalystPipelineInfo);
  }

  void CatalystCGNSMesh::Delete() { this->catManager->DeletePipeline(catalystPipelineInfo); }

  void CatalystCGNSMesh::AddStructuredZoneData(const ZoneData &zoneData)
  {
    int dims[3] = {zoneData.ni + 1, zoneData.nj + 1, zoneData.nk + 1};

    // if this is an empty block, we just need to make a NULL grid with the
    // proper name, and we don't need to add any data variables
    // alternative is to make block with dims of (I think) -1,-1,-1
    // or extents of [0,-1,0,-1,0,-1] (I think)
    if ((zoneData.ni == 0) or (zoneData.nj == 0) or (zoneData.nk == 0)) {
      if (zone_id_to_zone_location_map.find(zoneData.zone_id) ==
          zone_id_to_zone_location_map.end()) {
        const auto         pdsIdx                      = vpdc->GetNumberOfPartitionedDataSets();
        zone_id_to_zone_location_map[zoneData.zone_id] = pdsIdx;

        vtkNew<vtkPartitionedDataSet> pds;
        vpdc->SetPartitionedDataSet(pdsIdx, pds);
        vpdc->GetMetaData(pdsIdx)->Set(vtkCompositeDataSet::NAME(), zoneData.zone_name);
        auto assembly = vpdc->GetDataAssembly();
        auto node     = assembly->AddNode(zoneData.zone_name.c_str(),
                                          assembly->GetFirstNodeByPath("/IOSS/structured_blocks"));
        assembly->SetAttribute(node, "label", zoneData.zone_name.c_str());
        assembly->AddDataSetIndex(node, pdsIdx);
    }
    return;
  }

  if (zone_id_to_zone_location_map.find(zoneData.zone_id) == zone_id_to_zone_location_map.end()) {
    const auto         pdsIdx = vpdc->GetNumberOfPartitionedDataSets();
    vtkStructuredGrid *sg     = vtkStructuredGrid::New();
    vtkPoints         *pts    = vtkPoints::New();
    sg->SetDimensions(dims);
    pts->Allocate(dims[0] * dims[1] * dims[2]);
    sg->SetPoints(pts);
    pts->Delete();
    zone_id_to_zone_location_map[zoneData.zone_id] = pdsIdx;

    vtkNew<vtkPartitionedDataSet> pds;
    pds->SetPartition(pds->GetNumberOfPartitions(), sg);
    sg->Delete();
    vpdc->SetPartitionedDataSet(pdsIdx, pds);
    vpdc->GetMetaData(pdsIdx)->Set(vtkCompositeDataSet::NAME(), zoneData.zone_name);

    auto assembly = vpdc->GetDataAssembly();
    auto node     = assembly->AddNode(zoneData.zone_name.c_str(),
                                      assembly->GetFirstNodeByPath("/IOSS/structured_blocks"));
    assembly->SetAttribute(node, "label", zoneData.zone_name.c_str());
    assembly->AddDataSetIndex(node, pdsIdx);
  }

  int                zone_location = zone_id_to_zone_location_map[zoneData.zone_id];
  vtkStructuredGrid *sg =
      vtkStructuredGrid::SafeDownCast(vpdc->GetPartitionedDataSet(zone_location)->GetPartition(0));

  int index = -1;
  if (zoneData.data_name == "mesh_model_coordinates_x") {
    index = 0;
  }
  else if (zoneData.data_name == "mesh_model_coordinates_y") {
    index = 1;
  }
  else if (zoneData.data_name == "mesh_model_coordinates_z") {
    index = 2;
  }

  if (index >= 0) {
    vtkPoints *pts = sg->GetPoints();
    for (int i = 0; i < zoneData.size; i++) {
      double p[3];
      pts->GetPoint(i, p);
      p[index] = zoneData.data[i];
      pts->InsertPoint(i, p);
    }
  }
  else {
    vtkDoubleArray *da = vtkDoubleArray::New();
    da->SetName(zoneData.data_name.c_str());
    da->SetNumberOfComponents(zoneData.comp_count);
    da->SetNumberOfTuples(zoneData.size);
    for (int j = 0; j < zoneData.size; j++) {
      da->InsertTuple(j, zoneData.data + (zoneData.comp_count * j));
    }

    if (zoneData.is_cell_field) {
      sg->GetCellData()->AddArray(da);
    }
    else {
      sg->GetPointData()->AddArray(da);
    }
    da->Delete();
  }
}

std::string CatalystCGNSMesh::createFieldVariableName(std::string fieldNamePrefix,
                                                      char fieldSuffixSeparator, int componentIndex,
                                                      int componentCount)
{
  std::string name;
  if (componentCount == 1) {
    name = fieldNamePrefix;
  }
  else {
    std::ostringstream oss;
    oss << componentIndex + 1;
    name = fieldNamePrefix + fieldSuffixSeparator + oss.str();
  }

  return name;
}

} // namespace Iovs_cgns
