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
#include "vtkMultiBlockDataSet.h"
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

    this->multiBlock           = vtkMultiBlockDataSet::New();
    this->catManager           = cm;
    this->catalystPipelineInfo = catalystPipelineInfo;
    vtkMultiBlockDataSet *b    = vtkMultiBlockDataSet::New();
    this->multiBlock->SetBlock(BASES_BLOCK_ID, b);
    this->multiBlock->GetMetaData(BASES_BLOCK_ID)
        ->Set(vtkCompositeDataSet::NAME(), BASES_BLOCK_NAME);
    b->Delete();
  }

  CatalystCGNSMesh::~CatalystCGNSMesh()
  {
    this->multiBlock->Delete();
    this->multiBlock = nullptr;
  }

  vtkMultiBlockDataSet *CatalystCGNSMesh::getMultiBlockDataSet() { return this->multiBlock; }

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

  void CatalystCGNSMesh::CreateBase(int base_id, const std::string &base_name)
  {

    if (base_id_to_base_map.find(base_id) != base_id_to_base_map.end()) {
      return;
    }

    vtkMultiBlockDataSet *b =
        vtkMultiBlockDataSet::SafeDownCast(this->multiBlock->GetBlock(BASES_BLOCK_ID));
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
    bs.base_location             = location;
    base_id_to_base_map[base_id] = bs;
  }

  void CatalystCGNSMesh::AddStructuredZoneData(const ZoneData& zoneData)
  {

    if (base_id_to_base_map.find(zoneData.base_id) == base_id_to_base_map.end()) {
      return;
    }

    base                 &bs      = base_id_to_base_map[zoneData.base_id];
    int                   dims[3] = {zoneData.ni + 1, zoneData.nj + 1, zoneData.nk + 1};
    vtkMultiBlockDataSet *bases =
        vtkMultiBlockDataSet::SafeDownCast(this->multiBlock->GetBlock(BASES_BLOCK_ID));
    vtkMultiBlockDataSet *base =
        vtkMultiBlockDataSet::SafeDownCast(bases->GetBlock(bs.base_location));
    vtkMultiBlockDataSet *zones =
        vtkMultiBlockDataSet::SafeDownCast(base->GetBlock(ZONES_BLOCK_ID));

    // if this is an empty block, we just need to make a NULL grid with the
    // proper name, and we don't need to add any data variables
    // alternative is to make block with dims of (I think) -1,-1,-1
    // or extents of [0,-1,0,-1,0,-1] (I think)
    if ((zoneData.ni == 0) or (zoneData.nj == 0) or (zoneData.nk == 0)) {
      if (bs.zone_id_to_zone_location_map.find(zoneData.zone_id) == bs.zone_id_to_zone_location_map.end()) {
        int                location = zones->GetNumberOfBlocks();
        vtkStructuredGrid *sg       = nullptr;
        zones->SetBlock(location, sg);
        zones->GetMetaData(location)->Set(vtkCompositeDataSet::NAME(), zoneData.zone_name);
        bs.zone_id_to_zone_location_map[zoneData.zone_id] = location;

        const auto                    pdsIdx = vpdc->GetNumberOfPartitionedDataSets();
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

    if (bs.zone_id_to_zone_location_map.find(zoneData.zone_id) == bs.zone_id_to_zone_location_map.end()) {

      int                location = zones->GetNumberOfBlocks();
      vtkStructuredGrid *sg       = vtkStructuredGrid::New();
      vtkPoints         *pts      = vtkPoints::New();
      sg->SetDimensions(dims);
      pts->Allocate(dims[0] * dims[1] * dims[2]);
      sg->SetPoints(pts);
      zones->SetBlock(location, sg);
      zones->GetMetaData(location)->Set(vtkCompositeDataSet::NAME(), zoneData.zone_name);
      sg->Delete();
      pts->Delete();
      bs.zone_id_to_zone_location_map[zoneData.zone_id] = location;

      const auto                    pdsIdx = vpdc->GetNumberOfPartitionedDataSets();
      vtkNew<vtkPartitionedDataSet> pds;
      pds->SetPartition(pds->GetNumberOfPartitions(), sg);
      vpdc->SetPartitionedDataSet(pdsIdx, pds);
      vpdc->GetMetaData(pdsIdx)->Set(vtkCompositeDataSet::NAME(), zoneData.zone_name);

      auto assembly = vpdc->GetDataAssembly();
      auto node =
          assembly->AddNode(zoneData.zone_name.c_str(), assembly->GetFirstNodeByPath("/IOSS/structured_blocks"));
      assembly->SetAttribute(node, "label", zoneData.zone_name.c_str());
      assembly->AddDataSetIndex(node, pdsIdx);
    }

    int                zone_location = bs.zone_id_to_zone_location_map[zoneData.zone_id];
    vtkStructuredGrid *sg = vtkStructuredGrid::SafeDownCast(zones->GetBlock(zone_location));

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
                                                        char        fieldSuffixSeparator,
                                                        int componentIndex, int componentCount)
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
