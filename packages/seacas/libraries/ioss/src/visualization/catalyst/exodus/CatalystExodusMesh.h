/*
 * Copyright(C) 1999-2017 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of NTESS nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef __CATALYST_EXODUS_MESH_H
#define __CATALYST_EXODUS_MESH_H

#include "CatalystExodusMeshBase.h"
#include <map>
#include <vector>

class vtkMultiBlockDataSet;
class CatalystManager;
class vtkVariant;
class vtkPoints;

class CatalystExodusMesh : public CatalystExodusMeshBase {

public:

    CatalystExodusMesh(CatalystManager *cm);

    ~CatalystExodusMesh();

    void PerformCoProcessing(std::vector<int> &error_and_warning_codes,
                             std::vector<std::string> &error_and_warning_messages);

    void SetTimeData(double currentTime, int timeStep);

    void ReleaseMemory();

    void logMemoryUsageAndTakeTimerReading();

    void Delete();

    void CreateGlobalVariable(std::vector<std::string> &component_names,
                              const double *data);

    void CreateGlobalVariable(std::vector<std::string> &component_names,
                              const int *data);

    void InitializeGlobalPoints(int num_points, int dimension, const double *data);

    void InitializeElementBlocks(const std::vector<int> &element_block_id_list);

    void CreateElementBlock(const char *elem_block_name, int elem_block_id,
                            const std::string &elem_type, int nodes_per_elem, int num_elem,
                            const int64_t *global_elem_ids, int *connectivity);

    void CreateElementBlock(const char *elem_block_name, int elem_block_id,
                            const std::string &elem_type, int nodes_per_elem, int num_elem,
                            const int64_t *global_elem_ids, int64_t *connectivity);

    void CreateNodeSet(const char *node_set_name, int node_set_id,
                       int num_ids, const int *data);

    void CreateNodeSet(const char *node_set_name, int node_set_id,
                       int num_ids, const int64_t *data);

    void CreateSideSet(const char *ss_owner_name, int side_set_id, int num_ids,
                       const int *element_ids, const int *face_ids);

    void CreateSideSet(const char *ss_owner_name, int side_set_id, int num_ids,
                       const int64_t *element_ids, const int64_t *face_ids);

    void CreateElementVariable(std::vector<std::string> &component_names, int elem_block_id,
                               const double *data);

    void CreateElementVariable(std::vector<std::string> &component_names, int elem_block_id,
                               const int *data);

    void CreateElementVariable(std::vector<std::string> &component_names, int elem_block_id,
                               const int64_t *data);

    void CreateNodalVariable(std::vector<std::string> &component_names,
                             const double *data);

    void CreateNodalVariable(std::vector<std::string> &component_names,
                             const int *data);

    void CreateNodalVariable(std::vector<std::string> &component_names,
                             const int64_t *data);

    // Description:
    // If true (the default), vector variables will contain a
    // trailing underscore in their name.  The default behavior
    // is consistent with the ParaView Exodus II file reader.
    bool UnderscoreVectorsON();
    bool SetUnderscoreVectors(bool status);

    // Description:
    // If true (the default), displacements will be applied to the
    // mesh nodes before being sent to the in-situ pipeline.  The node
    // displacement variable is called either DISPL or displ.  The
    // default behavior is consistent with the ParaView Exodus II
    // file reader.
    bool ApplyDisplacementsON();
    bool SetApplyDisplacements(bool status);

    const std::string& getCatalystPipelineName();
    void SetCatalystPipelineName(const std::string& value);

    vtkMultiBlockDataSet* getMultiBlockDataSet();

private:

    const unsigned int ELEMENT_BLOCK_MBDS_ID = 0;
    const char * ELEMENT_BLOCK_MBDS_NAME = "Element Blocks";

    const unsigned int SIDE_SETS_MBDS_ID = 1;
    const char * SIDE_SETS_MBDS_NAME = "Side Sets";

    const unsigned int NODE_SETS_MBDS_ID = 2;
    const char * NODE_SETS_MBDS_NAME = "Node Sets";

    const int HEXAHEDRON_FACE_MAP[6] = {2, 1, 3, 0, 4, 5};
    const int WEDGE_FACE_MAP[5] = {2, 3, 4, 0, 1};

    // see ssinfomap below, lets us combine sidesets which span multiple
    // blocks int a single sideset entity
    class Ve2mSideSetInfo {
    public:
        Ve2mSideSetInfo() { this->bid = -1; }
        ~Ve2mSideSetInfo() {
            this->unique_points.clear();
            this->object_ids.clear();
        }
        int bid;
        std::map<int, int> unique_points;
        std::vector<int> object_ids;
    };

    std::map<int, std::map<int, int>> ebmap;
    std::map<int, std::map<int, int>> ebmap_reverse;
    std::map<int, std::map<int, int>> global_elem_id_map;
    std::vector<int> global_point_id_to_global_elem_id;
    std::map<int, unsigned int> ebidmap;
    std::map<int, unsigned int> nsidmap;
    std::map<int, std::map<int, int>> nsmap;
    std::map<int, unsigned int> ssidmap;

    // ssinfomap is used to help track when we see a new sideset. CreateSideSet
    // is called once for each sideset for each block which the sideset spans,
    // and we combine those into a single sideset entity in the vtk
    // representation; also lets us do the necessary bookkeeping to combine
    // the data from the different blocks into the same sideset
    std::map<int, Ve2mSideSetInfo *> ssinfomap;

    std::map<int, std::map<int, int>> ssmap;
    void ContainsVector(std::vector<std::string> &component_names,
                        std::vector<std::string> &prefix_name);
    double GetArrayValue(vtkVariant &v, const void *data, int index);
    void       ReleaseGlobalPoints();
    vtkPoints* global_points;
    int        num_global_points;
    bool writeCatalystMesh;
    std::string catalystMeshFilePrefix;

    void CreateElementBlockInternal(const char * elem_block_name,
        int elem_block_id, const std::string &elem_type, int nodes_per_elem,
            int num_elem, vtkVariant &v, const int64_t *global_elem_ids,
                void *connectivity);

    void CreateGlobalVariableVariant(std::vector<std::string> &component_names,
                                     vtkVariant &v, const void *data);
    void CreateGlobalVariableInternal(std::vector<std::string> &component_names,
                                      vtkMultiBlockDataSet *eb, unsigned int bid, vtkVariant &v,
                                      const void *data);

    void CreateNodalVariableVariant(std::vector<std::string> &component_names,
                                    vtkVariant &v, const void *data);
    void CreateNodalVariableInternal(std::vector<std::string> &component_names,
                                     vtkMultiBlockDataSet *eb, std::map<int, unsigned int> &id_map,
                                     std::map<int, std::map<int, int>> &point_map, vtkVariant &v,
                                     const void *data);

    void CreateElementVariableVariant(std::vector<std::string> &component_names,
                                      int elem_block_id, vtkVariant &v,
                                      const void *data);
    void CreateElementVariableInternal(std::vector<std::string> &component_names,
                                       vtkMultiBlockDataSet *eb, unsigned int bid, vtkVariant &v,
                                       const void *data);

    void CreateNodeSetVariant(const char *node_set_name, int node_set_id,
                              int num_ids, vtkVariant &v, const void *ids);

    void CreateSideSetVariant(const char *ss_owner_name, int side_set_id,
                              int num_ids, vtkVariant &v,
                              const void *element_ids, const void *face_ids);

    void ReleaseMemoryInternal(vtkMultiBlockDataSet *eb);

    CatalystExodusMesh();
    CatalystExodusMesh(const CatalystExodusMesh &) = delete;
    CatalystExodusMesh &operator=(const CatalystExodusMesh &) = delete;

    vtkMultiBlockDataSet* multiBlock = nullptr;
    CatalystManager* catManager = nullptr;
    std::string catalystPipelineName;
    bool UnderscoreVectors;
    bool ApplyDisplacements;
};

#endif // __CATALYST_EXODUS_MESH_H
