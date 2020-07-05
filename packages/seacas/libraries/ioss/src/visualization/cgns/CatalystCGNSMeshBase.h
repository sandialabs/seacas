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
#ifndef __CATALYST_CGNS_MESH_BASE_H
#define __CATALYST_CGNS_MESH_BASE_H

#include <vector>
#include <string>

namespace Iovs_cgns {

class CatalystCGNSMeshBase {

public:
    CatalystCGNSMeshBase() {};
    virtual ~CatalystCGNSMeshBase() {};

    // Description:
    // Calls the ParaView Catalyst pipeline to run co-processing for this time iteration.
    virtual void PerformCoProcessing(std::vector<int> &error_and_warning_codes,
                                     std::vector<std::string> &error_and_warning_messages) = 0;

    // Description:
    // Sets time data for this ParaView Catalyst co-processing iteration.
    // currentTime is the current Ioss simulation time and timeStep is
    // the current time iteration count.
    virtual void SetTimeData(double currentTime, int timeStep) = 0;

    // Description:
    // Clears all nodal and element variables from the vtkMultiBlockDataSet.
    // Clears the global vtkPoints.
    virtual void ReleaseMemory() = 0;

    // Description:
    // Collects memory usage information from all processors and
    // writes the min, max, and mean to the log file.  Also writes the
    // min, max, and mean of the elapsed time since this method was
    // last called.
    virtual void logMemoryUsageAndTakeTimerReading() = 0;

    virtual void Delete() = 0;

    virtual void CreateBase(int base_id,
                            const std::string& base_name) = 0;

    virtual void AddStructuredZoneData(int base_id,
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
                                       int size) = 0;
};

} // namespace Iovs_cgns

#endif // __CATALYST_CGNS_MESH_BASE_H
