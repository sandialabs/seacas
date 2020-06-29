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
#ifndef __CATALYST_MANAGER_H
#define __CATALYST_MANAGER_H

#include "CatalystManagerBase.h"
#include <map>
#include <time.h>

class coProcessor;
class vtkDoubleArray;
class vtkCPPythonScriptPipeline;
class vtkCPDataDescription;
class vtkCPProcessor;
class vtkMultiBlockDataSet;

class CatalystManager : public CatalystManagerBase {

public:

    CatalystManager();
    ~CatalystManager();

    std::unique_ptr<CatalystExodusMeshBase> createCatalystExodusMesh(
        CatalystExodusMeshInit& cmInit);

    std::unique_ptr<CatalystCGNSMeshBase> createCatalystCGNSMesh(
        CatalystMeshInit& cmInit);

    int getCatalystOutputIDNumber();

    // Description:
    // Deletes pipeline with name results_output_filename and any associated
    // logging data.
    void DeletePipeline(const char *results_output_filename);

    // Description:
    // Calls the ParaView Catalyst pipeline to run co-processing for this time iteration.
    void PerformCoProcessing(const char *results_output_filename,
                             std::vector<int> & error_and_warning_codes,
                             std::vector<std::string> & error_and_warning_messages);

    // Description:
    // Sets time data for this ParaView Catalyst co-processing iteration.
    // currentTime is the current Ioss simulation time and timeStep is
    // the current time iteration count.
    void SetTimeData(double currentTime, int timeStep,
                     const char *results_output_filename);

    // Description:
    // Collects memory usage information from all processors and
    // writes the min, max, and mean to the log file.  Also writes the
    // min, max, and mean of the elapsed time since this method was
    // last called.
    void logMemoryUsageAndTakeTimerReading(const char *results_output_filename);

    void WriteToLogFile(const char *results_output_filename);

private:

    typedef std::pair<vtkCPPythonScriptPipeline *, vtkCPDataDescription *>
        PipelineDataDescPair;
    typedef std::pair<clock_t, clock_t> TimerPair;
    typedef std::pair<TimerPair, vtkDoubleArray *> LoggingPair;

    CatalystManager(const CatalystManager &) = delete;
    CatalystManager &operator=(const CatalystManager &) = delete;

    void initializeIfNeeded();
    void finalizeIfNeeded();
    bool canCoProcess();
    void incrementOutputCounts();

    void initCatalystLogging(CatalystMeshInit& cmInit);
    void initCatalystPipeline(CatalystMeshInit& cmInit,
        vtkMultiBlockDataSet* mbds);

    int catalystOutputIDNumber;
    int catalystOutputReferenceCount;
    vtkCPProcessor * coProcessor;
    std::map<std::string, PipelineDataDescPair> pipelines;
    std::map<std::string, LoggingPair> logging;
};

extern "C" {
    CatalystManagerBase* CreateCatalystManagerInstance();
}

#endif /* __CATALYST_MANAGER_H */
