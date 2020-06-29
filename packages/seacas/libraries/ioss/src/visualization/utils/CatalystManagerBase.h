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
#ifndef __CATALYST_MANAGER_BASE_H
#define __CATALYST_MANAGER_BASE_H

#include <string>
#include <vector>
#include <memory>
#include "CatalystExodusMeshBase.h"
#include "CatalystCGNSMeshBase.h"

class CatalystManagerBase {

public:
    CatalystManagerBase() {};
    virtual ~CatalystManagerBase() {};

    virtual int getCatalystOutputIDNumber() = 0;

    // Parameters:
    //   cataystPythonFilename - Python file with instructions for Catalyst.
    //   restartTag - if not empty, contains the current restart iteration string, ie s0001
    //   enableLogging - turn on logging in the adapter. Default is off.
    //   debugLevel - enable catalyst debug output 0, 1, 2. Default is 0.
    //   resultsOutputFilename - filename associated with the Ioss results output block.
    //   catalystOutputDirectory - name of the output directory for storing Catalyst output.
    //                              Default is CatalystOutput.
    //   catalystData - string data vector for development and debugging.
    struct CatalystMeshInit {
        std::string catalystPythonFilename;
        std::string catalystBlockJSON;
        std::string catalystSeparatorCharacter;
        std::string catalystInputDeckName;
        std::string restartTag;
        bool enableLogging;
        int debugLevel;
        std::string resultsOutputFilename;
        std::string catalystOutputDirectory;
        std::vector<std::string> catalystData;
    };

    virtual std::unique_ptr<CatalystCGNSMeshBase> createCatalystCGNSMesh(
        CatalystMeshInit& cmInit) = 0;

    // Parameters:
    //   underscoreVectors - joined vector variable names end in an underscore.
    //   applyDisplacements - a nodal variable named DISPL or displ is applied to
    //                        the mesh node coordinates each time-step.
    struct CatalystExodusMeshInit : CatalystMeshInit {
        bool underScoreVectors;
        bool applyDisplacements;
    };

    virtual std::unique_ptr<CatalystExodusMeshBase> createCatalystExodusMesh(
        CatalystExodusMeshInit& cmInit) = 0;
};

#endif // __CATALYST_MANAGER_BASE_H
