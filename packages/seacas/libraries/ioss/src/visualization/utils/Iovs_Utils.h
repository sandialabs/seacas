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

#ifndef IOSS_IOVS_UTILS_H
#define IOSS_IOVS_UTILS_H

#include "CatalystManagerBase.h"
#include <string>
#include <Ioss_PropertyManager.h>
#include <Ioss_DBUsage.h>

namespace Iovs {

  class Utils {

  public:

    static Utils &getInstance() {
        static Utils instance;
        return instance;
    }

    static bool fileExists(const std::string &filepath);

    std::string getCatalystPythonDriverPath();

    void checkDbUsage(Ioss::DatabaseUsage db_usage);

    struct DatabaseInfo {
        std::string databaseFilename;
        std::string separatorCharacter;
        int myRank;
        MPI_Comm communicator;
    };

    void createDatabaseOutputFile(const DatabaseInfo & dbinfo);

    std::unique_ptr<Iovs_exodus::CatalystExodusMeshBase>
        createCatalystExodusMesh(const DatabaseInfo & dbinfo,
            const Ioss::PropertyManager & props);

    std::unique_ptr<Iovs_cgns::CatalystCGNSMeshBase>
        createCatalystCGNSMesh(const DatabaseInfo & dbinfo,
            const Ioss::PropertyManager & props);

    std::string getDatabaseOutputFilePath(
        const std::string & databaseFilename,
                const Ioss::PropertyManager &properties);

    void reportCatalystErrorMessages(const std::vector<int> & error_codes,
        const std::vector<std::string> & error_messages, int myRank);

  private:

    Utils();
    ~Utils();
    Utils(const Utils &) = delete;
    Utils &operator=(const Utils &) = delete;

    CatalystManagerBase& getCatalystManager();

    CatalystManagerBase* catalystManager = nullptr;

    CatalystManagerBase*
        createCatalystManagerInstance();

    void initMeshFromIOSSProps(CatalystManagerBase::CatalystMeshInit & cmInit,
        const DatabaseInfo & dbinfo, const Ioss::PropertyManager & props);

    std::string getRestartTag(const std::string & databaseFilename);

    void broadCastString(std::string & s, const DatabaseInfo & dbinfo);

    void broadCastStatusCode(bool & statusCode, const DatabaseInfo & dbinfo);

    void loadPluginLibrary();

    std::string getCatalystPluginPath();

    std::string getSierraInstallDirectory();

    std::string getCatalystAdapterInstallDirectory();

    void* getDlHandle();

    void* dlHandle = nullptr;

#if defined(__APPLE__)
    const char* CATALYST_PLUGIN_DYNAMIC_LIBRARY =\
        "libcatalystioss.dylib";
#else
    const char* CATALYST_PLUGIN_DYNAMIC_LIBRARY =\
        "libcatalystioss.so";
#endif
    const char* CATALYST_PLUGIN_PYTHON_MODULE = "PhactoriDriver.py";
    const char* CATALYST_PLUGIN_PATH = "viz/catalyst/install";
    const char* CATALYST_FILE_SUFFIX = ".dummy.pv.catalyst.e";
    const char* CATALYST_OUTPUT_DIRECTORY = "CatalystOutput";
    const char* CATALYST_INSTALL_LIB_DIR = "/lib/";
    const char* CATALYST_INSTALL_PHACTORI_DIR = "/phactori/";
  };

} // namespace Iovs

#endif
