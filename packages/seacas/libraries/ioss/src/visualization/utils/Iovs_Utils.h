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

class ParaViewCatalystIossAdapterBase;
class ParaViewCatalystCGNSAdapterBase;

namespace Iovs {

  class Utils {

  public:

    static Utils &getInstance() {
        static Utils instance;
        return instance;
    }

    CatalystManagerBase& getCatalystManager();

    static bool fileExists(const std::string &filepath);

    std::string getCatalystPythonDriverPath();

    int parseCatalystFile(const std::string &filepath,
        std::string &json_result);

    ParaViewCatalystIossAdapterBase *
        createParaViewCatalystIossAdapterInstance();

    ParaViewCatalystCGNSAdapterBase *
        createParaViewCatalystCGNSAdapterInstance();

    void checkDbUsage(Ioss::DatabaseUsage db_usage);

    void createDatabaseOutputFile(const std::string &filename,
        MPI_Comm communicator);

    std::string getExodusDatabaseOutputFilePath(
        const std::string & inputDeckName,
            const Ioss::PropertyManager &properties);

    std::string getCGNSDatabaseOutputFilePath(
        const std::string & inputDeckName,
            const Ioss::PropertyManager &properties);

    void incrementNumCGNSCatalystOutputs();
    void incrementNumExodusCatalystOutputs();

  private:

    Utils();
    ~Utils();
    Utils(const Utils &) = delete;
    Utils &operator=(const Utils &) = delete;

    CatalystManagerBase* catalystManager = nullptr;

    CatalystManagerBase*
        createCatalystManagerInstance();

    void loadPluginLibrary();

    std::string getCatalystPluginPath();

    std::string getSierraInstallDirectory();

    std::string getCatalystAdapterInstallDirectory();

    std::string getDatabaseOutputFilePath(
        const std::string & inputDeckName,
            int numberOfCatalystOutputs,
                const Ioss::PropertyManager &properties);

    void* getDlHandle();

    void* dlHandle = nullptr;
    int numCGNSCatalystOutputs = 0;
    int numExodusCatalystOutputs = 0;

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
