// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#ifndef IOSS_IOVS_CATALYST_MANAGER_H
#define IOSS_IOVS_CATALYST_MANAGER_H

#include "iocatalyst_export.h"
#include <Ioss_ParallelUtils.h>

namespace Iocatalyst {

  class IOCATALYST_EXPORT CatalystManager
  {
  public:
    using CatalystPipelineID                                  = unsigned int;
    inline static const std::string CATALYST_OUTPUT_DIRECTORY = "CatalystOutput";
    inline static const std::string CATALYST_INPUT_NAME       = "input";

    static CatalystManager &getInstance()
    {
      static CatalystManager instance;
      return instance;
    }

    std::string getCatalystPythonDriverPath() { return "/todo/create/real/path"; }

    void writeToCatalystLogFile(const Ioss::ParallelUtils   &putils,
                                const Ioss::PropertyManager &props);

    struct CatalystProps
    {
      CatalystProps()
      {
        catalystPipelineID               = 0;
        enableLogging                    = false;
        debugLevel                       = 0;
        catalystOutputDirectory          = CATALYST_OUTPUT_DIRECTORY;
        catalystInputName                = CATALYST_INPUT_NAME;
        enableCatalystMultiInputPipeline = false;
      }
      CatalystPipelineID catalystPipelineID;
      std::string        catalystBlockJSON;
      std::string        catalystPythonFilename;
      std::string        catalystScriptExtraFile;
      std::string        catalystInputDeckName;
      bool               enableLogging;
      int                debugLevel;
      std::string        catalystOutputDirectory;
      std::string        catalystInputName;
      bool               enableCatalystMultiInputPipeline;
      std::string        catalystMultiInputPipelineName;
    };

    CatalystProps registerDatabase(const Ioss::PropertyManager &props,
                                   const Ioss::ParallelUtils   &putils);

  private:
    CatalystManager();
    ~CatalystManager();
    CatalystManager(const CatalystManager &)            = delete;
    CatalystManager &operator=(const CatalystManager &) = delete;

    void broadCastString(std::string &s, const Ioss::ParallelUtils &putils);
    void broadCastStatusCode(bool &statusCode, const Ioss::ParallelUtils &putils);

    void incrementOutputCounts();

    CatalystPipelineID catalystOutputIDNumber;
  };
} // namespace Iocatalyst

#endif