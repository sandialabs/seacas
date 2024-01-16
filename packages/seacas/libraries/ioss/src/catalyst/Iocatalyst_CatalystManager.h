// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#ifndef IOSS_IOVS_CATALYST_MANAGER_H
#define IOSS_IOVS_CATALYST_MANAGER_H

#include "iocatalyst_export.h"
#include <Ioss_ParallelUtils.h>
#include <catalyst.hpp>

namespace Iocatalyst {

  class IOCATALYST_EXPORT CatalystManager
  {
  public:
    using CatalystPipelineID = unsigned int;

    inline static const std::string CATALYST_BLOCK_PARSE_INPUT_DECK_NAME =
        "CATALYST_BLOCK_PARSE_INPUT_DECK_NAME";
    inline static const std::string CATALYST_BLOCK_PARSE_JSON_STRING =
        "CATALYST_BLOCK_PARSE_JSON_STRING";
    inline static const std::string CATALYST_DEBUG_LEVEL      = "CATALYST_DEBUG_LEVEL";
    inline static const std::string CATALYST_ENABLE_LOGGING   = "CATALYST_ENABLE_LOGGING";
    inline static const std::string CATALYST_OUTPUT_DIRECTORY = "CATALYST_OUTPUT_DIRECTORY";
    inline static const std::string CATALYST_OUTPUT_DEFAULT   = "CatalystOutput";
    inline static const std::string CATALYST_INPUT_NAME       = "CATALYST_INPUT_NAME";
    inline static const std::string CATALYST_INPUT_DEFAULT    = "input";
    inline static const std::string CATALYST_MULTI_INPUT_PIPELINE_NAME =
        "CATALYST_MULTI_INPUT_PIPELINE_NAME";
    inline static const std::string CATALYST_SCRIPT            = "CATALYST_SCRIPT";
    inline static const std::string CATALYST_SCRIPT_EXTRA_FILE = "CATALYST_SCRIPT_EXTRA_FILE";
    inline static const std::string PHACTORI_JSON_SCRIPT       = "PHACTORI_JSON_SCRIPT";

    static CatalystManager &getInstance()
    {
      static CatalystManager instance;
      return instance;
    }

    std::string getCatalystPythonDriverPath() { return "/todo/create/real/path"; }

    const conduit_cpp::Node &getInitializeConduit() { return initializeConduit; };

    void writeToCatalystLogFile(const Ioss::ParallelUtils   &putils,
                                const Ioss::PropertyManager &props);

    struct CatalystProps
    {
      CatalystProps()
      {
        catalystPipelineID               = 0;
        enableLogging                    = false;
        debugLevel                       = 0;
        catalystOutputDirectory          = CATALYST_OUTPUT_DEFAULT;
        catalystInputName                = CATALYST_INPUT_DEFAULT;
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

    CatalystProps initialize(const Ioss::PropertyManager &props, const Ioss::ParallelUtils &putils);

  private:
    CatalystManager();
    ~CatalystManager();
    CatalystManager(const CatalystManager &)            = delete;
    CatalystManager &operator=(const CatalystManager &) = delete;

    void broadCastString(std::string &s, const Ioss::ParallelUtils &putils);
    void broadCastStatusCode(bool &statusCode, const Ioss::ParallelUtils &putils);

    void incrementOutputCounts();

    CatalystPipelineID catalystOutputIDNumber;
    conduit_cpp::Node  initializeConduit;
  };
} // namespace Iocatalyst

#endif