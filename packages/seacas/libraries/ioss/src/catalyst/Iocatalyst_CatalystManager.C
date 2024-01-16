// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catalyst/Iocatalyst_CatalystLogging.h>
#include <catalyst/Iocatalyst_CatalystManager.h>
#include <fstream>

namespace Iocatalyst {

  CatalystManager::CatalystManager() { catalystOutputIDNumber = 0; }

  CatalystManager::~CatalystManager() {}

  void CatalystManager::writeToCatalystLogFile(const Ioss::ParallelUtils   &putils,
                                               const Ioss::PropertyManager &props)
  {
    if (putils.parallel_rank() == 0) {
      CatalystLogging catLog = CatalystLogging();
      catLog.setProperties(&props);
      if (catLog.isCatalystLoggingON()) {
        catLog.writeToLogFile();
      }
    }
    putils.barrier();
  }

  CatalystManager::CatalystProps CatalystManager::initialize(const Ioss::PropertyManager &props,
                                                             const Ioss::ParallelUtils   &putils)
  {
    CatalystManager::CatalystProps catalystProps;
    catalystProps.catalystPipelineID = catalystOutputIDNumber;
    incrementOutputCounts();

    if (props.exists(CATALYST_BLOCK_PARSE_JSON_STRING)) {
      catalystProps.catalystBlockJSON = props.get(CATALYST_BLOCK_PARSE_JSON_STRING).get_string();
    }
    else if (props.exists(PHACTORI_JSON_SCRIPT)) {
      bool        readOkay             = false;
      std::string phactoriJSONFilePath = props.get(PHACTORI_JSON_SCRIPT).get_string();
      if (putils.parallel_rank() == 0) {
        std::ifstream f(phactoriJSONFilePath);
        if (f) {
          std::ostringstream ss;
          ss << f.rdbuf();
          catalystProps.catalystBlockJSON = ss.str();
          readOkay                        = true;
        }
      }
      this->broadCastStatusCode(readOkay, putils);
      if (!readOkay) {
        std::ostringstream errmsg;
        errmsg << "Unable to read input file: " << phactoriJSONFilePath << "\n";
        IOSS_ERROR(errmsg);
      }
      else {
        this->broadCastString(catalystProps.catalystBlockJSON, putils);
      }
    }

    if (props.exists(CATALYST_SCRIPT)) {
      catalystProps.catalystPythonFilename = props.get(CATALYST_SCRIPT).get_string();
    }
    else {
      catalystProps.catalystPythonFilename = this->getCatalystPythonDriverPath();
    }

    if (props.exists(CATALYST_SCRIPT_EXTRA_FILE)) {
      catalystProps.catalystScriptExtraFile = props.get(CATALYST_SCRIPT_EXTRA_FILE).get_string();
    }

    if (props.exists(CATALYST_BLOCK_PARSE_INPUT_DECK_NAME)) {
      catalystProps.catalystInputDeckName =
          props.get(CATALYST_BLOCK_PARSE_INPUT_DECK_NAME).get_string();
    }

    if (props.exists(CATALYST_ENABLE_LOGGING)) {
      catalystProps.enableLogging = props.get(CATALYST_ENABLE_LOGGING).get_int();
    }

    if (props.exists(CATALYST_DEBUG_LEVEL)) {
      catalystProps.debugLevel = props.get(CATALYST_DEBUG_LEVEL).get_int();
    }

    if (props.exists(CATALYST_OUTPUT_DIRECTORY)) {
      catalystProps.catalystOutputDirectory = props.get(CATALYST_OUTPUT_DIRECTORY).get_string();
    }

    if (props.exists(CATALYST_INPUT_NAME)) {
      catalystProps.catalystInputName = props.get(CATALYST_INPUT_NAME).get_string();
    }

    if (props.exists(CATALYST_MULTI_INPUT_PIPELINE_NAME)) {
      catalystProps.enableCatalystMultiInputPipeline = true;
      catalystProps.catalystMultiInputPipelineName =
          props.get(CATALYST_MULTI_INPUT_PIPELINE_NAME).get_string();
    }

    return catalystProps;
  }

  void CatalystManager::incrementOutputCounts() { catalystOutputIDNumber++; }

  void CatalystManager::broadCastString(IOSS_MAYBE_UNUSED std::string &s,
                                        IOSS_MAYBE_UNUSED const Ioss::ParallelUtils &putils)
  {
    IOSS_PAR_UNUSED(s);
    IOSS_PAR_UNUSED(dbinfo);
#ifdef SEACAS_HAVE_MPI
    int size = s.size();
    putils.broadcast(size);
    if (putils.parallel_rank() != 0) {
      s.resize(size);
    }
    putils.broadcast(s);
#endif
  }

  void CatalystManager::broadCastStatusCode(IOSS_MAYBE_UNUSED bool &statusCode,
                                            IOSS_MAYBE_UNUSED const Ioss::ParallelUtils &putils)
  {
    IOSS_PAR_UNUSED(statusCode);
    IOSS_PAR_UNUSED(dbinfo);
#ifdef SEACAS_HAVE_MPI

    int code = statusCode;
    putils.broadcast(code);
    statusCode = code;
#endif
  }

} // namespace Iocatalyst