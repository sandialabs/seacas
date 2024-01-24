// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_ParallelUtils.h>
#include <catalyst/Iocatalyst_CatalystManager.h>
#include <catalyst_tests/Iocatalyst_DatabaseIOTest.h>
#include <catalyst_tests/Iocatalyst_LoggingTest.h>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <stdio.h>

using namespace Iocatalyst;

class ManagerTest : public ::testing::Test
{
protected:
  Ioss::PropertyManager          props;
  Ioss::ParallelUtils            putils;
  CatalystManager::CatalystProps catalystProps;
  void                           reset() { CatalystManager::getInstance().reset(); }
  void                           initialize()
  {
    auto id       = CatalystManager::getInstance().initialize(props, putils);
    catalystProps = CatalystManager::getInstance().getCatalystProps(id);
  }
};

void compareConduit(const conduit_cpp::Node &n, const conduit_cpp::Node &m)
{
  EXPECT_EQ(n.to_string(), m.to_string());
}

TEST_F(LoggingTest, LoggingDefault)
{
  Ioss::ParallelUtils putils;
  CatalystManager::getInstance().writeToCatalystLogFile(putils, props);
  EXPECT_FALSE(isFileExists(CatalystLogging::getDefaultLogFileName().c_str()));
}

TEST_F(LoggingTest, LoggingNotEnabled)
{
  Ioss::ParallelUtils putils;
  props.add(Ioss::Property("CATALYST_LOGGING_ENABLED", false));
  CatalystManager::getInstance().writeToCatalystLogFile(putils, props);
  EXPECT_FALSE(isFileExists(CatalystLogging::getDefaultLogFileName().c_str()));
}

TEST_F(LoggingTest, LoggingEnabled)
{
  Ioss::ParallelUtils putils;
  props.add(Ioss::Property("CATALYST_LOGGING_ENABLED", true));
  props.add(Ioss::Property("CATALYST_LOGGING_STRING_PROP", "foo"));
  props.add(Ioss::Property("CATALYST_LOGGING_INTEGER_PROP", 6));
  props.add(Ioss::Property("CATALYST_LOGGING_REAL_PROP", 3.7556));
  CatalystManager::getInstance().writeToCatalystLogFile(putils, props);
  EXPECT_TRUE(isFileExists(CatalystLogging::getDefaultLogFileName().c_str()));
}

TEST_F(ManagerTest, CatalystPipelineID)
{
  initialize();
  EXPECT_EQ(catalystProps.catalystPipelineID, 0);

  initialize();
  EXPECT_EQ(catalystProps.catalystPipelineID, 1);

  initialize();
  EXPECT_EQ(catalystProps.catalystPipelineID, 2);
}

TEST_F(ManagerTest, CATALYST_BLOCK_PARSE_JSON_STRING)
{
  std::string jsonScript = "{foo: 12}";
  props.add(Ioss::Property("CATALYST_BLOCK_PARSE_JSON_STRING", jsonScript));
  initialize();
  EXPECT_EQ(catalystProps.catalystBlockJSON, jsonScript);
}

TEST_F(ManagerTest, PHACTORI_JSON_SCRIPT)
{
  std::string   jsonFileName = "jsonFile.json";
  std::string   jsonScript   = "{foo: 12}";
  std::ofstream jsonFile;
  jsonFile.open(jsonFileName);
  jsonFile << jsonScript;
  jsonFile.close();
  props.add(Ioss::Property(CatalystManager::PHACTORI_JSON_SCRIPT, jsonFileName));
  initialize();
  EXPECT_EQ(catalystProps.catalystBlockJSON, jsonScript);
  remove(jsonFileName.c_str());
}

TEST_F(ManagerTest, CATALYST_SCRIPT)
{
  initialize();
  EXPECT_EQ(catalystProps.catalystPythonFilename,
            CatalystManager::getInstance().getCatalystPythonDriverPath());

  std::string catalystFileName = "/path/to/file/catalystFile.txt";
  props.add(Ioss::Property(CatalystManager::CATALYST_SCRIPT, catalystFileName));
  initialize();
  EXPECT_EQ(catalystProps.catalystPythonFilename, catalystFileName);
}

TEST_F(ManagerTest, CATALYST_SCRIPT_EXTRA_FILE)
{
  std::string extraFileName = "extraFileName.txt";
  props.add(Ioss::Property(CatalystManager::CATALYST_SCRIPT_EXTRA_FILE, extraFileName));
  initialize();
  EXPECT_EQ(catalystProps.catalystScriptExtraFile, extraFileName);
}

TEST_F(ManagerTest, CATALYST_BLOCK_PARSE_INPUT_DECK_NAME)
{
  std::string inputDeckName = "contact.i";
  props.add(Ioss::Property(CatalystManager::CATALYST_BLOCK_PARSE_INPUT_DECK_NAME, inputDeckName));
  initialize();
  EXPECT_EQ(catalystProps.catalystInputDeckName, inputDeckName);
}

TEST_F(ManagerTest, CATALYST_ENABLE_LOGGING)
{
  initialize();
  EXPECT_FALSE(catalystProps.enableLogging);

  props.add(Ioss::Property(CatalystManager::CATALYST_ENABLE_LOGGING, true));
  initialize();
  EXPECT_TRUE(catalystProps.enableLogging);
}

TEST_F(ManagerTest, CATALYST_DEBUG_LEVEL)
{
  initialize();
  EXPECT_EQ(catalystProps.debugLevel, 0);

  props.add(Ioss::Property(CatalystManager::CATALYST_DEBUG_LEVEL, 3));
  initialize();
  EXPECT_EQ(catalystProps.debugLevel, 3);
}

TEST_F(ManagerTest, CATALYST_OUTPUT_DIRECTORY)
{
  initialize();
  EXPECT_EQ(catalystProps.catalystOutputDirectory,
            CatalystManager::getInstance().CATALYST_OUTPUT_DEFAULT);

  std::string catalystOutputDirectory = "catalyst";
  props.add(Ioss::Property(CatalystManager::CATALYST_OUTPUT_DIRECTORY, catalystOutputDirectory));
  initialize();
  EXPECT_EQ(catalystProps.catalystOutputDirectory, catalystOutputDirectory);
}

TEST_F(ManagerTest, CATALYST_INPUT_NAME)
{
  initialize();
  EXPECT_EQ(catalystProps.catalystInputName, CatalystManager::getInstance().CATALYST_INPUT_DEFAULT);

  std::string catalystInputName = "mesh";
  props.add(Ioss::Property(CatalystManager::CATALYST_INPUT_NAME, catalystInputName));
  initialize();
  EXPECT_EQ(catalystProps.catalystInputName, catalystInputName);
}

TEST_F(ManagerTest, CATALYST_MULTI_INPUT_PIPELINE_NAME)
{
  initialize();
  EXPECT_FALSE(catalystProps.enableCatalystMultiInputPipeline);

  std::string catalystMultiInputPipelineName = "multi";
  props.add(Ioss::Property(CatalystManager::CATALYST_MULTI_INPUT_PIPELINE_NAME,
                           catalystMultiInputPipelineName));
  initialize();
  EXPECT_EQ(catalystProps.catalystMultiInputPipelineName, catalystMultiInputPipelineName);
  EXPECT_TRUE(catalystProps.enableCatalystMultiInputPipeline);
}

TEST_F(ManagerTest, InitializeConduitDefault)
{
  reset();
  conduit_cpp::Node n;
  compareConduit(CatalystManager::getInstance().getInitializeConduit(), n);
}

TEST_F(ManagerTest, InitializeConduitCatalystFile)
{
  reset();
  std::string catalystFileName = "/path/to/file/catalystFile.txt";
  props.add(Ioss::Property(CatalystManager::CATALYST_SCRIPT, catalystFileName));
  initialize();
  conduit_cpp::Node n;
  CatalystManager::getInstance().addScriptProps(n, catalystProps);
  compareConduit(CatalystManager::getInstance().getInitializeConduit(), n);
}

TEST_F(ManagerTest, InitializeConduitPhactoriJSON)
{
  reset();
  std::string js = "some json";
  props.add(Ioss::Property(CatalystManager::CATALYST_BLOCK_PARSE_JSON_STRING, js));
  props.add(Ioss::Property(CatalystManager::CATALYST_INPUT_NAME, "data"));
  props.add(Ioss::Property(CatalystManager::CATALYST_SCRIPT_EXTRA_FILE, "extra"));
  props.add(Ioss::Property(CatalystManager::CATALYST_BLOCK_PARSE_INPUT_DECK_NAME, "adagio"));
  props.add(Ioss::Property(CatalystManager::CATALYST_OUTPUT_DIRECTORY, "temp"));
  props.add(Ioss::Property(CatalystManager::CATALYST_ENABLE_LOGGING, true));
  props.add(Ioss::Property(CatalystManager::CATALYST_DEBUG_LEVEL, 11));
  initialize();
  conduit_cpp::Node n;
  CatalystManager::getInstance().addScriptProps(n, catalystProps);
  compareConduit(CatalystManager::getInstance().getInitializeConduit(), n);
}

TEST_F(ManagerTest, InitializeConduitMultipleScripts)
{
  reset();
  std::string catalystFileName = "/path/to/file/catalystFile.txt";
  props.add(Ioss::Property(CatalystManager::CATALYST_SCRIPT, catalystFileName));
  initialize();

  Ioss::PropertyManager propsOne;
  std::string           otherFile = "/path/to/other/file";
  props.add(Ioss::Property(CatalystManager::CATALYST_SCRIPT, otherFile));
  auto id          = CatalystManager::getInstance().initialize(propsOne, putils);
  auto catPropsOne = CatalystManager::getInstance().getCatalystProps(id);

  Ioss::PropertyManager propsTwo;
  std::string           js = "json";
  props.add(Ioss::Property(CatalystManager::CATALYST_BLOCK_PARSE_JSON_STRING, js));
  id               = CatalystManager::getInstance().initialize(propsOne, putils);
  auto catPropsTwo = CatalystManager::getInstance().getCatalystProps(id);

  conduit_cpp::Node n;
  CatalystManager::getInstance().addScriptProps(n, catalystProps);
  CatalystManager::getInstance().addScriptProps(n, catPropsOne);
  CatalystManager::getInstance().addScriptProps(n, catPropsTwo);
  compareConduit(CatalystManager::getInstance().getInitializeConduit(), n);
}

TEST_F(ManagerTest, ExecuteConduitOneScript)
{
  reset();
  std::string catalystFileName = "/path/to/file/catalystFile.txt";
  props.add(Ioss::Property(CatalystManager::CATALYST_SCRIPT, catalystFileName));
  initialize();

  conduit_cpp::Node m;
  m["some/data"] = 32;

  auto c = CatalystManager::getInstance().execute(catalystProps.catalystPipelineID, 2, 10.2, m);

  conduit_cpp::Node n;
  CatalystManager::getInstance().addExecuteProps(n, catalystProps, 2, 10.2, m);
  compareConduit(c, n);
}

TEST_F(ManagerTest, ExecuteConduitInputName)
{
  reset();
  props.add(Ioss::Property(CatalystManager::CATALYST_INPUT_NAME, "dataset"));
  initialize();

  int               state = 10;
  double            time  = 4.5;
  conduit_cpp::Node m;
  m["other/data"] = 90;

  auto c = CatalystManager::getInstance().execute(catalystProps.catalystPipelineID, state, time, m);

  conduit_cpp::Node n;
  CatalystManager::getInstance().addExecuteProps(n, catalystProps, state, time, m);
  compareConduit(c, n);
}

TEST_F(ManagerTest, ManagerStateDefault)
{
  reset();
  EXPECT_EQ(CatalystManager::getInstance().getManagerState(), CatalystManager::mInit);
}

TEST_F(ManagerTest, InvalidIDGetCatalystProps)
{
  reset();
  EXPECT_THROW(CatalystManager::getInstance().getCatalystProps(1), std::runtime_error);
}

TEST_F(ManagerTest, ManagerExecuteStateChange)
{
  reset();
  initialize();
  EXPECT_EQ(CatalystManager::getInstance().getManagerState(), CatalystManager::mInit);

  conduit_cpp::Node m;
  CatalystManager::getInstance().execute(catalystProps.catalystPipelineID, 2, 10.2, m);
  EXPECT_EQ(CatalystManager::getInstance().getManagerState(), CatalystManager::mExecute);

  EXPECT_THROW(CatalystManager::getInstance().initialize(props, putils), std::runtime_error);
}

TEST_F(ManagerTest, ManagerPipelineState)
{
  reset();
  initialize();
  EXPECT_EQ(CatalystManager::getInstance().getPipelineState(catalystProps.catalystPipelineID),
            CatalystManager::pExecute);
}

TEST_F(ManagerTest, ManagerFinalizeStateChange)
{
  reset();
  initialize();
  CatalystManager::getInstance().finalize(catalystProps.catalystPipelineID);
  EXPECT_EQ(CatalystManager::getInstance().getManagerState(), CatalystManager::mFinalize);
  EXPECT_EQ(CatalystManager::getInstance().getPipelineState(catalystProps.catalystPipelineID),
            CatalystManager::pFinalize);
  EXPECT_THROW(CatalystManager::getInstance().initialize(props, putils), std::runtime_error);
  conduit_cpp::Node m;
  EXPECT_THROW(CatalystManager::getInstance().execute(catalystProps.catalystPipelineID, 2, 10.2, m),
               std::runtime_error);
}

TEST_F(ManagerTest, ManagerFinalizeStateChangeMultipleScripts)
{
  reset();
  initialize();

  auto idOne = CatalystManager::getInstance().initialize(props, putils);
  auto idTwo = CatalystManager::getInstance().initialize(props, putils);

  CatalystManager::getInstance().finalize(catalystProps.catalystPipelineID);
  EXPECT_EQ(CatalystManager::getInstance().getPipelineState(catalystProps.catalystPipelineID),
            CatalystManager::pFinalize);
  EXPECT_EQ(CatalystManager::getInstance().getManagerState(), CatalystManager::mExecute);

  conduit_cpp::Node m;
  CatalystManager::getInstance().execute(idOne, 2, 10.2, m);
  CatalystManager::getInstance().finalize(idOne);
  EXPECT_EQ(CatalystManager::getInstance().getPipelineState(idOne), CatalystManager::pFinalize);
  EXPECT_EQ(CatalystManager::getInstance().getManagerState(), CatalystManager::mExecute);
  EXPECT_THROW(CatalystManager::getInstance().execute(idOne, 2, 10.2, m), std::runtime_error);

  CatalystManager::getInstance().finalize(idTwo);
  EXPECT_EQ(CatalystManager::getInstance().getPipelineState(idTwo), CatalystManager::pFinalize);
  EXPECT_EQ(CatalystManager::getInstance().getManagerState(), CatalystManager::mFinalize);
}

TEST_F(ManagerTest, ManagerGetCatDataPath)
{
  EXPECT_EQ(CatalystManager::getInstance().getCatDataPath(props), "catalyst/channels/input/data");
  std::string name = "foo";
  props.add(Ioss::Property(CatalystManager::CATALYST_INPUT_NAME, name));
  EXPECT_EQ(CatalystManager::getInstance().getCatDataPath(props), "catalyst/channels/foo/data");
}
