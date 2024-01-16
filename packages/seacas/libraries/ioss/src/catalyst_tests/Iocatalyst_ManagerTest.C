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
#include <stdio.h>

class ManagerTest : public ::testing::Test
{
protected:
  Ioss::PropertyManager                      props;
  Ioss::ParallelUtils                        putils;
  Iocatalyst::CatalystManager::CatalystProps catalystProps;
  void                                       initialize()
  {
    catalystProps = Iocatalyst::CatalystManager::getInstance().initialize(props, putils);
  }
};

void compareConduit(const conduit_cpp::Node &n, const conduit_cpp::Node &m)
{
  EXPECT_EQ(n.to_string(), m.to_string());
}

TEST_F(LoggingTest, LoggingDefault)
{
  Ioss::ParallelUtils putils;
  Iocatalyst::CatalystManager::getInstance().writeToCatalystLogFile(putils, props);
  EXPECT_FALSE(isFileExists(Iocatalyst::CatalystLogging::getDefaultLogFileName().c_str()));
}

TEST_F(LoggingTest, LoggingNotEnabled)
{
  Ioss::ParallelUtils putils;
  props.add(Ioss::Property("CATALYST_LOGGING_ENABLED", false));
  Iocatalyst::CatalystManager::getInstance().writeToCatalystLogFile(putils, props);
  EXPECT_FALSE(isFileExists(Iocatalyst::CatalystLogging::getDefaultLogFileName().c_str()));
}

TEST_F(LoggingTest, LoggingEnabled)
{
  Ioss::ParallelUtils putils;
  props.add(Ioss::Property("CATALYST_LOGGING_ENABLED", true));
  props.add(Ioss::Property("CATALYST_LOGGING_STRING_PROP", "foo"));
  props.add(Ioss::Property("CATALYST_LOGGING_INTEGER_PROP", 6));
  props.add(Ioss::Property("CATALYST_LOGGING_REAL_PROP", 3.7556));
  Iocatalyst::CatalystManager::getInstance().writeToCatalystLogFile(putils, props);
  EXPECT_TRUE(isFileExists(Iocatalyst::CatalystLogging::getDefaultLogFileName().c_str()));
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
  props.add(Ioss::Property(Iocatalyst::CatalystManager::PHACTORI_JSON_SCRIPT, jsonFileName));
  initialize();
  EXPECT_EQ(catalystProps.catalystBlockJSON, jsonScript);
  remove(jsonFileName.c_str());
}

TEST_F(ManagerTest, CATALYST_SCRIPT)
{
  initialize();
  EXPECT_EQ(catalystProps.catalystPythonFilename,
            Iocatalyst::CatalystManager::getInstance().getCatalystPythonDriverPath());

  std::string catalystFileName = "/path/to/file/catalystFile.txt";
  props.add(Ioss::Property(Iocatalyst::CatalystManager::CATALYST_SCRIPT, catalystFileName));
  initialize();
  EXPECT_EQ(catalystProps.catalystPythonFilename, catalystFileName);
}

TEST_F(ManagerTest, CATALYST_SCRIPT_EXTRA_FILE)
{
  std::string extraFileName = "extraFileName.txt";
  props.add(Ioss::Property(Iocatalyst::CatalystManager::CATALYST_SCRIPT_EXTRA_FILE, extraFileName));
  initialize();
  EXPECT_EQ(catalystProps.catalystScriptExtraFile, extraFileName);
}

TEST_F(ManagerTest, CATALYST_BLOCK_PARSE_INPUT_DECK_NAME)
{
  std::string inputDeckName = "contact.i";
  props.add(Ioss::Property(Iocatalyst::CatalystManager::CATALYST_BLOCK_PARSE_INPUT_DECK_NAME,
                           inputDeckName));
  initialize();
  EXPECT_EQ(catalystProps.catalystInputDeckName, inputDeckName);
}

TEST_F(ManagerTest, CATALYST_ENABLE_LOGGING)
{
  initialize();
  EXPECT_FALSE(catalystProps.enableLogging);

  props.add(Ioss::Property(Iocatalyst::CatalystManager::CATALYST_ENABLE_LOGGING, true));
  initialize();
  EXPECT_TRUE(catalystProps.enableLogging);
}

TEST_F(ManagerTest, CATALYST_DEBUG_LEVEL)
{
  initialize();
  EXPECT_EQ(catalystProps.debugLevel, 0);

  props.add(Ioss::Property(Iocatalyst::CatalystManager::CATALYST_DEBUG_LEVEL, 3));
  initialize();
  EXPECT_EQ(catalystProps.debugLevel, 3);
}

TEST_F(ManagerTest, CATALYST_OUTPUT_DIRECTORY)
{
  initialize();
  EXPECT_EQ(catalystProps.catalystOutputDirectory,
            Iocatalyst::CatalystManager::getInstance().CATALYST_OUTPUT_DEFAULT);

  std::string catalystOutputDirectory = "catalyst";
  props.add(Ioss::Property(Iocatalyst::CatalystManager::CATALYST_OUTPUT_DIRECTORY,
                           catalystOutputDirectory));
  initialize();
  EXPECT_EQ(catalystProps.catalystOutputDirectory, catalystOutputDirectory);
}

TEST_F(ManagerTest, CATALYST_INPUT_NAME)
{
  initialize();
  EXPECT_EQ(catalystProps.catalystInputName,
            Iocatalyst::CatalystManager::getInstance().CATALYST_INPUT_DEFAULT);

  std::string catalystInputName = "mesh";
  props.add(Ioss::Property(Iocatalyst::CatalystManager::CATALYST_INPUT_NAME, catalystInputName));
  initialize();
  EXPECT_EQ(catalystProps.catalystInputName, catalystInputName);
}

TEST_F(ManagerTest, CATALYST_MULTI_INPUT_PIPELINE_NAME)
{
  initialize();
  EXPECT_FALSE(catalystProps.enableCatalystMultiInputPipeline);

  std::string catalystMultiInputPipelineName = "multi";
  props.add(Ioss::Property(Iocatalyst::CatalystManager::CATALYST_MULTI_INPUT_PIPELINE_NAME,
                           catalystMultiInputPipelineName));
  initialize();
  EXPECT_EQ(catalystProps.catalystMultiInputPipelineName, catalystMultiInputPipelineName);
  EXPECT_TRUE(catalystProps.enableCatalystMultiInputPipeline);
}

TEST_F(ManagerTest, initializeConduitDefault)
{
  conduit_cpp::Node n;
  compareConduit(Iocatalyst::CatalystManager::getInstance().getInitializeConduit(), n);
}

TEST_F(ManagerTest, initializeConduitOnePipe) {}