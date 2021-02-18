
#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "SimpleHiFireTest1", "[cgns to catalyst script]") {
    char *myArgv[4] = {
     "ioss2catalyst",
     "-i",
     "test_data/test2.json",
     "test_data/aero_blunt_wedge_test3.cgns"
    };
    int myArgc = 4;
    runApplicationWithFakeCommandLine(myArgc, myArgv);
    checkTestOutputFileExists("CatalystOutput/test2.0000.png");
}

