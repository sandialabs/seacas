
#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "SimpleAeroBluntWedgeCgnsTest1", "[cgns to catalyst script]") {

    runPhactoriJSONTest("test2.json", "aero_blunt_wedge_test3.cgns");
    checkTestOutputFileExists("CatalystOutput/test2.0000.png");
}
