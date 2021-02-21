
#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "SimpleBlockCrushTest1", "[exodus to catalyst script]") {

    runPhactoriJSONTest("test1.json", "block_crush_1.ex2");
    checkTestOutputFileExists("CatalystOutput/test1.0010.png");
}
