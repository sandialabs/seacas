
#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "SimpleBlockCrushTest1", "[exodus to catalyst script]") {

    runPhactoriJSONTest("../test_data/test1.json",
        "../test_data/block_crush_1.ex2");
    checkTestOutputFileExists("CatalystOutput/test1.0010.png");
}

