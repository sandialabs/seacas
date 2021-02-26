// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "TwoGridInputTest1_cgns_ex2", "[cgns to catalyst script]") {

    runPhactoriJSONTestTwoGrid("test3.json", "aero_blunt_wedge_test3.cgns",  "block_crush_1.ex2");
    checkTestOutputFileExists("CatalystOutput/test3.0000.png");
}

/*
TEST_CASE_METHOD(CatalystTestFixture,
    "TwoGridInputTest2_ex2_cgns", "[cgns to catalyst script]") {

    runPhactoriJSONTestTwoGrid("test2.json", "block_crush_1.ex2", "aero_blunt_wedge_test3.cgns");
    checkTestOutputFileExists("CatalystOutput/test3.0000.png");
}

TEST_CASE_METHOD(CatalystTestFixture,
    "TwoGridInputTest3_cgns_cgns", "[cgns to catalyst script]") {

    runPhactoriJSONTestTwoGrid("test2.json", "aero_blunt_wedge_test3.cgns", "aero_blunt_wedge_test3.cgns");
    checkTestOutputFileExists("CatalystOutput/test3.0000.png");
}

TEST_CASE_METHOD(CatalystTestFixture,
    "TwoGridInputTest4_ex2_ex2", "[cgns to catalyst script]") {

    runPhactoriJSONTestTwoGrid("test2.json", "block_crush_1.ex2", "block_crush_1.ex2");
    checkTestOutputFileExists("CatalystOutput/test3.0000.png");
}
*/

