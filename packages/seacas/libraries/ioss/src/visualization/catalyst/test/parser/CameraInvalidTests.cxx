#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraInvalidMultipleLookAt", "[camera invalid]") {

     std::string ps = R"(
     begin catalyst
       begin camera MyCamera
         look at absolute point = 0 0 0
         look at relative point = 0 0 0.5
       end camera MyCamera
     end
     )";

    checkPhactoriStringInvalidParse(ps);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraInvalidLookAtInvalidElement", "[camera invalid]") {

    std::string ps = R"(
     begin catalyst
       begin camera MyCamera
         look at element = -1
       end camera MyCamera
     end
     )";

    checkPhactoriStringInvalidParse(ps);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraInvalidLookAtInvalidNode", "[camera invalid]") {

     std::string ps = R"(
     begin catalyst
       begin camera MyCamera
         look at node = -1
       end camera MyCamera
     end
     )";

    checkPhactoriStringInvalidParse(ps);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraInvalidLookDirectionZeros", "[camera invalid]") {

     std::string ps = R"(
     begin catalyst
       begin camera MyCamera
         look direction = 0 0 0
       end camera MyCamera
     end
     )";

    checkPhactoriStringInvalidParse(ps);
}
