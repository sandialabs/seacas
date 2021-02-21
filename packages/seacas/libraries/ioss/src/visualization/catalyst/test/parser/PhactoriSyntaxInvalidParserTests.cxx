#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraInvalidMultipleLookAt", "[parser invalid]") {
    checkPhactoriStringInvalidParse(
        cameraInvalidPhactoriSyntax["CameraInvalidMultipleLookAt"]);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraInvalidLookAtInvalidElement", "[parser invalid]") {
    checkPhactoriStringInvalidParse(
        cameraInvalidPhactoriSyntax["CameraInvalidLookAtInvalidElement"]);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraInvalidLookAtInvalidNode", "[parser invalid]") {
    checkPhactoriStringInvalidParse(
        cameraInvalidPhactoriSyntax["CameraInvalidLookAtInvalidNode"]);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraInvalidLookDirectionZeros", "[parser invalid]") {
    checkPhactoriStringInvalidParse(
        cameraInvalidPhactoriSyntax["CameraInvalidLookDirectionZeros"]);
}
