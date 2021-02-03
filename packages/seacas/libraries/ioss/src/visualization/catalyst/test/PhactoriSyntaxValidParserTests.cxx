#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "Camera", "[parser valid]") {
    checkPhactoriStringValidParse(
        cameraValidPhactoriSyntax["Camera"],
            cameraValidPhactoriJSON["Camera"]);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraLookAtAbsolutePoint", "[parser valid]") {
    checkPhactoriStringValidParse(
        cameraValidPhactoriSyntax["CameraLookAtAbsolutePoint"],
            cameraValidPhactoriJSON["CameraLookAtAbsolutePoint"]);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraLookDirection", "[parser valid]") {
    checkPhactoriStringValidParse(
        cameraValidPhactoriSyntax["CameraLookDirection"],
            cameraValidPhactoriJSON["CameraLookDirection"]);
}

