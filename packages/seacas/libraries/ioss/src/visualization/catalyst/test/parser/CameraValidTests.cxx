#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture,
    "Camera", "[camera valid]") {

     std::string ps = R"(
     begin catalyst
       begin camera fooCamera
       end camera
       begin imageset fooImageset
         camera = fooCamera
         image size = 800 450
       end imageset
     end
     )";

    Json::Value camera;
    camera["camera type"] = "camera";

    Json::Value imageSet;
    imageSet["camera"] = "fooCamera";
    imageSet["image size"][0] = 800;
    imageSet["image size"][1] = 450;

    Json::Value dj = getDefaultPhactoriJSON();
    dj["camera blocks"]["fooCamera"] = camera;
    dj["imageset blocks"]["fooImageset"] = imageSet;

    checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraLookAtAbsolutePoint", "[camera valid]") {

     std::string ps = R"(
     begin catalyst
       begin camera fooCamera
         look at absolute point = 1.1 2 3e-8
       end camera
       begin imageset fooImageset
         camera = fooCamera
         image size = 800 450
       end imageset
     end
     )";

    Json::Value camera;
    camera["camera type"] = "camera";
    camera["look at absolute point"][0] = 1.1;
    camera["look at absolute point"][1] = 2.0;
    camera["look at absolute point"][2] = 3e-8;

    Json::Value imageSet;
    imageSet["camera"] = "fooCamera";
    imageSet["image size"][0] = 800;
    imageSet["image size"][1] = 450;

    Json::Value dj = getDefaultPhactoriJSON();
    dj["camera blocks"]["fooCamera"] = camera;
    dj["imageset blocks"]["fooImageset"] = imageSet;

    checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture,
    "CameraLookDirection", "[camera valid]") {

     std::string ps = R"(
     begin catalyst
       begin camera fooCamera
         look direction = 1 2 3
       end camera
       begin imageset fooImageset
         camera = fooCamera
         image size = 800 450
       end imageset
     end
     )";

    Json::Value camera;
    camera["camera type"] = "camera";
    camera["look direction"][0] = 1.0;
    camera["look direction"][1] = 2.0;
    camera["look direction"][2] = 3.0;

    Json::Value imageSet;
    imageSet["camera"] = "fooCamera";
    imageSet["image size"][0] = 800;
    imageSet["image size"][1] = 450;

    Json::Value dj = getDefaultPhactoriJSON();
    dj["camera blocks"]["fooCamera"] = camera;
    dj["imageset blocks"]["fooImageset"] = imageSet;

    checkPhactoriStringValidParse(ps, dj);
}
