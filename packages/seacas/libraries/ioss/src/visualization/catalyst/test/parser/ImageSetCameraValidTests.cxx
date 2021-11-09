// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetCameraLookAtAbsolutePoint", "[ImageSetCameraValid]")
{

  std::string ps = R"(
    begin catalyst
      begin imageset fooImageset
        look at absolute point = 1.1 2 3e-8
        image size = 800 450
      end imageset
    end
    )";

  Json::Value  dj                       = getDefaultImageSetJSON();
  Json::Value &imageset                 = dj["imageset blocks"]["fooImageset"];
  imageset["look at absolute point"][0] = 1.1;
  imageset["look at absolute point"][1] = 2.0;
  imageset["look at absolute point"][2] = 3e-8;

  checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetCameraLookAtRelativePoint", "[ImageSetCameraValid]")
{

  std::string ps = R"(
     begin catalyst
      begin imageset fooImageset
        look at relative point = 1.5 0.5 0.5
        image size = 800 450
      end imageset
    end
    )";

  Json::Value  dj                       = getDefaultImageSetJSON();
  Json::Value &imageset                 = dj["imageset blocks"]["fooImageset"];
  imageset["look at relative point"][0] = 1.5;
  imageset["look at relative point"][1] = 0.5;
  imageset["look at relative point"][2] = 0.5;

  checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetCameraLookAtElement", "[ImageSetCameraValid]")
{

  std::string ps = R"(
     begin catalyst
      begin imageset fooImageset
        look at element = 17
        image size = 800 450
      end imageset
    end
    )";

  Json::Value  dj             = getDefaultImageSetJSON();
  Json::Value &imageset       = dj["imageset blocks"]["fooImageset"];
  imageset["look at element"] = 17;

  checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetCameraLookAtNode", "[ImageSetCameraValid]")
{

  std::string ps = R"(
     begin catalyst
      begin imageset fooImageset
        look at node = 20
        image size = 800 450
      end imageset
    end
    )";

  Json::Value  dj          = getDefaultImageSetJSON();
  Json::Value &imageset    = dj["imageset blocks"]["fooImageset"];
  imageset["look at node"] = 20;

  checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetCameraLookAtRelativeDistance",
                 "[ImageSetCameraValid]")
{

  std::string ps = R"(
     begin catalyst
      begin imageset fooImageset
        look at relative distance = 2.0
        image size = 800 450
      end imageset
    end
    )";

  Json::Value  dj                       = getDefaultImageSetJSON();
  Json::Value &imageset                 = dj["imageset blocks"]["fooImageset"];
  imageset["look at relative distance"] = 2.0;

  checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetCameraLookAtAbsoluteDistance",
                 "[ImageSetCameraValid]")
{

  std::string ps = R"(
     begin catalyst
      begin imageset fooImageset
        look at absolute distance = 15.0
        image size = 800 450
      end imageset
    end
    )";

  Json::Value  dj                       = getDefaultImageSetJSON();
  Json::Value &imageset                 = dj["imageset blocks"]["fooImageset"];
  imageset["look at absolute distance"] = 15.0;

  checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetCameraUpVector", "[ImageSetCameraValid]")
{

  std::string ps = R"(
     begin catalyst
      begin imageset fooImageset
        up vector = 0 1 2
        image size = 800 450
      end imageset
    end
    )";

  Json::Value  dj          = getDefaultImageSetJSON();
  Json::Value &imageset    = dj["imageset blocks"]["fooImageset"];
  imageset["up vector"][0] = 0.0;
  imageset["up vector"][1] = 1.0;
  imageset["up vector"][2] = 2.0;

  checkPhactoriStringValidParse(ps, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetCameraFOV", "[ImageSetCameraValid]")
{

  std::string ps = R"(
     begin catalyst
      begin imageset fooImageset
        camera fov = 45
        look at relative point = 0.0 0.0 0.0
        look at absolute distance = 10.0
        image size = 800 450
      end imageset
    end
    )";

  Json::Value  dj                       = getDefaultImageSetJSON();
  Json::Value &imageset                 = dj["imageset blocks"]["fooImageset"];
  imageset["camera fov"]                = 45.0;
  imageset["look at relative point"][0] = 0.0;
  imageset["look at relative point"][1] = 0.0;
  imageset["look at relative point"][2] = 0.0;
  imageset["look at absolute distance"] = 10.0;

  checkPhactoriStringValidParse(ps, dj);
}