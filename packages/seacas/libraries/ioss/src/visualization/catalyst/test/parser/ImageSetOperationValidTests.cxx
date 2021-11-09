// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "CatalystTestFixture.h"
#include "catch.hpp"

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetOperationThreshold1", "[ImageSetOperationValid]")
{

  std::string ps = R"(
    begin catalyst
      begin camera singletestcamera
        look direction = -1 -1 -1
      end
      begin imageset fooImageset
        camera = singletestcamera
        threshold = scalar VON_MISES keep between 28.5 30.5
        image size = 800 450
      end imageset
    end
    )";

  Json::Value dj = getDefaultImageSetWithCameraJSON();

  Json::Value &imageset = dj["imageset blocks"]["fooImageset"];
  imageset["threshold"] = "scalar VON_MISES keep between 28.5 30.5";
  Json::Value th;
  th[0] = "scalar";
  th[1] = "VON_MISES";
  th[2] = "keep between";
  Json::Value kb;
  kb[0] = 28.5;
  kb[1] = 30.5;
  th[3] = kb;
  imageset["threshold"] = th;

  checkPhactoriStringValidParse(ps, dj);
}