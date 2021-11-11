// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "CatalystTestFixture.h"
#include "catch.hpp"
#include "ImageSetRepresentationValidTests.h"

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetRepresentationSurfaces",
                 "[ImageSetRepresentationValid]")
{
  Json::Value dj = getDefaultImageSetWithCameraJSON();

  Json::Value &imageset     = dj["imageset blocks"]["fooImageset"];
  imageset["show surfaces"] = false;
  imageset["show edges"]    = true;

  checkPhactoriStringValidParse(ImagesetRepresentationSurfaces, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetRepresentationEdges",
                 "[ImageSetRepresentationValid]")
{
  Json::Value dj = getDefaultImageSetWithCameraJSON();

  Json::Value &imageset  = dj["imageset blocks"]["fooImageset"];
  imageset["show edges"] = true;

  checkPhactoriStringValidParse(ImagesetRepresentationEdges, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetRepresentationBoundingBox",
                 "[ImageSetRepresentationValid]")
{
  Json::Value dj = getDefaultImageSetWithCameraJSON();

  Json::Value &imageset         = dj["imageset blocks"]["fooImageset"];
  imageset["show surfaces"]     = false;
  imageset["show bounding box"] = true;

  checkPhactoriStringValidParse(ImagesetRepresentationBoundingBox, dj);
}

TEST_CASE_METHOD(CatalystTestFixture, "ImagesetRepresentationColorByScalar",
                 "[ImageSetRepresentationValid]")
{
  Json::Value dj = getDefaultImageSetWithCameraJSON();

  Json::Value &imageset       = dj["imageset blocks"]["fooImageset"];
  imageset["color by scalar"] = "VON_MISES";

  checkPhactoriStringValidParse(ImagesetRepresentationColorByScalar, dj);
}