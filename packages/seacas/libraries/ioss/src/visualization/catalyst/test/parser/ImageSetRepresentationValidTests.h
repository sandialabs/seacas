// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#ifndef __IMAGE_SET_REPRESENTATION_VALID_TESTS_H
#define __IMAGE_SET_REPRESENTATION_VALID_TESTS_H

#include <string>

std::string ImagesetRepresentationSurfaces = R"(
    begin catalyst
     begin camera singletestcamera
        look direction = -1 -1 -1
      end
      begin imageset fooImageset
        camera = singletestcamera
        show surfaces = false
        show edges = true
        image size = 800 450
      end imageset
    end
    )";

std::string ImagesetRepresentationEdges = R"(
    begin catalyst
     begin camera singletestcamera
        look direction = -1 -1 -1
      end
      begin imageset fooImageset
        camera = singletestcamera
        show edges = true
        image size = 800 450
      end imageset
    end
    )";

std::string ImagesetRepresentationBoundingBox = R"(
    begin catalyst
     begin camera singletestcamera
        look direction = -1 -1 -1
      end
      begin imageset fooImageset
        camera = singletestcamera
        show surfaces = false
        show bounding box = true
        image size = 800 450
      end imageset
    end
    )";

std::string ImagesetRepresentationColorByScalar = R"(
    begin catalyst
     begin camera singletestcamera
        look direction = -1 -1 -1
      end
      begin imageset fooImageset
        camera = singletestcamera
        color by scalar = VON_MISES
        image size = 800 450
      end imageset
    end
    )";

#endif /* __IMAGE_SET_REPRESENTATION_VALID_TESTS_H */