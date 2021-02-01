#include "CatalystTestFixture.h"

std::map<std::string, std::string>
CatalystTestFixture::cameraValidPhactoriSyntax = {

    {"Camera",
     R"(
     begin catalyst
       begin camera fooCamera
       end camera
       begin imageset fooImageset
         camera = fooCamera
         image size = 800 450
       end imageset
     end
     )"
    },

    {"CameraLookAtAbsolutePoint",
     R"(
     begin catalyst
       begin camera fooCamera
         look at absolute point = 1.1 2 3e-8
       end camera
       begin imageset fooImageset
         camera = fooCamera
         image size = 800 450
       end imageset
     end
     )"
    }

};
