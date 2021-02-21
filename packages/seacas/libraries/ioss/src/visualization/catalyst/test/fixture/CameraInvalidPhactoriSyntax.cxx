#include "CatalystTestFixture.h"

std::map<std::string, std::string>
CatalystTestFixture::cameraInvalidPhactoriSyntax = {

    {"CameraInvalidMultipleLookAt",
     R"(
     begin catalyst
       begin camera MyCamera
         look at absolute point = 0 0 0
         look at relative point = 0 0 0.5
       end camera MyCamera
     end
     )"
    },

    {"CameraInvalidLookAtInvalidElement",
     R"(
     begin catalyst
       begin camera MyCamera
         look at element = -1
       end camera MyCamera
     end
     )"
    },

    {"CameraInvalidLookAtInvalidNode",
     R"(
     begin catalyst
       begin camera MyCamera
         look at node = -1
       end camera MyCamera
     end
     )"
    },

    {"CameraInvalidLookDirectionZeros",
     R"(
     begin catalyst
       begin camera MyCamera
         look direction = 0 0 0
       end camera MyCamera
     end
     )"
    }
};
