#include "CatalystTestFixture.h"

std::map<std::string, std::string>
CatalystTestFixture::cameraValidPhactoriJSON = {

    {"Camera",
     R"({
     "camera blocks": {"fooCamera":{"camera type":"camera"}},
     "representation blocks": {},
     "imageset blocks": {"fooImageset":{"camera":"fooCamera","image size":[800,450]}},
     "operation blocks": {},
     "scatter plot blocks": {},
     "plot over time blocks": {},
     "visual marker blocks" : {},
     "onoff criteria blocks" : {},
     "experimental blocks" : {}
     })"
    },

    {"CameraLookAtAbsolutePoint",
     R"({
     "camera blocks": {"fooCamera":{"camera type":"camera","look at absolute point": [1.1, 2.0, 3e-8]}},
     "representation blocks": {},
     "imageset blocks": {"fooImageset":{"camera":"fooCamera","image size":[800,450]}},
     "operation blocks": {},
     "scatter plot blocks": {},
     "plot over time blocks": {},
     "visual marker blocks" : {},
     "onoff criteria blocks" : {},
     "experimental blocks" : {}
     })"
    },

    {"CameraLookDirection",
     R"({
    "camera blocks": {"fooCamera":{"camera type":"camera","look direction": [1.0, 2.0, 3.0]}},
    "representation blocks": {},
    "imageset blocks": {"fooImageset":{"camera":"fooCamera","image size":[800,450]}},
     "operation blocks": {},
     "scatter plot blocks": {},
     "plot over time blocks": {},
     "visual marker blocks" : {},
     "onoff criteria blocks" : {},
     "experimental blocks" : {}
     })"
    }

};
