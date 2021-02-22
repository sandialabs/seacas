
#ifndef CatalystTestFixture_h
#define CatalystTestFixture_h

#include "IossApplication.h"
#include "vtk_jsoncpp.h"
#include <map>

class CatalystTestFixture {
public:
    CatalystTestFixture();
    ~CatalystTestFixture();

    void checkPhactoriStringValidParse(const std::string& phactoriSyntax,
        const Json::Value& parsedJSONResult);

    void checkPhactoriStringInvalidParse(const std::string& phactoriSyntax);
   
    void runPhactoriJSONTest(const std::string& jsonFile,
        const std::string& inputFile);

    void checkTestOutputFileExists(const char *fileName);

    Json::Value getDefaultPhactoriJSON();

    Json::Value getDefaultCameraJSON();
    Json::Value getDefaultCameraParallelProjectionJSON();

private:
    IossApplication ioapp;
};

#endif
