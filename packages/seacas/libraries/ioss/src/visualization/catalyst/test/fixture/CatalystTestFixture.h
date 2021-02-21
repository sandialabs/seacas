
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
   
    Json::Value getDefaultPhactoriJSON();

    void runPhactoriJSONTest(const std::string& jsonFile,
        const std::string& inputFile);

    static std::map<std::string, std::string> cameraInvalidPhactoriSyntax;
    static std::map<std::string, std::string> cameraValidPhactoriSyntax;
    static std::map<std::string, std::string> cameraValidPhactoriJSON;

    void checkTestOutputFileExists(const char *fileName);

private:
    IossApplication ioapp;
};

#endif
