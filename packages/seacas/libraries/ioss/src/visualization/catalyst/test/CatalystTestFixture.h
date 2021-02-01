
#ifndef CatalystTestFixture_h
#define CatalystTestFixture_h

#include "IossApplication.h"
#include <map>

class CatalystTestFixture {
public:
    CatalystTestFixture();
    ~CatalystTestFixture();

    void checkPhactoriStringValidParse(const std::string& phactoriSyntax,
        const std::string& parsedJSONResult);

    void checkPhactoriStringInvalidParse(const std::string& phactoriSyntax);

    static std::map<std::string, std::string> cameraInvalidPhactoriSyntax;
    static std::map<std::string, std::string> cameraValidPhactoriSyntax;
    static std::map<std::string, std::string> cameraValidPhactoriJSON;

private:
    IossApplication ioapp;
};

#endif
