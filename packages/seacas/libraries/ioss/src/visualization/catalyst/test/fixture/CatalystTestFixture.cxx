#include "CatalystTestFixture.h"
#include "TestDataDirectoryPath.h"
#include "catch.hpp"
#include <Iovs_Utils.h>
#include <cstdlib>

CatalystTestFixture::CatalystTestFixture() {

}

CatalystTestFixture::~CatalystTestFixture() {

}

void CatalystTestFixture::runPhactoriJSONTest(
    const std::string& jsonFile, const std::string& inputFile) {

    std::string td = std::string(TEST_DATA_DIRECTORY_PATH);
    ioapp.setPhactoriInputJSON(td + jsonFile);
    ioapp.setFileName(td + inputFile);
    ioapp.runApplication();
    REQUIRE(ioapp.getApplicationExitCode() == EXIT_SUCCESS);
}

void CatalystTestFixture::checkPhactoriStringValidParse(
    const std::string& phactoriSyntax, const Json::Value& parsedJSONResult) {

    Iovs::CatalystManagerBase::ParseResult pres;
    Iovs::Utils::getInstance().getCatalystManager().parsePhactoriString(
        phactoriSyntax, pres);
    REQUIRE(!pres.parseFailed);

    Json::CharReaderBuilder builder {};
    auto reader = std::unique_ptr<Json::CharReader>( builder.newCharReader() );
    Json::Value parseRoot {};
    std::string errors {};

    auto parseWorked = reader->parse(pres.jsonParseResult.c_str(),
        pres.jsonParseResult.c_str() + pres.jsonParseResult.length(),
            &parseRoot, &errors );
 
    REQUIRE(parseWorked);
    REQUIRE(parseRoot == parsedJSONResult);
}

void CatalystTestFixture::checkPhactoriStringInvalidParse(
    const std::string& phactoriSyntax) {

    Iovs::CatalystManagerBase::ParseResult pres;
    Iovs::Utils::getInstance().getCatalystManager().parsePhactoriString(
        phactoriSyntax, pres);
    REQUIRE(pres.parseFailed);
}

Json::Value CatalystTestFixture::getDefaultPhactoriJSON() {
    Json::Value defPhac;
    defPhac["camera blocks"] = Json::objectValue;
    defPhac["representation blocks"] = Json::objectValue;
    defPhac["operation blocks"] = Json::objectValue;
    defPhac["imageset blocks"] = Json::objectValue;
    defPhac["scatter plot blocks"] = Json::objectValue;
    defPhac["plot over time blocks"] = Json::objectValue;
    defPhac["onoff criteria blocks"] = Json::objectValue;
    defPhac["visual marker blocks"] = Json::objectValue;
    defPhac["experimental blocks"] = Json::objectValue;
    return defPhac;
}

void CatalystTestFixture::checkTestOutputFileExists(const char *fileName) {
    FILE *fp = fopen(fileName, "r");
    bool outputFileExists = false;
    if (fp != NULL) {
      outputFileExists = true;
      fclose(fp);
    }
    REQUIRE(outputFileExists);
}

