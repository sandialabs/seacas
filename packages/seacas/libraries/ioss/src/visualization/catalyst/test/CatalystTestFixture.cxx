#include "CatalystTestFixture.h"
#include "catch.hpp"
#include <Iovs_Utils.h>
#include "vtk_jsoncpp.h"

CatalystTestFixture::CatalystTestFixture() {

}

CatalystTestFixture::~CatalystTestFixture() {

}

void CatalystTestFixture::checkPhactoriStringValidParse(
    const std::string& phactoriSyntax, const std::string& parsedJSONResult) {

    Iovs::CatalystManagerBase::ParseResult pres;
    Iovs::Utils::getInstance().getCatalystManager().parsePhactoriString(
        phactoriSyntax, pres);
    REQUIRE(!pres.parseFailed);

    Json::CharReaderBuilder builder {};
    auto reader = std::unique_ptr<Json::CharReader>( builder.newCharReader() );
    Json::Value parseRoot {};
    Json::Value goldRoot {};
    std::string errors {};

    auto parseWorked = reader->parse(pres.jsonParseResult.c_str(),
        pres.jsonParseResult.c_str() + pres.jsonParseResult.length(),
            &parseRoot, &errors );
 
    REQUIRE(parseWorked);

    reader->parse(parsedJSONResult.c_str(),
        parsedJSONResult.c_str() + parsedJSONResult.length(),
            &goldRoot, &errors );

    REQUIRE(parseWorked);
    REQUIRE(parseRoot == goldRoot);
}

void CatalystTestFixture::checkPhactoriStringInvalidParse(
    const std::string& phactoriSyntax) {

    Iovs::CatalystManagerBase::ParseResult pres;
    Iovs::Utils::getInstance().getCatalystManager().parsePhactoriString(
        phactoriSyntax, pres);
    REQUIRE(pres.parseFailed);
}
