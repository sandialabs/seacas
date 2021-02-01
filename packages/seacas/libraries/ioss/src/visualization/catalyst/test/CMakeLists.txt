CMAKE_MINIMUM_REQUIRED(VERSION 3.8.0)

LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/test/Catch2/contrib")
INCLUDE(CTest)
ADD_SUBDIRECTORY(Catch2)
INCLUDE(Catch)
INCLUDE_DIRECTORIES("../ioss2catalyst/")

ADD_LIBRARY(catalystiosstest SHARED
            CatalystTestFixture.cxx
            CameraInvalidPhactoriSyntax.cxx
            CameraValidPhactoriSyntax.cxx
            CameraValidPhactoriJSON.cxx
            TestDriverMain.cxx)
TARGET_LINK_LIBRARIES(catalystiosstest
                      catalystiossapp
                      Catch2::Catch2
                      VTK::jsoncpp
                      MPI::MPI_C)

ADD_EXECUTABLE(PhactoriSyntaxValidParserTests
               PhactoriSyntaxValidParserTests.cxx)
TARGET_LINK_LIBRARIES(PhactoriSyntaxValidParserTests
                      catalystiosstest
                      Catch2::Catch2)
CATCH_DISCOVER_TESTS(PhactoriSyntaxValidParserTests
                     TEST_PREFIX "Parser_Valid_")

ADD_EXECUTABLE(PhactoriSyntaxInvalidParserTests
               PhactoriSyntaxInvalidParserTests.cxx)
TARGET_LINK_LIBRARIES(PhactoriSyntaxInvalidParserTests
                      catalystiosstest
                      Catch2::Catch2)
CATCH_DISCOVER_TESTS(PhactoriSyntaxInvalidParserTests
                     TEST_PREFIX "Parser_Invalid_")
