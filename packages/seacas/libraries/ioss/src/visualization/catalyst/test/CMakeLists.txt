CMAKE_MINIMUM_REQUIRED(VERSION 3.8.0)

LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/test/Catch2/")
INCLUDE(CTest)
INCLUDE(Catch)
INCLUDE_DIRECTORIES("../ioss2catalyst/")
INCLUDE_DIRECTORIES(Catch2)

FILE(COPY test_data DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY(catalystiosstest SHARED
            CatalystTestFixture.cxx
            CameraInvalidPhactoriSyntax.cxx
            CameraValidPhactoriSyntax.cxx
            CameraValidPhactoriJSON.cxx
            TestDriverMain.cxx)
TARGET_LINK_LIBRARIES(catalystiosstest
                      catalystiossapp
                      VTK::jsoncpp
                      MPI::MPI_C)

ADD_EXECUTABLE(PhactoriSyntaxValidParserTests
               PhactoriSyntaxValidParserTests.cxx)
TARGET_LINK_LIBRARIES(PhactoriSyntaxValidParserTests
                      catalystiosstest
                      ${Python3_LIBRARY})
CATCH_DISCOVER_TESTS(PhactoriSyntaxValidParserTests
                     TEST_PREFIX "Parser_Valid_")

ADD_EXECUTABLE(PhactoriSyntaxInvalidParserTests
               PhactoriSyntaxInvalidParserTests.cxx)
TARGET_LINK_LIBRARIES(PhactoriSyntaxInvalidParserTests
                      catalystiosstest
                      ${Python3_LIBRARY})
CATCH_DISCOVER_TESTS(PhactoriSyntaxInvalidParserTests
                     TEST_PREFIX "Parser_Invalid_")

ADD_EXECUTABLE(ExodusToCatalystToScriptTests
               ExodusToCatalystToScriptTests.cxx)
TARGET_LINK_LIBRARIES(ExodusToCatalystToScriptTests
                      catalystiosstest
                      ${Python3_LIBRARY})
CATCH_DISCOVER_TESTS(ExodusToCatalystToScriptTests
                     TEST_PREFIX "Exodus_2_Script_")

ADD_EXECUTABLE(CgnsToCatalystToScriptTests
               CgnsToCatalystToScriptTests.cxx)
TARGET_LINK_LIBRARIES(CgnsToCatalystToScriptTests
                      catalystiosstest
                      ${Python3_LIBRARY})
CATCH_DISCOVER_TESTS(CgnsToCatalystToScriptTests
                     TEST_PREFIX "Cgns_2_Script_")

ADD_SUBDIRECTORY(PhactoriTest)

