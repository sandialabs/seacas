
ADD_EXECUTABLE(ExodusToCatalystToScriptTests
               ExodusToCatalystToScriptTests.cxx)
TARGET_LINK_LIBRARIES(ExodusToCatalystToScriptTests
                      catalystiosstest)

ADD_EXECUTABLE(CgnsToCatalystToScriptTests
               CgnsToCatalystToScriptTests.cxx)
TARGET_LINK_LIBRARIES(CgnsToCatalystToScriptTests
                      catalystiosstest)

addCatalystTest("Exodus_Catalyst_Script" "1;4"
    ${CMAKE_CURRENT_BINARY_DIR}/ExodusToCatalystToScriptTests "")

addCatalystTest("CGNS_Catalyst_Script" "1;4"
    ${CMAKE_CURRENT_BINARY_DIR}/CgnsToCatalystToScriptTests "")
