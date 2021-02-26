
ADD_EXECUTABLE(ExodusToCatalystToScriptTests
               ExodusToCatalystToScriptTests.cxx)
TARGET_LINK_LIBRARIES(ExodusToCatalystToScriptTests
                      catalystiosstest)

ADD_EXECUTABLE(CgnsToCatalystToScriptTests
               CgnsToCatalystToScriptTests.cxx)
TARGET_LINK_LIBRARIES(CgnsToCatalystToScriptTests
                      catalystiosstest)

ADD_EXECUTABLE(TwoGridsToCatalystToScriptTests
               TwoGridsToCatalystToScriptTests.cxx)
TARGET_LINK_LIBRARIES(TwoGridsToCatalystToScriptTests
                      catalystiosstest)

ADD_EXECUTABLE(TwoGridsToCatalystToScriptTests2
               TwoGridsToCatalystToScriptTests2.cxx)
TARGET_LINK_LIBRARIES(TwoGridsToCatalystToScriptTests2
                      catalystiosstest)

addCatalystTest("Exodus_Catalyst_Script" "1;4"
    ${CMAKE_CURRENT_BINARY_DIR}/ExodusToCatalystToScriptTests "")

addCatalystTest("CGNS_Catalyst_Script" "1;4"
    ${CMAKE_CURRENT_BINARY_DIR}/CgnsToCatalystToScriptTests "")

addCatalystTest("Two_Grids_Catalyst_Script" "1;4"
    ${CMAKE_CURRENT_BINARY_DIR}/TwoGridsToCatalystToScriptTests "")

addCatalystTest("Two_Grids_Catalyst_Script2" "1;4"
    ${CMAKE_CURRENT_BINARY_DIR}/TwoGridsToCatalystToScriptTests2 "")


