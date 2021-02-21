
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

TARGET_INCLUDE_DIRECTORIES(catalystiosstest PUBLIC
                           ${PROJECT_SOURCE_DIR}/ioss2catalyst
                           ${PROJECT_SOURCE_DIR}/test/Catch2
                           ${CMAKE_CURRENT_SOURCE_DIR})
