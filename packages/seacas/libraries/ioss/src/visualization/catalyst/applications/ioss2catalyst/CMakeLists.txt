
SET(CATALYST_PLUGIN_INSTALL_DIR ${CMAKE_INSTALL_PREFIX})
SET(CATALYST_PLUGIN_BUILD_DIR ${PROJECT_BINARY_DIR})
CONFIGURE_FILE(CatalystPluginPaths.h.in CatalystPluginPaths.h @ONLY)

ADD_LIBRARY(ioss2catalyst SHARED
    IossRegionReport.cxx
    IossApplication.cxx)

TARGET_INCLUDE_DIRECTORIES(ioss2catalyst
    PUBLIC
        ${SEACAS_INCLUDE_DIRS}
        ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(ioss2catalyst
    PRIVATE
        -L${SEACAS_INSTALL_DIR}/lib
        Ionit
        Ioss
        MPI::MPI_C)

INSTALL(TARGETS ioss2catalyst LIBRARY DESTINATION lib)
