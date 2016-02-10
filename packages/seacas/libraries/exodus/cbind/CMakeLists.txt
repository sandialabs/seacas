INCLUDE(TribitsLibraryMacros)
INCLUDE(InstallSymLink)

CONFIGURE_FILE(
  ${CMAKE_CURRENT_SOURCE_DIR}/include/exodusII_cfg.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/include/exodusII_cfg.h
  @ONLY IMMEDIATE)

INCLUDE_DIRECTORIES(
  "${CMAKE_CURRENT_SOURCE_DIR}/include"
  "${CMAKE_CURRENT_BINARY_DIR}/include"
)

FILE(GLOB SOURCES src/*.c)

SET_AND_INC_DIRS(DIR ${CMAKE_CURRENT_SOURCE_DIR})

IF (TPL_Netcdf_Enables_Netcdf4)
   ADD_DEFINITIONS(-DENABLE_NETCDF4)
ENDIF()

SET(HEADERS
  include/exodusII.h
  include/exodusII_par.h
)

IF (NOT SEACASExodus_ENABLE_MPI)
  LIST(REMOVE_ITEM SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/src/ex_create_par.c")
  LIST(REMOVE_ITEM SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/src/ex_open_par.c")
  LIST(REMOVE_ITEM HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/include/exodusII_par.h")
ENDIF()

TRIBITS_ADD_LIBRARY(
	exodus
	HEADERS	${HEADERS}
	SOURCES ${SOURCES}
)

if (${CMAKE_PROJECT_NAME} STREQUAL "SEACASProj")
# Many users of the SEACAS installations still want a static exodus library...
if (BUILD_SHARED_LIBS)
TRIBITS_ADD_LIBRARY(
	exodus_static
	STATIC
	NOINSTALLHEADERS
	SOURCES ${SOURCES}
)
set_target_properties(exodus_static PROPERTIES OUTPUT_NAME exodus)
endif(BUILD_SHARED_LIBS)

# For backwards compatability, the C exodus library was called libexoIIv2c.a
# Create a symbolic link from libexodus.a to libexoIIv2c.a -- do only for static lib
InstallSymLink(libexodus.a ${CMAKE_INSTALL_PREFIX}/lib/libexoIIv2c.a)
endif()

TRIBITS_ADD_TEST_DIRECTORIES(test)

