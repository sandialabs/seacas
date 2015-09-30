if(USE_TRIBITS)
  INCLUDE(TribitsLibraryMacros)
else()
  find_path( NETCDF_INCLUDE_DIR NAMES netcdf.h
           PATHS
           $ENV{NETCDF_DIR}/include
           $ENV{NETCDF_DIR}/libsrc
           $ENV{ACCESS}/include
           NO_DEFAULT_PATH
           )
  find_path( NETCDF_INCLUDE_DIR NAMES netcdf.h)

  find_library( NETCDF_LIBRARY
              NAMES netcdf
              PATHS "$ENV{NETCDF_DIR}/lib"
                    "$ENV{NETCDF_DIR}/libsrc/.libs"
                    "$ENV{ACCESS}/lib"
              NO_DEFAULT_PATH)
  find_library( NETCDF_LIBRARY NAMES netcdf)

  find_program (NETCDF_NCDUMP ncdump
   $ENV{ACCESS}/bin
   $ENV{NETCDF_DIR}/bin
   $ENV{NETCDF_DIR}/ncdump
  )

IF(SEACAS_ENABLE_NETCDF4_SUPPORT)
# Hack for HDF5
  find_library( HDF5_LIBRARY NAMES hdf5
    $ENV{HDF5_DIR}/lib
    $ENV{HDF5_DIR}/libsrc/.libs
    $ENV{ACCESS}/lib/shared
    $ENV{ACCESS}/lib
    /usr/local/hdf5/lib
    NO_DEFAULT_PATH
  )
  find_library( HDF5_LIBRARY NAMES hdf5)

  find_library( HDF5HL_LIBRARY NAMES hdf5_hl
    $ENV{HDF5_DIR}/lib
    $ENV{HDF5_DIR}/libsrc/.libs
    $ENV{ACCESS}/lib/shared
    $ENV{ACCESS}/lib
    /usr/local/hdf5/lib
  )
  find_library( HDF5HL_LIBRARY NAMES hdf5_hl)

  # Check for ZLib, but only if using HDF5.
  FIND_PACKAGE(ZLIB)
  IF(NOT ZLIB_LIBRARY)
     MESSAGE(FATAL_ERROR "HDF5 Support specified, cannot find ZLib.")
  ENDIF()
  SET(USE_ZLIB ON)
  INCLUDE_DIRECTORIES(${ZLIB_INCLUDE_DIRS})

  # Check for curl. Only needed if using an installed netcdf built with curl.
  FIND_PACKAGE(CURL)
  IF(CURL_FOUND)
     SET(CURL ${CURL_LIBRARY})
  ENDIF()

else()
  ADD_DEFINITIONS("-DNOT_NETCDF4")
endif()
ENDIF()

CONFIGURE_FILE(
  ${CMAKE_CURRENT_SOURCE_DIR}/include/exodusII_cfg.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/include/exodusII_cfg.h
  @ONLY IMMEDIATE)


INCLUDE_DIRECTORIES(
  "${CMAKE_CURRENT_SOURCE_DIR}/include"
  "${CMAKE_CURRENT_BINARY_DIR}/include"
  "${NETCDF_INCLUDE_DIR}"
)

FILE(GLOB SOURCES src/*.c)

SET(HEADERS
  include/exodusII.h
  include/exodusII_par.h
)

IF (NOT SEACASExodus_ENABLE_MPI)
  LIST(REMOVE_ITEM SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/src/ex_create_par.c")
  LIST(REMOVE_ITEM SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/src/ex_open_par.c")
  LIST(REMOVE_ITEM HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/include/exodusII_par.h")
ENDIF()

if(USE_TRIBITS)
  TRIBITS_ADD_LIBRARY(
	exodus
	HEADERS	${HEADERS}
	SOURCES ${SOURCES}
  )
  TRIBITS_ADD_TEST_DIRECTORIES(test)

else()
  cmake_policy(SET CMP0003 NEW)
  add_library(exodus ${HEADERS} ${SOURCES})

  SET(required_libraries ${NETCDF_LIBRARY} ${HDF5HL_LIBRARY} ${HDF5_LIBRARY} ${ZLIB_LIBRARY} ${CURL})
  IF(NOT MSVC)
    SET(required_libraries ${required_libraries} m dl)
  ENDIF(NOT MSVC)
  TARGET_LINK_LIBRARIES(exodus ${required_libraries})

  IF(MSVC)
    set_target_properties(exodus PROPERTIES COMPILE_DEFINITIONS _CRT_SECURE_NO_WARNINGS)
  ENDIF(MSVC)

  IF(EXODUS_LIBRARY_PROPERTIES)
    SET_TARGET_PROPERTIES(exodus PROPERTIES ${EXODUS_LIBRARY_PROPERTIES})
  ENDIF(EXODUS_LIBRARY_PROPERTIES)

  IF(NOT EXODUS_INSTALL_NO_LIBRARIES)
    INSTALL(TARGETS exodus
      RUNTIME DESTINATION lib COMPONENT Runtime
      LIBRARY DESTINATION lib COMPONENT Runtime
      ARCHIVE DESTINATION lib COMPONENT Development)
    INSTALL(FILES ${cbind_HEADERS} DESTINATION include COMPONENT Development)
  ENDIF(NOT EXODUS_INSTALL_NO_LIBRARIES)

  INCLUDE (${CMAKE_ROOT}/Modules/Dart.cmake)

  IF (BUILD_TESTING)
    ENABLE_TESTING()
    SUBDIRS(test)
  ENDIF (BUILD_TESTING)
endif()

