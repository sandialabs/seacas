cmake_minimum_required(VERSION 2.6)
PROJECT(ExodusII C CXX Fortran)

SET(EXODUSII_VERSION_MAJOR "5")
SET(EXODUSII_VERSION_MINOR "14")
SET(EXODUSII_VERSION_PATCH "0")
SET(EXODUSII_VERSION "${EXODUSII_VERSION_MAJOR}.${EXODUSII_VERSION_MINOR}")
SET(EXODUSII_VERSION_FULL "${EXODUSII_VERSION}.${EXODUSII_VERSION_PATCH}")

OPTION(BUILD_SHARED "Build the exodus libraries shared." OFF)

IF(BUILD_SHARED)
  SET(EXODUS_LIBRARY_TYPE SHARED)
ELSE(BUILD_SHARED)
  SET(EXODUS_LIBRARY_TYPE STATIC)
ENDIF(BUILD_SHARED)

SUBDIRS(cbind forbind)

find_path( NETCDF_INCLUDE_DIR netcdf.h
   $ENV{ACCESS}/inc
   $ENV{NETCDF_DIR}/inc
   $ENV{NETCDF_DIR}/include
   $ENV{NETCDF_DIR}/libsrc
)	

find_library( NETCDF_LIBRARY
   NAMES netcdf
   PATHS "$ENV{ACCESS}/lib"
         "$ENV{NETCDF_DIR}/lib"
	 "$ENV{NETCDF_DIR}/libsrc/.libs"
)

find_program (NETCDF_NCDUMP ncdump
   $ENV{ACCESS}/bin
   $ENV{NETCDF_DIR}/bin
   $ENV{NETCDF_DIR}/ncdump
)

# Hack for HDF5
find_library( HDF5_LIBRARY hdf5
  $ENV{ACCESS}/lib/shared
  $ENV{ACCESS}/lib
  $ENV{NETCDF_DIR}/lib
  $ENV{NETCDF_DIR}/libsrc/.libs
  /usr/local/hdf5/lib
  )
find_library( HDF5HL_LIBRARY hdf5_hl
  $ENV{ACCESS}/lib/shared
  $ENV{ACCESS}/lib
  $ENV{NETCDF_DIR}/lib
  $ENV{NETCDF_DIR}/libsrc/.libs
  /usr/local/hdf5/lib
  )

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


find_program(PYTHON "python")

if (PYTHON)
  set(EXODUS_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/exodus.py.in")
  set(EXODUS_PY "${CMAKE_CURRENT_BINARY_DIR}/exodus.py")

  configure_file(${EXODUS_PY_IN} ${EXODUS_PY} @ONLY)

  INSTALL(FILES ${EXODUS_PY} DESTINATION ${PYTHON_INSTALL})

endif()
