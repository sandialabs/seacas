#
# A) Define your project name and set up major project options
#

# Must set the project name as a variable at very beginning before including anything else
# We set the project name in a separate file so CTest scripts can use it.
INCLUDE(${CMAKE_SOURCE_DIR}/ProjectName.cmake)

SET(SEACASProj_ENABLE_CXX11_DEFAULT ON)
SET(SEACASProj_ENABLE_Zoltan_DEFAULT ON)

# Define the TriBITS minimum required CMake version
SET(TRIBITS_CMAKE_MINIMUM_REQUIRED 3.1)

# CMake requires this be in the top file and not in an include file :-(
CMAKE_MINIMUM_REQUIRED(VERSION ${TRIBITS_CMAKE_MINIMUM_REQUIRED} FATAL_ERROR)

# CMake requires that you declare the CMake project in the top-level file and
# not in an include file :-(
PROJECT(${PROJECT_NAME} NONE)

#
# B) Pull in the TriBITS system and execute
#

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/cmake/tribits/TriBITS.cmake)
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules/")
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${${PROJECT_NAME}_TRIBITS_DIR}/common_tpls/utils)

# Make Trilinos create <Package>Config.cmake files by default
SET(${PROJECT_NAME}_ENABLE_INSTALL_CMAKE_CONFIG_FILES_DEFAULT ON)
# Make Trilinos set up CPack support by default
SET(${PROJECT_NAME}_ENABLE_CPACK_PACKAGING_DEFAULT ON)
# Don't allow disabled subpackages to be excluded from tarball
SET(${PROJECT_NAME}_EXCLUDE_DISABLED_SUBPACKAGES_FROM_DISTRIBUTION_DEFAULT FALSE)

SET(TPL_ENABLE_Pthread OFF CACHE BOOL "")

OPTION(SEACASProj_ENABLE_DOXYGEN "Enable Doxygen documentation creation." ON)

# Do all of the processing for this Tribits project
TRIBITS_PROJECT()

