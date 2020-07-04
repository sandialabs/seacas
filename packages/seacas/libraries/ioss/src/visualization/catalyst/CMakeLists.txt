# Copyright(C) 1999-2020 National Technology & Engineering Solutions
# of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
# NTESS, the U.S. Government retains certain rights in this software.
#
# See packages/seacas/LICENSE for details

cmake_minimum_required (VERSION 3.8.0)

project(CatalystIoss)

set (CMAKE_CXX_STANDARD 11)

set(PVCSA_VERSION CACHE STRING "Version string to append to catalyst adapter library.")

if(PVCSA_VERSION)
 SET(PVCSA_LIBRARY_NAME catalystioss_${PVCSA_VERSION})
else()
 SET(PVCSA_LIBRARY_NAME catalystioss)
endif()

find_package(ParaView REQUIRED)

set(PARAVIEW_PYTHON_LIBRARY_RPATH_DIRECTORY CACHE
    FILEPATH "Path to the directory containing the Python library used by ParaView.")

if(PARAVIEW_PYTHON_LIBRARY_RPATH_DIRECTORY)
  cmake_policy(SET CMP0015 OLD)
  link_directories(${PARAVIEW_PYTHON_LIBRARY_RPATH_DIRECTORY})
endif()

SET(CMAKE_SKIP_BUILD_RPATH FALSE)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib" ${PARAVIEW_PYTHON_LIBRARY_RPATH_DIRECTORY})
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
ENDIF("${isSystemDir}" STREQUAL "-1")

include_directories("manager")
include_directories("parser")
include_directories("../utils/")
include_directories("../exodus/")
include_directories("../cgns/")

add_subdirectory(parser)
add_library(${PVCSA_LIBRARY_NAME} SHARED manager/CatalystManager.cxx
                                         manager/CatalystMeshWriter.cxx
                                         exodus/CatalystExodusMesh.cxx
                                         cgns/CatalystCGNSMesh.cxx)

target_link_libraries(${PVCSA_LIBRARY_NAME}
    INTERFACE
      VTK::PythonUsed
    PUBLIC
      phactori_parser
    PRIVATE
      ParaView::PythonCatalyst
      VTK::CommonDataModel
      VTK::CommonExecutionModel
      VTK::ParallelMPI
      VTK::IOParallelXML
      VTK::IOXML)

OPTION(BUILD_CATALYST_ADAPTER_APPLICATIONS
    "Build exo2catalyst and cgns2catalyst applications" OFF)
IF(BUILD_CATALYST_ADAPTER_APPLICATIONS)
    FILE(COPY phactori DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    ADD_SUBDIRECTORY(applications)
ENDIF()

INSTALL(TARGETS ${PVCSA_LIBRARY_NAME} LIBRARY DESTINATION lib)
INSTALL(DIRECTORY phactori DESTINATION .)
