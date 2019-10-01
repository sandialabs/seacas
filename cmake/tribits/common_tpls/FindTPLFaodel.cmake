# @HEADER
# ************************************************************************
#
#            TriBITS: Tribal Build, Integrate, and Test System
#                    Copyright 2016 Sandia Corporation
#
# Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
# the U.S. Government retains certain rights in this software.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the Corporation nor the names of the
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA CORPORATION OR THE
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# ************************************************************************
# @HEADER

#
# First, set up the variables for the (backward-compatible) TriBITS way of
# finding Netcdf.  These are used in case FIND_PACKAGE(NetCDF ...) is not
# called or does not find NetCDF.  Also, these variables need to be non-null
# in order to trigger the right behavior in the function
# TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES().
#
SET(REQUIRED_HEADERS "kelpie/Kelpie.hh")
SET(REQUIRED_LIBS_NAMES tcmalloc spinlock sbl faodel-common webhook faodel-services nnti lunasa opbox dirman kelpie)

#
# Second, search for Netcdf components (if allowed) using the standard
# FIND_PACKAGE(Faodel ...).
#
TRIBITS_TPL_ALLOW_PRE_FIND_PACKAGE(Faodel Faodel_ALLOW_PREFIND)
IF (Faodel_ALLOW_PREFIND)

  MESSAGE(STATUS "Using FIND_PACKAGE(Faodel...) ...")

  SET(CMAKE_MODULE_PATH
    "${CMAKE_MODULE_PATH}"
    "${CMAKE_CURRENT_LIST_DIR}/find_modules"
    "${CMAKE_CURRENT_LIST_DIR}/utils"
     )
  
   find_package(Faodel REQUIRED CONFIG
     HINTS
     ${Faodel_ROOT}
     ${Faodel_ROOT}/include
     ${Faodel_ROOT}/include/faodel
     ${Faodel_ROOT}/lib/cmake/Faodel
     COMPONENTS
     tcmalloc spinlock sbl common webhook services nnti lunasa opbox dirman kelpie
     )

   # Faodel::tcmalloc Faodel::spinlock Faodel::sbl Faodel::common Faodel::webhook Faodel::services Faodel::nnti Faodel::lunasa Faodel::opbox Faodel::dirman Faodel::kelpie
  IF (Faodel_FOUND)
    set(TPL_Faodel_LIBRARIES ${Faodel_LIBRARIES} CACHE PATH
      "List of semi-colon seprated (full) paths to the Faodel libraries")
    set(TPL_Faodel_INCLUDE_DIRS ${Faodel_INCLUDE_DIRS} CACHE PATH
      "List of semi-colon seprated list of directories containing Faodel header files")
  ENDIF()

ENDIF()

#
# Third, call TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES()
#
TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES( Faodel
  REQUIRED_HEADERS ${REQUIRED_HEADERS}
  REQUIRED_LIBS_NAMES ${REQUIRED_LIBS_NAMES})
