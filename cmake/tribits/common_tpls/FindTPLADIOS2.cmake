# @HEADER
# ************************************************************************
#
#            TriBITS: Tribal Build, Integrate, and Test System
#                    Copyright 2016, 2024 Sandia Corporation
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

if (${CMAKE_VERSION} GREATER "3.13")
     cmake_policy(SET CMP0074 NEW)
endif()

# First, set up the variables for the (backward-compatible) TriBITS way of
# finding ADIOS2.  These are used in case find_package(ADIOS2 ...) is not
# called or does not find HDF5.  Also, these variables need to be non-null
# in order to trigger the right behavior in the function
# tribits_tpl_find_include_dirs_and_libraries().

set(REQUIRED_HEADERS adios2.h)
set(REQUIRED_LIBS_NAMES adios2_c adios2_cxx11)

if (TPL_ENABLE_MPI)
  set(REQUIRED_LIBS_NAMES ${REQUIRED_LIBS_NAMES} adios2_c_mpi adios2_cxx11_mpi)
endif()

#
# Second, search for ADIOS2 components (if allowed) using the standard
# find_package(ADIOS2 ...).
#
tribits_tpl_allow_pre_find_package(ADIOS2  ADIOS2_ALLOW_PREFIND)
if (ADIOS2_ALLOW_PREFIND)

  message("-- Using find_package(ADIOS2 ...) ...")
  set(CMAKE_MODULE_PATH
    "${CMAKE_MODULE_PATH}"
    "${CMAKE_CURRENT_LIST_DIR}/find_modules"
    "${CMAKE_CURRENT_LIST_DIR}/utils"
     )

  find_package(ADIOS2)
endif()

#
# Third, call tribits_tpl_find_include_dirs_and_libraries()
#
tribits_tpl_find_include_dirs_and_libraries( ADIOS2
  REQUIRED_HEADERS ${REQUIRED_HEADERS}
  REQUIRED_LIBS_NAMES ${REQUIRED_LIBS_NAMES}
  )
# NOTE: If find_package(ADIOS2 ...) was called and successfully found ADIOS2,
# then tribits_tpl_find_include_dirs_and_libraries() will use the already-set
# variables TPL_ADIOS2_INCLUDE_DIRS and TPL_ADIOS2_LIBRARIES and then print
# them out (and set some other standard variables as well).  This is the final
# "hook" into the TriBITS TPL system.

# If the `find_package(ADIOS2)` is not run, then this may not be set
# Need to determine how this is set in the library that is being used...

if ("${TPL_ADIOS2_PARALLEL}" STREQUAL "")
   assert_defined(TPL_ADIOS2_INCLUDE_DIRS)
   if ("${TPL_ADIOS2_PARALLEL}" STREQUAL "")
      set(TPL_ADIOS2_PARALLEL False CACHE INTERNAL
          "True if ADIOS2 compiled with parallel enabled")
   endif()
endif()
message(STATUS "TPL_ADIOS2_PARALLEL is ${TPL_ADIOS2_PARALLEL}")
