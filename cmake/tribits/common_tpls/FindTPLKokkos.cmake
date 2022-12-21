# @HEADER
# ************************************************************************
#
#            TriBITS: Tribal Build, Integrate, and Test System
#                    Copyright 2016, 2022 Sandia Corporation
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
# finding Netcdf.  These are used in case find_package(NetCDF ...) is not
# called or does not find NetCDF.  Also, these variables need to be non-null
# in order to trigger the right behavior in the function
# tribits_tpl_find_include_dirs_and_libraries().
#
set(REQUIRED_HEADERS Kokkos_Core.h)
set(REQUIRED_LIBS_NAMES kokkos)

#
# Second, search for Netcdf components (if allowed) using the standard
# find_package(CGNS ...).
#
tribits_tpl_allow_pre_find_package(Kokkos  Kokkos_ALLOW_PREFIND)
if (Kokkos_ALLOW_PREFIND)

  message("-- Using find_package(Kokkos ...) ...")

  set(CMAKE_MODULE_PATH
    "${CMAKE_MODULE_PATH}"
    "${CMAKE_CURRENT_LIST_DIR}/find_modules"
    "${CMAKE_CURRENT_LIST_DIR}/utils"
     )
  
  find_package(Kokkos)

  if (Kokkos_FOUND)
    set(TPL_Kokkos_LIBRARY_DIRS ${_hdf5_LIBRARY_SEARCH_DIRS} CACHE PATH
      "${DOCSTR} library files")
    set(TPL_Kokkos_LIBRARIES ${Kokkos_LIBRARIES} CACHE PATH
      "List of semi-colon seprated (full) paths to the Kokkos libraries")
    set(TPL_Kokkos_INCLUDE_DIRS ${Kokkos_INCLUDE_DIRS} CACHE PATH
      "List of semi-colon seprated list of directories containing Kokkos header files")
  endif()

endif()

#
# Third, call tribits_tpl_find_include_dirs_and_libraries()
#
tribits_tpl_find_include_dirs_and_libraries( Kokkos
  REQUIRED_HEADERS ${REQUIRED_HEADERS}
  REQUIRED_LIBS_NAMES ${REQUIRED_LIBS_NAMES})
