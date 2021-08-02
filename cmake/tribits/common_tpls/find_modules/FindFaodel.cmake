# -*- mode: cmake -*-
# @HEADER
# ************************************************************************
#
#            Trilinos: An Object-Oriented Solver Framework
#                 Copyright (2001) Sandia Corporation
#
#
# Copyright (2001) Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000, there is a non-exclusive license for use of this
# work by or on behalf of the U.S. Government.  Export of this program
# may require a license from the United States Government.
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
# NOTICE:  The United States Government is granted for itself and others
# acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
# license in this data to reproduce, prepare derivative works, and
# perform publicly and display publicly.  Beginning five (5) years from
# July 25, 2001, the United States Government is granted for itself and
# others acting on its behalf a paid-up, nonexclusive, irrevocable
# worldwide license in this data to reproduce, prepare derivative works,
# distribute copies to the public, perform publicly and display
# publicly, and to permit others to do so.
#
# NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES DEPARTMENT
# OF ENERGY, NOR SANDIA CORPORATION, NOR ANY OF THEIR EMPLOYEES, MAKES
# ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR
# RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY
# INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS
# THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
#
# ************************************************************************
# @HEADER
#
# Based on the CGNS Find Module in this repository
#
# Usage:
#    Control the search through Faodel_DIR or setting environment variable
#    Faodel_ROOT to the Faodel installation prefix.
#
#    This module does not search default paths! 
#
#    Following variables are set:
#    Faodel_FOUND            (BOOL)       Flag indicating if Faodel was found
#    Faodel_INCLUDE_DIR      (PATH)       Path to the Faodel include file
#    Faodel_INCLUDE_DIRS     (LIST)       List of all required include files
#    Faodel_LIBRARY_DIR      (PATH)       Path to the Faodel library
#    Faodel_LIBRARY          (FILE)       Faodel library
#    Faodel_LIBRARIES        (LIST)       List of all required Faodel libraries
#
# #############################################################################

# Standard CMake modules see CMAKE_ROOT/Modules
include(FindPackageHandleStandardArgs)

# MSTK CMake functions see <root>/cmake/modules for source
include(AddPackageDependency)

# If Faodel_ROOT was defined in the environment, use it.
# Definition from the command line will take precedence.
if (NOT Faodel_ROOT AND NOT $ENV{Faodel_ROOT} STREQUAL "")
  set(Faodel_ROOT $ENV{Faodel_ROOT})
endif()

# Faodel_DIR is DEPRECATED WARN THE USER if it is set
if (NOT Faodel_ROOT AND Faodel_DIR )
  message(WARNING "The configuration parameter Faodel_DIR is deprecated."
    " Please use Faodel_ROOT instead to define the Faodel installation")
  set(Faodel_ROOT ${Faodel_DIR})
endif()  

# Add the usual paths for searching using the Faodel_ROOT variable
if (Faodel_ROOT)
  list(APPEND _faodel_INCLUDE_SEARCH_DIRS 
    ${Faodel_ROOT}/include/faodel
    ${Faodel_ROOT})

  list(APPEND _faodel_LIBRARY_SEARCH_DIRS 
    ${Faodel_ROOT}/lib
    ${Faodel_ROOT})

  list(APPEND _faodel_BINARY_SEARCH_DIRS 
    ${Faodel_ROOT}/bin
    ${Faodel_ROOT})
endif()

if ( Faodel_LIBRARIES AND Faodel_INCLUDE_DIRS )

  # Do nothing. Variables are set. No need to search again

else()

  # Cache variables
  if(Faodel_ROOT)
    set(Faodel_ROOT "${Faodel_ROOT}" CACHE PATH "Path to search for Faodel include and library files")
  endif()

  if(Faodel_INCLUDE_DIR)
    set(Faodel_INCLUDE_DIR "${Faodel_INCLUDE_DIR}" CACHE PATH "Path to search for Faodel include files")
  endif()

  if(Faodel_LIBRARY_DIR)
    set(Faodel_LIBRARY_DIR "${Faodel_LIBRARY_DIR}" CACHE PATH "Path to search for Faodel library files")
  endif()


  # Search for include files
  # Search order preference:
  #  (1) Faodel_INCLUDE_DIR - check existence of path AND if the include files exist
  #  (2) Faodel_ROOT/<include>
  #  (3) Default CMake paths See cmake --html-help=out.html file for more information.
  #
  set(faodel_inc_names "kelpie/Kelpie.hh")
  if (Faodel_INCLUDE_DIR)

    if (EXISTS "${Faodel_INCLUDE_DIR}")

      find_path(cdf_test_include_path
        NAMES ${faodel_inc_names}
        HINTS ${Faodel_ROOT}/include/faodel
        NO_DEFAULT_PATH)
      if(NOT cdf_test_include_path)
        message(SEND_ERROR "Can not locate ${faodel_inc_names} in ${Faodel_INCLUDE_DIR}")
      endif()
      set(Faodel_INCLUDE_DIR "${cdf_test_include_path}")

    else()
      message(SEND_ERROR "Faodel_INCLUDE_DIR=${Faodel_INCLUDE_DIR} does not exist")
      set(Faodel_INCLUDE_DIR "Faodel_INCLUDE_DIR-NOTFOUND")
    endif()

  else() 

    set(faodel_inc_suffixes "include/faodel")
    if(Faodel_ROOT)

      if (EXISTS "${Faodel_ROOT}" )

        find_path(Faodel_INCLUDE_DIR
          NAMES ${faodel_inc_names}
          HINTS ${Faodel_ROOT}/include/faodel
          PATH_SUFFIXES ${faodel_inc_suffixes}
          NO_DEFAULT_PATH)

      else()
        message(SEND_ERROR "Faodel_ROOT=${Faodel_ROOT} does not exist")
        set(Faodel_INCLUDE_DIR "Faodel_INCLUDE_DIR-NOTFOUND")
      endif()    


    else()

      find_path(Faodel_INCLUDE_DIR
        NAMES ${faodel_inc_names}
        PATH_SUFFIXES ${faodel_inc_suffixes})

    endif()

  endif()


  if ( NOT Faodel_INCLUDE_DIR )
    message(SEND_ERROR "Can not locate Faodel include directory")
  endif()

  # Search for libraries 
  # Search order preference:
  #  (1) Faodel_LIBRARY_DIR - check existence of path AND if the include files exist
  #  (2) Faodel_ROOT/<lib,Lib>
  #  (3) Default CMake paths See cmake --html-help=out.html file for more information.
  #
  if (Faodel_LIBRARY_DIR)

    if (EXISTS "${Faodel_LIBRARY_DIR}")

      find_library(Faodel_LIBRARY
        NAMES tcmalloc spinlock sbl faodel-common whookie faodel-services nnti lunasa opbox dirman kelpie
        HINTS ${Faodel_LIBRARY_DIR}
        NO_DEFAULT_PATH)

    else()
      message(SEND_ERROR "Faodel_LIBRARY_DIR=${Faodel_LIBRARY_DIR} does not exist")
      set(Faodel_LIBRARY "Faodel_LIBRARY-NOTFOUND")
    endif()

  else() 

    if(Faodel_ROOT)

      if (EXISTS "${Faodel_ROOT}" )

        find_library(Faodel_LIBRARY
          NAMES tcmalloc spinlock sbl faodel-common whookie faodel-services nnti lunasa opbox dirman kelpie
          HINTS ${Faodel_ROOT} ${Faodel_ROOT}/lib
          PATH_SUFFIXES "lib" "Lib"
          NO_DEFAULT_PATH)

      else()
        message(SEND_ERROR "Faodel_ROOT=${Faodel_ROOT} does not exist")
        set(Faodel_LIBRARY "Faodel_LIBRARY-NOTFOUND")
      endif()    


    else()

      find_library(Faodel_LIBRARY
        NAMES tcmalloc spinlock sbl faodel-common whookie faodel-services nnti lunasa opbox dirman kelpie
        PATH_SUFFIXES ${faodel_lib_suffixes})

    endif()

  endif()

  if ( NOT Faodel_LIBRARY )
    message(SEND_ERROR "Can not locate Faodel library")
  endif()    

  # Define the LIBRARIES and INCLUDE_DORS
  set(Faodel_INCLUDE_DIRS ${Faodel_INCLUDE_DIR})
  set(Faodel_LIBRARIES    ${Faodel_CXX_LIBRARY} ${Faodel_LIBRARY})

endif()


# Send useful message if everything is found
find_package_handle_standard_args(Faodel DEFAULT_MSG
  Faodel_LIBRARIES
  Faodel_INCLUDE_DIRS)

# find_package)handle)standard_args should set Faodel_FOUND but it does not!
if ( Faodel_LIBRARIES AND Faodel_INCLUDE_DIRS)
  set(Faodel_FOUND TRUE)
else()
  set(Faodel_FOUND FALSE)
endif()

# --- Provide a summary of what the module found
if ( NOT Faodel_FIND_QUIETLY )

  # Create a not found list
  message(STATUS "\tFaodel_FOUND             = ${Faodel_FOUND}")
  message(STATUS "\tFaodel_INCLUDE_DIR       = ${Faodel_INCLUDE_DIR}")
  message(STATUS "\tFaodel_INCLUDE_DIRS      = ${Faodel_INCLUDE_DIRS}")
  message(STATUS "\tFaodel_LIBRARY_DIR       = ${Faodel_LIBRARY_DIR}")
  message(STATUS "\tFaodel_LIBRARY           = ${Faodel_LIBRARY}")
  message(STATUS "\tFaodel_LIBRARIES         = ${Faodel_LIBRARIES}")

endif()
# For compatability with TriBITS:
set(DOCSTR "List of semi-colon separated paths to look for the TPL Faodel")

set(TPL_Faodel_LIBRARIES ${Faodel_LIBRARIES} CACHE PATH ${DOCSTR})
set(TPL_Faodel_INCLUDE_DIRS ${Faodel_INCLUDE_DIRS} CACHE PATH ${DOCSTR})

mark_as_advanced(
  Faodel_INCLUDE_DIR
  Faodel_INCLUDE_DIRS
  Faodel_LIBRARY
  Faodel_CXX_LIBRARY
  Faodel_LIBRARIES
  Faodel_LIBRARY_DIR
  )
