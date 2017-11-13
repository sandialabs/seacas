# -*- mode: cmake -*-

#
# MSTK Matio Find Module
#
# Usage:
#    Control the search through Matio_DIR or setting environment variable
#    Matio_ROOT to the Matio installation prefix.
#
#    This module does not search default paths! 
#
#    Following variables are set:
#    Matio_FOUND            (BOOL)       Flag indicating if Matio was found
#    Matio_INCLUDE_DIR      (PATH)       Path to the Matio include file
#    Matio_INCLUDE_DIRS     (LIST)       List of all required include files
#    Matio_LIBRARY_DIR      (PATH)       Path to the Matio library
#    Matio_LIBRARY          (FILE)       Matio library
#    Matio_LIBRARIES        (LIST)       List of all required Matio libraries
#
# #############################################################################

# Standard CMake modules see CMAKE_ROOT/Modules
include(FindPackageHandleStandardArgs)

# MSTK CMake functions see <root>/cmake/modules for source
include(AddPackageDependency)

# Matio_DIR is DEPRECATED WARN THE USER if it is set
  if (NOT Matio_ROOT AND Matio_DIR )
    message(WARNING "The configuration parameter Matio_DIR is deprecated."
                    " Please use Matio_ROOT instead to define the MatIO installation")
      set(Matio_ROOT ${Matio_DIR})
    endif()

if ( Matio_LIBRARIES AND Matio_INCLUDE_DIRS )

    # Do nothing. Variables are set. No need to search again

else(Matio_LIBRARIES AND Matio_INCLUDE_DIRS)

    # Cache variables
    if(Matio_ROOT)
        set(Matio_ROOT "${Matio_ROOT}" CACHE PATH "Path to search for Matio include and library files")
    endif()

    if(Matio_INCLUDE_DIR)
        set(Matio_INCLUDE_DIR "${Matio_INCLUDE_DIR}" CACHE PATH "Path to search for Matio include files")
    endif()

    if(Matio_LIBRARY_DIR)
        set(Matio_LIBRARY_DIR "${Matio_LIBRARY_DIR}" CACHE PATH "Path to search for Matio library files")
    endif()

    
    # Search for include files
    # Search order preference:
    #  (1) Matio_INCLUDE_DIR - check existence of path AND if the include files exist
    #  (2) Matio_ROOT/<include>
    #  (3) Default CMake paths See cmake --html-help=out.html file for more information.
    #
    set(matio_inc_names "matio.h")
    if (Matio_INCLUDE_DIR)

        if (EXISTS "${Matio_INCLUDE_DIR}")

            find_path(cdf_test_include_path
                      NAMES ${matio_inc_names}
                      HINTS ${Matio_INCLUDE_DIR}
                      NO_DEFAULT_PATH)
            if(NOT cdf_test_include_path)
                message(SEND_ERROR "Can not locate ${matio_inc_names} in ${Matio_INCLUDE_DIR}")
            endif()
            set(Matio_INCLUDE_DIR "${cdf_test_include_path}")

        else()
            message(SEND_ERROR "Matio_INCLUDE_DIR=${Matio_INCLUDE_DIR} does not exist")
            set(Matio_INCLUDE_DIR "Matio_INCLUDE_DIR-NOTFOUND")
        endif()

    else() 

        set(matio_inc_suffixes "include")
        if(Matio_ROOT)

            if (EXISTS "${Matio_ROOT}" )

                find_path(Matio_INCLUDE_DIR
                          NAMES ${matio_inc_names}
                          HINTS ${Matio_ROOT}/include
                          PATH_SUFFIXES ${matio_inc_suffixes}
                          NO_DEFAULT_PATH)

            else()
                 message(SEND_ERROR "Matio_ROOT=${Matio_ROOT} does not exist")
                 set(Matio_INCLUDE_DIR "Matio_INCLUDE_DIR-NOTFOUND")
            endif()    


        else()

            find_path(Matio_INCLUDE_DIR
                      NAMES ${matio_inc_names}
                      PATH_SUFFIXES ${matio_inc_suffixes})

        endif()

    endif()


    if ( NOT Matio_INCLUDE_DIR )
        message(SEND_ERROR "Can not locate Matio include directory")
    endif()

    # Search for libraries 
    # Search order preference:
    #  (1) Matio_LIBRARY_DIR - check existence of path AND if the include files exist
    #  (2) Matio_ROOT/<lib,Lib>
    #  (3) Default CMake paths See cmake --html-help=out.html file for more information.
    #
    if (Matio_LIBRARY_DIR)

        if (EXISTS "${Matio_LIBRARY_DIR}")

            find_library(Matio_LIBRARY
                         NAMES matio
                         HINTS ${Matio_LIBRARY_DIR}
                         NO_DEFAULT_PATH)

        else()
            message(SEND_ERROR "Matio_LIBRARY_DIR=${Matio_LIBRARY_DIR} does not exist")
            set(Matio_LIBRARY "Matio_LIBRARY-NOTFOUND")
        endif()

    else() 

        if(Matio_ROOT)

            if (EXISTS "${Matio_ROOT}" )

                find_library(Matio_LIBRARY
                             NAMES matio
                             HINTS ${Matio_ROOT}
                             PATH_SUFFIXES "lib" "Lib"
                             NO_DEFAULT_PATH)

            else()
                 message(SEND_ERROR "Matio_ROOT=${Matio_ROOT} does not exist")
                 set(Matio_LIBRARY "Matio_LIBRARY-NOTFOUND")
            endif()    


        else()

            find_library(Matio_LIBRARY
                         NAMES matio
                         PATH_SUFFIXES ${matio_lib_suffixes})
            
        endif()

    endif()

    if ( NOT Matio_LIBRARY )
        message(SEND_ERROR "Can not locate Matio library")
    endif()    
    
    # Define the LIBRARIES and INCLUDE_DORS
    set(Matio_INCLUDE_DIRS ${Matio_INCLUDE_DIR})
    set(Matio_LIBRARIES    ${Matio_CXX_LIBRARY} ${Matio_LIBRARY})

    # Need to find the Matio config script to check for HDF5
    message(STATUS "Assuming that Matio requires HDF5")
    if ( NOT TARGET hdf5)
      add_package_dependency(Matio DEPENDS_ON HDF5)
    endif()

endif(Matio_LIBRARIES AND Matio_INCLUDE_DIRS )    

# Send useful message if everything is found
find_package_handle_standard_args(Matio DEFAULT_MSG
                                        Matio_LIBRARIES
                                        Matio_INCLUDE_DIRS)

# find_package)handle)standard_args should set Matio_FOUND but it does not!
if ( Matio_LIBRARIES AND Matio_INCLUDE_DIRS)
    set(Matio_FOUND TRUE)
else()
    set(Matio_FOUND FALSE)
endif()

# --- Provide a summary of what the module found
if ( NOT Matio_FIND_QUIETLY )

  # Create a not found list
  message(STATUS "\tMatio_INCLUDE_DIRS      = ${Matio_INCLUDE_DIRS}")
  message(STATUS "\tMatio_LIBRARIES         = ${Matio_LIBRARIES}")

endif()
# For compatability with TriBITS:
SET(DOCSTR "List of semi-colon separated paths to look for the TPL Matio")

set(TPL_Matio_LIBRARIES ${Matio_LIBRARIES} CACHE PATH ${DOCSTR})
set(TPL_Matio_INCLUDE_DIRS ${Matio_INCLUDE_DIRS} CACHE PATH ${DOCSTR})

mark_as_advanced(
  Matio_INCLUDE_DIR
  Matio_INCLUDE_DIRS
  Matio_LIBRARY
  Matio_CXX_LIBRARY
  Matio_LIBRARIES
  Matio_LIBRARY_DIR
)
