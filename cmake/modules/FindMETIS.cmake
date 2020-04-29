# -*- mode: cmake -*-

#
# MSTK METIS Find Module
#
# Usage:
#    Control the search through METIS_DIR or setting environment variable
#    METIS_ROOT to the METIS installation prefix.
#
#    This module does not search default paths! 
#
#    Following variables are set:
#    METIS_FOUND            (BOOL)       Flag indicating if METIS was found
#    METIS_INCLUDE_DIR      (PATH)       Path to the METIS include file
#    METIS_INCLUDE_DIRS     (LIST)       List of all required include files
#    METIS_LIBRARY_DIR      (PATH)       Path to the METIS library
#    METIS_LIBRARY          (FILE)       METIS library
#    METIS_LIBRARIES        (LIST)       List of all required METIS libraries
#
# #############################################################################

# Standard CMake modules see CMAKE_ROOT/Modules
include(FindPackageHandleStandardArgs)

# MSTK CMake functions see <root>/cmake/modules for source
include(AddPackageDependency)

# METIS_DIR is DEPRECATED WARN THE USER if it is set
  if (NOT METIS_ROOT AND METIS_DIR )
    message(WARNING "The configuration parameter METIS_DIR is deprecated."
                    " Please use METIS_ROOT instead to define the METIS installation")
      set(METIS_ROOT ${METIS_DIR})
    endif()

if ( METIS_LIBRARIES AND METIS_INCLUDE_DIRS )

    # Do nothing. Variables are set. No need to search again

else(METIS_LIBRARIES AND METIS_INCLUDE_DIRS)

    # Cache variables
    if(METIS_ROOT)
        set(METIS_ROOT "${METIS_ROOT}" CACHE PATH "Path to search for METIS include and library files")
    endif()

    if(METIS_INCLUDE_DIR)
        set(METIS_INCLUDE_DIR "${METIS_INCLUDE_DIR}" CACHE PATH "Path to search for METIS include files")
    endif()

    if(METIS_LIBRARY_DIR)
        set(METIS_LIBRARY_DIR "${METIS_LIBRARY_DIR}" CACHE PATH "Path to search for METIS library files")
    endif()

    
    # Search for include files
    # Search order preference:
    #  (1) METIS_INCLUDE_DIR - check existence of path AND if the include files exist
    #  (2) METIS_ROOT/<include>
    #  (3) Default CMake paths See cmake --html-help=out.html file for more inforMETISn.
    #
    set(METIS_inc_names "metis.h")
    if (METIS_INCLUDE_DIR)

        if (EXISTS "${METIS_INCLUDE_DIR}")

            find_path(cdf_test_include_path
                      NAMES ${METIS_inc_names}
                      HINTS ${METIS_INCLUDE_DIR}
                      NO_DEFAULT_PATH)
            if(NOT cdf_test_include_path)
                message(SEND_ERROR "Can not locate ${METIS_inc_names} in ${METIS_INCLUDE_DIR}")
            endif()
            set(METIS_INCLUDE_DIR "${cdf_test_include_path}")

        else()
            message(SEND_ERROR "METIS_INCLUDE_DIR=${METIS_INCLUDE_DIR} does not exist")
            set(METIS_INCLUDE_DIR "METIS_INCLUDE_DIR-NOTFOUND")
        endif()

    else() 

	print_var(METIS_ROOT)
	print_var(METIS_inc_names)

        set(METIS_inc_suffixes "include")
        if(METIS_ROOT)

            if (EXISTS "${METIS_ROOT}" )

                find_path(METIS_INCLUDE_DIR
                          NAMES ${METIS_inc_names}
                          HINTS ${METIS_ROOT}/include
                          PATH_SUFFIXES ${METIS_inc_suffixes}
                          NO_DEFAULT_PATH)

            else()
                 message(SEND_ERROR "METIS_ROOT=${METIS_ROOT} does not exist")
                 set(METIS_INCLUDE_DIR "METIS_INCLUDE_DIR-NOTFOUND")
            endif()    


        else()

            find_path(METIS_INCLUDE_DIR
                      NAMES ${METIS_inc_names}
                      PATH_SUFFIXES ${METIS_inc_suffixes})

        endif()

    endif()


    if ( NOT METIS_INCLUDE_DIR )
        message(SEND_ERROR "Can not locate METIS include directory")
    endif()

    # Search for libraries 
    # Search order preference:
    #  (1) METIS_LIBRARY_DIR - check existence of path AND if the include files exist
    #  (2) METIS_ROOT/<lib,Lib>
    #  (3) Default CMake paths See cmake --html-help=out.html file for more inforMETISn.
    #
    if (METIS_LIBRARY_DIR)

        if (EXISTS "${METIS_LIBRARY_DIR}")

            find_library(METIS_LIBRARY
                         NAMES metis
                         HINTS ${METIS_LIBRARY_DIR}
                         NO_DEFAULT_PATH)

        else()
            message(SEND_ERROR "METIS_LIBRARY_DIR=${METIS_LIBRARY_DIR} does not exist")
            set(METIS_LIBRARY "METIS_LIBRARY-NOTFOUND")
        endif()

    else() 

        if(METIS_ROOT)

            if (EXISTS "${METIS_ROOT}" )

                find_library(METIS_LIBRARY
                             NAMES metis
                             HINTS ${METIS_ROOT}
                             PATH_SUFFIXES "lib" "Lib"
                             NO_DEFAULT_PATH)

            else()
                 message(SEND_ERROR "METIS_ROOT=${METIS_ROOT} does not exist")
                 set(METIS_LIBRARY "METIS_LIBRARY-NOTFOUND")
            endif()    


        else()

            find_library(METIS_LIBRARY
                         NAMES metis
                         PATH_SUFFIXES ${METIS_lib_suffixes})
            
        endif()

    endif()

    if ( NOT METIS_LIBRARY )
        message(SEND_ERROR "Can not locate METIS library")
    endif()    
    
    # Define the LIBRARIES and INCLUDE_DORS
    set(METIS_INCLUDE_DIRS ${METIS_INCLUDE_DIR})
    set(METIS_LIBRARIES    ${METIS_LIBRARY})

endif(METIS_LIBRARIES AND METIS_INCLUDE_DIRS )    

# Send useful message if everything is found
find_package_handle_standard_args(METIS DEFAULT_MSG
                                        METIS_LIBRARIES
                                        METIS_INCLUDE_DIRS)



# find_package)handle)standard_args should set METIS_FOUND but it does not!
if ( METIS_LIBRARIES AND METIS_INCLUDE_DIRS)
    set(METIS_FOUND TRUE)
else()
    set(METIS_FOUND FALSE)
endif()

# --- Provide a summary of what the module found
if ( NOT METIS_FIND_QUIETLY )

  # Create a not found list
  message(STATUS "\tMETIS_INCLUDE_DIRS      = ${METIS_INCLUDE_DIRS}")
  message(STATUS "\tMETIS_LIBRARIES         = ${METIS_LIBRARIES}")

endif()
# For compatability with TriBITS:
SET(DOCSTR "List of semi-colon separated paths to look for the TPL METIS")

set(TPL_METIS_LIBRARIES ${METIS_LIBRARIES} CACHE PATH ${DOCSTR})
set(TPL_METIS_INCLUDE_DIRS ${METIS_INCLUDE_DIRS} CACHE PATH ${DOCSTR})

mark_as_advanced(
  METIS_INCLUDE_DIR
  METIS_INCLUDE_DIRS
  METIS_LIBRARY
  METIS_LIBRARIES
  METIS_LIBRARY_DIR
)
