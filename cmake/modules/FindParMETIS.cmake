# -*- mode: cmake -*-

#
# MSTK ParMETIS Find Module
#
# Usage:
#    Control the search through ParMETIS_DIR or setting environment variable
#    ParMETIS_ROOT to the ParMETIS installation prefix.
#
#    This module does not search default paths! 
#
#    Following variables are set:
#    ParMETIS_FOUND            (BOOL)       Flag indicating if ParMETIS was found
#    ParMETIS_INCLUDE_DIR      (PATH)       Path to the ParMETIS include file
#    ParMETIS_INCLUDE_DIRS     (LIST)       List of all required include files
#    ParMETIS_LIBRARY_DIR      (PATH)       Path to the ParMETIS library
#    ParMETIS_LIBRARY          (FILE)       ParMETIS library
#    ParMETIS_LIBRARIES        (LIST)       List of all required ParMETIS libraries
#
# #############################################################################

# Standard CMake modules see CMAKE_ROOT/Modules
include(FindPackageHandleStandardArgs)

# MSTK CMake functions see <root>/cmake/modules for source
include(AddPackageDependency)

# ParMETIS_DIR is DEPRECATED WARN THE USER if it is set
  if (NOT ParMETIS_ROOT AND ParMETIS_DIR )
    message(WARNING "The configuration parameter ParMETIS_DIR is deprecated."
                    " Please use ParMETIS_ROOT instead to define the ParMETIS installation")
      set(ParMETIS_ROOT ${ParMETIS_DIR})
    endif()

if ( ParMETIS_LIBRARIES AND ParMETIS_INCLUDE_DIRS )

    # Do nothing. Variables are set. No need to search again

else(ParMETIS_LIBRARIES AND ParMETIS_INCLUDE_DIRS)

    # Cache variables
    if(ParMETIS_ROOT)
        set(ParMETIS_ROOT "${ParMETIS_ROOT}" CACHE PATH "Path to search for ParMETIS include and library files")
    endif()

    if(ParMETIS_INCLUDE_DIR)
        set(ParMETIS_INCLUDE_DIR "${ParMETIS_INCLUDE_DIR}" CACHE PATH "Path to search for ParMETIS include files")
    endif()

    if(ParMETIS_LIBRARY_DIR)
        set(ParMETIS_LIBRARY_DIR "${ParMETIS_LIBRARY_DIR}" CACHE PATH "Path to search for ParMETIS library files")
    endif()

    
    # Search for include files
    # Search order preference:
    #  (1) ParMETIS_INCLUDE_DIR - check existence of path AND if the include files exist
    #  (2) ParMETIS_ROOT/<include>
    #  (3) Default CMake paths See cmake --html-help=out.html file for more inforParMETISn.
    #
    set(ParMETIS_inc_names "parmetis.h")
    if (ParMETIS_INCLUDE_DIR)

        if (EXISTS "${ParMETIS_INCLUDE_DIR}")

            find_path(cdf_test_include_path
                      NAMES ${ParMETIS_inc_names}
                      HINTS ${ParMETIS_INCLUDE_DIR}
                      NO_DEFAULT_PATH)
            if(NOT cdf_test_include_path)
                message(SEND_ERROR "Can not locate ${ParMETIS_inc_names} in ${ParMETIS_INCLUDE_DIR}")
            endif()
            set(ParMETIS_INCLUDE_DIR "${cdf_test_include_path}")

        else()
            message(SEND_ERROR "ParMETIS_INCLUDE_DIR=${ParMETIS_INCLUDE_DIR} does not exist")
            set(ParMETIS_INCLUDE_DIR "ParMETIS_INCLUDE_DIR-NOTFOUND")
        endif()

    else() 

	print_var(ParMETIS_ROOT)
	print_var(ParMETIS_inc_names)

        set(ParMETIS_inc_suffixes "include")
        if(ParMETIS_ROOT)

            if (EXISTS "${ParMETIS_ROOT}" )

                find_path(ParMETIS_INCLUDE_DIR
                          NAMES ${ParMETIS_inc_names}
                          HINTS ${ParMETIS_ROOT}/include
                          PATH_SUFFIXES ${ParMETIS_inc_suffixes}
                          NO_DEFAULT_PATH)

            else()
                 message(SEND_ERROR "ParMETIS_ROOT=${ParMETIS_ROOT} does not exist")
                 set(ParMETIS_INCLUDE_DIR "ParMETIS_INCLUDE_DIR-NOTFOUND")
            endif()    


        else()

            find_path(ParMETIS_INCLUDE_DIR
                      NAMES ${ParMETIS_inc_names}
                      PATH_SUFFIXES ${ParMETIS_inc_suffixes})

        endif()

    endif()


    if ( NOT ParMETIS_INCLUDE_DIR )
        message(SEND_ERROR "Can not locate ParMETIS include directory")
    endif()

    # Search for libraries 
    # Search order preference:
    #  (1) ParMETIS_LIBRARY_DIR - check existence of path AND if the include files exist
    #  (2) ParMETIS_ROOT/<lib,Lib>
    #  (3) Default CMake paths See cmake --html-help=out.html file for more inforParMETISn.
    #
    if (ParMETIS_LIBRARY_DIR)

        if (EXISTS "${ParMETIS_LIBRARY_DIR}")

            find_library(ParMETIS_LIBRARY
                         NAMES parmetis
                         HINTS ${ParMETIS_LIBRARY_DIR}
                         NO_DEFAULT_PATH)

        else()
            message(SEND_ERROR "ParMETIS_LIBRARY_DIR=${ParMETIS_LIBRARY_DIR} does not exist")
            set(ParMETIS_LIBRARY "ParMETIS_LIBRARY-NOTFOUND")
        endif()

    else() 

        if(ParMETIS_ROOT)

            if (EXISTS "${ParMETIS_ROOT}" )

                find_library(ParMETIS_LIBRARY
                             NAMES parmetis
                             HINTS ${ParMETIS_ROOT}
                             PATH_SUFFIXES "lib" "Lib"
                             NO_DEFAULT_PATH)

            else()
                 message(SEND_ERROR "ParMETIS_ROOT=${ParMETIS_ROOT} does not exist")
                 set(ParMETIS_LIBRARY "ParMETIS_LIBRARY-NOTFOUND")
            endif()    


        else()

            find_library(ParMETIS_LIBRARY
                         NAMES parmetis
                         PATH_SUFFIXES ${ParMETIS_lib_suffixes})
            
        endif()

    endif()

    if ( NOT ParMETIS_LIBRARY )
        message(SEND_ERROR "Can not locate ParMETIS library")
    endif()    
    
    # Define the LIBRARIES and INCLUDE_DORS
    set(ParMETIS_INCLUDE_DIRS ${ParMETIS_INCLUDE_DIR})
    set(ParMETIS_LIBRARIES    ${ParMETIS_LIBRARY})

endif(ParMETIS_LIBRARIES AND ParMETIS_INCLUDE_DIRS )    

# Send useful message if everything is found
find_package_handle_standard_args(ParMETIS DEFAULT_MSG
                                        ParMETIS_LIBRARIES
                                        ParMETIS_INCLUDE_DIRS)

add_package_dependency(ParMETIS DEPENDS_ON METIS)

# find_package)handle)standard_args should set ParMETIS_FOUND but it does not!
if ( ParMETIS_LIBRARIES AND ParMETIS_INCLUDE_DIRS)
    set(ParMETIS_FOUND TRUE)
else()
    set(ParMETIS_FOUND FALSE)
endif()

# --- Provide a summary of what the module found
if ( NOT ParMETIS_FIND_QUIETLY )

  # Create a not found list
  message(STATUS "\tParMETIS_INCLUDE_DIRS      = ${ParMETIS_INCLUDE_DIRS}")
  message(STATUS "\tParMETIS_LIBRARIES         = ${ParMETIS_LIBRARIES}")

endif()
# For compatability with TriBITS:
SET(DOCSTR "List of semi-colon separated paths to look for the TPL ParMETIS")

set(TPL_ParMETIS_LIBRARIES ${ParMETIS_LIBRARIES} CACHE PATH ${DOCSTR})
set(TPL_ParMETIS_INCLUDE_DIRS ${ParMETIS_INCLUDE_DIRS} CACHE PATH ${DOCSTR})

mark_as_advanced(
  ParMETIS_INCLUDE_DIR
  ParMETIS_INCLUDE_DIRS
  ParMETIS_LIBRARY
  ParMETIS_LIBRARIES
  ParMETIS_LIBRARY_DIR
)
