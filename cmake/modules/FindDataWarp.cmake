# - Try to find libdatawarp
# Once done this will define
#  DATAWARP_FOUND - System has libdatawarp
#  DATAWARP_INCLUDE_DIRS - The libdatawarp include directories
#  DATAWARP_LIBRARIES - The libraries needed to use libdatawarp

FIND_PATH(WITH_DATAWARP_PREFIX
    NAMES include/datawarp.h
)

FIND_LIBRARY(DATAWARP_LIBRARIES
    NAMES datawarp
    HINTS ${WITH_DATAWARP_PREFIX}/lib
)

FIND_PATH(DATAWARP_INCLUDE_DIRS
    NAMES datawarp.h
    HINTS ${WITH_DATAWARP_PREFIX}/include
)

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(DATAWARP DEFAULT_MSG
    DATAWARP_LIBRARIES
    DATAWARP_INCLUDE_DIRS
)

# Hide these vars from ccmake GUI
MARK_AS_ADVANCED(
	DATAWARP_LIBRARIES
	DATAWARP_INCLUDE_DIRS
)