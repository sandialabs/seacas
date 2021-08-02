include(TribitsCMakePolicies)
include(Split)


#
# Read in the <build>/Testing/TAG file contents
#
#
function(tribits_read_ctest_tag_file  TAG_FILE_IN  BUILD_START_TIME_OUT  CDASH_TRACK_OUT)
  file(READ "${TAG_FILE_IN}" TAG_FILE_STR)
  split("${TAG_FILE_STR}" "\n" TAG_FILE_STR_LIST)
  list(GET TAG_FILE_STR_LIST 0 BUILD_START_TIME)
  list(GET TAG_FILE_STR_LIST 1 CDASH_TRACK)
  set(${BUILD_START_TIME_OUT} "${BUILD_START_TIME}" PARENT_SCOPE)
  set(${CDASH_TRACK_OUT} "${CDASH_TRACK}" PARENT_SCOPE)
endfunction()
