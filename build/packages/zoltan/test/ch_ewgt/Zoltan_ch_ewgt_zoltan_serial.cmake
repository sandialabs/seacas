
#
# This is a CMake script and must be run as "cmake -P <SCRIPT_NAME>"
#
# NOTE: To see what commands this script runs, run it as:
#
#    $ cmake -DSHOW_COMMANDS_ONLY=ON -P <SCRIPT_NAME>
#

#
# Variables
#

SET( TEST_NAME Zoltan_ch_ewgt_zoltan_serial )

SET( TEST_0_CMND "/scratch/gdsjaar/bin/perl" "../ctest_zoltan.pl" "--np" "1" "--debug" "--pkg" "Zoltan" )

SET(PROJECT_NAME SEACASProj)

SET(SEACASProj_TRIBITS_DIR /scratch/gdsjaar/seacas.git/cmake/tribits)

SET(TEST_NAME Zoltan_ch_ewgt_zoltan_serial)

SET(NUM_CMNDS 1)

SET(OVERALL_WORKING_DIRECTORY "")

SET(SKIP_CLEAN_OVERALL_WORKING_DIRECTORY "FALSE")

SET(FAIL_FAST FALSE)

SET(SHOW_START_END_DATE_TIME OFF)

SET(SHOW_MACHINE_LOAD OFF)

SET(PROCESSORS 1)

SET(TIMEOUT )

#
# Test invocation
#

SET(CMAKE_MODULE_PATH /scratch/gdsjaar/seacas.git/cmake/tribits/core/utils)

INCLUDE(DriveAdvancedTest)

DRIVE_ADVANCED_TEST()
