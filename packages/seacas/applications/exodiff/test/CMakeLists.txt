TRIBITS_ADD_EXECUTABLE(
  test_generate_len_name32
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_generate_len_name32.c
  COMM serial mpi
  )

TRIBITS_ADD_EXECUTABLE(
  test_tolerance_nonfinite
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_tolerance_nonfinite.C ../Tolerance.C
  COMM serial mpi
  )

TRIBITS_ADD_EXECUTABLE(
  test_generate_time_floor
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_generate_time_floor.c
  COMM serial mpi
  )

TRIBITS_ADD_EXECUTABLE(
  test_generate_permuted_window
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_generate_permuted_window.c
  COMM serial mpi
  )

TRIBITS_ADD_ADVANCED_TEST(
  exodiff_test_tolerance_nonfinite
  TEST_0 CMND ${CMAKE_CURRENT_BINARY_DIR}/test_tolerance_nonfinite
  COMM serial mpi
  OVERALL_NUM_MPI_PROCS 1
  FINAL_PASS_REGULAR_EXPRESSION
  XHOSTTYPE Windows
  )

IF (${CMAKE_PROJECT_NAME}_ENABLE_SEACASExodus)

TRIBITS_ADD_ADVANCED_TEST(
 exodiff_test_permuted_connectivity_window
 TEST_0 EXEC ${CMAKE_CURRENT_BINARY_DIR}/test_generate_permuted_window
        NOEXEPREFIX NOEXESUFFIX
 TEST_1 CMND /bin/sh ARGS -c "! ${CMAKE_CURRENT_BINARY_DIR}/../exodiff -allow_permuted_connectivity ${CMAKE_CURRENT_BINARY_DIR}/permuted-window-a.exo ${CMAKE_CURRENT_BINARY_DIR}/permuted-window-b.exo > exodiff-permuted-window.stdout 2>&1 && grep -q 'Connectivities in block id' exodiff-permuted-window.stdout && grep -q 'Differences found in mesh' exodiff-permuted-window.stdout"
 COMM mpi serial
 OVERALL_NUM_MPI_PROCS 1
 FINAL_PASS_REGULAR_EXPRESSION
 XHOSTTYPE Windows
)

TRIBITS_ADD_ADVANCED_TEST(
 exodiff_test_time_floor_default
 TEST_0 EXEC ${CMAKE_CURRENT_BINARY_DIR}/test_generate_time_floor
        NOEXEPREFIX NOEXESUFFIX
 TEST_1 CMND ${CMAKE_CURRENT_BINARY_DIR}/../exodiff
        ARGS ${CMAKE_CURRENT_BINARY_DIR}/time-floor-a.exo
             ${CMAKE_CURRENT_BINARY_DIR}/time-floor-b.exo
            OUTPUT_FILE exodiff-time-floor.stdout
            PASS_REGULAR_EXPRESSION "exodiff: Files are the same"
 TEST_2 CMND ${CMAKE_CURRENT_BINARY_DIR}/../exodiff
        ARGS -summary ${CMAKE_CURRENT_BINARY_DIR}/time-floor-a.exo
             OUTPUT_FILE exodiff-time-floor-summary.stdout
             PASS_REGULAR_EXPRESSION "TIME STEPS relative 1.e-6 floor 1.e-15"
 COMM mpi serial
 OVERALL_NUM_MPI_PROCS 1
 FINAL_PASS_REGULAR_EXPRESSION
 XHOSTTYPE Windows
)

TRIBITS_ADD_ADVANCED_TEST(
 exodiff_test_len_name_32
 TEST_0 EXEC ${CMAKE_CURRENT_BINARY_DIR}/test_generate_len_name32
        NOEXEPREFIX NOEXESUFFIX
 TEST_1 CMND ${CMAKE_CURRENT_BINARY_DIR}/../exodiff
        ARGS -f ${CMAKE_CURRENT_SOURCE_DIR}/len-name-32.exodiff
		         ${CMAKE_CURRENT_BINARY_DIR}/len-name-32-a.exo
		         ${CMAKE_CURRENT_BINARY_DIR}/len-name-32-b.exo
		    OUTPUT_FILE exodiff-len-name-32.stdout
		    PASS_REGULAR_EXPRESSION "exodiff: Files are the same"
 TEST_2 CMND ${CMAKE_CURRENT_BINARY_DIR}/../exodiff
        ARGS -f ${CMAKE_CURRENT_SOURCE_DIR}/len-name-32.exodiff
		         ${CMAKE_CURRENT_BINARY_DIR}/len-name-32-a.exo
		         ${CMAKE_CURRENT_BINARY_DIR}/len-name-32-b.exo
		         ${CMAKE_CURRENT_BINARY_DIR}/len-name-32-diff.exo
		    OUTPUT_FILE exodiff-len-name-32-diff.stdout
		    FAIL_REGULAR_EXPRESSION "is too long"
 COMM mpi serial
 OVERALL_NUM_MPI_PROCS 1
 FINAL_PASS_REGULAR_EXPRESSION
 XHOSTTYPE Windows
)

endif()
