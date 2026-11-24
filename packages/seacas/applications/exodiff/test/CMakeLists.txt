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

TRIBITS_ADD_EXECUTABLE(
  test_generate_nodeset_df
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_generate_nodeset_df.c
  COMM serial mpi
  )

TRIBITS_ADD_EXECUTABLE(
  test_generate_set_mismatch
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_generate_set_mismatch.c
  COMM serial mpi
  )

TRIBITS_ADD_EXECUTABLE(
  test_generate_element_attribute
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_generate_element_attribute.c
  COMM serial mpi
  )

TRIBITS_ADD_EXECUTABLE(
  test_nodeset_read_error
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_nodeset_read_error.C ../node_set.C ../exo_entity.C ../util.C ../stringx.C ../ED_SystemInterface.C ../Tolerance.C
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
 exodiff_test_nodeset_read_error
 TEST_0 CMND /bin/sh ARGS -c "! ${CMAKE_CURRENT_BINARY_DIR}/test_nodeset_read_error > exodiff-nodeset-read-error.stdout 2>&1 && grep -q 'Failed to read node set' exodiff-nodeset-read-error.stdout"
 COMM mpi serial
 OVERALL_NUM_MPI_PROCS 1
 FINAL_PASS_REGULAR_EXPRESSION
 XHOSTTYPE Windows
)

TRIBITS_ADD_ADVANCED_TEST(
 exodiff_test_nodeset_distribution_factors
 TEST_0 EXEC ${CMAKE_CURRENT_BINARY_DIR}/test_generate_nodeset_df
        NOEXEPREFIX NOEXESUFFIX
 TEST_1 CMND /bin/sh ARGS -c "! ${CMAKE_CURRENT_BINARY_DIR}/../exodiff ${CMAKE_CURRENT_BINARY_DIR}/nodeset-df-a.exo ${CMAKE_CURRENT_BINARY_DIR}/nodeset-df-b.exo > exodiff-nodeset-df.stdout 2>&1 && grep -q 'Nodeset Distribution Factors' exodiff-nodeset-df.stdout && grep -q 'diff:' exodiff-nodeset-df.stdout"
 TEST_2 CMND ${CMAKE_CURRENT_BINARY_DIR}/../exodiff
        ARGS -ignore_nodeset_df ${CMAKE_CURRENT_BINARY_DIR}/nodeset-df-a.exo
             ${CMAKE_CURRENT_BINARY_DIR}/nodeset-df-b.exo
            OUTPUT_FILE exodiff-nodeset-df-ignore.stdout
            PASS_REGULAR_EXPRESSION "exodiff: Files are the same"
 COMM mpi serial
 OVERALL_NUM_MPI_PROCS 1
 FINAL_PASS_REGULAR_EXPRESSION
 XHOSTTYPE Windows
)

TRIBITS_ADD_ADVANCED_TEST(
 exodiff_test_set_mismatch_verdicts
 TEST_0 EXEC ${CMAKE_CURRENT_BINARY_DIR}/test_generate_set_mismatch
        NOEXEPREFIX NOEXESUFFIX
 TEST_1 CMND /bin/sh ARGS -c "! ${CMAKE_CURRENT_BINARY_DIR}/../exodiff ${CMAKE_CURRENT_BINARY_DIR}/nodeset-missing-a.exo ${CMAKE_CURRENT_BINARY_DIR}/nodeset-missing-b.exo > exodiff-nodeset-missing.stdout 2>&1 && grep -q 'Nodeset id 100 exists in first file but not the second' exodiff-nodeset-missing.stdout && grep -q 'Differences found in node set metadata' exodiff-nodeset-missing.stdout"
 TEST_2 CMND /bin/sh ARGS -c "! ${CMAKE_CURRENT_BINARY_DIR}/../exodiff ${CMAKE_CURRENT_BINARY_DIR}/sideset-missing-a.exo ${CMAKE_CURRENT_BINARY_DIR}/sideset-missing-b.exo > exodiff-sideset-missing.stdout 2>&1 && grep -q 'Sideset id 200 exists in first file but not the second' exodiff-sideset-missing.stdout && grep -q 'Differences found in side set metadata' exodiff-sideset-missing.stdout"
 TEST_3 CMND /bin/sh ARGS -c "printf 'ELEMENT VARIABLES\\n\\tev\\n' > entity-var.exodiff && ! ${CMAKE_CURRENT_BINARY_DIR}/../exodiff -f entity-var.exodiff ${CMAKE_CURRENT_BINARY_DIR}/entity-var-a.exo ${CMAKE_CURRENT_BINARY_DIR}/entity-var-b.exo > exodiff-entity-var.stdout 2>&1 && grep -q 'DIFFERENCE Element Block variable' exodiff-entity-var.stdout && grep -q 'id 2 in the second file' exodiff-entity-var.stdout && grep -q 'Files are different' exodiff-entity-var.stdout"
 COMM mpi serial
 OVERALL_NUM_MPI_PROCS 1
 FINAL_PASS_REGULAR_EXPRESSION
 XHOSTTYPE Windows
)

TRIBITS_ADD_ADVANCED_TEST(
 exodiff_test_element_attribute_verdicts
 TEST_0 EXEC ${CMAKE_CURRENT_BINARY_DIR}/test_generate_element_attribute
        NOEXEPREFIX NOEXESUFFIX
 TEST_1 CMND /bin/sh ARGS -c "printf 'ELEMENT ATTRIBUTES\\n\\tthick\\n' > element-attr.exodiff && ! ${CMAKE_CURRENT_BINARY_DIR}/../exodiff -f element-attr.exodiff ${CMAKE_CURRENT_BINARY_DIR}/element-attr-a.exo ${CMAKE_CURRENT_BINARY_DIR}/element-attr-b.exo > exodiff-element-attr.stdout 2>&1 && grep -q 'Element attributes' exodiff-element-attr.stdout && grep -q 'thick' exodiff-element-attr.stdout && grep -q 'Files are different' exodiff-element-attr.stdout"
 TEST_2 CMND /bin/sh ARGS -c "printf 'ELEMENT ATTRIBUTES\\n\\tthick\\n' > element-attr.exodiff && ! ${CMAKE_CURRENT_BINARY_DIR}/../exodiff -f element-attr.exodiff ${CMAKE_CURRENT_BINARY_DIR}/element-attr-missing-a.exo ${CMAKE_CURRENT_BINARY_DIR}/element-attr-missing-b.exo > exodiff-element-attr-missing.stdout 2>&1 && grep -q 'element attribute variable' exodiff-element-attr-missing.stdout && grep -q 'first file, but not the second file' exodiff-element-attr-missing.stdout && grep -q 'Files are different' exodiff-element-attr-missing.stdout"
 TEST_3 CMND /bin/sh ARGS -c "printf 'ELEMENT ATTRIBUTES\\n\\tthick\\n' > element-attr.exodiff && ! ${CMAKE_CURRENT_BINARY_DIR}/../exodiff -map -f element-attr.exodiff ${CMAKE_CURRENT_BINARY_DIR}/element-attr-map-a.exo ${CMAKE_CURRENT_BINARY_DIR}/element-attr-map-b.exo > exodiff-element-attr-map.stdout 2>&1 && grep -q 'Element attributes' exodiff-element-attr-map.stdout && grep -q 'thick' exodiff-element-attr-map.stdout && grep -q 'Files are different' exodiff-element-attr-map.stdout"
 COMM mpi serial
 OVERALL_NUM_MPI_PROCS 1
 FINAL_PASS_REGULAR_EXPRESSION
 XHOSTTYPE Windows
)

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
