
TRIBITS_SUBPACKAGE(Exodiff)

INCLUDE_DIRECTORIES(
  "${CMAKE_CURRENT_SOURCE_DIR}"
)

SET(HEADERS "")
APPEND_GLOB(SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.C)

TRIBITS_ADD_EXECUTABLE(
  exodiff
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES ${SOURCES}
  COMM serial mpi
  INSTALLABLE
  )

TRIBITS_ADD_EXECUTABLE(
  test_generate_exo_files
  NOEXEPREFIX
  NOEXESUFFIX
  SOURCES test_generate_exo_files.c
  COMM serial mpi
  )

IF (${CMAKE_PROJECT_NAME}_ENABLE_TESTS)
IF (${CMAKE_PROJECT_NAME}_ENABLE_SEACASExodus)

TRIBITS_ADD_ADVANCED_TEST(
 exodiff_test_edge_face
 TEST_0 EXEC ${CMAKE_CURRENT_BINARY_DIR}/test_generate_exo_files
        NOEXEPREFIX NOEXESUFFIX
 TEST_1 CMND ${CMAKE_CURRENT_BINARY_DIR}/exodiff 
        ARGS ${CMAKE_CURRENT_BINARY_DIR}/edge-face-gold.exo
		         ${CMAKE_CURRENT_BINARY_DIR}/edge-face-no-diff.exo
		    OUTPUT_FILE exodiff-no-diff.stdout
		    PASS_REGULAR_EXPRESSION "exodiff: Files are the same"
 TEST_2 CMND ${CMAKE_CURRENT_BINARY_DIR}/exodiff 
        ARGS ${CMAKE_CURRENT_BINARY_DIR}/edge-face-gold.exo
		         ${CMAKE_CURRENT_BINARY_DIR}/edge-diff.exo
		    OUTPUT_FILE exodiff-edge-diff.stdout
		    PASS_REGULAR_EXPRESSION "exodiff: Files are different"
 TEST_3 CMND ${CMAKE_CURRENT_BINARY_DIR}/exodiff 
        ARGS ${CMAKE_CURRENT_BINARY_DIR}/edge-face-gold.exo
		         ${CMAKE_CURRENT_BINARY_DIR}/face-diff.exo
		    OUTPUT_FILE exodiff-face-diff.stdout
		    PASS_REGULAR_EXPRESSION "exodiff: Files are different"
 COMM mpi serial
 OVERALL_NUM_MPI_PROCS 1
 FINAL_PASS_REGULAR_EXPRESSION
 XHOSTTYPE Windows
)

endif()
endif()

TRIBITS_SUBPACKAGE_POSTPROCESS()
