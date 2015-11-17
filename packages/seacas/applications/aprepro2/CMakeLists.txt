INCLUDE(TribitsSubPackageMacros)
INCLUDE(TribitsLibraryMacros)

TRIBITS_SUBPACKAGE(Aprepro2)

INCLUDE_DIRECTORIES(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
)

SET(SOURCES
	aprepro.C
)

TRIBITS_ADD_EXECUTABLE(
	aprepro2
	NOEXEPREFIX
	NOEXESUFFIX
	SOURCES ${SOURCES}
)

install_executable(aprepro2)

if(${CMAKE_PROJECT_NAME}_ENABLE_TESTS)

TRIBITS_ADD_ADVANCED_TEST(
 aprepro2_unit_test
 TEST_0 CMND ${CMAKE_CURRENT_BINARY_DIR}/aprepro2 
        ARGS -q ${CMAKE_CURRENT_SOURCE_DIR}/test.inp_app test.output
  OUTPUT_FILE test.stderr
  PASS_ANY
 TEST_1 CMND sed ARGS s+${CMAKE_CURRENT_SOURCE_DIR}++ 
		       ${CMAKE_CURRENT_BINARY_DIR}/test.stderr
  OUTPUT_FILE filtered.output
  PASS_ANY
 TEST_2 CMND diff ARGS -w
		       ${CMAKE_CURRENT_SOURCE_DIR}/test_standard.out
		       ${CMAKE_CURRENT_BINARY_DIR}/test.output
 TEST_3 CMND diff ARGS -w
		       ${CMAKE_CURRENT_SOURCE_DIR}/test.stderr.gold
		       ${CMAKE_CURRENT_BINARY_DIR}/filtered.output
 COMM serial		      
 FINAL_PASS_REGULAR_EXPRESSION 
)

TRIBITS_ADD_ADVANCED_TEST(
 aprepro2_array_test
 TEST_0 CMND ${CMAKE_CURRENT_BINARY_DIR}/aprepro2
	ARGS -q --include=${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/test-array.i
	test-array.out
  OUTPUT_FILE test-array.stderr
  PASS_ANY
 TEST_1 CMND diff ARGS -w
		       ${CMAKE_CURRENT_SOURCE_DIR}/test-array.gold
		       ${CMAKE_CURRENT_BINARY_DIR}/test-array.out
 TEST_2 CMND diff ARGS -w
		       ${CMAKE_CURRENT_SOURCE_DIR}/test-array.stderr.gold
		       ${CMAKE_CURRENT_BINARY_DIR}/test-array.stderr
 COMM serial		      
 FINAL_PASS_REGULAR_EXPRESSION 
)

endif()

TRIBITS_SUBPACKAGE_POSTPROCESS()
