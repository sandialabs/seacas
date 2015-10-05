INCLUDE(TribitsAddExecutableAndTest)

TRIBITS_ADD_EXECUTABLE(
  Utst_ioel
  SOURCES Utst_ioel.C
)

TRIBITS_ADD_TEST(
  Utst_ioel
  NAME Utst_ioel
  UM_MPI_PROCS 1
)

if(${CMAKE_PROJECT_NAME} STREQUAL "SEACAS")
  SET(ENABLED Exodus)
else()
  SET(ENABLED SEACASExodus)
endif()
ASSERT_DEFINED(${PACKAGE_NAME}_ENABLE_${ENABLED})
IF (${PACKAGE_NAME}_ENABLE_${ENABLED})
  TRIBITS_ADD_EXECUTABLE(
    Utst_superelement
    SOURCES Utst_superelement.C
  )

  TRIBITS_ADD_TEST(
    Utst_superelement
    NAME Utst_superelement
    NUM_MPI_PROCS 1
    COMM serial
    ARGS ${CMAKE_CURRENT_SOURCE_DIR}/cbr2.ncf
  )
ENDIF()

INCLUDE_DIRECTORIES(
  "${CMAKE_CURRENT_SOURCE_DIR}/../"
  "${CMAKE_CURRENT_BINARY_DIR}/../"
)
