SET(SOURCES "")

APPEND_GLOB(SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.C)

list(APPEND SOURCES 
     ../EJ_index_sort.C
     ../EJ_mapping.C
     ../EJ_match.C
     ../EJ_SystemInterface.C
     ../EJ_vector3d.C
     ../EJoin.C
)

TRIBITS_ADD_EXECUTABLE(
 Utst_ejoin
 SOURCES ${SOURCES}
)


TRIBITS_ADD_TEST(
        Utst_ejoin
        NAME Utst_ejoin
        NUM_MPI_PROCS 1
)

TRIBITS_INCLUDE_DIRECTORIES(
  "${CMAKE_CURRENT_SOURCE_DIR}"
  "${CMAKE_CURRENT_SOURCE_DIR}/../"
  "${CMAKE_CURRENT_BINARY_DIR}/../"
  "${CMAKE_CURRENT_SOURCE_DIR}/../../utest_util"
)
