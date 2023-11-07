TRIBITS_ADD_LIBRARY(
	Iocatalyst_TestDriverMain
HEADERS
        Iocatalyst_BlockMesh.h
        Iocatalyst_BlockMeshSet.h
        Iocatalyst_DatabaseIOTest.h
SOURCES
        Iocatalyst_BlockMesh.C
        Iocatalyst_BlockMeshSet.C
        Iocatalyst_DatabaseIOTest.C
        Iocatalyst_TestDriverMain.C
)

TRIBITS_ADD_EXECUTABLE(
    Iocatalyst_BlockMeshTest
 SOURCES
    Iocatalyst_BlockMeshTest.C
)

TRIBITS_ADD_TEST(
	Iocatalyst_BlockMeshTest
	NAME Iocatalyst_BlockMeshTest
	NUM_MPI_PROCS 1
)

TRIBITS_ADD_EXECUTABLE(
    Iocatalyst_BlockMeshSetTest
 SOURCES
    Iocatalyst_BlockMeshSetTest.C
)

TRIBITS_ADD_TEST(
	Iocatalyst_BlockMeshSetTest
	NAME Iocatalyst_BlockMeshSetTest
	NUM_MPI_PROCS 1
)

TRIBITS_ADD_EXECUTABLE(
    Iocatalyst_LoggingTest
 SOURCES
    Iocatalyst_LoggingTest.C
)

TRIBITS_ADD_TEST(
	Iocatalyst_LoggingTest
	NAME Iocatalyst_LoggingTest
	NUM_MPI_PROCS 1
)

TRIBITS_ADD_EXECUTABLE(
    Iocatalyst_ManagerTest
 SOURCES
    Iocatalyst_ManagerTest.C
)

TRIBITS_ADD_TEST(
	Iocatalyst_ManagerTest
	NAME Iocatalyst_ManagerTest
	NUM_MPI_PROCS 1
)

TRIBITS_ADD_EXECUTABLE(
    Iocatalyst_StructuredBlockTest
 SOURCES
    Iocatalyst_StructuredBlockTest.C
)

TRIBITS_ADD_EXECUTABLE(
    Iocatalyst_ElementBlockTest
 SOURCES
    Iocatalyst_ElementBlockTest.C
)

SET(CATALYST_IOSS_ENTITY_TESTS
    Iocatalyst_StructuredBlockTest
    Iocatalyst_ElementBlockTest
)

FOREACH(CATALYST_TEST ${CATALYST_IOSS_ENTITY_TESTS})
    FOREACH(MPI_RANKS 1 4)
        TRIBITS_ADD_TEST(
	        ${CATALYST_TEST}
	        NAME ${CATALYST_TEST}
	        NUM_MPI_PROCS ${MPI_RANKS}
        )
    ENDFOREACH()
ENDFOREACH()

function(catalyst_test_ioshell_generated test_name
                                         ioshell_gen_args
                                         ioshell_output_file_name
                                         test_time
                                         diff_command
                                         diff_args)

set(CATALYST_FNAME catalyst_time_${test_time}_${ioshell_output_file_name})
set(IOSHELL_FNAME ioshell_time_${test_time}_${ioshell_output_file_name})

TRIBITS_ADD_ADVANCED_TEST(${test_name}
   TEST_0 EXEC io_shell ARGS --in_type generated ${ioshell_gen_args} ${ioshell_output_file_name}
     DIRECTORY ../main
     NOEXEPREFIX NOEXESUFFIX
     NUM_MPI_PROCS 1
   TEST_1 EXEC io_shell ARGS -out_type catalyst ${ioshell_output_file_name} ${test_time}
     DIRECTORY ../main
     NOEXEPREFIX NOEXESUFFIX
     NUM_MPI_PROCS 1
   TEST_2 EXEC io_shell ARGS -in_type catalyst ${test_time} ${CATALYST_FNAME}
     DIRECTORY ../main
     NOEXEPREFIX NOEXESUFFIX
     NUM_MPI_PROCS 1
   TEST_3 EXEC io_shell ARGS -select_times ${test_time} ${ioshell_output_file_name}
         ${IOSHELL_FNAME}
     DIRECTORY ../main
     NOEXEPREFIX NOEXESUFFIX
     NUM_MPI_PROCS 1
   TEST_4 EXEC ${diff_command} ARGS ${diff_args} ${IOSHELL_FNAME} ${CATALYST_FNAME}
     DIRECTORY ../../../../applications/exodiff
     NOEXEPREFIX NOEXESUFFIX
     NUM_MPI_PROCS 1
   ENVIRONMENT CATALYST_DATA_DUMP_DIRECTORY=${CMAKE_CURRENT_BINARY_DIR}/${test_name}
  )
endfunction()

catalyst_test_ioshell_generated("catalyst_ioshell_10x10x10"
    "10x10x10+times:4+variables:element,2,nodal,3"
    "ioshell_10x10x10.g" "3" "exodiff" "-pedantic")

catalyst_test_ioshell_generated("catalyst_ioshell_10x10x10_tets"
    "10x10x10+tets:+times:2+variables:element,2,nodal,3"
    "ioshell_10x10x10_tets.g" "1" "exodiff" "-pedantic")

catalyst_test_ioshell_generated("catalyst_ioshell_10x10x10_pyramids"
    "10x10x10+pyramids:+times:2+variables:element,2,nodal,3"
    "ioshell_10x10x10_pyramids.g" "1" "exodiff" "-pedantic")

catalyst_test_ioshell_generated("catalyst_ioshell_10x10x10_shell"
    "10x10x10+shell:xX:+times:2+variables:element,2,nodal,3"
    "ioshell_10x10x10_shell.g" "1" "exodiff" "-pedantic")

catalyst_test_ioshell_generated("catalyst_ioshell_10x10x10_nodeset"
    "10x10x10+nodeset:xX:+times:2+variables:element,2,nodal,3,nodeset,4"
    "ioshell_10x10x10_nodeset.g" "1" "exodiff" "-pedantic")

catalyst_test_ioshell_generated("catalyst_ioshell_10x10x10_sideset"
    "10x10x10+sideset:xX:+times:2+variables:element,2,nodal,3,sideset,4"
    "ioshell_10x10x10_sideset.g" "1" "exodiff" "-pedantic")
