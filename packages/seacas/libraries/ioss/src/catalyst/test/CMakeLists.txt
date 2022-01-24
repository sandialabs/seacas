TRIBITS_ADD_LIBRARY(
	TestDriverMain
SOURCES 
	TestDriverMain.C
DEPLIBS Ionit
)

TRIBITS_ADD_EXECUTABLE(
    BlockMeshTest
 SOURCES 
    BlockMeshTest.C
 DEPLIBS TestDriverMain
)

TRIBITS_ADD_TEST(
	BlockMeshTest
	NAME BlockMeshTest
	NUM_MPI_PROCS 1
)

TRIBITS_ADD_EXECUTABLE(
    BlockMeshSetTest
 SOURCES 
    BlockMeshSetTest.C
 DEPLIBS TestDriverMain
)

TRIBITS_ADD_TEST(
	BlockMeshSetTest
	NAME BlockMeshSetTest
	NUM_MPI_PROCS 1
)