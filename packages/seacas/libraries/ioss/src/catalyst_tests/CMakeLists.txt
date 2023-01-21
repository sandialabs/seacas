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
    Iocatalyst_StructuredBlockTest
 SOURCES
    Iocatalyst_StructuredBlockTest.C
)

TRIBITS_ADD_TEST(
	Iocatalyst_StructuredBlockTest
	NAME Iocatalyst_StructuredBlockTest
	NUM_MPI_PROCS 1
)

TRIBITS_ADD_TEST(
	Iocatalyst_StructuredBlockTest
	NAME Iocatalyst_StructuredBlockTest
	NUM_MPI_PROCS 4
)
