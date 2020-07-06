
ADD_EXECUTABLE(faodel2catalyst_cgns faodel2catalyst_cgns.cxx)
TARGET_INCLUDE_DIRECTORIES(faodel2catalyst_cgns PUBLIC ${SEACAS_INCLUDE_DIRS})
GET_TARGET_PROPERTY(IOSS_2_CATALYST_SOURCE_DIR ioss2catalyst SOURCE_DIR)
TARGET_INCLUDE_DIRECTORIES(faodel2catalyst_cgns PUBLIC ${IOSS_2_CATALYST_SOURCE_DIR})
TARGET_LINK_LIBRARIES(faodel2catalyst_cgns
    PRIVATE
        ioss2catalyst
        VTK::mpi)
INSTALL(TARGETS faodel2catalyst_cgns RUNTIME DESTINATION bin)
