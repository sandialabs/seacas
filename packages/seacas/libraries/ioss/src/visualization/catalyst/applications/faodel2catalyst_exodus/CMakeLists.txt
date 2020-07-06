
ADD_EXECUTABLE(faodel2catalyst_exodus faodel2catalyst_exodus.cxx)
TARGET_INCLUDE_DIRECTORIES(faodel2catalyst_exodus PUBLIC ${SEACAS_INCLUDE_DIRS})
GET_TARGET_PROPERTY(IOSS_2_CATALYST_SOURCE_DIR ioss2catalyst SOURCE_DIR)
TARGET_INCLUDE_DIRECTORIES(faodel2catalyst_exodus PUBLIC ${IOSS_2_CATALYST_SOURCE_DIR})
TARGET_LINK_LIBRARIES(faodel2catalyst_exodus
    PRIVATE
        ioss2catalyst
        VTK::mpi)
INSTALL(TARGETS faodel2catalyst_exodus RUNTIME DESTINATION bin)
