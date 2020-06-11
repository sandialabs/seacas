ADD_LIBRARY(ioss2catalyst
    IossRegionReport.cxx
    IossApplication.cxx)
TARGET_INCLUDE_DIRECTORIES(ioss2catalyst PUBLIC ${SEACAS_INCLUDE_DIRS})
TARGET_LINK_LIBRARIES(ioss2catalyst
    PRIVATE
        Ionit
        Ioss
        VTK::mpi)

INSTALL(TARGETS ioss2catalyst LIBRARY DESTINATION lib)
