
TRIBITS_SUBPACKAGE(Ioss)

ADD_SUBDIRECTORY(src)

if (${CMAKE_PROJECT_NAME} STREQUAL "SEACASProj")
   # add a target to generate API documentation with Doxygen
   find_package(Doxygen)
   if(DOXYGEN_FOUND)
     add_custom_target(ioss_doc ALL
        ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Generating IOSS API documentation with Doxygen" VERBATIM
        )     
   endif(DOXYGEN_FOUND)
endif()

TRIBITS_SUBPACKAGE_POSTPROCESS()
