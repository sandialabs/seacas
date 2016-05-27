INCLUDE(TribitsSubPackageMacros)
INCLUDE(TribitsLibraryMacros)
INCLUDE(FortranSettings)

TRIBITS_SUBPACKAGE(Exodus_for)

INCLUDE_DIRECTORIES(
   "${CMAKE_CURRENT_SOURCE_DIR}/include"
   "${CMAKE_CURRENT_SOURCE_DIR}/test"
   )

SET_AND_INC_DIRS(DIR ${CMAKE_CURRENT_SOURCE_DIR})
SET(SOURCES ${DIR}/src/exo_jack.c ${DIR}/src/addrwrap.F)

SET(HEADERS include/exodusII.inc )

TRIBITS_ADD_LIBRARY(
  exodus_for
  HEADERS ${HEADERS}
  SOURCES ${SOURCES}
  )

if (${CMAKE_PROJECT_NAME} STREQUAL "SEACASProj")
  if (BUILD_SHARED_LIBS)
    ADD_LIBRARY(
      exodus_for_static
      STATIC
      ${SOURCES}
    )
    set_target_properties(exodus_for_static PROPERTIES OUTPUT_NAME exodus_for)
    INSTALL(TARGETS exodus_for_static DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
  endif()

  # For backwards compatability, the Fortran exodus library was called libexoIIv2for.a
  # Create a symbolic link from libexodus_for.a to libexoIIv2for.a -- do only for static lib
  InstallSymLink(libexodus_for.a ${CMAKE_INSTALL_PREFIX}/lib/libexoIIv2for.a)
endif()

if (${CMAKE_PROJECT_NAME} STREQUAL "SEACASProj")
  TRIBITS_ADD_LIBRARY(
    exoIIv2for32
    HEADERS ${HEADERS}
    SOURCES ${DIR}/src/exo_jack32.c
    )

  if (BUILD_SHARED_LIBS)
    ADD_LIBRARY(
      exoIIv2for32_static
      STATIC
      ${DIR}/src/exo_jack32.c
    )
    set_target_properties(exoIIv2for32_static PROPERTIES OUTPUT_NAME exoIIv2for32)
    INSTALL(TARGETS exoIIv2for32_static DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
  endif()
endif()

TRIBITS_ADD_TEST_DIRECTORIES(test)

TRIBITS_SUBPACKAGE_POSTPROCESS()
