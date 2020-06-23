# Copyright(C) 1999-2020 National Technology & Engineering Solutions
# of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
# NTESS, the U.S. Government retains certain rights in this software.
#
# See packages/seacas/LICENSE for details

FIND_PACKAGE(BISON REQUIRED)
SET(BisonOutput ${CMAKE_CURRENT_BINARY_DIR}/catalystParser.tab.cpp
                ${CMAKE_CURRENT_BINARY_DIR}/catalystParser.tab.hpp)
IF(BISON_FOUND)
    ADD_CUSTOM_COMMAND(
      OUTPUT ${BisonOutput}
      COMMAND ${BISON_EXECUTABLE}
              -d ${CMAKE_CURRENT_SOURCE_DIR}/catalystParser.ypp
              -p catalyst
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/catalystParser.ypp
      COMMENT "Generating catalystParser.tab.cpp and catalystParser.tab.hpp"
    )
ENDIF()

FIND_PACKAGE(FLEX REQUIRED)
SET(FlexOutput ${CMAKE_CURRENT_BINARY_DIR}/lex.yy.c)
IF(FLEX_FOUND)
    ADD_CUSTOM_COMMAND(
      OUTPUT ${FlexOutput}
      COMMAND ${FLEX_EXECUTABLE}
              --outfile=${FlexOutput}
              ${CMAKE_CURRENT_SOURCE_DIR}/catalystParser.l
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/catalystParser.l
      COMMENT "Generating lex.yy.c"
    )
ENDIF()

SET_SOURCE_FILES_PROPERTIES(${FlexOutput} PROPERTIES GENERATED TRUE)
SET_SOURCE_FILES_PROPERTIES(${BisonOutput} PROPERTIES GENERATED TRUE)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

FIND_PACKAGE(ParaView QUIET)

ADD_LIBRARY(lexer SHARED ${FlexOutput} ${BisonOutput})
TARGET_LINK_LIBRARIES(lexer
    PRIVATE
      VTK::jsoncpp)

ADD_LIBRARY(catalystparser SHARED ${BisonOutput}
              ${CMAKE_CURRENT_SOURCE_DIR}/CatalystParserInterface.cpp)

TARGET_LINK_LIBRARIES(catalystparser
    PRIVATE
      lexer
      VTK::jsoncpp)

ADD_EXECUTABLE(catalyst_parser_driver ${CMAKE_CURRENT_SOURCE_DIR}/driver.cpp)
TARGET_LINK_LIBRARIES(catalyst_parser_driver 
    PRIVATE
      catalystparser
      VTK::jsoncpp)

ADD_EXECUTABLE(catalyst_parser_tester ${CMAKE_CURRENT_SOURCE_DIR}/driver_test.cpp)
TARGET_LINK_LIBRARIES(catalyst_parser_tester
    PRIVATE
      catalystparser
      VTK::jsoncpp)

INSTALL(TARGETS lexer LIBRARY DESTINATION lib)
INSTALL(TARGETS catalystparser LIBRARY DESTINATION lib)
INSTALL(TARGETS catalyst_parser_driver RUNTIME DESTINATION bin)
INSTALL(TARGETS catalyst_parser_tester RUNTIME DESTINATION bin)
