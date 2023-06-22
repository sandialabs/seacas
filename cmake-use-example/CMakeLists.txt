# Almost all CMake files should start with this
# You should always specify a range with the newest
# and oldest tested versions of CMake. This will ensure
# you pick up the best policies.
cmake_minimum_required(VERSION 3.1...3.26)
project(ExodusCMakeExample VERSION 1.0 LANGUAGES C Fortran)

###
### Generate Makefile with:   
#  * mkdir build; cd build
#  * CMAKE_PREFIX_PATH={path_to_root_of_seacas_install} ccmake ..

#### C ####
find_package(SEACASExodus CONFIG)
add_executable(ExodusWriteC ExodusWrite.c)
target_link_libraries(ExodusWriteC PRIVATE SEACASExodus::all_libs)


#### FORTRAN #####
IF ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcray-pointer -fdefault-real-8 -fdefault-integer-8 -fno-range-check")
ELSEIF ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "XL")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qintsize=8 -qrealsize=8")
ELSEIF ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -sdefault64")
ELSE()
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -i8")
ENDIF()

find_package(SEACASExodus_for CONFIG)
add_executable(ExodusReadFor ExodusRead.f)
target_link_libraries(ExodusReadFor PRIVATE SEACASExodus_for::all_libs)
