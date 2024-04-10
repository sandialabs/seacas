cmake_minimum_required(VERSION 3.1...3.26)
project(IossCMakeExample VERSION 1.0 LANGUAGES CXX)

#### C++ IOSS ####
find_package(SEACASIoss CONFIG)
add_executable(IossExample IossExample.C)
target_link_libraries(IossExample PRIVATE SEACASIoss::all_libs)
