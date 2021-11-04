# Special logic to disable SEACAS subpackages depending on Fortran enabled or not

if (SEACAS_SOURCE_DIR)
  include("${SEACAS_SOURCE_DIR}/cmake/SeacasDisableSubpackagesDependingOnFortran.cmake")
  seacas_disable_subpackages_depending_on_fortran()
endif()
