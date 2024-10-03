#
# Define the list of TPLs, their find module names, and their classification
#
# TPL_NAME:
#
#   The name of the TPL used in the CMake cache variables TPL_ENABLE_${TPL_NAME}
#
# TPL_FINDMOD:
#
#   The name of the find module under that is used to get the names of the
#   TPLs.  If ends in '/' then this gives the directory and the standard module
#   name will be used which is FindTPL${TPL_NAME}.cmake.
#
# TPL_CLASSIFICATION:
#
#   PS: Primary Stable TPL
#
#     Primary Stable TPLs are those TPLs that a Trilinos developer must have
#     installed on their machine in order to be able to do Trilinos
#     development.  For example, we require that you have BLAS, LAPACK, and
#     MPI installed in order to do Trilinos development.  These are
#     fundamental dependencies that are needed in order to do precheckin
#     testing.
#
#   SS: Secondary Stable TPL
#
#     Secondary Stable TPLs are those TPLs that are not required in order to
#     be able to develop and test Trilinos before checkins but are none the
#     less officially supported.  Support for SS TPLs is tested as part of the
#     nightly testing process.
#
#   TS: Tertiary Stable TPL
#
#     Tertiary Stable TPLs are those TPLs that are supported TPLs but can not
#     be included in the set of SS TPLs because they may conflicit with other
#     SS Code.  For example, METIS is listed as a TS TPL because it conflicts
#     with ParMETIS which is declared as a SS TPL.
#
#   EX: Experimental TPL
#
#     Experimental TPLs are not officially supported.  They represent
#     experimental capabilities of Trilinos packages.  Support for EX TPLs is
#     never tested as part of the main nightly testing process.  However,
#     package developers are encouraged to set up their own nightly testing
#     for their EX TPLs for their packages.
#
# The default enable for all TPLs is empty "" regardless of the category.
# The idea is that the enabling of the TPL will be done by the package and
# other enables that the user has to set.
#
# NOTE: The TPLs must be listed in the order of increasing dependencies (if
# such dependencies exist).
#

SET( Seacas_TPLS_FINDMODS_CLASSIFICATIONS
  X11             "cmake/TPLs/"    SS
  DLlib           "cmake/TPLs/"    SS
  fmt             "cmake/TPLs/"    PS
  Catch2          "cmake/TPLs/"    SS
  GTest           "cmake/TPLs/"    SS
  Zlib            "cmake/TPLs/"    SS
  Pthread         "cmake/TPLs/"    PS
  MPI             "${${PROJECT_NAME}_TRIBITS_DIR}/core/std_tpls/" PS
  HDF5            "${${PROJECT_NAME}_TRIBITS_DIR}/common_tpls/"  SS
  Pnetcdf         "${${PROJECT_NAME}_TRIBITS_DIR}/common_tpls/"  SS
  Netcdf          "${${PROJECT_NAME}_TRIBITS_DIR}/common_tpls/"  PS
  CGNS            "${${PROJECT_NAME}_TRIBITS_DIR}/common_tpls/"  PS
  DataWarp        "cmake/TPLs/"    SS
  METIS           "cmake/TPLs/"    SS
  ParMETIS        "cmake/TPLs/"    SS
  Pamgen          "TRIBITS_PKG"    SS
  Matio           "cmake/TPLs/"    SS
  CUDA            "cmake/TPLs/"    SS
  Kokkos          "${${PROJECT_NAME}_TRIBITS_DIR}/common_tpls/"  SS
  Faodel          "${PROJECT_SOURCE_DIR}/packages/seacas/cmake/tpls/"    SS
  Cereal          "${PROJECT_SOURCE_DIR}/packages/seacas/cmake/tpls/"    SS
  ADIOS2          "${${PROJECT_NAME}_TRIBITS_DIR}/common_tpls/"  EX
  Catalyst2       "${PROJECT_SOURCE_DIR}/packages/seacas/cmake/tpls/FindTPLCatalyst2.cmake" EX
  # TPLs defined in Zoltan/cmake/Dependencies.cmake that are never enabled
  PaToH           "cmake/TPLs/"    EX
  Scotch          "cmake/TPLs/"    EX
  CCOLAMD         "cmake/TPLs/"    EX
  OVIS            "cmake/TPLs/"    EX
  )
