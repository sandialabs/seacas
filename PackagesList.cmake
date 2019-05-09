#
# Define the SEACASProj packages
#
TRIBITS_REPOSITORY_DEFINE_PACKAGES(
  Zoltan                packages/zoltan                   PT
  SEACAS                packages/seacas                   PT # Depends on netcdf, optionally hdf5, pamgen
  )

# Allow builds even if some packages are missing

TRIBITS_DISABLE_PACKAGE_ON_PLATFORMS(SEACAS Windows)
TRIBITS_DISABLE_PACKAGE_ON_PLATFORMS(Zoltan Windows)
