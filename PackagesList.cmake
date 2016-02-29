#
# Define the SEACASProj packages
#
TRIBITS_REPOSITORY_DEFINE_PACKAGES(
  SEACAS                packages/seacas                   PT # Depends on netcdf, optionally hdf5, xdmf, pamgen
  )

# Allow builds even if some packages are missing

TRIBITS_DISABLE_PACKAGE_ON_PLATFORMS(SEACAS Windows)
