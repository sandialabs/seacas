// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#ifndef IOSS_Ioss_CopyDatabase_h
#define IOSS_Ioss_CopyDatabase_h

namespace Ioss {
  class Region;
  class MeshCopyOptions;

  //! Copy the mesh in `region` to `output_region`.  Behavior can be controlled
  //! via options in `options`
  void copy_database(Ioss::Region &region, Ioss::Region &output_region,
                     Ioss::MeshCopyOptions &options);

  //! Same as copy_database, but pays attention to the define_geometry
  //! variable in `options`. used to call multiple times with the same
  //! database at different timestep ranges
  void copy_database_v2(Ioss::Region &region, Ioss::Region &output_region,
			Ioss::MeshCopyOptions &options);

  void transfer_assemblies(Ioss::Region &region, Ioss::Region &output_region,
                           const Ioss::MeshCopyOptions &options, int rank);

  void transfer_blobs(Ioss::Region &region, Ioss::Region &output_region,
                      const Ioss::MeshCopyOptions &options, int rank);

  void transfer_coordinate_frames(Ioss::Region &region, Ioss::Region &output_region);

} // namespace Ioss

#endif
