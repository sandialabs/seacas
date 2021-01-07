// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#ifndef ZE_SystemInterface_h
#define ZE_SystemInterface_h

#include "GetLongOpt.h"   // for GetLongOption
#include "ZE_CodeTypes.h" // for StringIdVector, Omissions, etc
#include "ZE_vector3d.h"  // for vector3d
#include <climits>        // for INT_MAX
#include <string>         // for string
#include <vector>         // for vector

class SystemInterface
{
public:
  SystemInterface();
  ~SystemInterface();

  bool parse_options(int argc, char **argv);

  int debug() const { return debugLevel_; }

  bool ints64bit() const { return ints64bit_; }
  bool use_netcdf4() const { return useNetcdf4_; }
  int  grid_i() const { return gridI_; }
  int  grid_j() const { return gridJ_; }

  int  compression_level() const { return compressionLevel_; }
  bool zlib() const { return zlib_; }
  bool szip() const { return szip_; }

  static void show_version();

  // Make this private eventually...
  StringVector inputFiles_;
  std::string  outputName_;

private:
  void enroll_options();

  GetLongOption options_; //!< Options parsing

  int  gridI_{0};
  int  gridJ_{0};
  int  debugLevel_{0};
  int  compressionLevel_{0};
  bool ints64bit_{false};
  bool useNetcdf4_{false};
  bool zlib_{true};
  bool szip_{false};
};
#endif
