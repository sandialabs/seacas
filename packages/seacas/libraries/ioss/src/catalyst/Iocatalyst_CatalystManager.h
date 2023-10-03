// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#ifndef IOSS_IOVS_CATALYST_MANAGER_H
#define IOSS_IOVS_CATALYST_MANAGER_H

#include "iocatalyst_export.h"
#include <Ioss_ParallelUtils.h>

namespace Iocatalyst {

  class IOCATALYST_EXPORT CatalystManager
  {
  public:
    static CatalystManager &getInstance()
    {
      static CatalystManager instance;
      return instance;
    }

    void writeToCatalystLogFile(const Ioss::ParallelUtils   &putils,
                                const Ioss::PropertyManager &props);

  private:
    CatalystManager();
    ~CatalystManager();
    CatalystManager(const CatalystManager &)            = delete;
    CatalystManager &operator=(const CatalystManager &) = delete;
  };
} // namespace Iocatalyst

#endif