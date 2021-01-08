// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#ifndef ZE_CodeTypes_H
#define ZE_CodeTypes_H

#include <Ioss_Region.h>
#include <map>
#include <string>
#include <utility>
#include <vector>

#if defined(_MSC_VER)
#ifdef _WIN64
#define ssize_t __int64
#else
#define ssize_t long
#endif
#endif

using RegionMap    = std::map<std::string, std::shared_ptr<Ioss::Region>>;
using StringVector = std::vector<std::string>;
#endif
