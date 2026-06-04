// Copyright(C) 1999-2020, 2022, 2023, 2024, 2025 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#ifdef SEACAS_HAVE_MPI
#include <mpi.h>
#endif
#include <string>
#include <unordered_map>

#include <memory>
#include <string>
#if defined(_WIN32) && !defined(__MINGW32__)
#include <string.h>
#define strcasecmp  _stricmp
#define strncasecmp _strnicmp
#else
#include <strings.h>
#endif
#include <vector>

namespace utest_util {

std::string unique_filename(const std::string& base, const std::string& extension);

}
