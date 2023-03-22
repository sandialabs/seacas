// Copyright(C) 2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <null/Ionull_DatabaseIO.h>
#include <null/Ionull_IOFactory.h>

#if defined(SEACAS_HAVE_MPI)
#include <null/Ionull_ParallelDatabaseIO.h>
#endif
#include <tokenize.h>

#include <cstddef>          // for nullptr
#include <fmt/ostream.h>
#include <string>           // for string

#include "Ioss_CodeTypes.h" // for Ioss_MPI_Comm
#include "Ioss_DBUsage.h"   // for DatabaseUsage
#include "Ioss_IOFactory.h" // for IOFactory

namespace Ioss {
  class DatabaseIO;
} // namespace Ioss

#if defined(SEACAS_HAVE_MPI)
namespace {
  std::string check_decomposition_property(const Ioss::PropertyManager &properties,
                                           Ioss::DatabaseUsage          db_usage);
  bool        check_composition_property(const Ioss::PropertyManager &properties,
                                         Ioss::DatabaseUsage          db_usage);
} // namespace
#endif

namespace Ionull {

  const IOFactory *IOFactory::factory()
  {
    static IOFactory registerThis;
    return &registerThis;
  }

  IOFactory::IOFactory() : Ioss::IOFactory("null") {}

  Ioss::DatabaseIO *IOFactory::make_IO(const std::string &filename, Ioss::DatabaseUsage db_usage,
                                       Ioss_MPI_Comm                communicator,
                                       const Ioss::PropertyManager &properties) const
  {
#if defined(SEACAS_HAVE_MPI)
    Ioss::ParallelUtils pu(communicator);
    int                 proc_count = pu.parallel_size();

    bool decompose = false;
    if (proc_count > 1) {
      if (db_usage == Ioss::WRITE_RESULTS || db_usage == Ioss::WRITE_RESTART) {
        if (check_composition_property(properties, db_usage)) {
          decompose = true;
        }
      }
    }

    // Could call Ionull::ParallelDatabaseIO constructor directly, but that leads to some circular
    // dependencies and other yuks.
#if defined(SEACAS_HAVE_MPI)
    if (decompose)
      return new Ionull::ParallelDatabaseIO(nullptr, filename, db_usage, communicator, properties);
    else
#endif
      return new Ionull::DatabaseIO(nullptr, filename, db_usage, communicator, properties);
  }
} // namespace Ionull

#if defined(SEACAS_HAVE_MPI)
namespace {
  bool check_composition_property(const Ioss::PropertyManager &properties,
                                  Ioss::DatabaseUsage          db_usage)
  {
    bool        compose          = false;
    std::string compose_property = "COMPOSE_INVALID";
    if (db_usage == Ioss::WRITE_RESULTS) {
      compose_property = "COMPOSE_RESULTS";
    }
    else if (db_usage == Ioss::WRITE_RESTART) {
      compose_property = "COMPOSE_RESTART";
    }

    Ioss::Utils::check_set_bool_property(properties, compose_property, compose);
    return compose;
  }
} // namespace
#endif
