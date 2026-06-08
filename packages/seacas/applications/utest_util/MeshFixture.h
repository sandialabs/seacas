// Copyright(C) 1999-2020, 2022, 2023, 2024, 2025 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include <gtest/gtest.h>
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
#include <ostream>
#include <vector>

#include "Ioss_CodeTypes.h" // for Ioss_MPI_Comm
#include "Ioss_DBUsage.h"   // for DatabaseUsage
#include "Ioss_IOFactory.h" // for IOFactory

#include "IossMesh.h"

namespace utest_util {

#define ThrowRequireWithMsg(expr, message)                                                         \
  do {                                                                                             \
    if (!(expr)) {                                                                                 \
      std::ostringstream internal_throw_require_oss;                                               \
      internal_throw_require_oss << message;                                                       \
      throw std::logic_error(internal_throw_require_oss.str());                                    \
    }                                                                                              \
  } while (false)

  class MeshFixture : public ::testing::Test
  {
  protected:
    MeshFixture() : m_communicator(Ioss::ParallelUtils::comm_world()), m_spatialDim(3)
    {
      setup_empty_mesh();
    }

    MeshFixture(unsigned spatialDim)
        : m_communicator(Ioss::ParallelUtils::comm_world()), m_spatialDim(spatialDim)
    {
      setup_empty_mesh();
    }

    MeshFixture(unsigned spatialDim, Ioss_MPI_Comm comm)
        : m_communicator(comm), m_spatialDim(spatialDim)
    {
      setup_empty_mesh();
    }

    virtual ~MeshFixture() {}

    void set_spatial_dimension(unsigned spatialDim) { m_spatialDim = spatialDim; }

    void setup_empty_mesh() { allocate_mesh(); }

    virtual void setup_mesh(const std::string &meshSpecification)
    {
      allocate_mesh();
      m_iossMesh->fill_mesh(meshSpecification);
    }

    void reset_mesh() { m_iossMesh.reset(); }

    virtual IossMesh &get_mesh()
    {
      ThrowRequireWithMsg(m_iossMesh != nullptr,
                          "Unit test error. Trying to get mesh before it has been initialized.");
      return *m_iossMesh;
    }

    virtual void allocate_mesh()
    {
      if (nullptr == m_iossMesh) {
        m_iossMesh = std::make_shared<IossMesh>(m_communicator);
      }
    }

    void set_mesh(std::shared_ptr<IossMesh> inMesh)
    {
      ThrowRequireWithMsg(m_iossMesh == nullptr, "Unit test error. Trying to reset non NULL mesh.");
      m_iossMesh = inMesh;
    }

    int get_parallel_size() { return Ioss::ParallelUtils(get_comm()).parallel_size(); }

    int get_parallel_rank() { return Ioss::ParallelUtils(get_comm()).parallel_rank(); }

    Ioss_MPI_Comm get_comm() const { return m_communicator; }

  protected:
    Ioss_MPI_Comm             m_communicator;
    unsigned                  m_spatialDim;
    std::shared_ptr<IossMesh> m_iossMesh;
  };

} // namespace utest_util
