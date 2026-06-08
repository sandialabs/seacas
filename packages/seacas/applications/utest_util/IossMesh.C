// Copyright(C) 1999-2020, 2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <string>
#include <vector>

#ifdef SEACAS_HAVE_MPI
#include "mpi.h"
#endif
#include "gtest/gtest.h"

#include "IossMesh.h"

#include "Ioss_Assembly.h"
#include "Ioss_CommSet.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_EntityType.h"     // for EntityType, etc
#include "Ioss_Field.h"          // for Field, etc
#include "Ioss_GroupingEntity.h" // for GroupingEntity
#include "Ioss_IOFactory.h"      // for IOFactory
#include "Ioss_MeshType.h"       // for MeshType, etc
#include "Ioss_NodeBlock.h"
#include "Ioss_NodeSet.h"
#include "Ioss_ParallelUtils.h"

namespace utest_util {

  IossMesh::IossMesh() : m_communicator(Ioss::ParallelUtils::comm_world()) {}

  IossMesh::IossMesh(Ioss_MPI_Comm comm) : m_communicator(comm) {}

  int IossMesh::get_parallel_size() { return Ioss::ParallelUtils(get_comm()).parallel_size(); }

  int IossMesh::get_parallel_rank() { return Ioss::ParallelUtils(get_comm()).parallel_rank(); }

  void IossMesh::fill_mesh(const std::string &meshDesc, const std::string &regionName)
  {
    std::pair<std::string, std::string> result = get_database_type_and_filename(meshDesc);

    std::string type     = result.first;
    std::string filename = result.second;

    create_database(filename, type);
    create_ioss_region(regionName);
  }

  void IossMesh::fill_mesh(const std::string &meshDesc) { fill_mesh(meshDesc, "input_region"); }

  void IossMesh::create_ioss_region(const std::string &regionName)
  {
    if (m_region == nullptr) {
      EXPECT_TRUE(m_database != nullptr);
      m_region = new Ioss::Region(m_database, regionName);
      EXPECT_TRUE(m_region != nullptr);
    }
  }

  void IossMesh::filename_substitution(std::string &filename)
  {
    // See if filename contains "%P" which is replaced by the number of processors...
    // Assumes that %P only occurs once...
    // filename is changed.
    size_t pos = filename.find("%P");
    if (pos != std::string::npos) {
      // Found the characters...  Replace with the processor count...
      int         num_proc = std::max(1, get_parallel_size());
      std::string tmp(filename, 0, pos);
      tmp += std::to_string(num_proc);
      tmp += filename.substr(pos + 2);
      filename = tmp;
    }
  }

  void IossMesh::create_database(const std::string &fileName, const std::string &meshType,
                                 Ioss::DatabaseUsage dbUsage)
  {
    if (m_database == nullptr) {
      std::string meshFileName = fileName;
      filename_substitution(meshFileName);
      m_database =
          Ioss::IOFactory::create(meshType, meshFileName, dbUsage, get_comm(), m_propertyManager);
      EXPECT_TRUE(m_database != nullptr);
      EXPECT_TRUE(m_database->ok(true));
    }
  }

  std::pair<std::string, std::string>
  IossMesh::get_database_type_and_filename(const std::string &meshDesc)
  {
    std::string type;
    std::string filename;

    size_t colon = meshDesc.find(':');
    if (colon != std::string::npos && colon > 0) {
      type     = meshDesc.substr(0, colon);
      filename = meshDesc.substr(colon + 1);
    }
    else {
      type     = "exodusII";
      filename = meshDesc;
    }

    return std::make_pair(type, filename);
  }

} // namespace utest_util
