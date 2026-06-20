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

#include <cstdarg> // for va_end, va_arg, va_list, etc
#include <cstddef> // for size_t
#include <cstdio>  // for stderr
#include <cstdlib> // for exit, malloc
#include <fmt/format.h>
#include <fmt/ostream.h>

#include "Ioss_CodeTypes.h" // for Ioss_MPI_Comm
#include "Ioss_DBUsage.h"   // for DatabaseUsage
#include "Ioss_ElementTopology.h"
#include "Ioss_IOFactory.h" // for IOFactory
#include "Ioss_Region.h"    // for IOFactory
#include "Ioss_MeshCopyOptions.h"
#include "Ioss_PropertyManager.h"

#include "IossMesh.h"

namespace utest_util {

  struct EntityProc
  {
    EntityProc(EntityId id_, int proc_) : id(id_), proc(proc_) {}
    EntityProc() : id(0), proc(-1) {}

    EntityId id{0};
    int proc{-1};
  };

  class EntityProcLess
  {
  public:
    EntityProcLess() = default;
    bool operator()(const EntityProc& lhs, const EntityProc& rhs) const
    {
      return lhs.id < rhs.id;
    }

    bool operator()(const EntityProc& lhs, const EntityId& rhs) const
    {
      return lhs.id < rhs;
    }

    bool operator()(const EntityId& lhs, const EntityProc& rhs) const
    {
      return lhs < rhs.id;
    }

    bool operator()(const EntityId& lhs, const EntityId& rhs) const
    {
      return lhs < rhs;
    }
  };

  class EntityProcEqual
  {
  public:
    EntityProcEqual() = default;
    bool operator()(const EntityProc& lhs, const EntityProc& rhs) const
    {
      return lhs.id == rhs.id && lhs.proc == rhs.proc;
    }
  };

  template <typename INT>
  class ElementPartition
  {
  public:
    ElementPartition(IossMesh* mesh, const std::vector<EntityProc>& procAssign, const int nProc);

    void write_nemesis_data(int exoid) const;

  private:
    IossMesh*               m_mesh{nullptr};

    int                     m_numProcs;
    std::vector<EntityProc> m_elemPartition;

    /* Nodal */
    std::vector<std::vector<INT>> int_nodes{};
    std::vector<std::vector<INT>> bor_nodes{};
    std::vector<std::vector<INT>> ext_nodes{};
    std::vector<std::vector<INT>> ext_procs{};

    /* Elemental */
    std::vector<std::vector<std::vector<INT>>> born_procs{};
    std::vector<std::vector<INT>>              int_elems{};
    std::vector<std::vector<INT>>              bor_elems{};
    std::vector<std::vector<INT>>              e_cmap_elems{};
    std::vector<std::vector<INT>>              e_cmap_sides{};
    std::vector<std::vector<INT>>              e_cmap_procs{};
    std::vector<std::vector<INT>>              e_cmap_neigh{};

    /* Define for the maximum number of nodes on an element side/face */
    const int MAX_SIDE_NODES = 9;
    /*
     * Define for the maximum number of sides (and hence communications
     * entries) that an element can have
     */
    const int MAX_ELEM_SIDES = 6;

  private:
    void fill_node_element_connectivity(std::vector<std::vector<INT>> &sur_elem, int &max_nsur);

    int get_element_processor(size_t localId);

    void find_beam_internal_and_border_elements(const std::vector<std::vector<INT>> &sur_elem, const int max_nsur);
    void categorize_elements(const std::vector<std::vector<INT>> &sur_elem, const int max_nsur);
    void categorize_nodes(const std::vector<std::vector<INT>> &sur_elem, const int max_nsur);

    void assign_border_node_processors(const std::vector<std::vector<INT>> &sur_elem, const int max_nsur);
    void order_element_communication_maps();

    void verify_input_partition();
    void generate_partition_maps();
  };
}
