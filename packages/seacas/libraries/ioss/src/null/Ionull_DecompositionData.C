// Copyright(C) 1999-2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_CodeTypes.h>
#include <null/Ionull_DecompositionData.h>
#if defined SEACAS_HAS_MPI
#include <Ioss_ElementTopology.h> // for ElementTopology
#include <Ioss_Field.h>           // for Field, etc
#include <Ioss_Map.h>             // for Map, MapContainer
#include <Ioss_PropertyManager.h> // for PropertyManager
#include <Ioss_SmartAssert.h>
#include <Ioss_Sort.h>
#include <Ioss_Utils.h>
#include <null/Ionull_Utils.h>

#include <algorithm> // for lower_bound, copy, etc
#include <cassert>   // for assert
#include <climits>   // for INT_MAX
#include <cmath>
#include <cstdlib>   // for exit, EXIT_FAILURE
#include <cstring>
#include <fmt/ostream.h>
#include <iostream> // for operator<<, ostringstream, etc
#include <iterator> // for distance
#include <map>      // for map
#include <numeric>  // for accumulate
#include <utility>  // for pair, make_pair

namespace Ionull {
  template DecompositionData<int>::DecompositionData(const Ioss::PropertyManager &props,
                                                     Ioss_MPI_Comm                communicator);
  template DecompositionData<int64_t>::DecompositionData(const Ioss::PropertyManager &props,
                                                         Ioss_MPI_Comm                communicator);

  template <typename INT>
  DecompositionData<INT>::DecompositionData(const Ioss::PropertyManager &props,
                                            Ioss_MPI_Comm                communicator)
      : DecompositionDataBase(communicator), m_decomposition(props, communicator)
  {
    Ioss::ParallelUtils pu(comm_);
    m_processor      = pu.parallel_rank();
    m_processorCount = pu.parallel_size();
  }

#ifndef DOXYGEN_SKIP_THIS
  template void DecompositionDataBase::communicate_node_data(int *file_data, int *ioss_data,
                                                             size_t comp_count) const;
  template void DecompositionDataBase::communicate_node_data(int64_t *file_data, int64_t *ioss_data,
                                                             size_t comp_count) const;
  template void DecompositionDataBase::communicate_node_data(double *file_data, double *ioss_data,
                                                             size_t comp_count) const;
#endif

  template <typename T>
  void DecompositionDataBase::communicate_node_data(T *file_data, T *ioss_data,
                                                    size_t comp_count) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int> *>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      this32->communicate_node_data(file_data, ioss_data, comp_count);
    }
    else {
      const DecompositionData<int64_t> *this64 =
          dynamic_cast<const DecompositionData<int64_t> *>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      this64->communicate_node_data(file_data, ioss_data, comp_count);
    }
  }

#ifndef DOXYGEN_SKIP_THIS
  template void DecompositionDataBase::communicate_element_data(int *file_data, int *ioss_data,
                                                                size_t comp_count) const;
  template void DecompositionDataBase::communicate_element_data(int64_t *file_data,
                                                                int64_t *ioss_data,
                                                                size_t   comp_count) const;
  template void DecompositionDataBase::communicate_element_data(double *file_data,
                                                                double *ioss_data,
                                                                size_t  comp_count) const;
#endif

  template <typename T>
  void DecompositionDataBase::communicate_element_data(T *file_data, T *ioss_data,
                                                       size_t comp_count) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int> *>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      this32->communicate_element_data(file_data, ioss_data, comp_count);
    }
    else {
      const DecompositionData<int64_t> *this64 =
          dynamic_cast<const DecompositionData<int64_t> *>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      this64->communicate_element_data(file_data, ioss_data, comp_count);
    }
  }

  void DecompositionDataBase::get_node_entity_proc_data(void                     *entity_proc,
                                                        const Ioss::MapContainer &node_map,
                                                        bool                      do_map) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int> *>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      this32->m_decomposition.get_node_entity_proc_data(reinterpret_cast<int *>(entity_proc),
                                                        node_map, do_map);
    }
    else {
      const DecompositionData<int64_t> *this64 =
          dynamic_cast<const DecompositionData<int64_t> *>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      this64->m_decomposition.get_node_entity_proc_data(reinterpret_cast<int64_t *>(entity_proc),
                                                        node_map, do_map);
    }
  }

  int DecompositionDataBase::get_set_mesh_double(int filePtr, ex_entity_type type, ex_entity_id id,
                                                 const Ioss::Field &field, double *ioss_data) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int> *>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      return this32->get_set_mesh_var(filePtr, type, id, field, ioss_data);
    }
    else {
      const DecompositionData<int64_t> *this64 =
          dynamic_cast<const DecompositionData<int64_t> *>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      return this64->get_set_mesh_var(filePtr, type, id, field, ioss_data);
    }
  }

  int DecompositionDataBase::get_set_mesh_var(int filePtr, ex_entity_type type, ex_entity_id id,
                                              const Ioss::Field &field, void *ioss_data) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int> *>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      return this32->get_set_mesh_var(filePtr, type, id, field, (int *)ioss_data);
    }
    else {
      const DecompositionData<int64_t> *this64 =
          dynamic_cast<const DecompositionData<int64_t> *>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      return this64->get_set_mesh_var(filePtr, type, id, field, (int64_t *)ioss_data);
    }
  }

  void DecompositionDataBase::get_block_connectivity(int filePtr, void *data, int64_t id,
                                                     size_t blk_seq, size_t nnpe) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int> *>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      this32->get_block_connectivity(filePtr, reinterpret_cast<int *>(data), id, blk_seq, nnpe);
    }
    else {
      const DecompositionData<int64_t> *this64 =
          dynamic_cast<const DecompositionData<int64_t> *>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      this64->get_block_connectivity(filePtr, reinterpret_cast<int64_t *>(data), id, blk_seq, nnpe);
    }
  }

  const Ioss::SetDecompositionData &DecompositionDataBase::get_decomp_set(ex_entity_type type,
                                                                          ex_entity_id   id) const
  {
    if (type == EX_NODE_SET) {
      for (const auto &node_set : node_sets) {
        if (node_set.id() == id) {
          return node_set;
        }
      }
    }
    else if (type == EX_SIDE_SET) {
      for (const auto &side_set : side_sets) {
        if (side_set.id() == id) {
          return side_set;
        }
      }
    }

    if (type != EX_NODE_SET && type != EX_SIDE_SET) {
      std::ostringstream errmsg;
      fmt::print(errmsg,
                 "ERROR: Invalid set type specified in get_decomp_set. Only node set or side set "
                 "supported\n");
      IOSS_ERROR(errmsg);
    }
    else {
      std::string        typestr = type == EX_NODE_SET ? "node set" : "side set";
      std::ostringstream errmsg;
      fmt::print(errmsg, "ERROR: Count not find {} {}\n", typestr, id);
      IOSS_ERROR(errmsg);
    }
    return node_sets[0];
  }

  template <typename INT>
  size_t DecompositionData<INT>::get_block_seq(ex_entity_type type, ex_entity_id id) const
  {
    m_decomposition.show_progress(__func__);
    if (type == EX_ELEM_BLOCK) {
      for (size_t i = 0; i < el_blocks.size(); i++) {
        if (el_blocks[i].id() == id) {
          return i;
        }
      }
    }
    return el_blocks.size();
  }

  template <typename INT>
  size_t DecompositionData<INT>::get_block_element_count(size_t blk_seq) const
  {
    m_decomposition.show_progress(__func__);
    // Determine number of file decomp elements are in this block;
    size_t bbeg  = std::max(m_decomposition.m_fileBlockIndex[blk_seq], decomp_elem_offset());
    size_t bend  = std::min(m_decomposition.m_fileBlockIndex[blk_seq + 1],
                            decomp_elem_offset() + decomp_elem_count());
    size_t count = 0;
    if (bend > bbeg) {
      count = bend - bbeg;
    }
    return count;
  }

  template <typename INT>
  size_t DecompositionData<INT>::get_block_element_offset(size_t blk_seq) const
  {
    m_decomposition.show_progress(__func__);
    size_t offset = 0;
    if (decomp_elem_offset() > m_decomposition.m_fileBlockIndex[blk_seq]) {
      offset = decomp_elem_offset() - m_decomposition.m_fileBlockIndex[blk_seq];
    }
    return offset;
  }

  template void DecompositionData<int>::create_implicit_global_map(
      const std::vector<int> &owning_proc, std::vector<int64_t> &global_implicit_map,
      Ioss::Map &node_map, int64_t *locally_owned_count, int64_t *processor_offset);
  template void DecompositionData<int64_t>::create_implicit_global_map(
      const std::vector<int> &owning_proc, std::vector<int64_t> &global_implicit_map,
      Ioss::Map &node_map, int64_t *locally_owned_count, int64_t *processor_offset);

  template <typename INT>
  void DecompositionData<INT>::create_implicit_global_map(const std::vector<int> &owning_proc,
                                                          std::vector<int64_t> &global_implicit_map,
                                                          Ioss::Map            &node_map,
                                                          int64_t              *locally_owned_count,
                                                          int64_t              *processor_offset)
  {
    m_decomposition.show_progress(__func__);
    // Used on composed output database...
    // If the node is locally owned, then its position is basically
    // determined by removing all shared nodes from the list and
    // then compressing the list. This location plus the proc_offset
    // gives its location in the global-implicit file.
    //
    // If it is shared, then need to communicate with the owning
    // processor to determine where that processor is putting it.
    //
    // First, iterate nodeOwningProcessor list and set the implicit
    // map for all locally-owned nodes and also determine how many
    // of my nodes are owned by which other processors.

    global_implicit_map.resize(owning_proc.size());

    std::vector<int64_t> snd_count(m_processorCount);
    std::vector<int64_t> rcv_count(m_processorCount);

    size_t position = 0;
    for (size_t i = 0; i < global_implicit_map.size(); i++) {
      snd_count[owning_proc[i]]++;
      if (owning_proc[i] == m_processor) {
        global_implicit_map[i] = position++;
      }
    }
    snd_count[m_processor] = 0;

    // The number of locally-owned nodes on this processor is 'position'
    *locally_owned_count = position;

    MPI_Allgather(locally_owned_count, 1, MPI_LONG_LONG_INT, &rcv_count[0], 1, MPI_LONG_LONG_INT,
                  comm_);
    m_decomposition.show_progress("\tAllgather finished");

    // Determine the offset of the nodes on this processor. The offset is the
    // total number of locally-owned nodes on all processors prior to this processor.
    *processor_offset = 0;
    for (int i = 0; i < m_processor; i++) {
      *processor_offset += rcv_count[i];
    }

    for (auto &i : global_implicit_map) {
      i += *processor_offset + 1;
    }

    // Now, tell the other processors how many nodes I will be sending
    // them (Nodes they own that I share with them)
    MPI_Alltoall(snd_count.data(), 1, MPI_LONG_LONG_INT, rcv_count.data(), 1, MPI_LONG_LONG_INT,
                 comm_);
    m_decomposition.show_progress("\tCommunication 1 finished");

    std::vector<int64_t> snd_offset(snd_count);
    Ioss::Utils::generate_index(snd_offset);
    std::vector<int64_t> snd_list(*snd_offset.rbegin() + *snd_count.rbegin());

    {
      std::vector<int64_t> tmp_disp(snd_offset);
      // Now create the list of nodes to send...
      for (size_t i = 0; i < global_implicit_map.size(); i++) {
        if (owning_proc[i] != m_processor) {
          int64_t global_id                    = node_map.map()[i + 1];
          snd_list[tmp_disp[owning_proc[i]]++] = global_id;
        }
      }
    }

    std::vector<int64_t> rcv_offset(rcv_count);
    Ioss::Utils::generate_index(rcv_offset);
    std::vector<int64_t> rcv_list(*rcv_offset.rbegin() + *rcv_count.rbegin());

    Ioss::MY_Alltoallv(snd_list, snd_count, snd_offset, rcv_list, rcv_count, rcv_offset, comm_);
    m_decomposition.show_progress("\tCommunication 2 finished");

    // Iterate rcv_list and convert global ids to the global-implicit position...
    for (auto &i : rcv_list) {
      int64_t local_id     = node_map.global_to_local(i) - 1;
      int64_t rcv_position = global_implicit_map[local_id];
      i                    = rcv_position;
    }

    // Send the data back now...
    Ioss::MY_Alltoallv(rcv_list, rcv_count, rcv_offset, snd_list, snd_count, snd_offset, comm_);
    m_decomposition.show_progress("\tCommunication 3 finished");

    // Fill in the remaining portions of the global_implicit_map...
    std::vector<int64_t> tmp_disp(snd_offset);
    for (size_t i = 0; i < global_implicit_map.size(); i++) {
      if (owning_proc[i] != m_processor) {
        int64_t implicit       = snd_list[tmp_disp[owning_proc[i]]++];
        global_implicit_map[i] = implicit;
      }
    }
  }
} // namespace Ionull
#else
IOSS_MAYBE_UNUSED const char ioss_exodus_decomposition_data_unused_symbol_dummy = '\0';
#endif
