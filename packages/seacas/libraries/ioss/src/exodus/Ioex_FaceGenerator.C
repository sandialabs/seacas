// Copyright(C) 2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_CodeTypes.h>
#include <Ioex_FaceGenerator.h>

#include <Ioss_CommSet.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_ElementTopology.h>
#include <Ioss_FaceGeneratorUtils.h>
#include <Ioss_Map.h>
#include <Ioss_ParallelUtils.h>

#include <algorithm>
#include <chrono>
#include <exodusII.h>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <functional>
#include <numeric>
#include <random>
#include <utility>

namespace {
  template <typename INT>
    std::vector<std::pair<INT, INT>> generate_node_communication(int exoid, INT /* dummy */)
    {
      // This node communication map needs to come from the Decomposition -- it doesn't
      // exist on the database since we are inside auto-decomp...
#if 0
        // entity_processor consists of node,proc, node,proc, entries.
        std::vector<INT> entity_processor;
        css->get_field_data("entity_processor_raw", entity_processor);

        proc_entity.reserve(entity_processor.size() / 2);
        for (size_t i = 0; i < entity_processor.size(); i += 2) {
          // Converts from 1-based to 0-based local nodes.
          proc_entity.push_back(std::make_pair(entity_processor[i + 1], entity_processor[i] - 1));
        }
#endif
      std::vector<std::pair<INT, INT>> proc_entity;
      return proc_entity;
    }

  template <typename INT>
  void internal_generate_faces(int exoid, ex_entity_id eb_id, Ioss::FaceUnorderedSet &faces,
                               const std::vector<INT> &ids, const std::vector<size_t> &hash_ids,
                               bool local_ids, INT /*dummy*/)
  {
    // Get the element block parameters...
    ex_block block_param;
    block_param.id   = eb_id;
    block_param.type = EX_ELEM_BLOCK;
    ex_get_block_param(exoid, &block_param);

    // Only handle continuum elements at this time...
    auto topology =
        Ioss::Utils::fixup_type(block_param.topology, block_param.num_nodes_per_entry, 3);
    const Ioss::ElementTopology *topo = Ioss::ElementTopology::factory(topology);
    if (topo->parametric_dimension() != 3) {
      return;
    }

    std::vector<INT> connectivity(block_param.num_entry * block_param.num_nodes_per_entry);
    ex_get_conn(exoid, EX_ELEM_BLOCK, eb_id, connectivity.data(), nullptr, nullptr);

    std::vector<INT> elem_ids(block_param.num_entry);

    // TODO("element block offset");
    int64_t eb_offset = 0;
    if (local_ids) {
      std::iota(elem_ids.begin(), elem_ids.end(), static_cast<INT>(eb_offset + 1));
    }
    else {
      ex_get_partial_id_map(exoid, EX_ELEM_MAP, eb_offset + 1, block_param.num_entry,
                            elem_ids.data());
    }

    int num_face_per_elem = topo->number_faces();
    assert(num_face_per_elem <= 6);
    std::array<Ioss::IntVector, 6> face_conn;
    std::array<int, 6>             face_node_count{};
    for (int face = 0; face < num_face_per_elem; face++) {
      face_conn[face]       = topo->face_connectivity(face + 1);
      face_node_count[face] = topo->face_type(face + 1)->number_corner_nodes();
    }

    int    num_node_per_elem = topo->number_nodes();
    size_t num_elem          = block_param.num_entry;

    for (size_t elem = 0, offset = 0; elem < num_elem; elem++, offset += num_node_per_elem) {
      for (int face = 0; face < num_face_per_elem; face++) {
        size_t id = 0;
        assert(face_node_count[face] <= 4);
        std::array<size_t, 4> conn = {{0, 0, 0, 0}};
        for (int j = 0; j < face_node_count[face]; j++) {
          size_t fnode = offset + face_conn[face][j];
          size_t lnode = connectivity[fnode]; // local since "connectivity_raw"
          conn[j]      = ids[lnode - 1];      // Convert to global
          id += hash_ids[lnode - 1];
        }
        create_face(faces, id, conn, elem_ids[elem], face);
      }
    }
  }

  template <typename INT>
    void resolve_parallel_faces(int exoid, MPI_Comm communicator, const Ioss::FaceUnorderedSet &faces,
                              const std::vector<size_t> &hash_ids, INT /*dummy*/)
  {
    PAR_UNUSED(exoid);
    PAR_UNUSED(communicator);
    PAR_UNUSED(faces);
    PAR_UNUSED(hash_ids);

#ifdef SEACAS_HAVE_MPI
    Ioss::ParallelUtils pu(communicator);
    
    auto proc_count = pu.parallel_size();
    auto my_proc = pu.parallel_rank();

    if (proc_count > 1) {
      // If parallel, resolve faces on processor boundaries.
      // For each boundary face, need to check whether all of the nodes on
      // the face are shared with the same processor.  If so, then that face
      // is *possibly* shared with that processor.
      //
      // With the current continuum element only restriction, then a face
      // can only be shared with one other processor...

      // get nodal communication data CommSet...

      // This needs to be passed in as a vector...
      // If inside auto-decomp, then there is no commset, but we do have a distribution of 
      // nodes to processors based on the linear decomposition...
      
      std::vector<std::pair<INT, INT>> proc_entity = generate_node_communication(exoid, INT(0));
      Ioss::Map node_map("nodemap", "unknown", 0);
      internal_resolve_parallel_faces(node_map, faces, hash_ids, proc_entity, communicator,  proc_count, INT(0));
    }
#endif
  }
}

namespace Ioex {
  // RAW EXODUS VERSION...
  template <typename INT>
    void FaceGenerator::generate_faces(MPI_Comm communicator, INT /*dummy*/, bool block_by_block, bool use_local_ids)
  {
    // Convert ids into hashed-ids
    auto node_count = ex_inquire_int(exodusFilePtr_, EX_INQ_NODES);
    std::vector<INT> ids(node_count);
    if (use_local_ids) {
      std::iota(ids.begin(), ids.end(), 1);
    }
    else {
      ex_get_id_map(exodusFilePtr_, EX_NODE_MAP, ids.data());
    }
    hash_node_ids(ids);

    auto blk_cnt = ex_inquire_int(exodusFilePtr_, EX_INQ_ELEM_BLK);
    std::vector<ex_entity_id> blk_ids(blk_cnt);
    ex_get_ids(exodusFilePtr_, EX_ELEM_BLOCK, blk_ids.data());
    std::vector<ex_block> ebs(blk_cnt);
    for (size_t i = 0; i < blk_cnt; i++) {
      ebs[i].id = blk_ids[i];
      ebs[i].type = EX_ELEM_BLOCK;
    }

    if (block_by_block) {
      for (auto &eb : ebs) {
	size_t             numel   = eb.num_entry;
	size_t             reserve = 3.2 * numel;
	faces_[eb.id].reserve(reserve);
	internal_generate_faces(eb, faces_[eb.id], ids, hashIds_, use_local_ids, (INT)0);
      }
      for (auto &eb : ebs) {
	auto  &my_faces = faces_[eb.id];
	resolve_parallel_faces(exodusFilePtr_, communicator, my_faces, hashIds_, (INT)0);
      }
    }
    else {
      auto  &my_faces = faces_[0];
      size_t numel    = ex_inquire_int(exodusFilePtr_, EX_INQ_ELEM);
      size_t reserve = 3.2 * numel;
      my_faces.reserve(reserve);

      for (auto &eb : ebs) {
	internal_generate_faces(eb, my_faces, ids, hashIds_, use_local_ids, (INT)0);
      }
      resolve_parallel_faces(exodusFilePtr_, communicator, my_faces, hashIds_, (INT)0);
    }
  }
}
