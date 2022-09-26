// Copyright(C) 1999-2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include <Ioss_CodeTypes.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_ElementTopology.h>
#include <Ioss_Face.h>

#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>

#define FG_USE_ROBIN
#if defined FG_USE_STD
#include <unordered_set>
#elif defined FG_USE_HOPSCOTCH
#include <hopscotch_set.h>
#elif defined FG_USE_ROBIN
#include <robin_set.h>
#endif

namespace Ioss {
  struct FaceHash
  {
    size_t operator()(const Face &face) const { return face.hashId_; }
  };

  struct FaceEqual
  {
    bool operator()(const Face &left, const Face &right) const
    {
      if (left.hashId_ != right.hashId_) {
        return false;
      }
      // Hash (hashId_) is equal
      // Check whether same vertices (can be in different order)
      // Most (All?) of the time, there are no hashId_ collisions, so this test will not
      // find a difference and the function will return 'true'
      // However, for some reason, removing this check does not change the execution time
      // appreiciably...
      for (auto lvert : left.connectivity_) {
        if (std::find(right.connectivity_.cbegin(), right.connectivity_.cend(), lvert) ==
            right.connectivity_.cend()) {
          // Not found, therefore not the same.
          return false;
        }
      }
      return true;
    }
  };

#if defined FG_USE_STD
  using FaceUnorderedSet = std::unordered_set<Face, FaceHash, FaceEqual>;
#elif defined FG_USE_HOPSCOTCH
  using FaceUnorderedSet = tsl::hopscotch_set<Face, FaceHash, FaceEqual>;
  // using FaceUnorderedSet = tsl::hopscotch_pg_set<Face, FaceHash, FaceEqual>;
#elif defined FG_USE_ROBIN
  using FaceUnorderedSet = tsl::robin_set<Face, FaceHash, FaceEqual>;
  // using FaceUnorderedSet = tsl::robin_pg_set<Face, FaceHash, FaceEqual>;
#endif

  inline void create_face(Ioss::FaceUnorderedSet &faces, size_t id, std::array<size_t, 4> &conn,
                          size_t element, int local_face)
  {
    auto face_iter = faces.emplace(id, conn);

    (*(face_iter.first)).add_element(element, local_face);
  }

  template <typename INT>
  void internal_generate_faces(Ioss::ElementBlock *eb, Ioss::FaceUnorderedSet &faces,
                               const std::vector<INT> &ids, const std::vector<size_t> &hash_ids,
                               bool local_ids, INT /*dummy*/)
  {
    const Ioss::ElementTopology *topo = eb->topology();

    // Only handle continuum elements at this time...
    if (topo->parametric_dimension() != 3) {
      return;
    }

    std::vector<INT> connectivity;
    eb->get_field_data("connectivity_raw", connectivity);

    std::vector<INT> elem_ids;
    if (local_ids) {
      elem_ids.resize(eb->entity_count());
      std::iota(elem_ids.begin(), elem_ids.end(), static_cast<INT>(eb->get_offset() + 1));
    }
    else {
      eb->get_field_data("ids", elem_ids);
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
    size_t num_elem          = eb->entity_count();

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

#ifdef SEACAS_HAVE_MPI
  template <typename INT>
  void internal_resolve_parallel_faces(const Ioss::Map                        &node_map,
                                       const Ioss::FaceUnorderedSet           &faces,
                                       const std::vector<size_t>              &hash_ids,
                                       const std::vector<std::pair<INT, INT>> &proc_entity,
                                       MPI_Comm communicator, int proc_count, INT /*dummy*/)
  {
    // 'id_span' gives index into proc_entity for all nodes.
    // 'id_span[local_node_id] .. id_span[local_node_id+1]' gives
    // the location in 'proc_entity' of the sharing information
    // for node 'local_node_id'
    std::vector<size_t> id_span(hash_ids.size() + 1);
    for (const auto &pe : proc_entity) {
      INT node = pe.second;
      assert(node >= 0 && node < (INT)id_span.size() - 1);
      id_span[node]++;
    }
    Ioss::Utils::generate_index(id_span);

    // Each boundary face ...
    // .. See if all of its nodes are shared with same processor.
    //  .. Iterate face nodes
    //  .. Determine shared proc.
    //  .. (for now, use a vector[proc] = count
    //   .. if potentially shared with 'proc', then count == num_nodes_face

    std::vector<INT> potential_count(proc_count);
    std::vector<int> shared_nodes(proc_count);
    for (auto &face : faces) {
      if (face.elementCount_ == 1) {
        // On 'boundary' -- try to determine whether on processor or exterior
        // boundary
        int face_node_count = 0;
        for (auto &gnode : face.connectivity_) {
          if (gnode > 0) {
            auto node = node_map.global_to_local(gnode, true) - 1;
            face_node_count++;
            size_t begin = id_span[node];
            size_t end   = id_span[node + 1];
            for (size_t j = begin; j < end; j++) {
              assert(proc_entity[j].second == node);
              int proc = proc_entity[j].first;
              shared_nodes[proc]++;
            }
          }
        }
        for (size_t i = 0; i < proc_count; i++) {
          if (shared_nodes[i] == face_node_count) {
            potential_count[i]++;
          }
          shared_nodes[i] = 0; // Reset for next trip through face.connectivity_ loop
        }
      }
    }

    std::vector<INT> potential_offset(potential_count.begin(), potential_count.end());
    Ioss::Utils::generate_index(potential_offset);

    size_t potential = potential_offset[proc_count - 1] + potential_count[proc_count - 1];
    std::vector<int64_t> potential_faces(6 * potential);

    for (auto &face : faces) {
      if (face.elementCount_ == 1) {
        // On 'boundary' -- try to determine whether on processor or exterior
        // boundary
        int face_node_count = 0;
        for (auto &gnode : face.connectivity_) {
          if (gnode > 0) {
            auto node = node_map.global_to_local(gnode, true) - 1;
            face_node_count++;
            size_t begin = id_span[node];
            size_t end   = id_span[node + 1];
            for (size_t j = begin; j < end; j++) {
              assert(proc_entity[j].second == node);
              int proc = proc_entity[j].first;
              shared_nodes[proc]++;
            }
          }
        }
        for (size_t i = 0; i < proc_count; i++) {
          if (shared_nodes[i] == face_node_count) {
            size_t offset                   = potential_offset[i];
            potential_faces[6 * offset + 0] = face.hashId_;
            potential_faces[6 * offset + 1] = face.connectivity_[0];
            potential_faces[6 * offset + 2] = face.connectivity_[1];
            potential_faces[6 * offset + 3] = face.connectivity_[2];
            potential_faces[6 * offset + 4] = face.connectivity_[3];
            potential_faces[6 * offset + 5] = face.element[0];
            assert(face.elementCount_ == 1);
            potential_offset[i]++;
          }
          shared_nodes[i] = 0; // Reset for next trip through face.connectivity_ loop
        }
      }
    }

    // Regenerate potential_offset since it was modified above...
    std::copy(potential_count.begin(), potential_count.end(), potential_offset.begin());
    Ioss::Utils::generate_index(potential_offset);

    // Now need to send to the other processors...
    // For now, use all-to-all; optimization is just send to processors with
    // data...
    std::vector<INT> check_count(proc_count);
    MPI_Alltoall(potential_count.data(), 1, Ioss::mpi_type((INT)0), check_count.data(), 1,
                 Ioss::mpi_type((INT)0), communicator);

    const int            values_per_face = 6; // id, 4-node-conn, element
    auto                 sum = std::accumulate(check_count.begin(), check_count.end(), 0);
    std::vector<int64_t> check_faces(values_per_face * sum);

    std::vector<INT> check_offset(check_count.begin(), check_count.end());
    Ioss::Utils::generate_index(check_offset);

    // Need to adjust counts and offsets to account for sending 6 values per
    // face...
    for (size_t i = 0; i < proc_count; i++) {
      potential_count[i] *= values_per_face;
      potential_offset[i] *= values_per_face;
      check_count[i] *= values_per_face;
      check_offset[i] *= values_per_face;
    }

    Ioss::MY_Alltoallv(potential_faces, potential_count, potential_offset, check_faces, check_count,
                       check_offset, communicator);

    // Now iterate the check_faces and see if any of them match one
    // of this processors faces...  If so, then mark as shared and
    // add the element...
    for (size_t i = 0; i < check_faces.size(); i += values_per_face) {
      size_t                id = check_faces[i + 0];
      std::array<size_t, 4> conn{};
      conn[0]            = check_faces[i + 1];
      conn[1]            = check_faces[i + 2];
      conn[2]            = check_faces[i + 3];
      conn[3]            = check_faces[i + 4];
      size_t     element = check_faces[i + 5];
      Ioss::Face face(id, conn);
      auto       face_iter = faces.find(face, face.hashId_);
      if (face_iter != faces.end()) {
        // we have a match... This is a shared interior face
        (*face_iter).add_element(element); // Already has face multiplied in.

        int proc = 0;
        for (size_t j = 0; j < check_count.size(); j++) {
          if (check_count[j] > 0 && check_offset[j] == (INT)i) {
            break;
          }
          proc++;
        }
      }
    }
  }
#endif
} // namespace Ioss
