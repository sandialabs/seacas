// Copyright(C) 1999-2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_CodeTypes.h>

#include <Ioss_CommSet.h>
#include <Ioss_DBUsage.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_ElementTopology.h>
#include <Ioss_FaceGenerator.h>
#include <Ioss_FaceGeneratorUtils.h>
#include <Ioss_IOFactory.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_ParallelUtils.h>
#include <Ioss_Property.h>
#include <Ioss_Region.h>

#include <algorithm>
#include <chrono>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <functional>
#include <numeric>
#include <random>
#include <utility>

#define DO_TIMING 0

namespace {
  template <typename INT>
  void resolve_parallel_faces(Ioss::Region &region, const Ioss::FaceUnorderedSet &faces,
                              const std::vector<size_t> &hash_ids, INT /*dummy*/)
  {
    PAR_UNUSED(region);
    PAR_UNUSED(faces);
    PAR_UNUSED(hash_ids);

#ifdef SEACAS_HAVE_MPI
    size_t proc_count = region.get_database()->util().parallel_size();

    if (proc_count > 1) {
      // If parallel, resolve faces on processor boundaries.
      // For each boundary face, need to check whether all of the nodes on
      // the face are shared with the same processor.  If so, then that face
      // is *possibly* shared with that processor.
      //
      // With the current continuum element only restriction, then a face
      // can only be shared with one other processor...

      // get nodal communication data CommSet...
      Ioss::CommSet *css = region.get_commset("commset_node");

      std::vector<std::pair<INT, INT>> proc_entity;
      {
        // entity_processor consists of node,proc, node,proc, entries.
        std::vector<INT> entity_processor;
        css->get_field_data("entity_processor_raw", entity_processor);

        proc_entity.reserve(entity_processor.size() / 2);
        for (size_t i = 0; i < entity_processor.size(); i += 2) {
          // Converts from 1-based to 0-based local nodes.
          proc_entity.push_back(std::make_pair(entity_processor[i + 1], entity_processor[i] - 1));
        }
      }

      internal_resolve_parallel_faces(region.get_node_map(), faces, hash_ids, proc_entity, region.get_database()->util().communicator(), proc_count, INT(0));
    }
#endif
  }

} // namespace

namespace Ioss {
  FaceGenerator::FaceGenerator(Ioss::Region &region) : region_(region) {}

  template void FaceGenerator::generate_faces(int, bool, bool);
  template void FaceGenerator::generate_faces(int64_t, bool, bool);

  template <typename INT>
  void FaceGenerator::generate_faces(INT /*dummy*/, bool block_by_block, bool use_local_ids)
  {
    // Convert ids into hashed-ids
    Ioss::NodeBlock *nb = region_.get_node_blocks()[0];

    std::vector<INT> ids;
    if (use_local_ids) {
      ids.resize(nb->entity_count());
      std::iota(ids.begin(), ids.end(), 1);
    }
    else {
      nb->get_field_data("ids", ids);
    }
#if DO_TIMING
    auto starth = std::chrono::steady_clock::now();
#endif
    hash_node_ids(ids);
#if DO_TIMING
    auto endh = std::chrono::steady_clock::now();
#endif

    const auto &ebs = region_.get_element_blocks();
    if (block_by_block) {
      for (auto &eb : ebs) {
	const std::string &name    = eb->name();
	size_t             numel   = eb->entity_count();
	size_t             reserve = 3.2 * numel;
	faces_[name].reserve(reserve);
	internal_generate_faces(eb, faces_[name], ids, hashIds_, use_local_ids, (INT)0);
      }
      for (auto &eb : ebs) {
	auto  &my_faces = faces_[eb->name()];
	resolve_parallel_faces(region_, my_faces, hashIds_, (INT)0);
      }
    }
    else {
      auto  &my_faces = faces_["ALL"];
      size_t numel    = region_.get_property("element_count").get_int();

      size_t reserve = 3.2 * numel;
      my_faces.reserve(reserve);
      const auto &ebs = region_.get_element_blocks();
      for (auto &eb : ebs) {
	internal_generate_faces(eb, my_faces, ids, hashIds_, use_local_ids, (INT)0);
      }
      resolve_parallel_faces(region_, my_faces, hashIds_, (INT)0);
    }
#if DO_TIMING
    auto endf = std::chrono::steady_clock::now();

    size_t face_count = 0;
    if (block_by_block) {
      for (auto &eb : ebs) {
	face_count += faces_[eb->name()].size();
      }
    }
    else {
      auto  &my_faces = faces_["ALL"];
      face_count = my_faces.size();
    }

    auto endp  = std::chrono::steady_clock::now();
    auto diffh = endh - starth;
    auto difff = endf - endh;
    fmt::print(
        "Node ID hash time:   \t{:.6} ms\t{:12} nodes/second\n"
        "Face generation time:\t{:.6} ms\t{:12} faces/second\n",
        std::chrono::duration<double, std::milli>(diffh).count(),
        fmt::group_digits(INT(hashIds_.size() / std::chrono::duration<double>(diffh).count())),
        std::chrono::duration<double, std::milli>(difff).count(),
        fmt::group_digits(INT(face_count / std::chrono::duration<double>(difff).count())));
#ifdef SEACAS_HAVE_MPI
    auto   diffp      = endp - endf;
    size_t proc_count = region_.get_database()->util().parallel_size();

    if (proc_count > 1) {
      fmt::print("Parallel time:       \t{:.6} ms\t{:12} faces/second.\n",
                 std::chrono::duration<double, std::milli>(diffp).count(),
                 fmt::group_digits(INT(face_count / std::chrono::duration<double>(diffp).count())));
    }
#endif
    fmt::print("Total time:          \t{:.6} ms\n\n",
               std::chrono::duration<double, std::milli>(endp - starth).count());
#endif
  }

  template <typename INT> void FaceGenerator::hash_node_ids(const std::vector<INT> &node_ids)
  {
    hashIds_.reserve(node_ids.size());
    for (auto &id : node_ids) {
      hashIds_.push_back(Ioss::Utils::id_hash(id));
    }
  }
} // namespace Ioss
