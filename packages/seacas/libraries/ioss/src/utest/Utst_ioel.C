// Copyright(C) 1999-2025 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "Ioss_CodeTypes.h"
#include "Ioss_ConcreteVariableType.h"
#include "Ioss_ElementPermutation.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_Initializer.h"
#include "Ioss_NullEntity.h"
#include "Ioss_ScopeGuard.h"
#include "Ioss_Utils.h"
#include "Ioss_VariableType.h"

#include <catch2/catch_session.hpp>
#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <array>
#include <cstdlib>
#include <fmt/ostream.h>
#include <set>
#include <stdio.h>
#include <string>
#include <vector>

// ========================================================================
namespace {
  using HexRow = std::array<Ioss::Ordinal, 8>;

  void test_aliases(const Ioss::NameList &elements);
  void test_element(const std::string &type);
  HexRow row_for(const Ioss::ElementPermutation &perm, Ioss::Permutation index);
  int    hex_orientation(const HexRow &row);
} // namespace
// ========================================================================

int main(IOSS_MAYBE_UNUSED int argc, char **argv)
{
  Ioss::StorageInitializer initialize_storage;
  Ioss::Initializer        initialize_topologies;

  Catch::Session session; // There must be exactly one instance
  int            returnCode = session.applyCommandLine(argc, argv);
  if (returnCode != 0) // Indicates a command line error
    return returnCode;

  auto ok = session.run() == 0;

  if (ok) {
    fmt::print(stderr, "\nSIERRA execution successful.\n");
    return EXIT_SUCCESS;
  }
  fmt::print(stderr, "\nSIERRA execution failed.\n");
  return EXIT_FAILURE;
}

TEST_CASE("nullentity")
{
  // Make sure Ioss::NullEntity works.  Not used in IOSS itself,
  // but some clients use it, so need to make sure it compiles
  // correctly.
  auto entity = std::make_unique<Ioss::NullEntity>();
  REQUIRE(entity);
  fmt::print(stderr, "\nThe null entity type is '{}' and it contains '{}'\n", entity->type_string(),
             entity->contains_string());
}

TEST_CASE("invalid element")
{
  // Check that asking for invalid element returns nullptr pointer.
  Ioss::ElementTopology *invalid = Ioss::ElementTopology::factory("Greg", true);
  CHECK(invalid == nullptr);
}

TEST_CASE("test_all_elements")
{
  Ioss::NameList elements = Ioss::ElementTopology::describe();

  for (const auto &element : elements) {
    DYNAMIC_SECTION(element) { test_element(element); }
  }
}

TEST_CASE("hex positive permutations are proper rotations")
{
  const auto *perm = Ioss::ElementPermutation::factory("hex");
  REQUIRE(perm != nullptr);
  REQUIRE(perm->num_permutation_nodes() == 8);
  REQUIRE(perm->num_permutations() == 24);
  REQUIRE(perm->num_positive_permutations() == 24);

  const HexRow canonical{0, 1, 2, 3, 4, 5, 6, 7};

  std::set<HexRow> rows;
  for (Ioss::Permutation i = 0; i < perm->num_positive_permutations(); ++i) {
    HexRow row = row_for(*perm, i);
    CHECK(std::is_permutation(row.begin(), row.end(), canonical.begin()));
    CHECK(hex_orientation(row) > 0);
    rows.insert(row);
  }

  CHECK(rows.size() == 24);
  CHECK(rows.count(HexRow{0, 1, 2, 3, 4, 5, 6, 7}) == 1);
  CHECK(rows.count(HexRow{0, 1, 5, 4, 3, 2, 6, 7}) == 0);
  CHECK(rows.count(HexRow{0, 4, 7, 3, 1, 5, 6, 2}) == 0);

  for (const auto &lhs : rows) {
    for (const auto &rhs : rows) {
      HexRow composed{};
      for (size_t i = 0; i < composed.size(); ++i) {
        composed[i] = lhs[rhs[i]];
      }
      CHECK(rows.count(composed) == 1);
    }
  }
}

TEST_CASE("test aliases")
{
  Ioss::NameList elements = Ioss::ElementTopology::describe();
  test_aliases(elements);
}

namespace {
  HexRow row_for(const Ioss::ElementPermutation &perm, Ioss::Permutation index)
  {
    auto indices = perm.permutation_indices(index);
    REQUIRE(indices.size() == 8);

    HexRow row{};
    std::copy(indices.begin(), indices.end(), row.begin());
    return row;
  }

  int hex_orientation(const HexRow &row)
  {
    constexpr std::array<std::array<int, 3>, 8> coords{{{{-1, -1, -1}},
                                                       {{1, -1, -1}},
                                                       {{1, 1, -1}},
                                                       {{-1, 1, -1}},
                                                       {{-1, -1, 1}},
                                                       {{1, -1, 1}},
                                                       {{1, 1, 1}},
                                                       {{-1, 1, 1}}}};

    auto edge = [&](size_t from, size_t to) {
      const auto &a = coords[static_cast<size_t>(row[from])];
      const auto &b = coords[static_cast<size_t>(row[to])];
      return std::array<int, 3>{{b[0] - a[0], b[1] - a[1], b[2] - a[2]}};
    };

    const auto x = edge(0, 1);
    const auto y = edge(0, 3);
    const auto z = edge(0, 4);

    return x[0] * (y[1] * z[2] - y[2] * z[1]) - x[1] * (y[0] * z[2] - y[2] * z[0]) +
           x[2] * (y[0] * z[1] - y[1] * z[0]);
  }

  void test_element(const std::string &type)
  {
    // NOTE: For true test, should run with purify checking enabled to
    //       ensure we are not running off the end of arrays...

    Ioss::ElementTopology *element = Ioss::ElementTopology::factory(type);
    REQUIRE(element != nullptr);

    // See if the name is an alias for another element (type != name())

    // Check that name is alias for name...
    CHECK(element->is_alias(type));

    // Output the 'shape'
    auto shape = Ioss::Utils::lowercase(Ioss::Utils::shape_to_string(element->shape()));

    std::string name = element->name();
    fmt::print(stderr, "\n{:<25}({})\t[{}]", type, name, shape);

    // Check that master element name is an alias...
    CHECK(element->is_alias(element->master_element_name()));

    if (type == "unknown" || type == "invalid_topology") {
      return;
    }

    // Check that the hash id method of selecting the element returns the correct element.
    unsigned int hash_val = Ioss::ElementTopology::get_unique_id(name);
    CHECK(Ioss::ElementTopology::factory(hash_val) == element);

    int order = element->order();

    bool homo_edges = element->edges_similar();
    bool homo_faces = element->faces_similar();

    int nn = element->number_nodes();
    CHECK(nn > 0);

    int ncn = element->number_corner_nodes();
    CHECK(ncn > 0);
    CHECK(ncn <= nn);

    auto *perm = element->permutation();
    REQUIRE(perm != nullptr);

    if (element->name() != "node") {
      REQUIRE(static_cast<int>(perm->num_permutation_nodes()) == element->number_corner_nodes());
      // Element shape matches permutation type
      CHECK(Ioss::Utils::lowercase(Ioss::Utils::shape_to_string(element->shape())) == perm->type());

      if (perm->num_permutations() > 0) {
        const auto &permutation = perm->permutation_indices(0);
        const auto  connect     = element->element_connectivity();
        for (size_t i = 0; i < permutation.size(); i++) {
          // permutation matches element connectivity
          CHECK(permutation[i] == connect[i]);
        }
      }
    }

    int ne = element->number_edges();
    CHECK(ne >= 0);

    int nf = element->number_faces();
    CHECK(nf >= 0);

    // Verify Euler's Formula holds... V-E+F=2
    if (element->parametric_dimension() == 3) {
      int euler = ncn - ne + nf;
      CHECK(euler == 2);
    }

    int nne = element->number_nodes_edge(0);
    if (nne == -1) {
      CHECK(homo_edges == false);
      for (int edge = 1; edge <= ne; edge++) {
        int nnei = element->number_nodes_edge(edge);
        CHECK(nnei >= 0);
        CHECK(nnei <= nn);
      }
    }
    else {
      CHECK(nne >= 0);
      CHECK(nne <= nn);
    }

    int nnf = element->number_nodes_face(0);
    CHECK(nnf <= nn);
    CHECK(nnf >= -1);

    // Check boundary and other topologies...
    if (nf > 0) {
      Ioss::ElementTopology *face0 = element->face_type(0);
      if (homo_faces == false) {
        CHECK(face0 == nullptr);
      }
      else {
        CHECK(face0 != nullptr);
      }

      for (int i = 1; i <= nf; i++) {
        Ioss::ElementTopology *face = element->face_type(i);
        CHECK(face != nullptr);
        if (face != nullptr) {
          unsigned int nnfi = element->number_nodes_face(i);
          CHECK(nnfi == static_cast<unsigned int>(face->number_nodes()));
          std::vector<int> conn = element->face_connectivity(i);
          CHECK(nnfi == conn.size());
        }
      }
    }

    // Edges...
    if (ne > 0) {
      Ioss::ElementTopology *edge0 = element->edge_type(0);
      if (homo_edges == false) {
        CHECK(edge0 == nullptr);
      }
      else {
        CHECK(edge0 != nullptr);
      }

      for (int i = 1; i <= ne; i++) {
        Ioss::ElementTopology *edge = element->edge_type(i);
        CHECK(edge != nullptr);
        if (edge != nullptr) {
          unsigned int nnei = element->number_nodes_edge(i);
          CHECK(nnei == static_cast<unsigned int>(edge->number_nodes()));
          std::vector<int> conn = element->edge_connectivity(i);
          CHECK(nnei == conn.size());
        }
      }
    }
    // Variable types...
    const Ioss::VariableType *vt = Ioss::VariableType::factory(element->name());
    REQUIRE(vt != nullptr);
    // See if component counts match...
    int vt_comp = vt->component_count();
    CHECK(nn == vt_comp);

    // For elements with parametric dimension == 3
    // Get face-edge-order
    // Get face-node-connectivity
    // Foreach edge in face, get nodal connectivity
    //   ensure that node is in face connectivity...
    //
    // For an edge:   1------3------2
    //
    // For a face: Corner nodes are first in connectivity
    //             Center nodes follow.
    // So:
    //
    //                        2           x
    //    3----6----2        / \          x
    //    |         |       /   \         x
    //    7         5      5     4        x
    //    |         |     /       \       x
    //    0----4----1    0----3----1      x
    //
    if (element->parametric_dimension() == 3) {
      for (int i = 1; i <= nf; i++) {
        unsigned int fncn           = element->face_type(i)->number_corner_nodes();
        unsigned int num_edges_face = element->number_edges_face(i);
        CHECK(fncn == num_edges_face);

        // Nodes defining face...
        std::vector<int> face_conn = element->face_connectivity(i);

        // Edges defining face...
        std::vector<int> face_edge_conn = element->face_edge_connectivity(i);
        REQUIRE(num_edges_face == face_edge_conn.size());
        for (unsigned int j = 0; j < num_edges_face; j++) {
          // Not implemented in all elements yet...
          std::vector<int> edge_conn = element->edge_connectivity(face_edge_conn[j] + 1);
          // Check that first two nodes in 'edge_conn' match
          // corresponding nodes in 'face_conn'
          bool ok = (edge_conn[0] == face_conn[j] || edge_conn[1] == face_conn[j]) &&
                    (edge_conn[0] == face_conn[(j + 1) % fncn] ||
                     edge_conn[1] == face_conn[(j + 1) % fncn]);
          CHECK(ok);

          if (element->name() == "wedge12" || element->name() == "hex16") {
            continue;
          }

          CHECK(edge_conn.size() == size_t(order) + 1);

          CHECK(order >= 1);
          CHECK(face_conn.size() >= fncn + (order - 1) * num_edges_face);
          if (order == 2) {
            CHECK(edge_conn[2] == face_conn[fncn + j]);
          }
          if (order == 3) {
            auto en2 = edge_conn[2];
            auto en3 = edge_conn[3];

            auto fn2 = face_conn[fncn + 2 * j];
            auto fn3 = face_conn[fncn + 2 * j + 1];
            // Mid-Side Node Edge Connectivity must match face connectivity
            CHECK((en2 == fn2 || en3 == fn2 || en2 == fn3 || en3 == fn3));
          }
        }
      }
    }
  }

  void test_aliases(const Ioss::NameList &elements)
  {
    int count = elements.size();
    fmt::print(stderr, "\n\nTesting Element Topology Aliases...\n");

    for (int i = 0; i < count; i++) {
      Ioss::ElementTopology *el_top = Ioss::ElementTopology::factory(elements[i]);
      if (el_top->name() == elements[i]) {
        fmt::print(stderr, "Element: {:<25}({}) has the following aliases:\n\t", elements[i],
                   el_top->master_element_name());
        for (int j = 0; j < count; j++) {
          if (i != j && el_top->is_alias(elements[j])) {
            fmt::print(stderr, "{}, ", elements[j]);
          }
        }
        fmt::print(stderr, "\n");
      }
    }
  }
} // namespace
