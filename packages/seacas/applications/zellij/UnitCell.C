// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "UnitCell.h"
#include <vector>

#include "Ioss_NodeBlock.h"
#include "Ioss_Region.h"
#include "Ioss_SmartAssert.h"
#include "fmt/format.h"

extern unsigned int debug_level;

namespace {
  bool approx_equal(double A, double B)
  {
    double maxRelDiff = std::numeric_limits<float>::epsilon();
    double maxDiff    = 100.0 * maxRelDiff;

    // Check if the numbers are really close -- needed
    // when comparing numbers near zero.
    double diff = std::abs(A - B);
    if (diff <= maxDiff)
      return true;

    A              = std::abs(A);
    B              = std::abs(B);
    double largest = std::max(B, A);

    return (diff <= largest * maxRelDiff);
  }

  void gather_face_nodes(std::vector<double> &coord, std::pair<double, double> &minmax,
                         std::vector<int64_t> &min_face, std::vector<int64_t> &max_face)
  {
    for (size_t i = 0; i < coord.size(); i++) {
      if (approx_equal(coord[i], minmax.first)) {
        min_face.push_back(i);
      }
      if (approx_equal(coord[i], minmax.second)) {
        max_face.push_back(i);
      }
    }
    SMART_ASSERT(min_face.size() == max_face.size())(min_face.size())(max_face.size());
  }

  template <typename INT>
  void sort_face_nodes(std::vector<INT> &face_nodes, const std::vector<double> &coord_j,
                       const std::vector<double> &coord_i)
  {
    std::sort(face_nodes.begin(), face_nodes.end(), [&coord_j, &coord_i](size_t a, size_t b) {
      return float(coord_i[a]) < float(coord_i[b]) ||
             (approx_equal(coord_i[a], coord_i[b]) && float(coord_j[a]) < float(coord_j[b]));
    });
  }
} // namespace

UnitCell::UnitCell(std::shared_ptr<Ioss::Region> region) : m_region(region)
{
  std::vector<double> coord_x;
  std::vector<double> coord_y;
  std::vector<double> coord_z;

  auto *nb = region->get_node_blocks()[0];
  nb->get_field_data("mesh_model_coordinates_x", coord_x);
  nb->get_field_data("mesh_model_coordinates_y", coord_y);
  nb->get_field_data("mesh_model_coordinates_z", coord_z);

  const auto x_min_max_it = std::minmax_element(coord_x.begin(), coord_x.end());
  const auto y_min_max_it = std::minmax_element(coord_y.begin(), coord_y.end());

  minmax_x = std::make_pair(*x_min_max_it.first, *x_min_max_it.second);
  minmax_y = std::make_pair(*y_min_max_it.first, *y_min_max_it.second);

  // Now iterate all nodes and categorize if on a face -- minx, maxx, miny, maxy,
  gather_face_nodes(coord_x, minmax_x, min_I_face, max_I_face);
  gather_face_nodes(coord_y, minmax_y, min_J_face, max_J_face);

  sort_face_nodes(min_I_face, coord_z, coord_y);
  sort_face_nodes(max_I_face, coord_z, coord_y);
  sort_face_nodes(min_J_face, coord_z, coord_x);
  sort_face_nodes(max_J_face, coord_z, coord_x);

  if (debug_level & 4) {
    // Output each set of nodes --
    fmt::print("\nSORTED:\n");
    fmt::print("Min I: {}\n\n", fmt::join(min_I_face, " "));
    fmt::print("Max I: {}\n\n", fmt::join(max_I_face, " "));
    fmt::print("Min J: {}\n\n", fmt::join(min_J_face, " "));
    fmt::print("Max J: {}\n\n", fmt::join(max_J_face, " "));
  }

#ifndef NDEBUG
  for (size_t i = 0; i < min_I_face.size(); i++) {
    auto minI = min_I_face[i];
    auto maxI = max_I_face[i];
    SMART_ASSERT(approx_equal(coord_y[minI], coord_y[maxI]))(coord_y[minI])(coord_y[maxI]);
    SMART_ASSERT(approx_equal(coord_z[minI], coord_z[maxI]))(coord_z[minI])(coord_z[maxI]);
  }
#endif

  // Determine 'K' -- size of minI_minJ corner list.
  cell_KK = 0;
  for (; cell_KK < min_I_face.size(); cell_KK++) {
    if (min_I_face[cell_KK] != min_J_face[cell_KK]) {
      break;
    }

#ifndef NDEBUG
    // Checking that we get the same on the maxI_maxJ corner...
    size_t i_x = max_I_face.size() - 1 - cell_KK;
    size_t i_y = max_J_face.size() - 1 - cell_KK;
    SMART_ASSERT(max_I_face[i_x] == max_J_face[i_y])
    (cell_KK)(i_x)(i_y)(max_I_face[i_x])(max_J_face[i_y]);
#endif
  }

  SMART_ASSERT(min_I_face.size() % cell_KK == 0)(min_I_face.size())(cell_KK);
  SMART_ASSERT(min_J_face.size() % cell_KK == 0)(min_J_face.size())(cell_KK);

  cell_II = min_J_face.size() / cell_KK;
  cell_JJ = min_I_face.size() / cell_KK;

  if (debug_level & 4) {
    fmt::print("\nUnitCell {}:\n", m_region->name());
    fmt::print("\tThe minI face contains {} nodes\n", min_I_face.size());
    fmt::print("\tThe minJ face contains {} nodes\n", min_J_face.size());
    fmt::print("\tThe calculated cell shape is {} x {} x {}\n", cell_II, cell_JJ, cell_KK);
  }
}

std::vector<int> UnitCell::categorize_nodes(bool neighbor_i, bool neighbor_j) const
{
  // Create a vector of `node_count` length which has the following values:
  // 0: Node that is not shared with any neighbors.
  // 1: Node on min_I face
  // 2: Node on min_J face
  // 3: Node on min_I-min_J line

  auto             node_count = m_region->get_property("node_count").get_int();
  std::vector<int> node_category(node_count);

  if (neighbor_i) {
    for (auto node : min_I_face) {
      node_category[node] = 1;
    }
  }

  if (neighbor_j) {
    for (auto node : min_J_face) {
      node_category[node] += 2;
    }
  }
  return node_category;
}
