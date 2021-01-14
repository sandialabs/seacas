// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#ifndef ZE_Grid_H
#define ZE_Grid_H

#include <vector>

#include "GridEntry.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Region.h"

// `grid` stores the data for the tesselation of size `IxJ`
//  * gridI -- extent in I direction
//  * gridJ -- extend in J direction
//  * grid(i,j) -- return database information at i,j location
//  * grid(index) -- return database information at i,j location corresponding to `index`
//  * max number of element blocks in a unit_cell mesh
//  * range of x, y size of unit_cell mesh. (assumes unit_cell minx, miny == 0.0)
//  * Ti, Tj, Tk -- size of regular mesh on boundary of unit_cell (? may need the 3 `Tk` values for
//  padding)
//  * std::vector<GridEntry> m_grid -- contains database information...

class Grid
{
public:
  Grid(std::unique_ptr<Ioss::Region> &region, size_t extent_i, size_t extent_j)
      : m_region(std::move(region)), m_gridI(extent_i), m_gridJ(extent_j)
  {
    m_grid.resize(m_gridI * m_gridJ);
  }

  GridEntry &get_cell(int i, int j)
  {
    size_t idx = i * m_gridJ + j;
    return get_cell(idx);
  }

  GridEntry &get_cell(int index) { return m_grid[index]; }
  size_t     II() const { return m_gridI; }
  size_t     JJ() const { return m_gridJ; }
  size_t     size() const { return m_gridI * m_gridJ; }

  void initialize(size_t i, size_t j, std::shared_ptr<UnitCell> unit_cell);
  void finalize();

  template <typename INT> void output_model(INT /*dummy*/);

  const Ioss::ParallelUtils &util() const { return m_pu; }

private:
  void output_nodal_coordinates();

  template <typename INT> void output_block_connectivity(INT /*dummy*/);

public:
  std::unique_ptr<Ioss::Region> m_region;

private:
  std::vector<GridEntry> m_grid{};
  Ioss::ParallelUtils    m_pu{MPI_COMM_WORLD};
  size_t                 m_gridI{0};
  size_t                 m_gridJ{0};
};
#endif
