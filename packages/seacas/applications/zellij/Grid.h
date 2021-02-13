// Copyright(C) 2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details
#ifndef ZE_Grid_H
#define ZE_Grid_H

#include <vector>

#include "Ioss_ParallelUtils.h"
#include "Ioss_Region.h"

// `grid` stores the data for the tessellation of size `IxJ`
//  * gridI -- extent in I direction
//  * gridJ -- extend in J direction
//  * grid(i,j) -- return database information at i,j location
//  * grid(index) -- return database information at i,j location corresponding to `index`
//  * max number of element blocks in a unit_cell mesh
//  * range of x, y size of unit_cell mesh. (assumes unit_cell minx, miny == 0.0)
//  * Ti, Tj, Tk -- size of regular mesh on boundary of unit_cell (? may need the 3 `Tk` values for
//  padding)
//  * std::vector<Cell> m_grid -- contains database information...

class SystemInterface;
class UnitCell;
class Cell;

class Grid
{
public:
  //! Create an empty grid of size `extent_i` x `extent_j`.  The output mesh will
  //! be written to the exodus database in the Ioss::Region `region`
  Grid(SystemInterface &interFace, size_t extent_i, size_t extent_j);

  //! Return a reference to the Cell cell at location `(i,j)`.
  //! Does not check that `i` and `j` are in bounds.
  Cell &get_cell(size_t i, size_t j)
  {
    size_t idx = i * m_gridJ + j;
    return m_grid[idx];
  }

  //! Return `I` extent of the grid / lattice
  size_t II() const { return m_gridI; }
  //! Return `J` extent of the grid / lattice
  size_t JJ() const { return m_gridJ; }
  //! Return total number of cells in the grid / lattice
  size_t size() const { return m_gridI * m_gridJ; }
  int    parallel_size() const { return m_parallelSize; }

  //! Are nodes at the boundaries of the unit cells equivalenced.
  bool equivalence_nodes() const { return m_equivalenceNodes; }

  //! Create a Cell object referencing the UnitCell `unit_cell` at location `(i,j)`
  void initialize(size_t i, size_t j, std::shared_ptr<UnitCell> unit_cell);

  //! Specify the X and Y location of each grid cell in the overall grid space.
  void set_coordinate_offsets();

  //! Once all Cell objects have been initialized, Determine the coordinate extents and
  //! offsets of each cell, the size of the output mesh, the node and element id offsets
  //! for each cell, the number of nodes and elements in the output mesh and initialize
  //! the output mesh.
  void finalize(int start_rank, int rank_count);

  void decompose(size_t ranks, const std::string &method);

  //! Output node coordinates and element block connectivities for the output mesh.
  template <typename INT> void output_model(int start_rank, int num_ranks, INT /*dummy*/);

  const Ioss::ParallelUtils &util() const { return m_pu; }

  Ioss::Region *output_region(int rank = 0) { return m_outputRegions[rank].get(); }

  unsigned int minimize_open_files() { return m_minimizeOpenFiles; }
  void         set_minimize_open_files(unsigned int mode) { m_minimizeOpenFiles = mode; }

private:
  void create_output_regions(SystemInterface &interFace);
  void categorize_processor_boundaries();

  void output_nodal_coordinates(const Cell &cell);
  template <typename INT>
  void output_block_connectivity(Cell &cell, const std::vector<INT> &node_map, int start_rank,
                                 int num_ranks);
  template <typename INT>
  void output_nodal_communication_map(Cell &cell, const std::vector<INT> &node_map, int start_rank,
                                      int num_ranks);
  template <typename INT>
  void output_element_map(Cell &cell, int start_rank, int num_ranks, INT /*dummy*/);
  template <typename INT>
  void output_node_map(const Cell &cell, int start_rank, int num_ranks, INT /*dummy*/);

  template <typename INT> void output_surfaces(Cell &cell, INT /*dummy*/);

  std::vector<std::unique_ptr<Ioss::Region>> m_outputRegions;
  std::vector<Cell>                          m_grid{};
  Ioss::ParallelUtils                        m_pu{MPI_COMM_WORLD};
  size_t                                     m_gridI{0};
  size_t                                     m_gridJ{0};
  int                                        m_parallelSize{1};
  bool                                       m_equivalenceNodes{true};
  unsigned int                               m_minimizeOpenFiles{0}; // 1: Unit, 2: output, 3: all
};
#endif
