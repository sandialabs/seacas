// Copyright(C) 1999-2020, 2022 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include <Ioss_CodeTypes.h>
#include <Ioss_EntityType.h> // for EntityType

#include <cstddef> // for size_t
#include <cstdint> // for int64_t
#include <map>     // for map, etc
#include <string>  // for string
#include <unordered_map>
#include <utility> // for pair
#include <vector>  // for vector

#include <assert.h>

#include "Ioss_ElementTopology.h"
#include "Ioss_StandardElementTypes.h"

#include "Iotm_TextMeshUtils.h"

namespace Iotm {
  class TopologyMapEntry
  {
  public:
    using Ordinal = uint16_t;
    using Permutation = uint8_t;

    static constexpr Ordinal InvalidOrdinal = 65535;
    static constexpr Permutation InvalidPermutation = 128;

    using DimensionArray = bool[4];

    TopologyMapEntry()
        : id(Ioss::ElementTopology::get_unique_id(std::string(Ioss::Unknown::name))),
          topology(Ioss::ElementTopology::factory(std::string(Ioss::Unknown::name))),
          isShell(false),
          numPermutations(0),
          numPositivePermutations(0),
          initialized(false)
    {
      set_valid_spatial_dimensions({false, false, false, false});
    }

    TopologyMapEntry(const std::string &name)
        : id(Ioss::ElementTopology::get_unique_id(name)),
          topology(Ioss::ElementTopology::factory(name)),
          isShell(false),
          numPermutations(0),
          numPositivePermutations(0),
          initialized(false)
    {
      set_valid_spatial_dimensions({false, false, false, false});
    }

    TopologyMapEntry(const TopologyMapEntry &topo) = default;

    bool operator==(const Ioss::ElementTopology *topo) const
    {
      return topo == topology;
    }

    bool defined_on_spatial_dimension(const unsigned spatialDim) const
    {
      if (spatialDim > 3) {
        return false;
      }
      return validSpatialDimensions[spatialDim];
    }

    const std::string name() const { return topology->name(); }

    int num_nodes() const { return topology->number_nodes(); }

    bool operator==(const TopologyMapEntry &rhs) const
    {
      return id == rhs.id && topology == rhs.topology &&
             equivalent_valid_spatial_dimensions(rhs.validSpatialDimensions);
    }

    bool operator!=(const TopologyMapEntry &rhs) const { return !(*this == rhs); }

    int num_sides() const
    {
      if (isShell) {
        if (topology->parametric_dimension() == 2) {
          return topology->number_faces();
        }
      }

      return topology->number_boundaries();
    }

    bool valid_side(unsigned side) const
    {
      unsigned numSides = num_sides();
      if (side > 0 && side <= numSides)
        return true;
      return false;
    }

    std::string side_topology_name(unsigned side) const
    {
      if (!valid_side(side))
        return "";

      Ioss::ElementTopology *sideTopology = topology->boundary_type(side);
      return sideTopology->name();
    }

    TopologyMapEntry side_topology(unsigned side) const
    {
      if (!valid_side(side)) return TopologyMapEntry();
      return *sideTopologies[side-1];
    }

    unsigned side_topology_num_nodes(unsigned side) const
    {
      if (!valid_side(side))
        return 0;

      Ioss::ElementTopology *sideTopology = topology->boundary_type(side);
      return sideTopology->number_nodes();
    }

    std::vector<Ordinal> side_topology_node_indices(unsigned side) const
    {
      if (!valid_side(side)) return std::vector<Ordinal>();

      Ioss::ElementTopology *sideTopology = topology->boundary_type(side);
      std::vector<Ordinal> elementNodeOrdinalVector(sideTopology->number_nodes());

      Ioss::IntVector connectivity = topology->boundary_connectivity(side);

      for(int i=0; i<sideTopology->number_nodes(); i++) {
        elementNodeOrdinalVector[i] = connectivity[i];
      }

      return elementNodeOrdinalVector;
    }

    bool is_shell() const { return isShell; }

    unsigned num_permutations() const { return numPermutations; }

    unsigned num_positive_permutations() const { return numPositivePermutations; }

    bool is_positive_polarity(Permutation permutation) const { return permutation < numPositivePermutations; }

    bool valid_permutation(Permutation permutation) const { return permutation < numPermutations; }

    bool fill_permutation_indices(Permutation permutation, std::vector<Ordinal>& nodeOrdinalVector) const
    {
      if (!valid_permutation(permutation)) return false;

      nodeOrdinalVector.resize(num_nodes());
      const auto& ordinals = permutationNodeOrdinals[permutation];
      for(int i=0; i<num_nodes(); i++) {
        nodeOrdinalVector[i] = ordinals[i];
      }

      return true;
    }

    std::vector<Ordinal> permutation_indices(Permutation permutation) const
    {
      std::vector<Ordinal> nodeOrdinalVector;
      fill_permutation_indices(permutation, nodeOrdinalVector);
      return nodeOrdinalVector;
    }

    static TopologyMapEntry* invalid_topology_factory()
    {
      static TopologyMapEntry entry;

      entry.initialized = true;

      return &entry;
    }

    static TopologyMapEntry* node_factory()
    {
      static TopologyMapEntry entry(Ioss::Node::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,true,true});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* line_2_factory()
    {
      static TopologyMapEntry entry(Ioss::Edge2::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,true});
        entry.set_permutation_data(2, 1, {{0, 1}, {1, 0}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* line_2_1d_factory()
    {
      static TopologyMapEntry entry(Ioss::Edge2::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,false,false});
        entry.set_permutation_data(2, 1, {{0, 1}, {1, 0}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* line_3_factory()
    {
      static TopologyMapEntry entry(Ioss::Edge3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,true});
        entry.set_permutation_data(2, 1, {{0, 1, 2}, {1, 0, 2}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* line_3_1d_factory()
    {
      static TopologyMapEntry entry(Ioss::Edge3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,false,false});
        entry.set_permutation_data(2, 1, {{0, 1, 2}, {1, 0, 2}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tri_3_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2},
                                    {2, 0, 1},
                                    {1, 2, 0},
                                    {0, 2, 1},
                                    {2, 1, 0},
                                    {1, 0, 2}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tri_4_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2, 3},
                                    {2, 0, 1, 3},
                                    {1, 2, 0, 3},
                                    {0, 2, 1, 3},
                                    {2, 1, 0, 3},
                                    {1, 0, 2, 3}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tri_6_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri6::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2,  3, 4, 5},
                                    {2, 0, 1,  5, 3, 4},
                                    {1, 2, 0,  4, 5, 3},
                                    {0, 2, 1,  5, 4, 3},
                                    {2, 1, 0,  4, 3, 5},
                                    {1, 0, 2,  3, 5, 4}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_4_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3},
                                    {3, 0, 1, 2},
                                    {2, 3, 0, 1},
                                    {1, 2, 3, 0},
                                    {0, 3, 2, 1},
                                    {3, 2, 1, 0},
                                    {2, 1, 0, 3},
                                    {1, 0, 3, 2}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_6_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad6::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3,  4, 5},
                                    {3, 0, 1, 2,  4, 5},
                                    {2, 3, 0, 1,  5, 4},
                                    {1, 2, 3, 0,  5, 4},
                                    {0, 3, 2, 1,  5, 4},
                                    {3, 2, 1, 0,  5, 4},
                                    {2, 1, 0, 3,  4, 5},
                                    {1, 0, 3, 2,  4, 5}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_8_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad8::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3,  4, 5, 6, 7},
                                    {3, 0, 1, 2,  7, 4, 5, 6},
                                    {2, 3, 0, 1,  6, 7, 4, 5},
                                    {1, 2, 3, 0,  5, 6, 7, 4},
                                    {0, 3, 2, 1,  7, 6, 5, 4},
                                    {3, 2, 1, 0,  6, 5, 4, 7},
                                    {2, 1, 0, 3,  5, 4, 7, 6},
                                    {1, 0, 3, 2,  4, 7, 6, 5}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_9_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad9::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3,  4, 5, 6, 7,  8},
                                    {3, 0, 1, 2,  7, 4, 5, 6,  8},
                                    {2, 3, 0, 1,  6, 7, 4, 5,  8},
                                    {1, 2, 3, 0,  5, 6, 7, 4,  8},
                                    {0, 3, 2, 1,  7, 6, 5, 4,  8},
                                    {3, 2, 1, 0,  6, 5, 4, 7,  8},
                                    {2, 1, 0, 3,  5, 4, 7, 6,  8},
                                    {1, 0, 3, 2,  4, 7, 6, 5,  8}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* particle_factory()
    {
      static TopologyMapEntry entry(Ioss::Sphere::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,true,true});
        entry.set_permutation_data(1, 1, {{0}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* beam_2_factory()
    {
      static TopologyMapEntry entry(Ioss::Beam2::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true ,true });
        entry.set_permutation_data(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({line_2_factory(), line_2_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* beam_3_factory()
    {
      static TopologyMapEntry entry(Ioss::Beam3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true ,true });
        entry.set_permutation_data(2, 1, {{0, 1, 2}, {1, 0, 2}});
        entry.set_side_topologies({line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* shell_line_2_factory()
    {
      static TopologyMapEntry entry(Ioss::ShellLine2D2::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,true ,false });
        entry.set_permutation_data(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({line_2_factory(), line_2_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* shell_line_3_factory()
    {
      static TopologyMapEntry entry(Ioss::ShellLine2D3::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,true,false});
        entry.set_permutation_data(2, 1, {{0, 1, 2}, {1, 0, 2}});
        entry.set_side_topologies({line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* spring_2_factory()
    {
      static TopologyMapEntry entry(Ioss::Spring2::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,true,true});
        entry.set_permutation_data(2, 2, {{0, 1}, {1, 0}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* spring_3_factory()
    {
      static TopologyMapEntry entry(Ioss::Spring3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,true,true});
        entry.set_permutation_data(2, 2, {{0, 1, 2}, {1, 0, 2}});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tri_3_2d_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,false});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2},
                                    {2, 0, 1},
                                    {1, 2, 0},
                                    {0, 2, 1},
                                    {2, 1, 0},
                                    {1, 0, 2}});
        entry.set_side_topologies({line_2_factory(), line_2_factory(), line_2_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tri_4_2d_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,false});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2, 3},
                                    {2, 0, 1, 3},
                                    {1, 2, 0, 3},
                                    {0, 2, 1, 3},
                                    {2, 1, 0, 3},
                                    {1, 0, 2, 3}});
        entry.set_side_topologies({line_2_factory(), line_2_factory(), line_2_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tri_6_2d_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri6::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,false});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2,  3, 4, 5},
                                    {2, 0, 1,  5, 3, 4},
                                    {1, 2, 0,  4, 5, 3},
                                    {0, 2, 1,  5, 4, 3},
                                    {2, 1, 0,  4, 3, 5},
                                    {1, 0, 2,  3, 5, 4}});
        entry.set_side_topologies({line_3_factory(), line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_4_2d_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,false});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3},
                                    {3, 0, 1, 2},
                                    {2, 3, 0, 1},
                                    {1, 2, 3, 0},
                                    {0, 3, 2, 1},
                                    {3, 2, 1, 0},
                                    {2, 1, 0, 3},
                                    {1, 0, 3, 2}});
        entry.set_side_topologies({line_2_factory(), line_2_factory(), line_2_factory(), line_2_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_8_2d_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad8::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,false});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3,  4, 5, 6, 7},
                                    {3, 0, 1, 2,  7, 4, 5, 6},
                                    {2, 3, 0, 1,  6, 7, 4, 5},
                                    {1, 2, 3, 0,  5, 6, 7, 4},
                                    {0, 3, 2, 1,  7, 6, 5, 4},
                                    {3, 2, 1, 0,  6, 5, 4, 7},
                                    {2, 1, 0, 3,  5, 4, 7, 6},
                                    {1, 0, 3, 2,  4, 7, 6, 5}});
        entry.set_side_topologies({line_3_factory(), line_3_factory(), line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_9_2d_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad9::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3,  4, 5, 6, 7,  8},
                                    {3, 0, 1, 2,  7, 4, 5, 6,  8},
                                    {2, 3, 0, 1,  6, 7, 4, 5,  8},
                                    {1, 2, 3, 0,  5, 6, 7, 4,  8},
                                    {0, 3, 2, 1,  7, 6, 5, 4,  8},
                                    {3, 2, 1, 0,  6, 5, 4, 7,  8},
                                    {2, 1, 0, 3,  5, 4, 7, 6,  8},
                                    {1, 0, 3, 2,  4, 7, 6, 5,  8}});
        entry.set_side_topologies({line_3_factory(), line_3_factory(), line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* shell_tri_3_factory()
    {
      static TopologyMapEntry entry(Ioss::TriShell3::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2},
                                    {2, 0, 1},
                                    {1, 2, 0},
                                    {0, 2, 1},
                                    {2, 1, 0},
                                    {1, 0, 2}});
        entry.set_side_topologies({tri_3_factory(), tri_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* shell_tri_4_factory()
    {
      static TopologyMapEntry entry(Ioss::TriShell4::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2, 3},
                                    {2, 0, 1, 3},
                                    {1, 2, 0, 3},
                                    {0, 2, 1, 3},
                                    {2, 1, 0, 3},
                                    {1, 0, 2, 3}});
        entry.set_side_topologies({tri_4_factory(), tri_4_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* shell_tri_6_factory()
    {
      static TopologyMapEntry entry(Ioss::TriShell6::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 3,
                                   {{0, 1, 2,  3, 4, 5},
                                    {2, 0, 1,  5, 3, 4},
                                    {1, 2, 0,  4, 5, 3},
                                    {0, 2, 1,  5, 4, 3},
                                    {2, 1, 0,  4, 3, 5},
                                    {1, 0, 2,  3, 5, 4}});
        entry.set_side_topologies({tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* shell_quad_4_factory()
    {
      static TopologyMapEntry entry(Ioss::Shell4::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3},
                                    {3, 0, 1, 2},
                                    {2, 3, 0, 1},
                                    {1, 2, 3, 0},
                                    {0, 3, 2, 1},
                                    {3, 2, 1, 0},
                                    {2, 1, 0, 3},
                                    {1, 0, 3, 2}});
        entry.set_side_topologies({quad_4_factory(), quad_4_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* shell_quad_8_factory()
    {
      static TopologyMapEntry entry(Ioss::Shell8::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3,  4, 5, 6, 7},
                                    {3, 0, 1, 2,  7, 4, 5, 6},
                                    {2, 3, 0, 1,  6, 7, 4, 5},
                                    {1, 2, 3, 0,  5, 6, 7, 4},
                                    {0, 3, 2, 1,  7, 6, 5, 4},
                                    {3, 2, 1, 0,  6, 5, 4, 7},
                                    {2, 1, 0, 3,  5, 4, 7, 6},
                                    {1, 0, 3, 2,  4, 7, 6, 5}});
        entry.set_side_topologies({quad_8_factory(), quad_8_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* shell_quad_9_factory()
    {
      static TopologyMapEntry entry(Ioss::Shell9::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(8, 4,
                                   {{0, 1, 2, 3,  4, 5, 6, 7,  8},
                                    {3, 0, 1, 2,  7, 4, 5, 6,  8},
                                    {2, 3, 0, 1,  6, 7, 4, 5,  8},
                                    {1, 2, 3, 0,  5, 6, 7, 4,  8},
                                    {0, 3, 2, 1,  7, 6, 5, 4,  8},
                                    {3, 2, 1, 0,  6, 5, 4, 7,  8},
                                    {2, 1, 0, 3,  5, 4, 7, 6,  8},
                                    {1, 0, 3, 2,  4, 7, 6, 5,  8}});
        entry.set_side_topologies({quad_9_factory(), quad_9_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tet_4_factory()
    {
      static TopologyMapEntry entry(Ioss::Tet4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(12, 12,
                                   {{0, 1, 2, 3},
                                    {1, 2, 0, 3},
                                    {2, 0, 1, 3},
                                    {0, 3, 1, 2},
                                    {3, 1, 0, 2},
                                    {1, 0, 3, 2},
                                    {0, 2, 3, 1},
                                    {2, 3, 0, 1},
                                    {3, 0, 2, 1},
                                    {1, 3, 2, 0},
                                    {3, 2, 1, 0},
                                    {2, 1, 3, 0}});
        entry.set_side_topologies({tri_3_factory(), tri_3_factory(), tri_3_factory(), tri_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tet_8_factory()
    {
      static TopologyMapEntry entry(Ioss::Tet8::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(1, 1, {{0, 1, 2, 3,  4, 5, 6, 7}});
        entry.set_side_topologies({tri_4_factory(), tri_4_factory(), tri_4_factory(), tri_4_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tet_10_factory()
    {
      static TopologyMapEntry entry(Ioss::Tet10::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(12, 12,
                                   {{0, 1, 2, 3,  4, 5, 6, 7, 8, 9},
                                    {1, 2, 0, 3,  5, 6, 4, 8, 9, 7},
                                    {2, 0, 1, 3,  6, 4, 5, 9, 7, 8},
                                    {0, 3, 1, 2,  7, 8, 4, 6, 9, 5},
                                    {3, 1, 0, 2,  8, 4, 7, 9, 5, 6},
                                    {1, 0, 3, 2,  4, 7, 8, 5, 6, 9},
                                    {0, 2, 3, 1,  6, 9, 7, 4, 5, 8},
                                    {2, 3, 0, 1,  9, 7, 6, 5, 8, 4},
                                    {3, 0, 2, 1,  7, 6, 9, 8, 4, 5},
                                    {1, 3, 2, 0,  8, 9, 5, 4, 7, 6},
                                    {3, 2, 1, 0,  9, 5, 8, 7, 6, 4},
                                    {2, 1, 3, 0,  5, 8, 9, 6, 4, 7}});
        entry.set_side_topologies({tri_6_factory(), tri_6_factory(), tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tet_11_factory()
    {
      static TopologyMapEntry entry(Ioss::Tet11::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(12, 12,
                                   {{0, 1, 2, 3,  4, 5, 6, 7, 8, 9,  10},
                                    {1, 2, 0, 3,  5, 6, 4, 8, 9, 7,  10},
                                    {2, 0, 1, 3,  6, 4, 5, 9, 7, 8,  10},
                                    {0, 3, 1, 2,  7, 8, 4, 6, 9, 5,  10},
                                    {3, 1, 0, 2,  8, 4, 7, 9, 5, 6,  10},
                                    {1, 0, 3, 2,  4, 7, 8, 5, 6, 9,  10},
                                    {0, 2, 3, 1,  6, 9, 7, 4, 5, 8,  10},
                                    {2, 3, 0, 1,  9, 7, 6, 5, 8, 4,  10},
                                    {3, 0, 2, 1,  7, 6, 9, 8, 4, 5,  10},
                                    {1, 3, 2, 0,  8, 9, 5, 4, 7, 6,  10},
                                    {3, 2, 1, 0,  9, 5, 8, 7, 6, 4,  10},
                                    {2, 1, 3, 0,  5, 8, 9, 6, 4, 7,  10}});
        entry.set_side_topologies({tri_6_factory(), tri_6_factory(), tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* pyramid_5_factory()
    {
      static TopologyMapEntry entry(Ioss::Pyramid5::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(4, 4,
                                   {{0, 1, 2, 3, 4},
                                    {1, 2, 3, 0, 4},
                                    {2, 3, 0, 1, 4},
                                    {3, 0, 1, 2, 4}});
        entry.set_side_topologies({tri_3_factory(), tri_3_factory(), tri_3_factory(), tri_3_factory(),
                                   quad_4_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* pyramid_13_factory()
    {
      static TopologyMapEntry entry(Ioss::Pyramid13::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(4, 4,
                                   {{0, 1, 2, 3, 4,  5, 6, 7, 8,   9, 10, 11, 12},
                                    {1, 2, 3, 0, 4,  6, 7, 8, 5,  10, 11, 12,  9},
                                    {2, 3, 0, 1, 4,  7, 8, 5, 6,  11, 12,  9, 10},
                                    {3, 0, 1, 2, 4,  8, 5, 6, 7,  12,  9, 10, 11}});
        entry.set_side_topologies({tri_6_factory(), tri_6_factory(), tri_6_factory(), tri_6_factory(),
                                   quad_8_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* pyramid_14_factory()
    {
      static TopologyMapEntry entry(Ioss::Pyramid14::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(4, 4,
                                   {{0, 1, 2, 3, 4,  5, 6, 7, 8,   9, 10, 11, 12,  13},
                                    {1, 2, 3, 0, 4,  6, 7, 8, 5,  10, 11, 12,  9,  13},
                                    {2, 3, 0, 1, 4,  7, 8, 5, 6,  11, 12,  9, 10,  13},
                                    {3, 0, 1, 2, 4,  8, 5, 6, 7,  12,  9, 10, 11,  13}});
        entry.set_side_topologies({tri_6_factory(), tri_6_factory(), tri_6_factory(), tri_6_factory(),
                                   quad_9_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* wedge_6_factory()
    {
      static TopologyMapEntry entry(Ioss::Wedge6::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 6,
                                   {{0, 1, 2, 3, 4, 5},
                                    {1, 2, 0, 4, 5, 3},
                                    {2, 0, 1, 5, 3, 4},
                                    {3, 5, 4, 0, 2, 1},
                                    {5, 4, 3, 2, 1, 0},
                                    {4, 3, 5, 1, 0, 2}});
        entry.set_side_topologies({quad_4_factory(), quad_4_factory(), quad_4_factory(),
                                   tri_3_factory(), tri_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* wedge_12_factory()
    {
      static TopologyMapEntry entry(Ioss::Wedge12::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 6,
                                   {{0, 1, 2, 3, 4, 5,  6,  7,  8,  9, 10, 11},
                                    {1, 2, 0, 4, 5, 3,  7,  8,  6, 10, 11,  9},
                                    {2, 0, 1, 5, 3, 4,  8,  6,  7, 11,  9, 10},
                                    {3, 5, 4, 0, 2, 1,  9, 11, 10,  8,  7,  6},
                                    {5, 4, 3, 2, 1, 0, 11, 10,  9,  7,  6,  8},
                                    {4, 3, 5, 1, 0, 2, 10,  9, 11,  6,  8,  7}});
        entry.set_side_topologies({quad_6_factory(), quad_6_factory(), quad_6_factory(),
                                   tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* wedge_18_factory()
    {
      static TopologyMapEntry entry(Ioss::Wedge18::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 6,
                                   {{0, 1, 2, 3, 4, 5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17},
                                    {1, 2, 0, 4, 5, 3,  7,  8,  6, 10, 11,  9, 13, 14, 12, 16, 17, 15},
                                    {2, 0, 1, 5, 3, 4,  8,  6,  7, 11,  9, 10, 14, 12, 13, 17, 15, 16},
                                    {3, 5, 4, 0, 2, 1, 14, 13, 12,  9, 11, 10,  8,  7,  6, 17, 16, 15},
                                    {5, 4, 3, 2, 1, 0, 13, 12, 14, 11, 10,  9,  7,  6,  8, 16, 15, 17},
                                    {4, 3, 5, 1, 0, 2, 12, 14, 13, 10,  9, 11,  6,  8,  7, 15, 17, 16}});
        entry.set_side_topologies({quad_9_factory(), quad_9_factory(), quad_9_factory(),
                                   tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* wedge_15_factory()
    {
      static TopologyMapEntry entry(Ioss::Wedge15::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(6, 6,
                                   {{0, 1, 2, 3, 4, 5,  6,  7,  8,  9, 10, 11, 12, 13, 14},
                                    {1, 2, 0, 4, 5, 3,  7,  8,  6, 10, 11,  9, 13, 14, 12},
                                    {2, 0, 1, 5, 3, 4,  8,  6,  7, 11,  9, 10, 14, 12, 13},
                                    {3, 5, 4, 0, 2, 1, 14, 13, 12,  9, 11, 10,  8,  7,  6},
                                    {5, 4, 3, 2, 1, 0, 13, 12, 14, 11, 10,  9,  7,  6,  8},
                                    {4, 3, 5, 1, 0, 2, 12, 14, 13, 10,  9, 11,  6,  8,  7}});
        entry.set_side_topologies({quad_8_factory(), quad_8_factory(), quad_8_factory(),
                                   tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* hex_8_factory()
    {
      static TopologyMapEntry entry(Ioss::Hex8::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(24, 24,
                                   {{0, 1, 2, 3, 4, 5, 6, 7},
                                    {0, 1, 5, 4, 3, 2, 6, 7},
                                    {0, 4, 7, 3, 1, 5, 6, 2},
                                    {1, 2, 3, 0, 5, 6, 7, 4},
                                    {1, 2, 6, 5, 0, 3, 7, 4},
                                    {1, 5, 4, 0, 2, 6, 7, 3},
                                    {2, 3, 0, 1, 6, 7, 4, 5},
                                    {2, 3, 7, 6, 1, 0, 4, 5},
                                    {2, 6, 5, 1, 3, 7, 4, 0},
                                    {3, 0, 1, 2, 7, 4, 5, 6},
                                    {3, 0, 4, 7, 2, 1, 5, 6},
                                    {3, 7, 6, 2, 0, 4, 5, 1},
                                    {4, 0, 1, 5, 7, 3, 2, 6},
                                    {4, 7, 3, 0, 5, 6, 2, 1},
                                    {4, 7, 6, 5, 0, 3, 2, 1},
                                    {5, 1, 2, 6, 4, 0, 3, 7},
                                    {5, 4, 0, 1, 6, 7, 3, 2},
                                    {5, 4, 7, 6, 1, 0, 3, 2},
                                    {6, 2, 3, 7, 5, 1, 0, 4},
                                    {6, 5, 1, 2, 7, 4, 0, 3},
                                    {6, 5, 4, 7, 2, 1, 0, 3},
                                    {7, 3, 0, 4, 6, 2, 1, 5},
                                    {7, 6, 2, 3, 4, 5, 1, 0},
                                    {7, 6, 5, 4, 3, 2, 1, 0}});
        entry.set_side_topologies({quad_4_factory(), quad_4_factory(), quad_4_factory(),
                                   quad_4_factory(), quad_4_factory(), quad_4_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* hex_20_factory()
    {
      static TopologyMapEntry entry(Ioss::Hex20::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(24, 24,
                                   {{0, 1, 2, 3, 4, 5, 6, 7,   8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19},
                                    {0, 1, 5, 4, 3, 2, 6, 7,   8, 13, 16, 12, 11,  9, 17, 19, 10, 14, 18, 15},
                                    {0, 4, 7, 3, 1, 5, 6, 2,  12, 19, 15, 11,  8, 16, 18, 10, 13, 17, 14,  9},
                                    {1, 2, 3, 0, 5, 6, 7, 4,   9, 10, 11,  8, 13, 14, 15, 12, 17, 18, 19, 16},
                                    {1, 2, 6, 5, 0, 3, 7, 4,   9, 14, 17, 13,  8, 10, 18, 16, 11, 15, 19, 12},
                                    {1, 5, 4, 0, 2, 6, 7, 3,  13, 16, 12,  8,  9, 17, 19, 11, 14, 18, 15, 10},
                                    {2, 3, 0, 1, 6, 7, 4, 5,  10, 11,  8,  9, 14, 15, 12, 13, 18, 19, 16, 17},
                                    {2, 3, 7, 6, 1, 0, 4, 5,  10, 15, 18, 14,  9, 11, 19, 17,  8, 12, 16, 13},
                                    {2, 6, 5, 1, 3, 7, 4, 0,  14, 17, 13,  9, 10, 18, 16,  8, 15, 19, 12, 11},
                                    {3, 0, 1, 2, 7, 4, 5, 6,  11,  8,  9, 10, 15, 12, 13, 14, 19, 16, 17, 18},
                                    {3, 0, 4, 7, 2, 1, 5, 6,  11, 12, 19, 15, 10,  8, 16, 18,  9, 13, 17, 14},
                                    {3, 7, 6, 2, 0, 4, 5, 1,  15, 18, 14, 10, 11, 19, 17,  9, 12, 16, 13,  8},
                                    {4, 0, 1, 5, 7, 3, 2, 6,  12,  8, 13, 16, 19, 11,  9, 17, 15, 10, 14, 18},
                                    {4, 7, 3, 0, 5, 6, 2, 1,  19, 15, 11, 12, 16, 18, 10,  8, 17, 14,  9, 13},
                                    {4, 7, 6, 5, 0, 3, 2, 1,  19, 18, 17, 16, 12, 15, 14, 13, 11, 10,  9,  8},
                                    {5, 1, 2, 6, 4, 0, 3, 7,  13,  9, 14, 17, 16,  8, 10, 18, 12, 11, 15, 19},
                                    {5, 4, 0, 1, 6, 7, 3, 2,  16, 12,  8, 13, 17, 19, 11,  9, 18, 15, 10, 14},
                                    {5, 4, 7, 6, 1, 0, 3, 2,  16, 19, 18, 17, 13, 12, 15, 14,  8, 11, 10,  9},
                                    {6, 2, 3, 7, 5, 1, 0, 4,  14, 10, 15, 18, 17,  9, 11, 19, 13,  8, 12, 16},
                                    {6, 5, 1, 2, 7, 4, 0, 3,  17, 13,  9, 14, 18, 16,  8, 10, 19, 12, 11, 15},
                                    {6, 5, 4, 7, 2, 1, 0, 3,  17, 16, 19, 18, 14, 13, 12, 15,  9,  8, 11, 10},
                                    {7, 3, 0, 4, 6, 2, 1, 5,  15, 11, 12, 19, 18, 10,  8, 16, 14,  9, 13, 17},
                                    {7, 6, 2, 3, 4, 5, 1, 0,  18, 14, 10, 15, 19, 17,  9, 11, 16, 13,  8, 12},
                                    {7, 6, 5, 4, 3, 2, 1, 0,  18, 17, 16, 19, 15, 14, 13, 12, 10,  9,  8, 11}});
        entry.set_side_topologies({quad_8_factory(), quad_8_factory(), quad_8_factory(),
                                   quad_8_factory(), quad_8_factory(), quad_8_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* hex_27_factory()
    {
      static TopologyMapEntry entry(Ioss::Hex27::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        entry.set_permutation_data(24, 24,
                                   {{0, 1, 2, 3, 4, 5, 6, 7,   8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,  20, 21, 22, 23, 24, 25, 26},
                                    {0, 1, 5, 4, 3, 2, 6, 7,   8, 13, 16, 12, 11,  9, 17, 19, 10, 14, 18, 15,  20, 25, 26, 23, 24, 21, 22},
                                    {0, 4, 7, 3, 1, 5, 6, 2,  12, 19, 15, 11,  8, 16, 18, 10, 13, 17, 14,  9,  20, 23, 24, 21, 22, 25, 26},
                                    {1, 2, 3, 0, 5, 6, 7, 4,   9, 10, 11,  8, 13, 14, 15, 12, 17, 18, 19, 16,  20, 21, 22, 25, 26, 24, 23},
                                    {1, 2, 6, 5, 0, 3, 7, 4,   9, 14, 17, 13,  8, 10, 18, 16, 11, 15, 19, 12,  20, 24, 23, 25, 26, 21, 22},
                                    {1, 5, 4, 0, 2, 6, 7, 3,  13, 16, 12,  8,  9, 17, 19, 11, 14, 18, 15, 10,  20, 25, 26, 21, 22, 24, 23},
                                    {2, 3, 0, 1, 6, 7, 4, 5,  10, 11,  8,  9, 14, 15, 12, 13, 18, 19, 16, 17,  20, 21, 22, 24, 23, 26, 25},
                                    {2, 3, 7, 6, 1, 0, 4, 5,  10, 15, 18, 14,  9, 11, 19, 17,  8, 12, 16, 13,  20, 26, 25, 24, 23, 21, 22},
                                    {2, 6, 5, 1, 3, 7, 4, 0,  14, 17, 13,  9, 10, 18, 16,  8, 15, 19, 12, 11,  20, 24, 23, 21, 22, 26, 25},
                                    {3, 0, 1, 2, 7, 4, 5, 6,  11,  8,  9, 10, 15, 12, 13, 14, 19, 16, 17, 18,  20, 21, 22, 26, 25, 23, 24},
                                    {3, 0, 4, 7, 2, 1, 5, 6,  11, 12, 19, 15, 10,  8, 16, 18,  9, 13, 17, 14,  20, 23, 24, 26, 25, 21, 22},
                                    {3, 7, 6, 2, 0, 4, 5, 1,  15, 18, 14, 10, 11, 19, 17,  9, 12, 16, 13,  8,  20, 26, 25, 21, 22, 23, 24},
                                    {4, 0, 1, 5, 7, 3, 2, 6,  12,  8, 13, 16, 19, 11,  9, 17, 15, 10, 14, 18,  20, 25, 26, 22, 21, 23, 24},
                                    {4, 7, 3, 0, 5, 6, 2, 1,  19, 15, 11, 12, 16, 18, 10,  8, 17, 14,  9, 13,  20, 23, 24, 25, 26, 22, 21},
                                    {4, 7, 6, 5, 0, 3, 2, 1,  19, 18, 17, 16, 12, 15, 14, 13, 11, 10,  9,  8,  20, 22, 21, 25, 26, 23, 24},
                                    {5, 1, 2, 6, 4, 0, 3, 7,  13,  9, 14, 17, 16,  8, 10, 18, 12, 11, 15, 19,  20, 24, 23, 22, 21, 25, 26},
                                    {5, 4, 0, 1, 6, 7, 3, 2,  16, 12,  8, 13, 17, 19, 11,  9, 18, 15, 10, 14,  20, 25, 26, 24, 23, 22, 21},
                                    {5, 4, 7, 6, 1, 0, 3, 2,  16, 19, 18, 17, 13, 12, 15, 14,  8, 11, 10,  9,  20, 22, 21, 24, 23, 25, 26},
                                    {6, 2, 3, 7, 5, 1, 0, 4,  14, 10, 15, 18, 17,  9, 11, 19, 13,  8, 12, 16,  20, 26, 25, 22, 21, 24, 23},
                                    {6, 5, 1, 2, 7, 4, 0, 3,  17, 13,  9, 14, 18, 16,  8, 10, 19, 12, 11, 15,  20, 24, 23, 26, 25, 22, 21},
                                    {6, 5, 4, 7, 2, 1, 0, 3,  17, 16, 19, 18, 14, 13, 12, 15,  9,  8, 11, 10,  20, 22, 21, 26, 25, 24, 23},
                                    {7, 3, 0, 4, 6, 2, 1, 5,  15, 11, 12, 19, 18, 10,  8, 16, 14,  9, 13, 17,  20, 23, 24, 22, 21, 26, 25},
                                    {7, 6, 2, 3, 4, 5, 1, 0,  18, 14, 10, 15, 19, 17,  9, 11, 16, 13,  8, 12,  20, 26, 25, 23, 24, 22, 21},
                                    {7, 6, 5, 4, 3, 2, 1, 0,  18, 17, 16, 19, 15, 14, 13, 12, 10,  9,  8, 11,  20, 22, 21, 23, 24, 26, 25}});
        entry.set_side_topologies({quad_9_factory(), quad_9_factory(), quad_9_factory(),
                                   quad_9_factory(), quad_9_factory(), quad_9_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

  private:
    bool equivalent_valid_spatial_dimensions(const DimensionArray &validSpatialDimensions_) const
    {
      return validSpatialDimensions[0] == validSpatialDimensions_[0] &&
             validSpatialDimensions[1] == validSpatialDimensions_[1] &&
             validSpatialDimensions[2] == validSpatialDimensions_[2] &&
             validSpatialDimensions[3] == validSpatialDimensions_[3];
    }

    void set_valid_spatial_dimensions(const DimensionArray &validSpatialDimensions_)
    {
      validSpatialDimensions[0] = validSpatialDimensions_[0];
      validSpatialDimensions[1] = validSpatialDimensions_[1];
      validSpatialDimensions[2] = validSpatialDimensions_[2];
      validSpatialDimensions[3] = validSpatialDimensions_[3];
    }

    void set_shell_flag(bool isShell_) {isShell = isShell_;}

    void set_side_topologies(const std::vector<TopologyMapEntry*>& sideTopologies_)
    {
      assert(static_cast<int>(sideTopologies_.size()) == num_sides());

      for(int side=1; side<=num_sides(); side++) {
        assert(topology->boundary_type(side) == sideTopologies_[side-1]->topology);
      }

      sideTopologies = sideTopologies_;
    }

    void set_permutation_data(uint8_t numPermutations_, uint8_t numPositivePermutations_,
                              const std::vector<std::vector<uint8_t>>& permutationNodeOrdinals_)
    {
      assert(permutationNodeOrdinals_.size() == numPermutations_);
      assert(numPositivePermutations_ <= numPermutations_);

      numPermutations = numPermutations_;
      numPositivePermutations = numPositivePermutations_;

      for(const auto& ordinals : permutationNodeOrdinals_) {
        assert(static_cast<int>(ordinals.size()) == num_nodes());

        for(const auto ordinal : ordinals) {
          assert(ordinal < num_nodes());
        }
      }

      permutationNodeOrdinals = permutationNodeOrdinals_;
    }

    void set_topology(const std::string &name)
    {
      id = Ioss::ElementTopology::get_unique_id(name);
      topology = Ioss::ElementTopology::factory(name);
    }

    unsigned int           id;
    Ioss::ElementTopology *topology;

    bool isShell;
    std::vector<TopologyMapEntry*> sideTopologies;

    uint8_t numPermutations;
    uint8_t numPositivePermutations;
    std::vector<std::vector<uint8_t>> permutationNodeOrdinals;

    // Defines what spatial dimension the topology is valid on
    DimensionArray validSpatialDimensions;

    bool initialized;
  };

  inline std::ostream &operator<<(std::ostream &out, const TopologyMapEntry &t)
  {
    return out << t.name();
  }

  class IossTopologyMapping : public text_mesh::TopologyMapping<TopologyMapEntry>
  {
  public:
    TopologyMapEntry invalid_topology() const override { return TopologyMapEntry(); }

    // clang-format off
  void initialize_topology_map() override
  {
    m_nameToTopology = {
        {"NODE",         *TopologyMapEntry::node_factory()},
        {"LINE_2",       *TopologyMapEntry::line_2_factory()},
        {"LINE_3",       *TopologyMapEntry::line_3_factory()},
        {"TRI_3",        *TopologyMapEntry::tri_3_factory()},
        {"TRI_4",        *TopologyMapEntry::tri_4_factory()},
        {"TRI_6",        *TopologyMapEntry::tri_6_factory()},
        {"QUAD_4",       *TopologyMapEntry::quad_4_factory()},
        {"QUAD_6",       *TopologyMapEntry::quad_6_factory()},
        {"QUAD_8",       *TopologyMapEntry::quad_8_factory()},
        {"QUAD_9",       *TopologyMapEntry::quad_9_factory()},
        {"PARTICLE",     *TopologyMapEntry::particle_factory()},
        {"LINE_2_1D",    *TopologyMapEntry::line_2_1d_factory()},
        {"LINE_3_1D",    *TopologyMapEntry::line_3_1d_factory()},
        {"BEAM_2",       *TopologyMapEntry::beam_2_factory()},
        {"BEAM_3",       *TopologyMapEntry::beam_3_factory()},
        {"SHELL_LINE_2", *TopologyMapEntry::shell_line_2_factory()},
        {"SHELL_LINE_3", *TopologyMapEntry::shell_line_3_factory()},
        {"SPRING_2",     *TopologyMapEntry::spring_2_factory()},
        {"SPRING_3",     *TopologyMapEntry::spring_3_factory()},
        {"TRI_3_2D",     *TopologyMapEntry::tri_3_2d_factory()},
        {"TRI_4_2D",     *TopologyMapEntry::tri_4_2d_factory()},
        {"TRI_6_2D",     *TopologyMapEntry::tri_6_2d_factory()},
        {"QUAD_4_2D",    *TopologyMapEntry::quad_4_2d_factory()},
        {"QUAD_8_2D",    *TopologyMapEntry::quad_8_2d_factory()},
        {"QUAD_9_2D",    *TopologyMapEntry::quad_9_2d_factory()},
        {"SHELL_TRI_3",  *TopologyMapEntry::shell_tri_3_factory()},
        {"SHELL_TRI_4",  *TopologyMapEntry::shell_tri_4_factory()},
        {"SHELL_TRI_6",  *TopologyMapEntry::shell_tri_6_factory()},
        {"SHELL_QUAD_4", *TopologyMapEntry::shell_quad_4_factory()},
        {"SHELL_QUAD_8", *TopologyMapEntry::shell_quad_8_factory()},
        {"SHELL_QUAD_9", *TopologyMapEntry::shell_quad_9_factory()},
        {"TET_4",        *TopologyMapEntry::tet_4_factory()},
        {"TET_8",        *TopologyMapEntry::tet_8_factory()},
        {"TET_10",       *TopologyMapEntry::tet_10_factory()},
        {"TET_11",       *TopologyMapEntry::tet_11_factory()},
        {"PYRAMID_5",    *TopologyMapEntry::pyramid_5_factory()},
        {"PYRAMID_13",   *TopologyMapEntry::pyramid_13_factory()},
        {"PYRAMID_14",   *TopologyMapEntry::pyramid_14_factory()},
        {"WEDGE_6",      *TopologyMapEntry::wedge_6_factory()},
        {"WEDGE_12",     *TopologyMapEntry::wedge_12_factory()},
        {"WEDGE_15",     *TopologyMapEntry::wedge_15_factory()},
        {"WEDGE_18",     *TopologyMapEntry::wedge_18_factory()},
        {"HEX_8",        *TopologyMapEntry::hex_8_factory()},
        {"HEX_20",       *TopologyMapEntry::hex_20_factory()},
        {"HEX_27",       *TopologyMapEntry::hex_27_factory()}
    };
  }
    // clang-format on
  };
} // namespace Iotm
