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

#include <fmt/ostream.h>
#include <assert.h>

#include "Ioss_Utils.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_StandardElementTypes.h"

#include "Iotm_TextMeshDataTypes.h"

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
        // Only interested in face boundaries, not edges
        if (topology->parametric_dimension() == 2) {
          return topology->number_faces();
        }
      }

      return topology->number_boundaries();
    }

    // Side references are one-based
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

    // The number of positive permutations must be less than or equal to the total number of permutations
    unsigned num_positive_permutations() const { return numPositivePermutations; }

    // Permutation data is stored such that the positive permutations are listed first ... the order of the
    // positive permutations within that group is irrelevant. The remaining permutations listed after the
    // positive ones are the negative permutations hence, any permutation index outside of the positive
    // range is a negative permutation. By convention, the first permutation listed matches the default
    // listed in the Exodus manual
    bool is_positive_polarity(Permutation permutation) const { return permutation < numPositivePermutations; }

    // Permutation type is unsigned so only need to check upper bound
    bool valid_permutation(Permutation permutation) const { return permutation < numPermutations; }

    // For a validated permutation, return the node ordinals
    bool fill_permutation_indices(Permutation permutation, std::vector<Ordinal>& nodeOrdinalVector) const
    {
      if (!valid_permutation(permutation)) return false;

      nodeOrdinalVector.resize(num_permutation_nodes());
      const auto& ordinals = permutationNodeOrdinals[permutation];
      for(unsigned i=0; i<num_permutation_nodes(); i++) {
        nodeOrdinalVector[i] = ordinals[i];
      }

      return true;
    }

    // For a given permutation, return the node ordinals
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

    // Node with no permutation and no sides
    static TopologyMapEntry* node_factory()
    {
      static TopologyMapEntry entry(Ioss::Node::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,true,true});
        entry.set_low_order_permutation(0, 0, {});
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::LINE -- topology::EDGE_RANK
    // 2 nodes with no sides defined in 2D/3D
    //***************************************************************************
    static TopologyMapEntry* line_2_factory()
    {
      static TopologyMapEntry entry(Ioss::Edge2::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,true});
        entry.set_low_order_permutation(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::LINE 1D -- topology::ELEMENT_RANK
    // 2 nodes with no sides only defined on 1d problems
    //***************************************************************************
    static TopologyMapEntry* line_2_1d_factory()
    {
      static TopologyMapEntry entry(Ioss::Edge2::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,false,false});
        entry.set_low_order_permutation(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::LINE -- topology::EDGE_RANK
    // 3 nodes with no sides defined in 2D/3D
    //***************************************************************************
    static TopologyMapEntry* line_3_factory()
    {
      static TopologyMapEntry entry(Ioss::Edge3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,true});
        entry.set_low_order_permutation(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::LINE 1D -- topology::ELEMENT_RANK
    // 3 nodes with no sides only defined on 1d problems
    //***************************************************************************
    static TopologyMapEntry* line_3_1d_factory()
    {
      static TopologyMapEntry entry(Ioss::Edge3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,false,false});
        entry.set_low_order_permutation(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::TRIANGLE -- topology::FACE_RANK
    // defined on spatial dimension 3d
    // 3, 4, or 6 nodes with 3 edges and no sides
    //***************************************************************************
    static TopologyMapEntry* tri_3_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_tri3_permutation(entry);
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tri_4_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_tri3_permutation(entry);
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* tri_6_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri6::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_tri3_permutation(entry);
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::QUADRILATERAL -- topology::FACE_RANK
    // defined on spatial dimension 3d
    // 4, 8, or 9 nodes with 4 edges and no sides
    //***************************************************************************
    static TopologyMapEntry* quad_4_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_quad4_permutation(entry);
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_6_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad6::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_quad4_permutation(entry);
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_8_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad8::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_quad4_permutation(entry);
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* quad_9_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad9::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_quad4_permutation(entry);
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // PARTICLE -- topology::ELEMENT_RANK
    // one node with no sides
    //***************************************************************************
    static TopologyMapEntry* particle_factory()
    {
      static TopologyMapEntry entry(Ioss::Sphere::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,true,true});
        entry.set_low_order_permutation(1, 1, {{0}});
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::BEAM_2 -- topology::ELEMENT_RANK
    // 2 nodes with 2 sides defined in 2D/3D
    //***************************************************************************
    static TopologyMapEntry* beam_2_factory()
    {
      static TopologyMapEntry entry(Ioss::Beam2::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true ,true });
        entry.set_low_order_permutation(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({line_2_factory(), line_2_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::BEAM_3 -- topology::ELEMENT_RANK
    // 3 nodes with 2 sides defined in 2D/3D
    //***************************************************************************
    static TopologyMapEntry* beam_3_factory()
    {
      static TopologyMapEntry entry(Ioss::Beam3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true ,true });
        entry.set_low_order_permutation(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::SHELL_LINE -- topology::ELEMENT_RANK
    // only defined on 2d problems
    // 2 or 3 nodes with two edges and 2 sides
    //***************************************************************************

    static TopologyMapEntry* shell_line_2_factory()
    {
      static TopologyMapEntry entry(Ioss::ShellLine2D2::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,true ,false });
        entry.set_low_order_permutation(2, 1, {{0, 1}, {1, 0}});
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
        entry.set_low_order_permutation(2, 1, {{0, 1}, {1, 0}});
        entry.set_side_topologies({line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::SPRING -- topology::ELEM_RANK
    // 2 or 3 nodes with no sides
    //***************************************************************************

    static TopologyMapEntry* spring_2_factory()
    {
      static TopologyMapEntry entry(Ioss::Spring2::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,true,true});
        entry.set_low_order_permutation(2, 2, {{0, 1}, {1, 0}});
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    static TopologyMapEntry* spring_3_factory()
    {
      static TopologyMapEntry entry(Ioss::Spring3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,true,true,true});
        entry.set_low_order_permutation(2, 2, {{0, 1}, {1, 0}});
        entry.set_side_topologies({});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::TRIANGLE 2D -- topology::ELEMENT_RANK
    // defined on spatial dimension 2d
    // 3, 4, or 6 nodes with 3 edges and 3 sides
    //***************************************************************************
    static TopologyMapEntry* tri_3_2d_factory()
    {
      static TopologyMapEntry entry(Ioss::Tri3::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,false});
        set_tri3_permutation(entry);
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
        set_tri3_permutation(entry);
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
        set_tri3_permutation(entry);
        entry.set_side_topologies({line_3_factory(), line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::QUADRILATERAL 2D -- topology::ELEMENT_RANK
    // defined on spatial dimension 2d
    // 4, 8, or 9 nodes with 4 edges and 4 sides
    //***************************************************************************

    static TopologyMapEntry* quad_4_2d_factory()
    {
      static TopologyMapEntry entry(Ioss::Quad4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,true,false});
        set_quad4_permutation(entry);
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
        set_quad4_permutation(entry);
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
        set_quad4_permutation(entry);
        entry.set_side_topologies({line_3_factory(), line_3_factory(), line_3_factory(), line_3_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    //***************************************************************************
    // topology::SHELL topology::TRIANGLE -- topology::ELEMENT_RANK
    // defined on spatial dimension 3d
    // 3, 4, or 6 nodes with 3 edges and 2 sides
    //***************************************************************************

    static TopologyMapEntry* shell_tri_3_factory()
    {
      static TopologyMapEntry entry(Ioss::TriShell3::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_tri3_permutation(entry);
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
        set_tri3_permutation(entry);
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
        set_tri3_permutation(entry);
        entry.set_side_topologies({tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static void set_tri3_permutation(TopologyMapEntry &entry)
    {
      entry.set_low_order_permutation(6, 3,
                                      {{0, 1, 2},
                                       {2, 0, 1},
                                       {1, 2, 0},
                                       {0, 2, 1},
                                       {2, 1, 0},
                                       {1, 0, 2}});
    }

    //***************************************************************************
    //  topology::SHELL topology::QUADRILATERAL -- topology::ELEMENT_RANK
    // defined on spatial dimension 3d
    // 4, 8, or 9 nodes with 4 edges and 2 sides
    //***************************************************************************
    static TopologyMapEntry* shell_quad_4_factory()
    {
      static TopologyMapEntry entry(Ioss::Shell4::name);

      if(!entry.initialized) {
        entry.set_shell_flag(true);
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_quad4_permutation(entry);
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
        set_quad4_permutation(entry);
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
        set_quad4_permutation(entry);
        entry.set_side_topologies({quad_9_factory(), quad_9_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static void set_quad4_permutation(TopologyMapEntry &entry)
    {
      entry.set_low_order_permutation(8, 4,
                                      {{0, 1, 2, 3},
                                       {3, 0, 1, 2},
                                       {2, 3, 0, 1},
                                       {1, 2, 3, 0},
                                       {0, 3, 2, 1},
                                       {3, 2, 1, 0},
                                       {2, 1, 0, 3},
                                       {1, 0, 3, 2}});
    }

    //***************************************************************************
    // topology::TETRAHEDRON -- topology::ELEMENT_RANK
    // defined on spatial dimension 3d
    // 4, 8, 10 or 11 nodes with 4 sides
    //***************************************************************************
    static TopologyMapEntry* tet_4_factory()
    {
      static TopologyMapEntry entry(Ioss::Tet4::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_tet4_permutation(entry);
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
        set_tet4_permutation(entry);
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
        set_tet4_permutation(entry);
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
        set_tet4_permutation(entry);
        entry.set_side_topologies({tri_6_factory(), tri_6_factory(), tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static void set_tet4_permutation(TopologyMapEntry &entry)
    {
      entry.set_low_order_permutation(12, 12,
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
    }

    //***************************************************************************
    // topology::PYRAMID -- topology::ELEMENT_RANK
    // defined on spatial dimension 3d
    // 5, 13 or 14 nodes with 5 sides
    //***************************************************************************
    static TopologyMapEntry* pyramid_5_factory()
    {
      static TopologyMapEntry entry(Ioss::Pyramid5::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_pyramid5_permutation(entry);
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
        set_pyramid5_permutation(entry);
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
        set_pyramid5_permutation(entry);
        entry.set_side_topologies({tri_6_factory(), tri_6_factory(), tri_6_factory(), tri_6_factory(),
                                   quad_9_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static void set_pyramid5_permutation(TopologyMapEntry &entry)
    {
      entry.set_low_order_permutation(4, 4,
                                      {{0, 1, 2, 3, 4},
                                       {1, 2, 3, 0, 4},
                                       {2, 3, 0, 1, 4},
                                       {3, 0, 1, 2, 4}});
    }

    //***************************************************************************
    // topology::WEDGE -- topology::ELEMENT_RANK
    // defined on spatial dimension 3d
    // 6, 12, 15 or 18 nodes with 5 sides
    //***************************************************************************
    static TopologyMapEntry* wedge_6_factory()
    {
      static TopologyMapEntry entry(Ioss::Wedge6::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_wedge6_permutation(entry);
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
        set_wedge6_permutation(entry);
        entry.set_side_topologies({quad_6_factory(), quad_6_factory(), quad_6_factory(),
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
        set_wedge6_permutation(entry);
        entry.set_side_topologies({quad_8_factory(), quad_8_factory(), quad_8_factory(),
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
        set_wedge6_permutation(entry);
        entry.set_side_topologies({quad_9_factory(), quad_9_factory(), quad_9_factory(),
                                   tri_6_factory(), tri_6_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static void set_wedge6_permutation(TopologyMapEntry &entry)
    {
      entry.set_low_order_permutation(6, 6,
                                      {{0, 1, 2, 3, 4, 5},
                                       {1, 2, 0, 4, 5, 3},
                                       {2, 0, 1, 5, 3, 4},
                                       {3, 5, 4, 0, 2, 1},
                                       {5, 4, 3, 2, 1, 0},
                                       {4, 3, 5, 1, 0, 2}});
    }

    //***************************************************************************
    // topology::HEXAHEDRON -- topology::ELEMENT_RANK
    // defined on spatial dimension 3d
    // 8, 20 or 27 nodes nodes with 6 sides
    //***************************************************************************
    static TopologyMapEntry* hex_8_factory()
    {
      static TopologyMapEntry entry(Ioss::Hex8::name);

      if(!entry.initialized) {
        entry.set_valid_spatial_dimensions({false,false,false,true});
        set_hex8_permutation(entry);
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
        set_hex8_permutation(entry);
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
        set_hex8_permutation(entry);
        entry.set_side_topologies({quad_9_factory(), quad_9_factory(), quad_9_factory(),
                                   quad_9_factory(), quad_9_factory(), quad_9_factory()});
        entry.initialized = true;
      }

      return &entry;
    }

    static void set_hex8_permutation(TopologyMapEntry &entry)
    {
      entry.set_low_order_permutation(24, 24,
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

    // Mark topology as a shell
    void set_shell_flag(bool isShell_) {isShell = isShell_;}

    // Create and set TopologyMapEntry based side topologies since some of the implementation
    // details of the class cannot be automatically constructed from the Ioss_ElementTopology
    // object .. this will not be necessary if/when they are migrated to Ioss_ElementTopology
    void set_side_topologies(const std::vector<TopologyMapEntry*>& sideTopologies_)
    {
//      assert(static_cast<int>(sideTopologies_.size()) == num_sides());
      int numSides = sideTopologies_.size();

      for(int side=1; side<=numSides; side++) {
        assert(topology->boundary_type(side) == sideTopologies_[side-1]->topology);
      }

      sideTopologies = sideTopologies_;
    }

    unsigned num_permutation_nodes() const
    {
      return topology->number_corner_nodes();
    }

    // Store low order permutation data regarding this topology .. the positive permutations are listed first
    // If this topology is a high order topology, the data is only for the nodes of the associated low order
    // topology. This implies that any usage of this assumes that the higher order nodes are numbered correctly
    // relative to the low order nodes.
    //
    //        {{0, 1, 2,  3, 4, 5},               {{0, 1, 2},
    //         {2, 0, 1,  5, 3, 4},                {2, 0, 1},
    //  Tri6   {1, 2, 0,  4, 5, 3},   -->    Tri3  {1, 2, 0},
    //         {0, 2, 1,  5, 4, 3},                {0, 2, 1},
    //         {2, 1, 0,  4, 3, 5},                {2, 1, 0},
    //         {1, 0, 2,  3, 5, 4}}                {1, 0, 2}}

    void set_low_order_permutation(uint8_t numPermutations_, uint8_t numPositivePermutations_,
                                   const std::vector<std::vector<uint8_t>>& permutationNodeOrdinals_)
    {
      assert(permutationNodeOrdinals_.size() == numPermutations_);
      assert(numPositivePermutations_ <= numPermutations_);

      numPermutations = numPermutations_;
      numPositivePermutations = numPositivePermutations_;

      for(const auto& ordinals : permutationNodeOrdinals_) {
        if(ordinals.size() != num_permutation_nodes()) {
          std::ostringstream errmsg;
          fmt::print(errmsg,
              "ERROR: Number of low order permutation ordinals: {} for topology: {} does not match low order topology value: {}",
              ordinals.size(), topology->name(), num_permutation_nodes());
          IOSS_ERROR(errmsg);
        }

        for(const auto ordinal : ordinals) {
          if(ordinal >= num_permutation_nodes()) {
            std::ostringstream errmsg;
            fmt::print(errmsg,
                "ERROR: Invalid value of ordinal: {} for topology: {}", ordinal, topology->name());
            IOSS_ERROR(errmsg);
          }
        }
      }

      permutationNodeOrdinals = permutationNodeOrdinals_;
    }

    unsigned int           id{0};
    Ioss::ElementTopology *topology = nullptr;

    bool isShell {false};
    std::vector<TopologyMapEntry*> sideTopologies{};

    uint8_t numPermutations{0};
    uint8_t numPositivePermutations{0};
    std::vector<std::vector<uint8_t>> permutationNodeOrdinals{};

    // Defines what spatial dimension the topology is valid on
    DimensionArray validSpatialDimensions;

    bool initialized{false};
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
