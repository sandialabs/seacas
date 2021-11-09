// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#ifndef IOSS_Iotm_TextMesh_h
#define IOSS_Iotm_TextMesh_h

#include <Ioss_CodeTypes.h>
#include <Ioss_EntityType.h>  // for EntityType

#include <cstddef>  // for size_t
#include <cstdint>  // for int64_t
#include <map>      // for map, etc
#include <string>   // for string
#include <utility>  // for pair
#include <vector>   // for vector
#include <unordered_map>

#include "Ioss_ElementTopology.h"
#include "Ioss_StandardElementTypes.h"

#include "Iotm_TextMeshUtils.h"

namespace Iotm
{
struct TopologyMapEntry
{
  using DimensionArray = bool[4];

  unsigned int            id;
  Ioss::ElementTopology*  topology;

  // Defines what spatial dimension the topology is valid on
  DimensionArray          validSpatialDimensions; 

  TopologyMapEntry()
  : id(Ioss::ElementTopology::get_unique_id(Ioss::Unknown::name))
  , topology(Ioss::ElementTopology::factory(Ioss::Unknown::name))
  {
    set_valid_spatial_dimensions({false,false,false,false});
  }

  TopologyMapEntry(const std::string& name, const DimensionArray& validSpatialDimensions_)
  : id(Ioss::ElementTopology::get_unique_id(name))
  , topology(Ioss::ElementTopology::factory(name))
  {
    set_valid_spatial_dimensions(validSpatialDimensions_);
  }

  TopologyMapEntry(const TopologyMapEntry& topo)
  : id(topo.id)
  , topology(topo.topology)
  {
    set_valid_spatial_dimensions(topo.validSpatialDimensions);
  }

  void set_valid_spatial_dimensions(const DimensionArray &validSpatialDimensions_)
  {
    validSpatialDimensions[0] = validSpatialDimensions_[0];
    validSpatialDimensions[1] = validSpatialDimensions_[1];
    validSpatialDimensions[2] = validSpatialDimensions_[2];
    validSpatialDimensions[3] = validSpatialDimensions_[3];
  }

  bool defined_on_spatial_dimension(const unsigned spatialDim) const
  {
    if(spatialDim > 3) {return false;}
    return validSpatialDimensions[spatialDim];
  }

  const std::string& name() const
  {
    return topology->name();
  }

  int num_nodes() const
  {
    return topology->number_nodes();
  }

  bool equivalent_valid_spatial_dimensions(const DimensionArray &validSpatialDimensions_) const
  {
    return validSpatialDimensions[0] == validSpatialDimensions_[0] &&
           validSpatialDimensions[1] == validSpatialDimensions_[1] &&
           validSpatialDimensions[2] == validSpatialDimensions_[2] &&
           validSpatialDimensions[3] == validSpatialDimensions_[3];
  }

  bool operator==(const TopologyMapEntry &rhs) const
  {
    return id == rhs.id && topology == rhs.topology && equivalent_valid_spatial_dimensions(rhs.validSpatialDimensions);
  }

  bool operator!=(const TopologyMapEntry &rhs) const
  {
    return !(*this == rhs);
  }
};

class IossTopologyMapping : public text_mesh::TopologyMapping<TopologyMapEntry>
{
public:
  TopologyMapEntry invalid_topology() const override
  {
    return TopologyMapEntry();
  }

  void initialize_topology_map() override
  {
    m_nameToTopology = {
        {"NODE",         TopologyMapEntry(Ioss::Node::name,        {false,true ,true ,true })},
        {"LINE_2",       TopologyMapEntry(Ioss::Edge2::name,       {false,false,true ,true })},
        {"LINE_3",       TopologyMapEntry(Ioss::Edge3::name,       {false,false,true ,true })},
        {"TRI_3",        TopologyMapEntry(Ioss::Tri3::name,        {false,false,false,true })},
        {"TRI_4",        TopologyMapEntry(Ioss::Tri4::name,        {false,false,false,true })},
        {"TRI_6",        TopologyMapEntry(Ioss::Tri6::name,        {false,false,false,true })},
        {"QUAD_4",       TopologyMapEntry(Ioss::Quad4::name,       {false,false,false,true })},
        {"QUAD_6",       TopologyMapEntry(Ioss::Quad6::name,       {false,false,false,true })},
        {"QUAD_8",       TopologyMapEntry(Ioss::Quad8::name,       {false,false,false,true })},
        {"QUAD_9",       TopologyMapEntry(Ioss::Quad9::name,       {false,false,false,true })},
        {"PARTICLE",     TopologyMapEntry(Ioss::Sphere::name,      {false,true ,true ,true })},
        {"LINE_2_1D",    TopologyMapEntry(Ioss::Edge2::name,       {false,true ,false,false})},
        {"LINE_3_1D",    TopologyMapEntry(Ioss::Edge2::name,       {false,true ,false,false})},
        {"BEAM_2",       TopologyMapEntry(Ioss::Beam2::name,       {false,false,true ,true })},
        {"BEAM_3",       TopologyMapEntry(Ioss::Beam3::name,       {false,false,true ,true })},
        {"SHELL_LINE_2", TopologyMapEntry(Ioss::ShellLine2D2::name,{false,false,true ,false})},
        {"SHELL_LINE_3", TopologyMapEntry(Ioss::ShellLine2D3::name,{false,false,true ,false})},
        {"SPRING_2",     TopologyMapEntry(Ioss::Spring2::name,     {false,true ,true ,true })},
        {"SPRING_3",     TopologyMapEntry(Ioss::Spring3::name,     {false,true ,true ,true })},
        {"TRI_3_2D",     TopologyMapEntry(Ioss::Tri3::name,        {false,false,true ,false})},
        {"TRI_4_2D",     TopologyMapEntry(Ioss::Tri4::name,        {false,false,true ,false})},
        {"TRI_6_2D",     TopologyMapEntry(Ioss::Tri6::name,        {false,false,true ,false})},
        {"QUAD_4_2D",    TopologyMapEntry(Ioss::Quad4::name,       {false,false,true ,false})},
        {"QUAD_8_2D",    TopologyMapEntry(Ioss::Quad8::name,       {false,false,true ,false})},
        {"QUAD_9_2D",    TopologyMapEntry(Ioss::Quad9::name,       {false,false,true ,false})},
        {"SHELL_TRI_3",  TopologyMapEntry(Ioss::TriShell3::name,   {false,false,false,true })},
        {"SHELL_TRI_4",  TopologyMapEntry(Ioss::TriShell4::name,   {false,false,false,true })},
        {"SHELL_TRI_6",  TopologyMapEntry(Ioss::TriShell6::name,   {false,false,false,true })},
        {"SHELL_QUAD_4", TopologyMapEntry(Ioss::Shell4::name,      {false,false,false,true })},
        {"SHELL_QUAD_8", TopologyMapEntry(Ioss::Shell8::name,      {false,false,false,true })},
        {"SHELL_QUAD_9", TopologyMapEntry(Ioss::Shell9::name,      {false,false,false,true })},
        {"TET_4",        TopologyMapEntry(Ioss::Tet4::name,        {false,false,false,true })},
        {"TET_8",        TopologyMapEntry(Ioss::Tet8::name,        {false,false,false,true })},
        {"TET_10",       TopologyMapEntry(Ioss::Tet10::name,       {false,false,false,true })},
        {"TET_11",       TopologyMapEntry(Ioss::Tet11::name,       {false,false,false,true })},
        {"PYRAMID_5",    TopologyMapEntry(Ioss::Pyramid5::name,    {false,false,false,true })},
        {"PYRAMID_13",   TopologyMapEntry(Ioss::Pyramid13::name,   {false,false,false,true })},
        {"PYRAMID_14",   TopologyMapEntry(Ioss::Pyramid14::name,   {false,false,false,true })},
        {"WEDGE_6",      TopologyMapEntry(Ioss::Wedge6::name,      {false,false,false,true })},
        {"WEDGE_12",     TopologyMapEntry(Ioss::Wedge12::name,     {false,false,false,true })},
        {"WEDGE_15",     TopologyMapEntry(Ioss::Wedge15::name,     {false,false,false,true })},
        {"WEDGE_18",     TopologyMapEntry(Ioss::Wedge18::name,     {false,false,false,true })},
        {"HEX_8",        TopologyMapEntry(Ioss::Hex8::name,        {false,false,false,true })},
        {"HEX_20",       TopologyMapEntry(Ioss::Hex20::name,       {false,false,false,true })},
        {"HEX_27",       TopologyMapEntry(Ioss::Hex27::name,       {false,false,false,true })}
    };
  }
};

using Topology = TopologyMapEntry;
using TextMeshData = text_mesh::TextMeshData<int64_t, TopologyMapEntry>;
using ElementData = text_mesh::ElementData<int64_t, TopologyMapEntry>;
using Coordinates = text_mesh::Coordinates<int64_t, TopologyMapEntry>;
using TextMeshParser = text_mesh::TextMeshParser<int64_t, IossTopologyMapping>;

inline
std::ostream & operator<<(std::ostream &out, Topology t)
{
  return out << t.name();
}

struct BlockPartition {
  size_t offset;
  std::string name;
  std::set<int64_t> elemIds;

  BlockPartition() : offset(0), name("") {}

  BlockPartition(size_t offset_, const std::string &name_, const std::set<int64_t> &elemIds_)
      : offset(offset_), name(name_), elemIds(elemIds_)
  {
  }
};

class TextMesh
{
 public:
  explicit TextMesh(const std::string &parameters, int proc_count = 1, int my_proc = 0);
  TextMesh(int proc_count = 1, int my_proc = 0);
  TextMesh();
  TextMesh(const TextMesh &) = delete;
  TextMesh &operator=(const TextMesh &) = delete;

  virtual ~TextMesh() = default;

  /**
   * Return number of nodes in the entire model.
   */
  virtual int64_t node_count() const;

  /**
   * Return number of nodes on this processor.
   */
  virtual int64_t node_count_proc() const;

  /**
   * Return number of element blocks in the entire model.
   */
  virtual int64_t block_count() const;

  /**
   * Return number of elements in all element blocks in the model.
   */
  virtual int64_t element_count() const;

  /**
   * Return number of elements in all element blocks on this processor.
   */
  virtual int64_t element_count_proc() const;

  int64_t timestep_count() const { return m_timestepCount; }
  /**
   * Return number of elements in the element block with id
   * 'block_number'.
   */
  virtual int64_t element_count(int64_t block_number) const;

  /**
   * Return number of elements on this processor in the element
   * block with id 'block_number'.
   */
  virtual int64_t element_count_proc(int64_t block_number) const;

  /**
   * Returns pair containing "topology type string" and "number of
   * nodes / element". The topology type string will be "hex8" for
   * the hex element block and "shell4" for the shell element blocks.
   */
  virtual std::pair<std::string, int> topology_type(int64_t block_number) const;

  virtual int64_t communication_node_count_proc() const;
  virtual void node_communication_map(Ioss::Int64Vector &map, std::vector<int> &proc);
  virtual void owning_processor(int *owner, int64_t num_node);
  /**
   * Fill the passed in 'map' argument with the node map
   * "map[local_position] = global_id" for the nodes on this
   * processor.
   */
  virtual void node_map(Ioss::Int64Vector &map) const;
  virtual void node_map(Ioss::IntVector &map) const;

  /**
   * Fill the passed in 'map' argument with the element map
   * "map[local_position] = global_id" for the elements on this
   * processor in block "block_number".
   */
  virtual void element_map(int64_t block_number, Ioss::Int64Vector &map) const;
  virtual void element_map(int64_t block_number, Ioss::IntVector &map) const;

  /**
   * Fill the passed in 'map' argument with the element map
   * "map[local_position] = global_id" for all elements on this
   * processor
   */
  virtual void element_map(Ioss::Int64Vector &map) const;
  virtual void element_map(Ioss::IntVector &map) const;

  /**
   * Return the connectivity for the elements on this processor in
   * the block with id 'block_number'. If the elements in this block
   * have 'npe' nodes per element, then the first 'npe' entries in
   * the 'conn' vector will be the nodal connectivity for the first
   * element; the next 'npe' entries are the nodal connectivity for
   * the second element.  The 'connect' vector will be resized to the
   * size required to contain the nodal connectivity for the
   * specified block; all information in 'connect' will be overwritten.
   */
  void connectivity(int64_t block_number, Ioss::Int64Vector &connect) const;
  void connectivity(int64_t block_number, Ioss::IntVector &connect) const;
  void connectivity(int64_t block_number, int64_t *connect) const;
  virtual void connectivity(int64_t block_number, int *connect) const;

  /**
   * Return the coordinates for all nodes on this processor.  The
   * first 3 entries in the 'coord' vector are the x, y, and z
   * coordinates of the first node, etc.  The 'coord' vector will be
   * resized to the size required to contain the nodal coordinates;
   * all information in 'coord' will be overwritten.
   */
  virtual void coordinates(std::vector<double> &coord) const;
  virtual void coordinates(double *coord) const;

  /**
   * Return the coordinates for all nodes on this processor in
   * separate vectors. The vectors will be resized to the size
   * required to contain the nodal coordinates; all information in
   * the vectors will be overwritten.
   */
  virtual void coordinates(std::vector<double> &x, std::vector<double> &y, std::vector<double> &z) const;

  /**
   * Return the coordinates for componenet 'comp' (1=x, 2=y, 3=z)
   * for all nodes on this processor. The
   * vector will be resized to the size required to contain the
   * nodal coordinates; all information in the vector will be
   * overwritten.
   * It is an error to request the coordinates via this function
   * if a rotation is defined.
   */
  virtual void coordinates(int component, std::vector<double> &xyz) const;
  virtual void coordinates(int component, double *xyz) const;

  size_t get_variable_count(Ioss::EntityType type) const
  {
    return m_variableCount.find(type) != m_variableCount.end() ? m_variableCount.find(type)->second : 0;
  }

  std::vector<std::string> get_part_names() const;
  int64_t get_part_id(const std::string &name) const;

  unsigned spatial_dimension() const;

 private:
  template <typename INT>
  void raw_element_map(int64_t block_number, std::vector<INT> &map) const;
  template <typename INT>
  void raw_element_map(std::vector<INT> &map) const;
  template <typename INT>
  void raw_connectivity(int64_t block_number, INT *connect) const;
  template <typename INT>
  void raw_node_map(std::vector<INT> &map) const;

  void set_variable_count(const std::string &type, size_t count);
  void parse_options(const std::vector<std::string> &groups);
  void parse_coordinates_option(const std::vector<std::string> &option);
  void parse_dimension_option(const std::vector<std::string> &option);

  void initialize();

  void build_part_to_topology_map();
  void build_block_partition_map();
  void build_element_connectivity_map();

  std::vector<int64_t> get_part_ids(const std::vector<std::string> &partNames);
  std::vector<size_t> get_part_offsets(const std::vector<int64_t> &partIds);

  Topology get_topology_for_part(int64_t id) const;

  std::set<int64_t> get_local_element_ids_for_block(int64_t id) const;

  unsigned m_dimension{3};
  size_t m_processorCount{0};
  size_t m_myProcessor{0};

  size_t m_timestepCount{0};
  std::map<Ioss::EntityType, size_t> m_variableCount;

  bool                m_coordinatesParsed{false};
  std::vector<double> m_rawCoordinates;

  TextMeshData m_data;
  Coordinates m_coordinates;

  text_mesh::ErrorHandler m_errorHandler;

  std::unordered_map<std::string, Topology> m_partToTopology;

  std::unordered_map<int64_t, BlockPartition> m_blockPartition;

  std::unordered_map<int64_t, std::vector<int64_t>> m_elementConnectivity;
};
}  // namespace Iotm
#endif
