// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

// #######################  Start Clang Header Tool Managed Headers ########################
// clang-format off
#include <ctype.h>                                   // for toupper
#include <stddef.h>                                  // for size_t
#include <algorithm>                                 // for remove, etc
#include <iterator>                                  // for insert_iterator
#include <map>
#include <set>                                       // for set
#include <sstream>                                   // for operator<<, etc
#include <string>                                    // for basic_string, etc
#include <utility>                                   // for pair
#include <vector>                                    // for vector
#include <unordered_map>
#include <sstream>                       // for ostringstream
#include <iostream>
#include <functional>
#include <stdexcept>
#include <numeric>
#include <fmt/ostream.h>
#include <strings.h>

// clang-format on
// #######################   End Clang Header Tool Managed Headers  ########################
namespace Iotm {
namespace text_mesh
{
using ErrorHandler = std::function<void(const std::ostringstream &)>;

template <class EXCEPTION>
void handle_error(const std::ostringstream &message)
{
  throw EXCEPTION((message).str());
}

inline void default_error_handler(const std::ostringstream &message)
{
  handle_error<std::logic_error>(message);
}

template <class ForwardIt, class T>
ForwardIt bound_search(ForwardIt first, ForwardIt last, const T &value)
{
  first = std::lower_bound(first, last, value);
  if (!(first == last) && !(value < *first)) return first;

  return last;
}

template <class ForwardIt, class T, class Compare>
ForwardIt bound_search(ForwardIt first, ForwardIt last, const T &value, Compare comp)
{
  first = std::lower_bound(first, last, value, comp);
  if (!(first == last) && !(comp(value, *first))) return first;

  return last;
}

inline std::string strip(const std::string &inpt)
{
  auto start_it = inpt.begin();
  auto end_it = inpt.rbegin();
  while (std::isspace(*start_it)) ++start_it;
  while (std::isspace(*end_it)) ++end_it;
  return std::string(start_it, end_it.base());
}

inline std::vector<std::string> get_tokens(const std::string &str, const std::string &separators)
{
  std::vector<std::string> tokens;
  auto first = std::begin(str);
  while (first != std::end(str)) {
    const auto second = std::find_first_of(first, std::end(str), std::begin(separators), std::end(separators));
    if (first != second) {
      std::string token = strip(std::string(first, second));
      tokens.emplace_back(token);
    }
    if (second == std::end(str)) {
      break;
    }
    first = std::next(second);
  }
  return tokens;
}

inline void convert_to_upper_case(std::string &str)
{
  std::transform(str.begin(), str.end(), str.begin(), ::toupper);
}

inline void convert_to_lower_case(std::string &str)
{
  std::transform(str.begin(), str.end(), str.begin(), ::tolower);
}

inline bool is_number(const std::string &str)
{
  for (char const &c : str) {
    if (std::isdigit(c) == 0) return false;
  }
  return true;
}

template <typename T>
std::set<T> transform_to_set(const std::vector<T> &dataAsVector)
{
  std::set<T> dataAsSet;

  for (const T &data : dataAsVector) {
    dataAsSet.insert(data);
  }

  return dataAsSet;
}

inline std::pair<unsigned, bool> get_id_from_part_name(const std::string &name, const std::string &prefix)
{
  const unsigned prefixLength = prefix.length();

  if (name.length() < prefixLength + 1) return std::make_pair(0, false);

  const std::string namePrefix = name.substr(0, prefixLength);
  const std::string nameSuffix = name.substr(prefixLength);

  if (strcasecmp(namePrefix.c_str(), prefix.c_str()) != 0) return std::make_pair(0, false);

  unsigned id;
  std::istringstream nameSuffixStream(nameSuffix);
  nameSuffixStream >> id;
  if (nameSuffixStream.fail()) {
    return std::make_pair(0, false);
  }
  return std::make_pair(id, true);
}

template <typename T>
class TopologyMapping
{
 public:
  using Topology = T;

  virtual ~TopologyMapping() {}

  Topology topology(const std::string &textMeshName) const
  {
    auto it = m_nameToTopology.find(textMeshName);
    return (it != m_nameToTopology.end() ? it->second : invalid_topology());
  }

  virtual Topology invalid_topology() const = 0;
  virtual void initialize_topology_map() = 0;

 protected:
  std::unordered_map<std::string, Topology> m_nameToTopology;
};

class PartIdMapping
{
 public:
  PartIdMapping() : m_idsAssigned(false)
  {
    set_error_handler([](const std::ostringstream &errmsg) { default_error_handler(errmsg); });
  }

  void register_part_name(const std::string &name)
  {
    m_partNames.push_back(name);
    handle_block_part(name);
  }

  void register_part_name_with_id(const std::string &name, unsigned id)
  {
    register_part_name(name);
    assign(name, id);
  }

  unsigned get(const std::string &name) const
  {
    if (!m_idsAssigned) assign_ids();
    return get_part_id(name);
  }

  std::string get(unsigned id) const
  {
    if (!m_idsAssigned) assign_ids();
    return get_part_name(id);
  }

  unsigned size() const
  {
    if (!m_idsAssigned) assign_ids();
    return m_ids.size();
  }

  std::vector<std::string> get_part_names_sorted_by_id() const
  {
    if (!m_idsAssigned) assign_ids();

    std::vector<std::string> names;
    names.reserve(m_parts.size());

    for (auto iter : m_parts) {
      names.push_back(iter.second);
    }

    return names;
  }

  const std::vector<std::string> &get_part_names() const { return m_partNames; }

  void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

 private:
  void handle_block_part(const std::string &name)
  {
    auto result = get_id_from_part_name(name, "BLOCK_");

    if (!result.second) return;

    assign(name, result.first);
  }

  void assign_ids() const
  {
    unsigned nextPartId = 1;
    for (const std::string &name : m_partNames) {
      if (m_ids.find(name) == m_ids.end()) {
        while (is_assigned(nextPartId)) nextPartId++;

        assign(name, nextPartId);
      }
    }

    m_idsAssigned = true;
  }

  void assign(const std::string &name, unsigned id) const
  {
    validate_name_and_id(name, id);
    m_ids[name] = id;
    m_parts[id] = name;
  }

  void validate_name_and_id(const std::string &name, unsigned id) const
  {
    if (is_registered(name)) {
      if (m_ids[name] != id) {
        std::ostringstream errmsg;
        errmsg << "Cannot assign part '" << name << "' two different ids: " << m_ids[name] << " and " << id;
        m_errorHandler(errmsg);
      }
    } else {
      if (is_assigned(id)) {
        std::ostringstream errmsg;
        errmsg << "Part id " << id << " has already been assigned, cannot assign it to part '" << name << "'";
        m_errorHandler(errmsg);
      }
    }
  }

  bool is_registered(const std::string &name) const { return m_ids.count(name) > 0; }

  bool is_assigned(unsigned id) const { return m_parts.count(id) > 0; }

  unsigned get_part_id(const std::string &name) const
  {
    auto it = m_ids.find(name);
    if (it == m_ids.end()) {
      std::ostringstream errmsg;
      errmsg << "PartIdMapping has no ID for invalid part name " << name;
      m_errorHandler(errmsg);
    }
    return it->second;
  }

  std::string get_part_name(unsigned id) const
  {
    auto it = m_parts.find(id);
    if (it == m_parts.end()) {
      std::ostringstream errmsg;
      errmsg << "PartIdMapping has no part name for invalid id " << id;
      m_errorHandler(errmsg);
    }
    return it->second;
  }

  std::vector<std::string> m_partNames;
  mutable std::unordered_map<std::string, unsigned> m_ids;
  mutable std::map<unsigned, std::string> m_parts;
  mutable bool m_idsAssigned;

  ErrorHandler m_errorHandler;
};

template <typename EntityId>
class Coordinates
{
 public:
  Coordinates()
  {
    set_error_handler([](const std::ostringstream &errmsg) { default_error_handler(errmsg); });
  }

  const std::vector<double> &operator[](const EntityId nodeId) const
  {
    auto it(m_nodalCoords.find(nodeId));

    if (it == m_nodalCoords.end()) {
      std::ostringstream errmsg;
      errmsg << "Could not find node id " << nodeId;
      m_errorHandler(errmsg);
    }

    return it->second;
  }

  void set_coordinate_data(
      const unsigned spatialDim, const std::set<EntityId> &nodeIds, const std::vector<double> &coordinates)
  {
    if (!coordinates.empty()) {
      validate_num_coordinates(spatialDim, nodeIds, coordinates);
      fill_coordinate_map(spatialDim, nodeIds, coordinates);
      m_hasCoordinateData = true;
    }
  }

  void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

  bool has_coordinate_data() const { return m_hasCoordinateData; }

 private:
  void validate_num_coordinates(
      const unsigned spatialDim, const std::set<EntityId> &nodeIds, const std::vector<double> &coordinates)
  {
    if (coordinates.size() != nodeIds.size() * spatialDim) {
      std::ostringstream errmsg;
      errmsg << "Number of coordinates: " << coordinates.size() << ", Number of nodes: " << nodeIds.size()
             << ", Spatial dimension: " << spatialDim;
      m_errorHandler(errmsg);
    }
  }

  void fill_coordinate_map(
      const unsigned spatialDim, const std::set<EntityId> &nodeIds, const std::vector<double> &coordinates)
  {
    std::vector<double>::const_iterator coordIter = coordinates.begin();
    for (const EntityId &nodeId : nodeIds) {
      m_nodalCoords[nodeId] = std::vector<double>(coordIter, coordIter + spatialDim);
      coordIter += spatialDim;
    }
  }

  bool m_hasCoordinateData{false};
  std::unordered_map<EntityId, std::vector<double>> m_nodalCoords;
  ErrorHandler m_errorHandler;
};

template <typename EntityId, typename Topology>
struct ElementData {
  int proc;
  EntityId identifier;
  Topology topology;
  std::vector<EntityId> nodeIds;
  std::string partName = "";

  operator EntityId() const { return identifier; }
};

template <typename EntityId, typename Topology>
struct ElementDataLess {
  bool operator()(const ElementData<EntityId, Topology> &lhs, const ElementData<EntityId, Topology> &rhs)
  {
    return lhs.identifier < rhs.identifier;
  };

  bool operator()(const ElementData<EntityId, Topology> &lhs, const EntityId rhs) { return lhs.identifier < rhs; };

  bool operator()(const EntityId lhs, const ElementData<EntityId, Topology> &rhs) { return lhs < rhs.identifier; };

  bool operator()(const EntityId lhs, const EntityId rhs) { return lhs < rhs; };
};

struct SideBlockInfo {
  std::string name;
  std::string parentName;
  std::string sideTopology;
  std::string elementTopology;
  std::string touchingBlock;
  std::vector<size_t> sideIndex;
  unsigned numNodesPerSide;
};

enum SplitType { TOPOLOGY, ELEMENT_BLOCK, NO_SPLIT, INVALID };

inline std::ostream &operator<<(std::ostream &out, const SplitType &t)
{
  switch (t) {
    case SplitType::TOPOLOGY:
      return out << "TOPOLOGY";
      break;
    case SplitType::ELEMENT_BLOCK:
      return out << "ELEMENT_BLOCK";
      break;
    case SplitType::NO_SPLIT:
      return out << "NO_SPLIT";
      break;
    default:
      return out << "INVALID";
      break;
  }
  return out << "INVALID[" << (unsigned) t << "]";
}

template <typename EntityId, typename T>
struct EntityGroupData {
  using DataType = T;
  static constexpr unsigned INVALID_ID = std::numeric_limits<unsigned>::max();

  bool hasInputName = false;
  unsigned id = INVALID_ID;
  std::string name = "";
  std::string type = "";
  std::vector<DataType> data;

  bool has_valid_id() const { return id != 0 && id != INVALID_ID; }
  bool has_name() const { return !name.empty(); }
};

template <typename EntityId, typename GroupData>
class EntityGroup
{
 private:
  using DataType = typename GroupData::DataType;

 public:
  EntityGroup(const std::string &type,
      const std::string &standardNamePrefix,
      const std::vector<std::string> &invalidNamePrefixes)
      : m_idsAssigned(false), m_type(type), m_standardPrefix(standardNamePrefix), m_invalidPrefixes(invalidNamePrefixes)
  {
    set_error_handler([](const std::ostringstream &errmsg) { default_error_handler(errmsg); });
  }

  virtual ~EntityGroup() {}

  virtual void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

  GroupData *add_group_data(const std::string &name, const std::vector<DataType> &data)
  {
    GroupData groupData;
    groupData.data = data;
    groupData.type = m_type;

    if (!name.empty()) {
      verify_name(name);
      groupData.name = name;
      groupData.hasInputName = true;
    }

    m_groupDataVec.push_back(groupData);

    return &m_groupDataVec.back();
  }

  void finalize_parse()
  {
    assign_id_from_standard_name();
    assign_id_and_name_for_empty_name();
    assign_id_for_non_standard_name();

    if (m_groupDataVec.size() != m_groupDataMap.size()) {
      std::ostringstream errmsg;
      errmsg << "Error populating " << m_type << " map";
      m_errorHandler(errmsg);
    }
    m_idsAssigned = true;
  }

  const std::vector<GroupData> &get_group_data() const { return m_groupDataVec; }

  const std::vector<std::string> &get_part_names() const { return m_partNames; }

  const GroupData *get_group_data(unsigned id) const
  {
    if (is_assigned(id)) {
      auto iter = m_parts.find(id);
      return &m_groupDataVec[m_groupDataMap[iter->second]];
    }

    return nullptr;
  }

  const GroupData *get_group_data(std::string name) const
  {
    convert_to_upper_case(name);
    if (is_registered(name)) {
      return &m_groupDataVec[m_groupDataMap[name]];
    }

    return nullptr;
  }

  bool is_registered(const std::string &name) const { return m_ids.count(name) > 0; }

 protected:
  EntityGroup();

  unsigned get_unassigned_id() const
  {
    unsigned nextPartId = 1;
    while (is_assigned(nextPartId)) nextPartId++;
    return nextPartId;
  }

  void validate_group_meta_data(const GroupData &groupData)
  {
    if (!groupData.has_name()) {
      std::ostringstream errmsg;
      errmsg << m_type << " has no name";
      m_errorHandler(errmsg);
    }

    if (!groupData.has_valid_id()) {
      std::ostringstream errmsg;
      errmsg << m_type << " named " << groupData.name << " has invalid id";
      m_errorHandler(errmsg);
    }

    if (is_registered(groupData.name)) {
      std::ostringstream errmsg;
      errmsg << "Multiple declarations of " << m_type << ": " << groupData.name;
      m_errorHandler(errmsg);
    }
  }

  void assign(size_t index)
  {
    GroupData &groupData = m_groupDataVec[index];

    convert_to_upper_case(groupData.name);
    validate_group_meta_data(groupData);

    m_partNames.push_back(groupData.name);
    m_ids[groupData.name] = groupData.id;
    m_parts[groupData.id] = groupData.name;
    m_groupDataMap[groupData.name] = index;
  }

  void assign_id_from_standard_name()
  {
    for (size_t i = 0; i < m_groupDataVec.size(); i++) {
      GroupData &groupData = m_groupDataVec[i];
      if (groupData.has_name()) {
        std::pair<unsigned, bool> result = get_id_from_part_name(groupData.name, m_standardPrefix);

        if (result.second) {
          groupData.id = result.first;
          assign(i);
        }
      }
    }
  }

  void assign_id_and_name_for_empty_name()
  {
    for (size_t i = 0; i < m_groupDataVec.size(); i++) {
      GroupData &groupData = m_groupDataVec[i];
      if (!groupData.has_name()) {
        unsigned id = get_unassigned_id();

        std::ostringstream oss;
        oss << m_standardPrefix;
        oss << id;
        std::string name = oss.str();

        groupData.id = id;
        groupData.name = name;
        assign(i);
      }
    }
  }

  void assign_id_for_non_standard_name()
  {
    for (size_t i = 0; i < m_groupDataVec.size(); i++) {
      GroupData &groupData = m_groupDataVec[i];
      if (groupData.has_name()) {
        std::pair<unsigned, bool> result = get_id_from_part_name(groupData.name, m_standardPrefix);

        if (!result.second) {
          groupData.id = get_unassigned_id();
          assign(i);
        }
      }
    }
  }

  bool is_assigned(unsigned id) const { return m_parts.count(id) > 0; }

  void verify_name(const std::string &name)
  {
    for (const std::string &invalidPrefix : m_invalidPrefixes) {
      const unsigned prefixLength = invalidPrefix.length();
      const std::string namePrefix = name.substr(0, prefixLength);

      if (strcasecmp(namePrefix.c_str(), invalidPrefix.c_str()) == 0) {
        std::ostringstream errmsg;
        errmsg << "Invalid name '" << name << "' for a " << m_type << " part";
        m_errorHandler(errmsg);
      }
    }
  }

  std::vector<std::string> m_partNames;
  mutable std::unordered_map<std::string, unsigned> m_ids;
  mutable std::unordered_map<unsigned, std::string> m_parts;
  mutable bool m_idsAssigned;
  mutable std::unordered_map<std::string, size_t> m_groupDataMap;

  std::string m_type;
  std::string m_standardPrefix;
  std::vector<std::string> m_invalidPrefixes;
  std::vector<GroupData> m_groupDataVec;

  ErrorHandler m_errorHandler;
};

template <typename EntityId>
using NodesetDataType = EntityId;

template <typename EntityId>
struct NodesetData : public EntityGroupData<EntityId, NodesetDataType<EntityId>> {
  using DataType = NodesetDataType<EntityId>;
};

template <typename EntityId>
class Nodesets : public EntityGroup<EntityId, NodesetData<EntityId>>
{
 public:
  Nodesets() : EntityGroup<EntityId, NodesetData<EntityId>>("NODESET", "NODELIST_", {"BLOCK_", "SURFACE_"}) {}
};

template <typename EntityId>
using SidesetDataType = std::pair<EntityId, int>;

template <typename EntityId, typename Topology>
struct SidesetData;

template <typename EntityId, typename Topology>
class SidesetSplitter
{
 public:
  SidesetSplitter(SplitType splitType) : m_splitType(splitType)
  {
    ErrorHandler errorHandler = [](const std::ostringstream &errmsg) { default_error_handler(errmsg); };
    set_error_handler(errorHandler);
  }

  SidesetSplitter() : m_splitType(INVALID)
  {
    ErrorHandler errorHandler = [](const std::ostringstream &errmsg) { default_error_handler(errmsg); };
    set_error_handler(errorHandler);
  }

  void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

  void split(
      const SidesetData<EntityId, Topology> &sideset, const std::vector<ElementData<EntityId, Topology>> &elementData)
  {
    m_splitMap.clear();
    m_sidesetName = sideset.name;

    if (get_split_type() == SplitType::TOPOLOGY) {
      split_by_topology(sideset, elementData);
    } else if (get_split_type() == SplitType::ELEMENT_BLOCK) {
      split_by_element_block(sideset, elementData);
    } else if (get_split_type() == SplitType::NO_SPLIT) {
      split_by_no_split(sideset, elementData);
    } else {
      std::ostringstream errmsg;
      errmsg << "Invalid split type: " << get_split_type();
      m_errorHandler(errmsg);
    }

    build_index_proc_map(sideset, elementData);
  }

  std::vector<SideBlockInfo> get_side_block_info() const
  {
    std::vector<SideBlockInfo> infoVec;
    infoVec.reserve(m_splitMap.size());

    for (auto iter = m_splitMap.begin(); iter != m_splitMap.end(); iter++) {
      const std::string &sideBlockName = iter->first;
      SideBlockInfo info = get_side_block_info(sideBlockName);
      infoVec.push_back(info);
    }
    return infoVec;
  }

  std::vector<size_t> get_indices_local_to_proc(const std::vector<size_t> &index, int proc) const
  {
    std::vector<size_t> indexForProc;
    indexForProc.reserve(index.size());

    for (size_t elemPairIndex : index) {
      if (is_index_local_to_proc(elemPairIndex, proc)) {
        indexForProc.push_back(elemPairIndex);
      }
    }

    indexForProc.resize(indexForProc.size());
    return indexForProc;
  }

  SideBlockInfo get_side_block_info(const std::string &name) const
  {
    SideBlockInfo info;

    auto iter = m_splitMap.find(name);
    if (iter != m_splitMap.end()) {
      const SplitData &splitData = iter->second;

      info.name = name;
      info.parentName = splitData.sidesetName;
      info.sideTopology = splitData.sideTopology;
      info.elementTopology = splitData.elemTopology;
      info.numNodesPerSide = splitData.sideNodeCount;
      info.touchingBlock = splitData.touchingBlock;
      info.sideIndex = splitData.index;
    }
    return info;
  }

  SplitType get_split_type() const { return m_splitType; }
  void set_split_type(SplitType inputSplitType) { m_splitType = inputSplitType; }

 private:
  void build_index_proc_map(
      const SidesetData<EntityId, Topology> &sideset, const std::vector<ElementData<EntityId, Topology>> &elementData)
  {
    for (size_t i = 0; i < sideset.data.size(); ++i) {
      const SidesetDataType<EntityId> &elemSidePair = sideset.data[i];
      EntityId elemId = elemSidePair.first;

      auto iter = bound_search(elementData.begin(), elementData.end(), elemId, ElementDataLess<EntityId, Topology>());
      if (iter == elementData.end()) {
        std::ostringstream errmsg;
        errmsg << "Error!  Sideset with id: " << sideset.id << " and name: " << sideset.name
               << " has reference to invalid element '" << elemId << "'.";
        m_errorHandler(errmsg);
      }

      m_indexProcMap[i] = iter->proc;
    }
  }

  bool is_index_local_to_proc(size_t elemPairIndex, int proc) const
  {
    auto iter = m_indexProcMap.find(elemPairIndex);

    if (iter == m_indexProcMap.end()) {
      std::ostringstream errmsg;
      errmsg << "Sideset with name: " << m_sidesetName << " is referencing an invalid index " << elemPairIndex;
      m_errorHandler(errmsg);
    }

    return iter->second == proc;
  }

  struct SplitData {
    bool metaDataSet;
    std::string sidesetName;
    std::string touchingBlock;
    std::string elemTopology;
    std::string sideTopology;
    int sideNodeCount;
    std::vector<size_t> index;

    SplitData() : metaDataSet(false), sideNodeCount(-1) {}
  };

  void fill_split_data(std::string key, size_t index, const ElementData<EntityId, Topology> &elemData, int side)
  {
    convert_to_upper_case(key);

    SplitData &splitData = m_splitMap[key];

    splitData.index.push_back(index);

    if (!splitData.metaDataSet) {
      splitData.sidesetName = m_sidesetName;
      splitData.elemTopology = elemData.topology.name();
      splitData.sideTopology = elemData.topology.side_topology_name(side);
      splitData.sideNodeCount = elemData.topology.side_topology_num_nodes(side);

      if (get_split_type() == ELEMENT_BLOCK) {
        splitData.touchingBlock = elemData.partName;
      }

      splitData.metaDataSet = true;
    }
  }

  using Criterion = std::function<std::string(
      const SidesetData<EntityId, Topology> &sideset, const ElementData<EntityId, Topology> &elemData, int side)>;

  void split_by_criterion(const SidesetData<EntityId, Topology> &sideset,
      const std::vector<ElementData<EntityId, Topology>> &elementData,
      Criterion criterion)
  {
    for (size_t index = 0; index < sideset.data.size(); ++index) {
      const SidesetDataType<EntityId> &elemSidePair = sideset.data[index];
      EntityId elemId = elemSidePair.first;
      int side = elemSidePair.second;

      auto iter = bound_search(elementData.begin(), elementData.end(), elemId, ElementDataLess<EntityId, Topology>());
      if (iter == elementData.end()) {
        std::ostringstream errmsg;
        errmsg << "Error!  Sideset with id: " << sideset.id << " and name: " << sideset.name
               << " has reference to invalid element '" << elemId << "'.";
        m_errorHandler(errmsg);
      }

      std::string key = criterion(sideset, *iter, side);
      fill_split_data(key, index, *iter, side);
    }
  }

  void split_by_topology(
      const SidesetData<EntityId, Topology> &sideset, const std::vector<ElementData<EntityId, Topology>> &elementData)
  {
    Criterion criterion = [](const SidesetData<EntityId, Topology> &sideSet,
                              const ElementData<EntityId, Topology> &elemData, int side) {
      if (sideSet.has_standard_name()) {
        return "SURFACE_" + elemData.topology.name() + "_" + elemData.topology.side_topology_name(side) + "_" +
               std::to_string(sideSet.id);
      }
      return sideSet.name + "_" + elemData.topology.name() + "_" + elemData.topology.side_topology_name(side);
    };

    split_by_criterion(sideset, elementData, criterion);
  }

  void split_by_element_block(
      const SidesetData<EntityId, Topology> &sideset, const std::vector<ElementData<EntityId, Topology>> &elementData)
  {
    Criterion criterion = [](const SidesetData<EntityId, Topology> &sideSet,
                              const ElementData<EntityId, Topology> &elemData, int side) {
      if (sideSet.has_standard_name()) {
        return "SURFACE_" + elemData.partName + "_" + elemData.topology.side_topology_name(side) + "_" +
               std::to_string(sideSet.id);
      }
      return sideSet.name + "_" + elemData.partName + "_" + elemData.topology.side_topology_name(side);
    };

    split_by_criterion(sideset, elementData, criterion);
  }

  void split_by_no_split(
      const SidesetData<EntityId, Topology> &sideset, const std::vector<ElementData<EntityId, Topology>> &elementData)
  {
    std::vector<size_t> splitIndex(sideset.data.size());
    std::iota(std::begin(splitIndex), std::end(splitIndex), 0);
    SplitData &splitData = m_splitMap[sideset.name];

    splitData.index = splitIndex;
    splitData.sidesetName = m_sidesetName;
    splitData.elemTopology = "unknown";
    splitData.sideTopology = "unknown";
    splitData.sideNodeCount = -1;
    splitData.metaDataSet = true;
  }

  SplitType m_splitType;
  std::string m_sidesetName;

  std::unordered_map<size_t, int> m_indexProcMap;
  std::unordered_map<std::string, SplitData> m_splitMap;
  ErrorHandler m_errorHandler;
};

template <typename EntityId, typename Topology>
struct TextMeshData;

template <typename EntityId, typename Topology>
class SideAdjacencyGraph
{
 public:
  using IndexType = int64_t;

  static constexpr int ANY_PROC = -1;
  static constexpr int INVALID_SIDE = -1;
  static constexpr IndexType INVALID_INDEX = -1;

  struct FaceConnection {
    FaceConnection() : thisSide(INVALID_SIDE), thatElement(INVALID_INDEX), thatSide(INVALID_SIDE){};

    FaceConnection(int thisSide_, IndexType otherElement_, int otherSide_)
        : thisSide(thisSide_), thatElement(otherElement_), thatSide(otherSide_)
    {
    }

    bool operator==(const FaceConnection &rhs) const
    {
      return ((thisSide == rhs.thisSide) && (thatElement == rhs.thatElement) && (thatSide == rhs.thatSide));
    }

    bool operator!=(const FaceConnection &rhs) const
    {
      return ((thisSide != rhs.thisSide) || (thatElement != rhs.thatElement) || (thatSide != rhs.thatSide));
    }

    bool operator<(const FaceConnection &rhs) const
    {
      if (thisSide < rhs.thisSide)
        return true;
      else if (thisSide == rhs.thisSide && thatElement < rhs.thatElement)
        return true;
      else if (thisSide == rhs.thisSide && thatElement == rhs.thatElement && thatSide < rhs.thatSide)
        return true;
      else
        return false;
    }

    int thisSide;
    IndexType thatElement;
    int thatSide;
  };

  struct FaceConnections {
    FaceConnections() : numSides(0u) {}
    FaceConnections(unsigned numSides_) : numSides(numSides_), sideReference(numSides_, 0) {}

    bool has_any_connection(int thisSide) const
    {
      FaceConnection minConnection(thisSide, INVALID_INDEX, 0);
      typename std::set<FaceConnection>::iterator lowerBound =
          std::lower_bound(connections.begin(), connections.end(), minConnection);

      return (lowerBound != connections.end() && lowerBound->thisSide == thisSide);
    }

    bool has_any_connection(int thisSide, IndexType thatElem) const
    {
      FaceConnection minConnection(thisSide, thatElem, 0);
      typename std::set<FaceConnection>::iterator lowerBound =
          std::lower_bound(connections.begin(), connections.end(), minConnection);

      return (
          lowerBound != connections.end() && lowerBound->thisSide == thisSide && lowerBound->thatElement == thatElem);
    }

    void add(const FaceConnection &connection)
    {
      auto iter = connections.find(connection);
      if (iter == connections.end()) {
        sideReference[connection.thisSide - 1]++;
        connections.insert(connection);
      }
    }

    void remove(const FaceConnection &connection)
    {
      auto iter = connections.find(connection);
      if (iter != connections.end()) {
        sideReference[connection.thisSide - 1]--;
        connections.erase(iter);
      }
    }

    unsigned numSides;
    std::vector<int> sideReference;
    std::set<FaceConnection> connections;
  };

  SideAdjacencyGraph()
  {
    ErrorHandler errorHandler = [](const std::ostringstream &errmsg) { default_error_handler(errmsg); };
    set_error_handler(errorHandler);
  }

  void create_graph(const TextMeshData<EntityId, Topology> &data, int proc = ANY_PROC) { create_graph(data, {}, proc); }

  void create_graph(
      const TextMeshData<EntityId, Topology> &data, const std::vector<std::string> &selectedBlocks, int proc = ANY_PROC)
  {
    m_indexGraph.clear();
    std::vector<size_t> localAndAuraElementIndex = get_local_and_aura_elements(data, selectedBlocks, proc);
    std::unordered_map<EntityId, std::set<size_t>> elementsForNode =
        get_elements_for_node_map(data.elementDataVec, localAndAuraElementIndex);
    build_side_connectivity_graph(data.elementDataVec, localAndAuraElementIndex, elementsForNode);
  }

  void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

  void dump(std::ostream &out = std::cout)
  {
    for (const auto &iter : m_indexGraph) {
      if (!iter.second.connections.empty()) {
        out << "Element index: " << iter.first << std::endl;
      }
      for (const auto &entry : iter.second.connections) {
        out << "\tConnected on side: " << entry.thisSide << " to element index: " << entry.thatElement
            << " and side: " << entry.thatSide << std::endl;
      }
    }
  }

  void dump(const std::vector<ElementData<EntityId, Topology>> &elemDataVec, std::ostream &out = std::cout)
  {
    for (const auto &iter : m_indexGraph) {
      if (!iter.second.connections.empty()) {
        out << "Element: " << elemDataVec[iter.first].identifier << " {" << elemDataVec[iter.first].topology << "}"
            << std::endl;
      }
      for (const auto &entry : iter.second.connections) {
        out << "\tConnected on side: " << entry.thisSide << " to element: " << elemDataVec[entry.thatElement].identifier
            << " {" << elemDataVec[entry.thatElement].topology << "}"
            << " and side: " << entry.thatSide << std::endl;
      }
    }
  }

  const FaceConnections &operator[](const IndexType elemIndex) const
  {
    auto it = m_indexGraph.find(elemIndex);

    if (it == m_indexGraph.end()) {
      std::ostringstream errmsg;
      errmsg << "Could not find element index " << elemIndex;
      m_errorHandler(errmsg);
    }

    return it->second;
  }

  size_t size() const { return m_indexGraph.size(); }

  typename std::unordered_map<size_t, FaceConnections>::const_iterator begin() const { return m_indexGraph.begin(); }
  typename std::unordered_map<size_t, FaceConnections>::const_iterator end() const { return m_indexGraph.end(); }

 private:
  using OrdinalType = typename Topology::Ordinal;
  using PermutationType = typename Topology::Permutation;

  struct StringCaseCompLess {
    bool operator()(const std::string &lhs, const std::string &rhs)
    {
      return (strcasecmp(lhs.c_str(), rhs.c_str()) < 0);
    };
  };

  struct CurrentAdjacency {
    CurrentAdjacency(const std::vector<ElementData<EntityId, Topology>> &elemDataVec_, size_t elementIndex_, int side_)
        : elemDataVec(elemDataVec_), elementIndex(elementIndex_), side(side_)
    {
    }

    const std::vector<ElementData<EntityId, Topology>> &elemDataVec;
    size_t elementIndex;
    int side;
    std::vector<int> connectedSides;
  };

  std::set<size_t> get_element_indices_with_common_nodes_on_side(const ElementData<EntityId, Topology> &elemData,
      int side,
      const std::unordered_map<EntityId, std::set<size_t>> &elementsForNode)
  {
    std::set<size_t> neighbors;
    std::vector<OrdinalType> sideNodeIndices = elemData.topology.side_topology_node_indices(side);

    if (!sideNodeIndices.empty()) {
      EntityId firstSideNode = elemData.nodeIds[sideNodeIndices[0]];
      neighbors = elementsForNode.at(firstSideNode);
    }

    for (size_t i = 1; i < sideNodeIndices.size(); ++i) {
      OrdinalType sideNodeIndex = sideNodeIndices[i];
      EntityId sideNode = elemData.nodeIds[sideNodeIndex];
      const std::set<size_t> &sideNodeElementIndices = elementsForNode.at(sideNode);

      std::set<size_t> intersection;
      std::set_intersection(neighbors.begin(), neighbors.end(), sideNodeElementIndices.begin(),
          sideNodeElementIndices.end(), std::inserter(intersection, intersection.begin()));

      neighbors = intersection;
    }

    return neighbors;
  }

  std::vector<EntityId> get_side_nodes(const ElementData<EntityId, Topology> &elemData, const int side)
  {
    std::vector<OrdinalType> sideNodeIndices = elemData.topology.side_topology_node_indices(side);
    std::vector<EntityId> sideNodes(sideNodeIndices.size());

    for (size_t i = 0; i < sideNodeIndices.size(); ++i) {
      OrdinalType sideNodeIndex = sideNodeIndices[i];
      sideNodes[i] = elemData.nodeIds[sideNodeIndex];
    }

    return sideNodes;
  }

  std::vector<EntityId> get_sorted_side_nodes(const ElementData<EntityId, Topology> &elemData, const int side)
  {
    std::vector<EntityId> sideNodes = get_side_nodes(elemData, side);
    std::sort(sideNodes.begin(), sideNodes.end());

    return sideNodes;
  }

  void internal_fill_sides_for_connected_element(CurrentAdjacency &adjacency, size_t connectedElementIndex)
  {
    adjacency.connectedSides.clear();

    std::vector<EntityId> sideNodes =
        get_sorted_side_nodes(adjacency.elemDataVec[adjacency.elementIndex], adjacency.side);

    const ElementData<EntityId, Topology> &connectedElemData = adjacency.elemDataVec[connectedElementIndex];
    for (int otherSide = 1; otherSide <= connectedElemData.topology.num_sides(); ++otherSide) {
      std::vector<EntityId> otherSideNodes = get_sorted_side_nodes(connectedElemData, otherSide);
      if (sideNodes == otherSideNodes) {
        adjacency.connectedSides.push_back(otherSide);
      }
    }
  }

  void fill_sides_for_connected_element(CurrentAdjacency &adjacency, size_t connectedElementIndex)
  {
    internal_fill_sides_for_connected_element(adjacency, connectedElementIndex);

    if (adjacency.connectedSides.empty()) {
      std::ostringstream errmsg;
      errmsg << "Neighboring reciprocity check for elements "
             << adjacency.elemDataVec[adjacency.elementIndex].identifier << " and "
             << adjacency.elemDataVec[connectedElementIndex].identifier << " failed.";
      m_errorHandler(errmsg);
    }
  }

  bool equivalent_node_permutation(const std::vector<EntityId> &controlNodes,
      const std::vector<EntityId> &permutedNodes,
      const std::vector<OrdinalType> permutationOrdinals)
  {
    const size_t numNodes = controlNodes.size();

    if ((numNodes != permutedNodes.size()) || (numNodes != permutationOrdinals.size())) {
      return false;
    }

    bool equivalent = true;
    for (size_t i = 0; equivalent && i < numNodes; ++i) {
      equivalent = controlNodes[permutationOrdinals[i]] == permutedNodes[i];
    }

    return equivalent;
  }

  std::pair<bool, PermutationType> get_permutation(const Topology &topology,
      const std::vector<EntityId> &controlNodes,
      const std::vector<EntityId> &permutedNodes,
      unsigned numPermutations)
  {
    PermutationType permutation = Topology::InvalidPermutation;
    bool equivalent = false;

    if (controlNodes.size() != permutedNodes.size()) return std::make_pair(equivalent, permutation);

    if (numPermutations > topology.num_permutations()) {
      std::ostringstream errmsg;
      errmsg << "Invalid number of permutations to check: " << numPermutations;
      m_errorHandler(errmsg);
    }

    std::vector<OrdinalType> permutationOrdinals;

    for (PermutationType i = 0; i < numPermutations; ++i) {
      if (topology.fill_permutation_indices(i, permutationOrdinals)) {
        if (equivalent_node_permutation(controlNodes, permutedNodes, permutationOrdinals)) {
          equivalent = true;
          permutation = i;
          break;
        }
      }
    }

    return std::make_pair(equivalent, permutation);
  }

  std::pair<bool, PermutationType> get_permutation(
      const Topology &topology, const std::vector<EntityId> &controlNodes, const std::vector<EntityId> &permutedNodes)
  {
    return get_permutation(topology, controlNodes, permutedNodes, topology.num_permutations());
  }

  std::pair<bool, PermutationType> get_positive_permutation(
      const Topology &topology, const std::vector<EntityId> &controlNodes, const std::vector<EntityId> &permutedNodes)
  {
    return get_permutation(topology, controlNodes, permutedNodes, topology.num_positive_permutations());
  }

  bool has_same_polarity(const ElementData<EntityId, Topology> &thisElem,
      const int thisSide,
      const ElementData<EntityId, Topology> &thatElem,
      const int thatSide)
  {
    std::vector<EntityId> thisNodes = get_side_nodes(thisElem, thisSide);
    std::vector<EntityId> thatNodes = get_side_nodes(thatElem, thatSide);

    Topology sideTopo = thisElem.topology.side_topology(thisSide);
    std::pair<bool, PermutationType> result = get_positive_permutation(sideTopo, thisNodes, thatNodes);

    bool samePolarity = result.first;
    return samePolarity;
  }

  bool verify_entry(IndexType elemIndex, int side)
  {
    if (INVALID_INDEX == elemIndex) return false;

    auto it = m_indexGraph.find(elemIndex);
    if (it == m_indexGraph.end()) return false;

    int numSides = it->second.numSides;
    if (side < 1 || side > numSides) return false;

    return true;
  }

  std::vector<FaceConnection> get_reciprocity(CurrentAdjacency &adjacency)
  {
    std::vector<FaceConnection> reciprocity;
    const FaceConnections &thisEntry = m_indexGraph[adjacency.elementIndex];

    for (const FaceConnection &connectionToThatElement : thisEntry.connections) {
      IndexType thatIndex = connectionToThatElement.thatElement;
      int thatSide = connectionToThatElement.thatSide;

      if (!verify_entry(thatIndex, thatSide)) continue;
      const FaceConnections &thatEntry = m_indexGraph[thatIndex];

      const FaceConnection connectionToThisElement(thatSide, adjacency.elementIndex, adjacency.side);
      if (std::binary_search(thatEntry.connections.begin(), thatEntry.connections.end(), connectionToThisElement)) {
        reciprocity.push_back(connectionToThatElement);
      }
    }

    return reciprocity;
  }

  void break_connection(CurrentAdjacency &adjacency)
  {
    std::vector<FaceConnection> reciprocity = get_reciprocity(adjacency);

    if (!reciprocity.empty()) {
      FaceConnections &thisEntry = m_indexGraph[adjacency.elementIndex];

      for (FaceConnection &connectionToThatElement : reciprocity) {
        IndexType thatIndex = connectionToThatElement.thatElement;
        int thatSide = connectionToThatElement.thatSide;

        FaceConnections &thatEntry = m_indexGraph[thatIndex];
        FaceConnection connectionToThisElement(thatSide, adjacency.elementIndex, adjacency.side);

        thisEntry.remove(connectionToThatElement);
        thatEntry.remove(connectionToThisElement);
      }
    }
  }

  inline bool is_shell_shell_connection(
      const ElementData<EntityId, Topology> &thisElem, const ElementData<EntityId, Topology> &thatElem)
  {
    return thisElem.topology.is_shell() && thatElem.topology.is_shell();
  }

  inline bool is_shell_solid_connection(
      const ElementData<EntityId, Topology> &thisElem, const ElementData<EntityId, Topology> &thatElem)
  {
    return thisElem.topology.is_shell() && !thatElem.topology.is_shell();
  }

  inline bool is_solid_shell_connection(
      const ElementData<EntityId, Topology> &thisElem, const ElementData<EntityId, Topology> &thatElem)
  {
    return !thisElem.topology.is_shell() && thatElem.topology.is_shell();
  }

  inline bool is_solid_solid_connection(
      const ElementData<EntityId, Topology> &thisElem, const ElementData<EntityId, Topology> &thatElem)
  {
    return !thisElem.topology.is_shell() && !thatElem.topology.is_shell();
  }

  using Criterion =
      std::function<bool(const ElementData<EntityId, Topology> &elem1, const ElementData<EntityId, Topology> &elem2)>;


  bool has_connection_type(const std::vector<ElementData<EntityId, Topology>> &elemDataVec,
      size_t thisIndex,
      int thisSide,
      Criterion criterion)
  {
    const ElementData<EntityId, Topology> &thisElem = elemDataVec[thisIndex];
    const FaceConnections &thisEntry = m_indexGraph[thisIndex];

    for (const FaceConnection &connection : thisEntry.connections) {
      IndexType thatIndex = connection.thatElement;

      if (connection.thisSide != thisSide || INVALID_INDEX == thatIndex) continue;

      const ElementData<EntityId, Topology> &thatElem = elemDataVec[thatIndex];
      if (criterion(thisElem, thatElem)) {
        return true;
      }
    }

    return false;
  }

  bool has_any_shell_connection(
      const std::vector<ElementData<EntityId, Topology>> &elemDataVec, size_t thisIndex, int thisSide)
  {
    Criterion criterion = [&](const ElementData<EntityId, Topology> &elem1,
                              const ElementData<EntityId, Topology> &elem2) { return elem2.topology.is_shell(); };

    return has_connection_type(elemDataVec, thisIndex, thisSide, criterion);
  }

  bool has_shell_shell_connection(
      const std::vector<ElementData<EntityId, Topology>> &elemDataVec, size_t thisIndex, int thisSide)
  {
    Criterion criterion = [&](const ElementData<EntityId, Topology> &elem1,
                              const ElementData<EntityId, Topology> &elem2) {
      return is_shell_shell_connection(elem1, elem2);
    };

    return has_connection_type(elemDataVec, thisIndex, thisSide, criterion);
  }

  bool has_shell_solid_connection(
      const std::vector<ElementData<EntityId, Topology>> &elemDataVec, size_t thisIndex, int thisSide)
  {
    Criterion criterion = [&](const ElementData<EntityId, Topology> &elem1,
                              const ElementData<EntityId, Topology> &elem2) {
      return is_shell_solid_connection(elem1, elem2);
    };

    return has_connection_type(elemDataVec, thisIndex, thisSide, criterion);
  }

  bool has_solid_shell_connection(
      const std::vector<ElementData<EntityId, Topology>> &elemDataVec, size_t thisIndex, int thisSide)
  {
    Criterion criterion = [&](const ElementData<EntityId, Topology> &elem1,
                              const ElementData<EntityId, Topology> &elem2) {
      return is_solid_shell_connection(elem1, elem2);
    };

    return has_connection_type(elemDataVec, thisIndex, thisSide, criterion);
  }

  bool has_solid_solid_connection(
      const std::vector<ElementData<EntityId, Topology>> &elemDataVec, size_t thisIndex, int thisSide)
  {
    Criterion criterion = [&](const ElementData<EntityId, Topology> &elem1,
                              const ElementData<EntityId, Topology> &elem2) {
      return is_solid_solid_connection(elem1, elem2);
    };

    return has_connection_type(elemDataVec, thisIndex, thisSide, criterion);
  }

  void add_connection(CurrentAdjacency &adjacency, size_t connectedElementIndex, int otherSide)
  {
    const ElementData<EntityId, Topology> &thisElem = adjacency.elemDataVec[adjacency.elementIndex];
    const ElementData<EntityId, Topology> &thatElem = adjacency.elemDataVec[connectedElementIndex];

    bool doConnect = false;
    bool breakConnection = false;

    if (is_shell_solid_connection(thisElem, thatElem)) {
      doConnect = !has_same_polarity(thisElem, adjacency.side, thatElem, otherSide);

      if (has_solid_solid_connection(adjacency.elemDataVec, connectedElementIndex, otherSide)) {
        breakConnection = doConnect;
      }
    } else if (is_solid_shell_connection(thisElem, thatElem)) {
      doConnect = !has_same_polarity(thisElem, adjacency.side, thatElem, otherSide);

      if (has_solid_solid_connection(adjacency.elemDataVec, adjacency.elementIndex, adjacency.side)) {
        breakConnection = doConnect;
      }
    } else if (is_solid_solid_connection(thisElem, thatElem)) {
      doConnect = !has_any_shell_connection(adjacency.elemDataVec, adjacency.elementIndex, adjacency.side) &&
                  !has_any_shell_connection(adjacency.elemDataVec, connectedElementIndex, otherSide) &&
                  !has_same_polarity(thisElem, adjacency.side, thatElem, otherSide);
    }

    if (breakConnection) {
      break_connection(adjacency);
    }

    if (doConnect) {
      m_indexGraph[adjacency.elementIndex].add(FaceConnection(adjacency.side, connectedElementIndex, otherSide));
      m_indexGraph[connectedElementIndex].add(FaceConnection(otherSide, adjacency.elementIndex, adjacency.side));
    }
  }

  void set_side_connectivity(CurrentAdjacency &adjacency, size_t connectedElementIndex)
  {
    fill_sides_for_connected_element(adjacency, connectedElementIndex);

    for (int otherSide : adjacency.connectedSides) {
      add_connection(adjacency, connectedElementIndex, otherSide);
    }
  }

  void enforce_coincident_shell_ownership(const std::vector<ElementData<EntityId, Topology>> &elemDataVec,
      IndexType connectedElemIndex1,
      IndexType connectedElemIndex2)
  {
    const ElementData<EntityId, Topology> &connectedElem1 = elemDataVec[connectedElemIndex1];
    const ElementData<EntityId, Topology> &connectedElem2 = elemDataVec[connectedElemIndex2];

    if (connectedElem1.topology.is_shell() && connectedElem2.topology.is_shell()) {
      if (connectedElem1.proc != connectedElem2.proc) {
        std::ostringstream errmsg;
        errmsg << "Invalid proc ownership for co-incident shells " << connectedElem1.identifier << " (proc "
               << connectedElem1.proc << ") and " << connectedElem2.identifier << " (proc " << connectedElem2.proc
               << ")."
               << " Co-incident shells must all exist on the same processor";
        m_errorHandler(errmsg);
      }
    }
  }

  void process_side_connectivity(
      CurrentAdjacency &adjacency, const std::unordered_map<EntityId, std::set<size_t>> &elementsForNode)
  {
    const ElementData<EntityId, Topology> &elemData = adjacency.elemDataVec[adjacency.elementIndex];
    int side = adjacency.side;

    std::set<size_t> elementIndicesConnectedToSide =
        get_element_indices_with_common_nodes_on_side(elemData, side, elementsForNode);
    for (size_t connectedElementIndex : elementIndicesConnectedToSide) {
      if (connectedElementIndex != adjacency.elementIndex) {
        enforce_coincident_shell_ownership(adjacency.elemDataVec, adjacency.elementIndex, connectedElementIndex);
        set_side_connectivity(adjacency, connectedElementIndex);
      }
    }
  }

  void build_side_connectivity_graph(const std::vector<ElementData<EntityId, Topology>> &elemDataVec,
      const std::vector<size_t> &elementIndices,
      const std::unordered_map<EntityId, std::set<size_t>> &elementsForNode)
  {
    initialize_side_connectivity_graph(elemDataVec, elementIndices);

    for (size_t elementIndex : elementIndices) {
      const ElementData<EntityId, Topology> &elemData = elemDataVec[elementIndex];

      int numSides = elemData.topology.num_sides();
      for (int side = 1; side <= numSides; ++side) {
        if (m_indexGraph[elementIndex].sideReference[side - 1] == 0) {
          CurrentAdjacency adjacency(elemDataVec, elementIndex, side);
          process_side_connectivity(adjacency, elementsForNode);
        }
      }
    }
  }

  void initialize_side_connectivity_graph(
      const std::vector<ElementData<EntityId, Topology>> &elemDataVec, const std::vector<size_t> &elementIndex)
  {
    for (size_t index : elementIndex) {
      const ElementData<EntityId, Topology> &elemData = elemDataVec[index];
      m_indexGraph[index] = FaceConnections(elemData.topology.num_sides());
    }
  }

  std::unordered_map<EntityId, std::set<size_t>> get_elements_for_node_map(
      const std::vector<ElementData<EntityId, Topology>> &elemDataVec, const std::vector<size_t> &elementIndex)
  {
    std::unordered_map<EntityId, std::set<size_t>> elementsForNode;
    for (size_t index : elementIndex) {
      const ElementData<EntityId, Topology> &elemData = elemDataVec[index];

      for (const EntityId &nodeId : elemData.nodeIds) {
        elementsForNode[nodeId].insert(index);
      }
    }

    return elementsForNode;
  }

  bool element_is_in_selected_blocks(
      const ElementData<EntityId, Topology> &elemData, const std::vector<std::string> &sortedSelectedBlocks)
  {
    if (sortedSelectedBlocks.empty()) return true;
    return std::binary_search(
        sortedSelectedBlocks.begin(), sortedSelectedBlocks.end(), elemData.partName, StringCaseCompLess());
  }

  bool is_selected_element(const TextMeshData<EntityId, Topology> &data,
      const ElementData<EntityId, Topology> &elemData,
      const std::vector<std::string> &sortedSelectedBlocks,
      int proc)
  {
    bool isGloballySelected = (ANY_PROC == proc);
    bool isLocallySelected = (elemData.proc == proc);
    bool hasLocalNode = element_has_any_node_on_proc(data, elemData, proc);
    bool isInSelectedBlocks = element_is_in_selected_blocks(elemData, sortedSelectedBlocks);

    return isInSelectedBlocks && (isGloballySelected || isLocallySelected || hasLocalNode);
  }

  std::vector<size_t> get_local_and_aura_elements(
      const TextMeshData<EntityId, Topology> &data, const std::vector<std::string> &selectedBlocks, int proc)
  {
    std::vector<size_t> localAndAuraElementIndex;
    localAndAuraElementIndex.reserve(data.elementDataVec.size());

    std::vector<std::string> sortedSelectedBlocks;
    for (const std::string &block : selectedBlocks) {
      sortedSelectedBlocks.push_back(block);
    }
    std::sort(sortedSelectedBlocks.begin(), sortedSelectedBlocks.end(), StringCaseCompLess());

    for (size_t i = 0; i < data.elementDataVec.size(); ++i) {
      const ElementData<EntityId, Topology> &elemData = data.elementDataVec[i];

      if (is_selected_element(data, elemData, sortedSelectedBlocks, proc)) {
        localAndAuraElementIndex.push_back(i);
      }
    }

    localAndAuraElementIndex.resize(localAndAuraElementIndex.size());
    return localAndAuraElementIndex;
  }

  bool element_has_any_node_on_proc(
      const TextMeshData<EntityId, Topology> &data, const ElementData<EntityId, Topology> &elemData, int proc)
  {
    for (const EntityId &nodeId : elemData.nodeIds) {
      const std::set<int> &procsForNode = data.procs_for_node(nodeId);
      if (procsForNode.count(proc) > 0) {
        return true;
      }
    }

    return false;
  }

  ErrorHandler m_errorHandler;
  std::unordered_map<size_t, FaceConnections> m_indexGraph;
};

template <typename EntityId, typename Topology>
struct SidesetData : public EntityGroupData<EntityId, SidesetDataType<EntityId>> {
  using DataType = SidesetDataType<EntityId>;
  using BaseClass = EntityGroupData<EntityId, SidesetDataType<EntityId>>;

  void set_split_type(SplitType splitType) { m_sidesetSplitter.set_split_type(splitType); }
  SplitType get_split_type() const { return m_sidesetSplitter.get_split_type(); }

  void set_skin_blocks(const std::vector<std::string> &skinBlocks) { m_skinBlocks = skinBlocks; }

  void set_error_handler(ErrorHandler errorHandler)
  {
    m_adjacencyGraph.set_error_handler(errorHandler);
    m_sidesetSplitter.set_error_handler(errorHandler);
  }

  void split(const std::vector<ElementData<EntityId, Topology>> &elementData)
  {
    m_sidesetSplitter.split(*this, elementData);
  }

  void skin_blocks(const TextMeshData<EntityId, Topology> &textMeshData)
  {
    using FaceConnections = typename SideAdjacencyGraph<EntityId, Topology>::FaceConnections;
    using FaceConnection = typename SideAdjacencyGraph<EntityId, Topology>::FaceConnection;

    populate_skin_blocks(textMeshData.partIds);

    if (!m_skinBlocks.empty()) {
      m_adjacencyGraph.create_graph(textMeshData, m_skinBlocks);

      for (auto iter = m_adjacencyGraph.begin(); iter != m_adjacencyGraph.end(); iter++) {
        size_t elemIndex = iter->first;
        const FaceConnections &faceConnections = iter->second;

        std::vector<bool> hasConnection(faceConnections.numSides, false);
        for (const FaceConnection &connection : faceConnections.connections) {
          hasConnection[connection.thisSide - 1] = true;
        }

        for (unsigned i = 0; i < faceConnections.numSides; i++) {
          if (!hasConnection[i]) {
            EntityId elemId = textMeshData.elementDataVec[elemIndex].identifier;
            int side = i + 1;
            BaseClass::data.push_back(std::make_pair(elemId, side));
          }
        }
      }
    }
  }

  std::vector<size_t> get_sideblock_indices_local_to_proc(const SideBlockInfo &info, int proc) const
  {
    return m_sidesetSplitter.get_indices_local_to_proc(info.sideIndex, proc);
  }

  SideBlockInfo get_side_block_info(const std::string &sideBlockName) const
  {
    return m_sidesetSplitter.get_side_block_info(sideBlockName);
  }

  std::vector<SideBlockInfo> get_side_block_info() const { return m_sidesetSplitter.get_side_block_info(); }

  bool has_standard_name() const
  {
    if (BaseClass::has_name()) {
      std::pair<unsigned, bool> result = get_id_from_part_name(BaseClass::name, "SURFACE_");
      return result.second;
    }

    return false;
  }

 private:
  void populate_skin_blocks(const PartIdMapping &partIds)
  {
    bool skinAll = false;
    for (const std::string &block : m_skinBlocks) {
      if (0 == strcasecmp("all", block.c_str())) {
        skinAll = true;
        break;
      }
    }

    if (skinAll) {
      m_skinBlocks = partIds.get_part_names();
    }
  }

  SideAdjacencyGraph<EntityId, Topology> m_adjacencyGraph;
  SidesetSplitter<EntityId, Topology> m_sidesetSplitter;
  std::vector<std::string> m_skinBlocks;
};

template <typename EntityId, typename Topology>
class Sidesets : public EntityGroup<EntityId, SidesetData<EntityId, Topology>>
{
 public:
  using BaseClass = EntityGroup<EntityId, SidesetData<EntityId, Topology>>;

  Sidesets() : BaseClass("SIDESET", "SURFACE_", {"BLOCK_", "NODELIST_"}) {}

  void set_error_handler(ErrorHandler errorHandler) override
  {
    BaseClass::set_error_handler(errorHandler);

    for (SidesetData<EntityId, Topology> &sidesetData : BaseClass::m_groupDataVec) {
      sidesetData.set_error_handler(errorHandler);
    }
  }

  void finalize_parse(const TextMeshData<EntityId, Topology> &data)
  {
    BaseClass::finalize_parse();

    for (SidesetData<EntityId, Topology> &sidesetData : BaseClass::m_groupDataVec) {
      sidesetData.skin_blocks(data);
      sidesetData.split(data.elementDataVec);
    }
  }
};

template <typename EntityId, typename Topology>
struct TextMeshData {
  unsigned spatialDim;
  std::vector<ElementData<EntityId, Topology>> elementDataVec;
  PartIdMapping partIds;
  std::set<EntityId> nodeIds;
  Coordinates<EntityId> coords;
  Sidesets<EntityId, Topology> sidesets;
  Nodesets<EntityId> nodesets;

  TextMeshData() : spatialDim(0) {}

  void add_element(const ElementData<EntityId, Topology> &elem)
  {
    elementDataVec.push_back(elem);
    for (const EntityId &nodeId : elem.nodeIds) {
      nodeIds.insert(nodeId);
      associate_node_with_proc(nodeId, elem.proc);
    }
  }

  const std::set<EntityId> &nodes_on_proc(int proc) const
  {
    auto it = m_nodesOnProc.find(proc);
    return it != m_nodesOnProc.end() ? it->second : m_emptyNodes;
  }

  unsigned num_nodes_on_proc(int proc) const
  {
    auto it = m_nodesOnProc.find(proc);
    return it != m_nodesOnProc.end() ? it->second.size() : 0;
  }

  const std::set<int> &procs_for_node(const EntityId nodeId) const
  {
    auto it = m_procsForNode.find(nodeId);
    return it != m_procsForNode.end() ? it->second : m_emptyProcs;
  }

 private:
  void associate_node_with_proc(const EntityId nodeId, int proc)
  {
    m_procsForNode[nodeId].insert(proc);
    m_nodesOnProc[proc].insert(nodeId);
  }

  std::unordered_map<EntityId, std::set<int>> m_procsForNode;
  std::unordered_map<int, std::set<EntityId>> m_nodesOnProc;

  std::set<int> m_emptyProcs;
  std::set<EntityId> m_emptyNodes;
};

class TextMeshLexer
{
 public:
  TextMeshLexer() : m_currentIndex(0), m_token(""), m_isNumber(false) {}

  void set_input_string(const std::string &input)
  {
    m_input = input;
    m_currentIndex = 0;
    read_next_token();
  }

  int get_int()
  {
    read_next_token();
    return std::stoi(m_oldToken);
  }

  unsigned get_unsigned()
  {
    read_next_token();
    return std::stoul(m_oldToken);
  }

  std::string get_string()
  {
    read_next_token();
    return make_upper_case(m_oldToken);
  }

  void get_newline() { read_next_token(); }

  bool has_token() const { return m_token != ""; }
  bool has_newline() const { return m_token == "\n"; }
  bool has_number() const { return has_token() && m_isNumber; }
  bool has_string() const { return has_token() && !has_number() && !has_newline(); }

 private:
  void read_next_token()
  {
    m_oldToken = m_token;

    if (char_is_newline()) {
      m_isNumber = false;
      m_token = "\n";
      m_currentIndex++;
      return;
    }

    m_token = "";
    m_isNumber = true;

    while (has_more_input()) {
      if (char_is_whitespace()) {
        m_currentIndex++;
        continue;
      }

      if (char_is_comma()) {
        m_currentIndex++;
        break;
      }

      if (char_is_newline()) {
        break;
      }

      m_isNumber &= char_is_digit();
      m_token += current_char();
      m_currentIndex++;
    }
  }

  bool has_more_input() { return m_currentIndex < m_input.size(); }

  bool char_is_whitespace() { return current_char() == ' '; }
  bool char_is_comma() { return current_char() == ','; }
  bool char_is_newline() { return current_char() == '\n'; }
  bool char_is_digit() { return std::isdigit(static_cast<unsigned char>(current_char())); }

  char current_char() { return m_input[m_currentIndex]; }

  std::string make_upper_case(std::string str)
  {
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    return str;
  }

  std::string m_input;
  unsigned m_currentIndex;

  std::string m_oldToken;
  std::string m_token;

  bool m_isNumber;
};

template <typename EntityId>
class SidesetParser
{
 public:
  SidesetParser() : m_splitType(NO_SPLIT)
  {
    ErrorHandler errorHandler = [](const std::ostringstream &errmsg) { default_error_handler(errmsg); };
    set_error_handler(errorHandler);
  }

  void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

  std::string get_name() { return m_name; }

  const std::vector<std::pair<EntityId, int>> &get_sideset_data() { return m_elemSidePairs; }

  SplitType get_split_type() { return m_splitType; }

  const std::vector<std::string> &get_skin_blocks() const { return m_skinnedBlocks; }

  void parse(const std::string &parseData)
  {
    auto options = get_tokens(parseData, ";");

    for (const auto &option : options) {
      parse_option_group(option);
    }
  }

  void verify_parse() const
  {
    if (!m_skinnedBlocks.empty() && !m_elemSidePairs.empty()) {
      std::ostringstream errmsg;
      errmsg << "Error!  Sideset with name: " << m_name
             << " is attempting to set element/side pair data *AND* use skinning.";
      m_errorHandler(errmsg);
    }
  }

 private:
  void parse_option(std::string optionName, const std::string &optionValue)
  {
    convert_to_lower_case(optionName);

    if (optionName == "name") {
      parse_name(optionValue);
    } else if (optionName == "data") {
      parse_element_side_pairs(optionValue);
    } else if (optionName == "split") {
      parse_split_type(optionValue);
    } else if (optionName == "skin") {
      parse_skin_blocks(optionValue);
    } else {
      std::ostringstream errmsg;
      errmsg << "Unrecognized sideset option: " << optionName;
      m_errorHandler(errmsg);
    }
  }

  void parse_option_group(const std::string &option)
  {
    if (!option.empty()) {
      auto optionTokens = get_tokens(option, "=");

      if (optionTokens.size() != 2) {
        std::ostringstream errmsg;
        errmsg << "Unrecognized sideset option: " << option;
        m_errorHandler(errmsg);
      }

      parse_option(optionTokens[0], optionTokens[1]);
    }
  }

  void parse_name(const std::string &data) { m_name = data; }

  void parse_skin_blocks(const std::string &data) { m_skinnedBlocks = get_tokens(data, ","); }

  void parse_element_side_pairs(const std::string &data)
  {
    auto sidesetData = get_tokens(data, ",");

    if (sidesetData.size() % 2 != 0) {
      std::ostringstream errmsg;
      errmsg << "Unmatched element/ordinal pairs in sideset data: " << data;
      m_errorHandler(errmsg);
    }

    for (unsigned i = 0; i < sidesetData.size(); i += 2) {
      EntityId elem = std::stoull(sidesetData[i]);
      int side = std::stoi(sidesetData[i + 1]);

      if (side <= 0) {
        std::ostringstream errmsg;
        errmsg << "Invalid element/ordinal pair {" << sidesetData[i] << "," << sidesetData[i + 1] << "}";
        m_errorHandler(errmsg);
      }

      m_elemSidePairs.push_back(std::make_pair(elem, side));
    }
  }

  void parse_split_type(std::string splitName)
  {
    convert_to_lower_case(splitName);

    if (splitName == "none") {
      m_splitType = NO_SPLIT;
    } else if (splitName == "block") {
      m_splitType = ELEMENT_BLOCK;
    } else if (splitName == "topology") {
      m_splitType = TOPOLOGY;
    } else {
      std::ostringstream errmsg;
      errmsg << "Unrecognized sideset split type: " << splitName;
      m_errorHandler(errmsg);
    }
  }

  std::vector<std::pair<EntityId, int>> m_elemSidePairs;
  std::string m_name;
  std::vector<std::string> m_skinnedBlocks;
  SplitType m_splitType;
  ErrorHandler m_errorHandler;
};

template <typename EntityId>
class NodesetParser
{
 public:
  NodesetParser()
  {
    ErrorHandler errorHandler = [](const std::ostringstream &errmsg) { default_error_handler(errmsg); };
    set_error_handler(errorHandler);
  }

  void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

  std::string get_name() { return m_name; }

  const std::vector<EntityId> &get_nodeset_data() { return m_nodeList; }

  void parse(const std::string &parseData)
  {
    auto options = get_tokens(parseData, ";");

    for (const auto &option : options) {
      parse_option_group(option);
    }
  }

  void verify_parse() const {}

 private:
  void parse_option(std::string optionName, const std::string &optionValue)
  {
    convert_to_lower_case(optionName);

    if (optionName == "name") {
      parse_name(optionValue);
    } else if (optionName == "data") {
      parse_node_data(optionValue);
    } else {
      std::ostringstream errmsg;
      errmsg << "Unrecognized nodeset option: " << optionName;
      m_errorHandler(errmsg);
    }
  }

  void parse_option_group(const std::string &option)
  {
    if (!option.empty()) {
      auto optionTokens = get_tokens(option, "=");

      if (optionTokens.size() != 2) {
        std::ostringstream errmsg;
        errmsg << "Unrecognized nodeset option: " << option;
        m_errorHandler(errmsg);
      }

      parse_option(optionTokens[0], optionTokens[1]);
    }
  }

  void parse_name(const std::string &data) { m_name = data; }

  void parse_node_data(const std::string &data)
  {
    auto nodesetData = get_tokens(data, ",");

    for (const std::string &nodeString : nodesetData) {
      if (!is_number(nodeString)) {
        std::ostringstream errmsg;
        errmsg << "Unrecognized nodeset node id: " << nodeString;
        m_errorHandler(errmsg);
      }
      EntityId node = std::stoull(nodeString);
      m_nodeList.push_back(node);
    }
  }

  std::vector<EntityId> m_nodeList;
  std::string m_name;
  ErrorHandler m_errorHandler;
};

template <typename EntityId, typename Topology>
class TextMeshOptionParser
{
 private:
  static constexpr int INVALID_DIMENSION = -1;
  static constexpr int DEFAULT_DIMENSION = 3;

  enum ParsedOptions {
    PARSED_NONE = 0,
    PARSED_DIMENSION = 1L << 0,
    PARSED_COORDINATES = 1L << 1,
    PARSED_SIDESET = 1L << 2,
    PARSED_NODESET = 1L << 3
  };

 public:
  TextMeshOptionParser(TextMeshData<EntityId, Topology> &data, unsigned enforcedDimension)
      : m_parsedOptionMask(PARSED_NONE),
        m_parsedDimension(INVALID_DIMENSION),
        m_constructorEnforcedDimension(enforcedDimension),
        m_data(data)
  {
  }

  TextMeshOptionParser(TextMeshData<EntityId, Topology> &data)
      : m_parsedOptionMask(PARSED_NONE),
        m_parsedDimension(INVALID_DIMENSION),
        m_constructorEnforcedDimension(INVALID_DIMENSION),
        m_data(data)
  {
  }

  void set_error_handler(ErrorHandler errorHandler)
  {
    m_errorHandler = errorHandler;
  }

  std::string get_mesh_connectivity_description() const { return m_meshConnectivityDescription; }

  void initialize_parse(const std::string &parameters)
  {
    if (!parameters.empty()) {
      std::vector<std::string> optionGroups = get_tokens(parameters, "|");
      parse_options(optionGroups);

      m_meshConnectivityDescription = optionGroups[0];
    }

    validate_dimension();
    set_dimension();
  }

  void finalize_parse()
  {
    set_coordinates();
    m_data.sidesets.finalize_parse(m_data);
    m_data.nodesets.finalize_parse();
    validate_sidesets();
    validate_nodesets();
  }

 private:
  bool parsed_dimension_provided() { return m_parsedOptionMask & PARSED_DIMENSION; }

  bool enforced_dimension_provided() { return m_constructorEnforcedDimension != INVALID_DIMENSION; }

  void validate_dimension()
  {
    if (enforced_dimension_provided()) {
      if (parsed_dimension_provided() && m_constructorEnforcedDimension != m_parsedDimension) {
        std::ostringstream errmsg;
        errmsg << "Error!  An enforced dimension of " << m_constructorEnforcedDimension
               << " was provided but does not match the parsed value of " << m_parsedDimension << ".";
        m_errorHandler(errmsg);
      }
    }
  }

  void set_dimension()
  {
    if (enforced_dimension_provided()) {
      m_data.spatialDim = m_constructorEnforcedDimension;
    } else if (parsed_dimension_provided()) {
      m_data.spatialDim = m_parsedDimension;
    } else {
      m_data.spatialDim = DEFAULT_DIMENSION;
    }
  }

  void parse_dimension_option(const std::vector<std::string> &option)
  {
    if (parsed_dimension_provided()) {
      std::ostringstream errmsg;
      errmsg << "Spatial dimension has already been parsed! Check syntax.";
      m_errorHandler(errmsg);
    }

    if (option.size() == 2) {
      m_parsedDimension = std::stoull(option[1]);
      if (m_parsedDimension != 2 && m_parsedDimension != 3) {
        std::ostringstream errmsg;
        errmsg << "Error!  Parsed spatial dimension (" << m_parsedDimension << " not defined to be 2 or 3.";
        m_errorHandler(errmsg);
      }

      m_parsedOptionMask |= PARSED_DIMENSION;
    } else {
      std::ostringstream errmsg;
      errmsg << "Error!  Invalid spatial dimension syntax.";
      m_errorHandler(errmsg);
    }
  }

  void deallocate_raw_coordinates()
  {
    std::vector<double> swapVectorForDeallocation;
    m_rawCoordinates.swap(swapVectorForDeallocation);
  }

  void set_coordinates()
  {
    if (parsed_coordinates_provided()) {
      m_data.coords.set_coordinate_data(m_data.spatialDim, m_data.nodeIds, m_rawCoordinates);
      deallocate_raw_coordinates();
    }
  }

  bool parsed_coordinates_provided() { return m_parsedOptionMask & PARSED_COORDINATES; }

  void parse_coordinates_option(const std::vector<std::string> &coordinatesOptionGroup)
  {
    if (parsed_coordinates_provided()) {
      std::ostringstream errmsg;
      errmsg << "Coordinates have already been parsed! Check syntax.";
      m_errorHandler(errmsg);
    }

    if (coordinatesOptionGroup.size() > 1) {
      const std::vector<std::string> &coordinateTokens = get_tokens(coordinatesOptionGroup[1], ",");
      m_rawCoordinates.reserve(coordinateTokens.size());
      for (const auto &token : coordinateTokens) {
        double coord = std::stod(token);
        m_rawCoordinates.push_back(coord);
      }

      m_parsedOptionMask |= PARSED_COORDINATES;
    }
  }

  template <typename DataType>
  void check_name_collision_with_entity_sets(const EntityGroupData<EntityId, DataType> &groupData,
      const std::string &entityType,
      const std::set<std::string> &entitySetNames)
  {
    std::string groupName = groupData.name;
    convert_to_upper_case(groupName);

    if (entitySetNames.count(groupName) > 0) {
      std::ostringstream errmsg;
      errmsg << "Error! " << groupData.type << " with id: " << groupData.id << " and name: " << groupData.name
             << " is referencing " << entityType << " with same name.";
      m_errorHandler(errmsg);
    }
  }

  void check_sideset_element_reference()
  {
    for (const SidesetData<EntityId, Topology> &sidesetData : m_data.sidesets.get_group_data()) {
      for (const std::pair<EntityId, int> &elemSidePair : sidesetData.data) {
        EntityId id = elemSidePair.first;
        if (!std::binary_search(m_data.elementDataVec.begin(), m_data.elementDataVec.end(), id)) {
          std::ostringstream errmsg;
          errmsg << "Error!  Sideset with id: " << sidesetData.id << " and name: " << sidesetData.name
                 << " has reference to invalid element '" << id << "'.";
          m_errorHandler(errmsg);
        }
      }
    }
  }

  void check_sideset_name_collision()
  {
    std::set<std::string> elemBlockNames = transform_to_set(m_data.partIds.get_part_names());

    for (const SidesetData<EntityId, Topology> &sidesetData : m_data.sidesets.get_group_data()) {
      check_name_collision_with_entity_sets(sidesetData, "element block", elemBlockNames);
    }
  }

  void validate_sidesets()
  {
    check_sideset_element_reference();
    check_sideset_name_collision();
  }

  void check_nodeset_node_reference()
  {
    for (const NodesetData<EntityId> &nodesetData : m_data.nodesets.get_group_data()) {
      for (const EntityId nodeId : nodesetData.data) {
        if (m_data.nodeIds.count(nodeId) == 0) {
          std::ostringstream errmsg;
          errmsg << "Error!  Nodeset with id: " << nodesetData.id << " and name: " << nodesetData.name
                 << " has reference to invalid node '" << nodeId << "'.";
          m_errorHandler(errmsg);
        }
      }
    }
  }

  void check_nodeset_name_collision()
  {
    std::set<std::string> elemBlockNames = transform_to_set(m_data.partIds.get_part_names());
    std::set<std::string> sidesetNames = transform_to_set(m_data.sidesets.get_part_names());

    for (const NodesetData<EntityId> &nodesetData : m_data.nodesets.get_group_data()) {
      check_name_collision_with_entity_sets(nodesetData, "element block", elemBlockNames);
      check_name_collision_with_entity_sets(nodesetData, "sideset", sidesetNames);
    }
  }

  void validate_nodesets()
  {
    check_nodeset_node_reference();
    check_nodeset_name_collision();
  }

  void parse_sideset_option(const std::vector<std::string> &sidesetOptionGroup)
  {
    if (sidesetOptionGroup.size() > 1) {
      SidesetParser<EntityId> parser;
      parser.set_error_handler(m_errorHandler);
      parser.parse(sidesetOptionGroup[1]);
      parser.verify_parse();

      SidesetData<EntityId, Topology> *sideset =
          m_data.sidesets.add_group_data(parser.get_name(), parser.get_sideset_data());
      sideset->set_split_type(parser.get_split_type());
      sideset->set_skin_blocks(parser.get_skin_blocks());
      m_parsedOptionMask |= PARSED_SIDESET;
    }
  }

  void parse_nodeset_option(const std::vector<std::string> &nodesetOptionGroup)
  {
    if (nodesetOptionGroup.size() > 1) {
      NodesetParser<EntityId> parser;
      parser.set_error_handler(m_errorHandler);
      parser.parse(nodesetOptionGroup[1]);
      parser.verify_parse();

      m_data.nodesets.add_group_data(parser.get_name(), parser.get_nodeset_data());
      m_parsedOptionMask |= PARSED_NODESET;
    }
  }

  void print_help_message(std::ostream &out = std::cout)
  {
    out << "\nValid Options for TextMesh parameter string:\n"
           "\tPROC_ID,ELEM_ID,TOPOLOGY,{NODE CONNECTIVITY LIST}[,PART_NAME[,PART_ID]] (specifies "
           "element list .. first "
           "argument)\n"
           "\t|coordinates:x_1,y_1[,z_1], x_2,y_2[,z_2], ...., x_n,y_n[,z_n] (specifies coordinate data)\n"
           "\t|sideset:[name=<name>;] data=elem_1,side_1,elem_2,side_2,....,elem_n,side_n; "
           "[split=<block|topology|none>;] "
           "(specifies sideset data)\n"
           "\t|nodeset:[name=<name>;] data=node_1,node_2,....,node_n (specifies nodeset data)\n"
           "\t|dimension:spatialDimension (specifies spatial dimension .. default is 3)\n"
           "\t|help -- show this list\n\n";
  }

  void handle_unrecognized_option(const std::string &optionType)
  {
    std::ostringstream errmsg;
    fmt::print(errmsg, "ERROR: Unrecognized option '{}'.  It will be ignored.\n", optionType);
    m_errorHandler(errmsg);
  }

  void parse_options(const std::vector<std::string> &optionGroups)
  {
    for (size_t i = 1; i < optionGroups.size(); i++) {
      std::vector<std::string> optionGroup = get_tokens(optionGroups[i], ":");
      std::string optionType = optionGroup[0];
      convert_to_lower_case(optionType);

      if (optionType == "coordinates") {
        parse_coordinates_option(optionGroup);
      } else if (optionType == "dimension") {
        parse_dimension_option(optionGroup);
      } else if (optionType == "sideset") {
        parse_sideset_option(optionGroup);
      } else if (optionType == "nodeset") {
        parse_nodeset_option(optionGroup);
      } else if (optionType == "help") {
        print_help_message();
      } else {
        handle_unrecognized_option(optionType);
      }
    }
  }

  unsigned long m_parsedOptionMask;

  int m_parsedDimension;
  int m_constructorEnforcedDimension;

  std::string m_meshConnectivityDescription;

  std::vector<double> m_rawCoordinates;
  ErrorHandler m_errorHandler;

  TextMeshData<EntityId, Topology> &m_data;
};

template <typename EntityId, typename TopologyMapping>
class TextMeshParser
{
 private:
  using Topology = typename TopologyMapping::Topology;

 public:
  TextMeshParser(unsigned enforcedDimension) : m_optionParser(m_data, enforcedDimension) { initialize_constructor(); }

  TextMeshParser() : m_optionParser(m_data) { initialize_constructor(); }

  TextMeshData<EntityId, Topology> parse(const std::string &meshDescription)
  {
    initialize_parse(meshDescription);
    parse_description();
    finalize_parse();
    return m_data;
  }

  void set_error_handler(ErrorHandler errorHandler)
  {
    m_errorHandler = errorHandler;
    m_data.partIds.set_error_handler(errorHandler);
    m_data.coords.set_error_handler(errorHandler);
    m_data.sidesets.set_error_handler(errorHandler);
    m_data.nodesets.set_error_handler(errorHandler);
    m_optionParser.set_error_handler(errorHandler);
  }

 private:
  void initialize_constructor()
  {
    ErrorHandler errorHandler = [](const std::ostringstream &errmsg) { default_error_handler(errmsg); };
    set_error_handler(errorHandler);
    m_topologyMapping.initialize_topology_map();
  }

  void initialize_connectivity_parse(const std::string &meshDescription)
  {
    m_lexer.set_input_string(meshDescription);
    m_lineNumber = 1;
    validate_required_field(m_lexer.has_token());
  }

  void initialize_parse(const std::string &meshDescription)
  {
    m_optionParser.initialize_parse(meshDescription);
    initialize_connectivity_parse(m_optionParser.get_mesh_connectivity_description());
  }

  void finalize_parse() { m_optionParser.finalize_parse(); }

  void parse_description()
  {
    while (m_lexer.has_token()) {
      ElementData<EntityId, Topology> elemData = parse_element();
      m_data.add_element(elemData);

      validate_no_extra_fields();
      parse_newline();
    }

    std::sort(m_data.elementDataVec.begin(), m_data.elementDataVec.end(), ElementDataLess<EntityId, Topology>());
  }

  ElementData<EntityId, Topology> parse_element()
  {
    ElementData<EntityId, Topology> elem;
    elem.proc = parse_proc_id();
    elem.identifier = parse_elem_id();
    elem.topology = parse_topology();
    elem.nodeIds = parse_node_ids(elem.topology);
    elem.partName = parse_part(elem.topology);
    return elem;
  }

  int parse_proc_id()
  {
    validate_required_field(m_lexer.has_number());
    return parse_int();
  }

  EntityId parse_elem_id()
  {
    validate_required_field(m_lexer.has_number());
    return parse_unsigned();
  }

  Topology parse_topology()
  {
    validate_required_field(m_lexer.has_string());
    std::string topologyName = parse_string();

    Topology topology = m_topologyMapping.topology(topologyName);
    validate_topology(topology, topologyName);

    return topology;
  }

  std::vector<EntityId> parse_node_ids(const Topology &topology)
  {
    std::vector<EntityId> nodeIds;
    while (m_lexer.has_number()) {
      nodeIds.push_back(parse_unsigned());
    }
    validate_node_count(topology, nodeIds.size());
    return nodeIds;
  }

  std::string parse_part(const Topology &topology)
  {
    std::string partName;

    if (m_lexer.has_string()) {
      partName = parse_string();
    } else {
      partName = "block_" + topology.name();
    }

    if (m_lexer.has_number()) {
      unsigned partId = parse_unsigned();
      m_data.partIds.register_part_name_with_id(partName, partId);
    } else {
      m_data.partIds.register_part_name(partName);
    }

    return partName;
  }

  int parse_int() { return m_lexer.get_int(); }
  unsigned parse_unsigned() { return m_lexer.get_unsigned(); }
  std::string parse_string() { return m_lexer.get_string(); }

  void parse_newline()
  {
    m_lexer.get_newline();
    m_lineNumber++;
  }

  void validate_required_field(bool hasNextRequiredField)
  {
    if (!hasNextRequiredField) {
      std::ostringstream errmsg;
      errmsg << "Error!  Each line must contain the following fields (with at least one node):  "
                "Processor, GlobalId, Element Topology, NodeIds.  Error on line "
             << m_lineNumber << ".";
      m_errorHandler(errmsg);
    }
  }

  void validate_no_extra_fields()
  {
    bool requiredCondition = !m_lexer.has_token() || m_lexer.has_newline();
    if (!requiredCondition) {
      std::ostringstream errmsg;
      errmsg << "Error!  Each line should not contain more than the following fields (with at "
                "least one node):  "
                "Processor, GlobalId, Element Topology, NodeIds, Part Name, PartId.  "
                "Error on line "
             << m_lineNumber << ".";
      m_errorHandler(errmsg);
    }
  }

  void validate_topology(const Topology &topology, const std::string &providedName)
  {
    if (topology == m_topologyMapping.invalid_topology()) {
      std::ostringstream errmsg;
      errmsg << "Error!  Topology = >>" << providedName << "<< is invalid from line " << m_lineNumber << ".";
      m_errorHandler(errmsg);
    }

    if (!topology.defined_on_spatial_dimension(m_data.spatialDim)) {
      std::ostringstream errmsg;
      errmsg << "Error on input line " << m_lineNumber << ".  Topology = " << topology
             << " is not defined on spatial dimension = " << m_data.spatialDim << " set in parser.";
      m_errorHandler(errmsg);
    }
  }

  void validate_node_count(const Topology &topology, size_t numNodes)
  {
    size_t numTopologyNodes = topology.num_nodes();
    if (numNodes != numTopologyNodes) {
      std::ostringstream errmsg;
      errmsg << "Error!  The input line appears to contain " << numNodes << " nodes, but the topology " << topology
             << " needs " << numTopologyNodes << " nodes on line " << m_lineNumber << ".";
      m_errorHandler(errmsg);
    }
  }

  unsigned m_lineNumber = 0;
  TextMeshData<EntityId, Topology> m_data;
  TextMeshLexer m_lexer;
  TopologyMapping m_topologyMapping;

  ErrorHandler m_errorHandler;

  TextMeshOptionParser<EntityId, Topology> m_optionParser;
};

}  // namespace text_mesh
}  // namespace Iotm
