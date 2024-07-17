// Copyright(C) 1999-2020, 2022, 2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.

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
#include <strings.h>

#include "Iotm_TextMeshFuncs.h"
#include "Iotm_TextMeshDataTypes.h"
#include "Iotm_TextMeshEntityGroup.h"

// clang-format on
// #######################   End Clang Header Tool Managed Headers  ########################
namespace Iotm {
  namespace text_mesh {

    using ErrorHandler = std::function<void(const std::ostringstream &)>;

    struct EdgeBlockInfo
    {
      std::string         name{};
      std::string         parentName{};
      std::string         edgeTopology{};
      std::string         elementTopology{};
      std::string         touchingBlock{};
      std::vector<size_t> edgeIndex{};
      unsigned            numNodesPerEdge{0};
    };

    template <typename EntityId> using EdgesetDataType = std::pair<EntityId, int>;

    template <typename EntityId, typename Topology> struct EdgesetData;

    template <typename EntityId, typename Topology> class EdgesetSplitter
    {
    public:
      explicit EdgesetSplitter(SplitType splitType) : m_splitType(splitType)
      {
        ErrorHandler errorHandler = [](const std::ostringstream &errmsg) {
          default_error_handler(errmsg);
        };
        set_error_handler(errorHandler);
      }

      EdgesetSplitter() : m_splitType(INVALID_SPLIT)
      {
        ErrorHandler errorHandler = [](const std::ostringstream &errmsg) {
          default_error_handler(errmsg);
        };
        set_error_handler(errorHandler);
      }

      void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

      void split(const EdgesetData<EntityId, Topology>              &edgeset,
                 const std::vector<ElementData<EntityId, Topology>> &elementData)
      {
        m_splitMap.clear();
        m_edgesetName = edgeset.name;

        if (get_split_type() == SplitType::TOPOLOGY) {
          split_by_topology(edgeset, elementData);
        }
        else if (get_split_type() == SplitType::ELEMENT_BLOCK) {
          split_by_element_block(edgeset, elementData);
        }
        else if (get_split_type() == SplitType::NO_SPLIT) {
          split_by_no_split(edgeset, elementData);
        }
        else {
          std::ostringstream errmsg;
          errmsg << "Invalid split type: " << get_split_type();
          m_errorHandler(errmsg);
        }

        build_index_proc_map(edgeset, elementData);
      }

      std::vector<EdgeBlockInfo> get_edge_block_info() const
      {
        std::vector<EdgeBlockInfo> infoVec;
        infoVec.reserve(m_splitMap.size());

        for (auto iter = m_splitMap.begin(); iter != m_splitMap.end(); iter++) {
          const std::string &edgeBlockName = iter->first;
          EdgeBlockInfo      info          = get_edge_block_info(edgeBlockName);
          infoVec.push_back(info);
        }
        return infoVec;
      }

      std::vector<std::string> get_edge_block_names() const
      {
        std::vector<std::string> nameVec;
        nameVec.reserve(m_splitMap.size());

        for (auto iter = m_splitMap.begin(); iter != m_splitMap.end(); iter++) {
          const std::string &edgeBlockName = iter->first;
          nameVec.push_back(edgeBlockName);
        }
        return nameVec;
      }

      std::vector<size_t> get_indices_local_to_proc(const std::vector<size_t> &index,
                                                    int                        proc) const
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

      EdgeBlockInfo get_edge_block_info(const std::string &name) const
      {
        EdgeBlockInfo info;

        auto iter = m_splitMap.find(name);
        if (iter != m_splitMap.end()) {
          const SplitData &splitData = iter->second;

          info.name            = name;
          info.parentName      = splitData.edgesetName;
          info.edgeTopology    = splitData.edgeTopology;
          info.elementTopology = splitData.elemTopology;
          info.numNodesPerEdge = splitData.edgeNodeCount;
          info.touchingBlock   = splitData.touchingBlock;
          info.edgeIndex       = splitData.index;
        }
        return info;
      }

      SplitType get_split_type() const { return m_splitType; }
      void      set_split_type(SplitType inputSplitType) { m_splitType = inputSplitType; }

    private:
      void build_index_proc_map(const EdgesetData<EntityId, Topology>              &edgeset,
                                const std::vector<ElementData<EntityId, Topology>> &elementData)
      {
        for (size_t i = 0; i < edgeset.data.size(); ++i) {
          const EdgesetDataType<EntityId> &elemEdgePair = edgeset.data[i];
          EntityId                         elemId       = elemEdgePair.first;

          auto iter = bound_search(elementData.begin(), elementData.end(), elemId,
                                   ElementDataLess<EntityId, Topology>());
          if (iter == elementData.end()) {
            std::ostringstream errmsg;
            errmsg << "Error!  Edgeset with id: " << edgeset.id << " and name: " << edgeset.name
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
          errmsg << "Edgeset with name: " << m_edgesetName << " is referencing an invalid index "
                 << elemPairIndex;
          m_errorHandler(errmsg);
        }

        return iter->second == proc;
      }

      struct SplitData
      {
        bool                metaDataSet{false};
        std::string         edgesetName{};
        std::string         touchingBlock{};
        std::string         elemTopology{};
        std::string         edgeTopology{};
        int                 edgeNodeCount{-1};
        std::vector<size_t> index{};

        SplitData() : metaDataSet(false), edgeNodeCount(-1) {}
      };

      void fill_split_data(std::string key, size_t index,
                           const ElementData<EntityId, Topology> &elemData, int edge)
      {
        convert_to_upper_case(key);

        SplitData &splitData = m_splitMap[key];

        splitData.index.push_back(index);

        if (!splitData.metaDataSet) {
          splitData.edgesetName   = m_edgesetName;
          splitData.elemTopology  = elemData.topology.name();
          splitData.edgeTopology  = elemData.topology.edge_topology_name(edge);
          splitData.edgeNodeCount = elemData.topology.edge_topology_num_nodes(edge);

          if (get_split_type() == ELEMENT_BLOCK) {
            splitData.touchingBlock = elemData.partName;
          }

          splitData.metaDataSet = true;
        }
      }

      using Criterion =
          std::function<std::string(const EdgesetData<EntityId, Topology> &edgeset,
                                    const ElementData<EntityId, Topology> &elemData, int edge)>;

      void split_by_criterion(const EdgesetData<EntityId, Topology>              &edgeset,
                              const std::vector<ElementData<EntityId, Topology>> &elementData,
                              Criterion                                           criterion)
      {
        for (size_t index = 0; index < edgeset.data.size(); ++index) {
          const EdgesetDataType<EntityId> &elemEdgePair = edgeset.data[index];
          EntityId                         elemId       = elemEdgePair.first;
          int                              edge         = elemEdgePair.second;

          auto iter = bound_search(elementData.begin(), elementData.end(), elemId,
                                   ElementDataLess<EntityId, Topology>());
          if (iter == elementData.end()) {
            std::ostringstream errmsg;
            errmsg << "Error!  Edgeset with id: " << edgeset.id << " and name: " << edgeset.name
                   << " has reference to invalid element '" << elemId << "'.";
            m_errorHandler(errmsg);
          }

          std::string key = criterion(edgeset, *iter, edge);
          fill_split_data(key, index, *iter, edge);
        }
      }

      void split_by_topology(const EdgesetData<EntityId, Topology>              &edgeset,
                             const std::vector<ElementData<EntityId, Topology>> &elementData)
      {
        Criterion criterion = [](const EdgesetData<EntityId, Topology> &edgeSet,
                                 const ElementData<EntityId, Topology> &elemData, int edge) {
          if (edgeSet.has_default_exodus_name()) {
            return "EDGELIST_" + elemData.topology.name() + "_" +
                   elemData.topology.edge_topology_name(edge) + "_" + std::to_string(edgeSet.id);
          }
          return edgeSet.name + "_" + elemData.topology.name() + "_" +
                 elemData.topology.edge_topology_name(edge);
        };

        split_by_criterion(edgeset, elementData, criterion);
      }

      void split_by_element_block(const EdgesetData<EntityId, Topology>              &edgeset,
                                  const std::vector<ElementData<EntityId, Topology>> &elementData)
      {
        Criterion criterion = [](const EdgesetData<EntityId, Topology> &edgeSet,
                                 const ElementData<EntityId, Topology> &elemData, int edge) {
          if (edgeSet.has_default_exodus_name()) {
            return "EDGELIST_" + elemData.partName + "_" +
                   elemData.topology.edge_topology_name(edge) + "_" + std::to_string(edgeSet.id);
          }
          return edgeSet.name + "_" + elemData.partName + "_" +
                 elemData.topology.edge_topology_name(edge);
        };

        split_by_criterion(edgeset, elementData, criterion);
      }

      void split_by_no_split(const EdgesetData<EntityId, Topology> &edgeset,
                             const std::vector<ElementData<EntityId, Topology>> & /* elementData */)
      {
        std::vector<size_t> splitIndex(edgeset.data.size());
        std::iota(std::begin(splitIndex), std::end(splitIndex), 0);
        SplitData &splitData = m_splitMap[edgeset.name];

        splitData.index         = splitIndex;
        splitData.edgesetName   = m_edgesetName;
        splitData.elemTopology  = "unknown";
        splitData.edgeTopology  = "unknown";
        splitData.edgeNodeCount = -1;
        splitData.metaDataSet   = true;
      }

      SplitType   m_splitType{INVALID_SPLIT};
      std::string m_edgesetName{};

      std::unordered_map<size_t, int>            m_indexProcMap;
      std::unordered_map<std::string, SplitData> m_splitMap;
      ErrorHandler                               m_errorHandler;
    };

  } // namespace text_mesh
} // namespace Iotm
