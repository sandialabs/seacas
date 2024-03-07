// Copyright(C) 1999-2020, 2022 National Technology & Engineering Solutions
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
#include "Iotm_TextMeshEdgesetSplitter.h"

// clang-format on
// #######################   End Clang Header Tool Managed Headers  ########################
namespace Iotm {
  namespace text_mesh {

    using ErrorHandler = std::function<void(const std::ostringstream &)>;

    template <typename EntityId> using EdgesetDataType = std::pair<EntityId, int>;

    template <typename EntityId, typename Topology>
    struct EdgesetData : public EntityGroupData<EdgesetDataType<EntityId>>
    {
      using DataType  = EdgesetDataType<EntityId>;
      using BaseClass = EntityGroupData<EdgesetDataType<EntityId>>;

      void      set_split_type(SplitType splitType) { m_edgesetSplitter.set_split_type(splitType); }
      SplitType get_split_type() const { return m_edgesetSplitter.get_split_type(); }

      void set_error_handler(ErrorHandler errorHandler)
      {
        m_edgesetSplitter.set_error_handler(errorHandler);
      }

      void split(const std::vector<ElementData<EntityId, Topology>> &elementData)
      {
        m_edgesetSplitter.split(*this, elementData);
      }

      std::vector<size_t> get_edgeblock_indices_local_to_proc(const EdgeBlockInfo &info,
                                                              int                  proc) const
      {
        return m_edgesetSplitter.get_indices_local_to_proc(info.edgeIndex, proc);
      }

      EdgeBlockInfo get_edge_block_info(const std::string &edgeBlockName) const
      {
        return m_edgesetSplitter.get_edge_block_info(edgeBlockName);
      }

      std::vector<EdgeBlockInfo> get_edge_block_info() const
      {
        return m_edgesetSplitter.get_edge_block_info();
      }

      std::vector<std::string> get_edge_block_names() const
      {
        return m_edgesetSplitter.get_edge_block_names();
      }

      bool has_default_exodus_name() const
      {
        if (BaseClass::has_name()) {
          std::pair<unsigned, bool> result = get_id_from_part_name(BaseClass::name, "EDGELIST_");
          return result.second;
        }

        return false;
      }

      EdgesetSplitter<EntityId, Topology> m_edgesetSplitter;
    };

    template <typename EntityId, typename Topology>
    class Edgesets : public EntityGroup<EdgesetData<EntityId, Topology>>
    {
    public:
      using BaseClass = EntityGroup<EdgesetData<EntityId, Topology>>;

      Edgesets() : BaseClass("EDGESET", "EDGELIST_", {"BLOCK_", "SURFACE_", "NODELIST_", "ASSEMBLY_"}) {}

      void set_error_handler(ErrorHandler errorHandler) override
      {
        BaseClass::set_error_handler(errorHandler);

        for (EdgesetData<EntityId, Topology> &edgesetData : BaseClass::m_groupDataVec) {
          edgesetData.set_error_handler(errorHandler);
        }
      }

      void finalize_parse(const TextMeshData<EntityId, Topology> &data)
      {
        BaseClass::finalize_parse();

        for (EdgesetData<EntityId, Topology> &edgesetData : BaseClass::m_groupDataVec) {
          edgesetData.split(data.elementDataVec);

          std::vector<std::string> edgeBlockNames = edgesetData.get_edge_block_names();
          for (const std::string& name : edgeBlockNames) {
            m_edgeBlockToEdgeSetMap[name] = edgesetData.id;
          }
        }
      }

      const EdgesetData<EntityId, Topology> *get_edge_block_parent(const std::string& edgeBlockName) const
      {
        auto iter = m_edgeBlockToEdgeSetMap.find(edgeBlockName);
        if (iter != m_edgeBlockToEdgeSetMap.end()) {
          const EntityId parentId = iter->second;
          return BaseClass::get_group_data(parentId);
        }

        return nullptr;
      }

    private:
      std::unordered_map<std::string, EntityId> m_edgeBlockToEdgeSetMap;
    };

    template <typename EntityId> class EdgesetParser
    {
    public:
      EdgesetParser() : m_splitType(NO_SPLIT)
      {
        ErrorHandler errorHandler = [](const std::ostringstream &errmsg) {
          default_error_handler(errmsg);
        };
        set_error_handler(errorHandler);
      }

      void set_error_handler(ErrorHandler errorHandler) { m_errorHandler = errorHandler; }

      std::string get_name() { return m_name; }

      const std::vector<std::pair<EntityId, int>> &get_edgeset_data() { return m_elemEdgePairs; }

      SplitType get_split_type() { return m_splitType; }

      // Expected format for edgeset string data is:
      // "name=<name>; data=elem_1,edge_1,elem_2,edge_2,....,elem_n,edge_n;
      // split=<block|topology>;"
      void parse(const std::string &parseData)
      {
        auto options = get_tokens(parseData, ";");

        for (const auto &option : options) {
          parse_option_group(option);
        }
      }

    private:
      void parse_option(std::string optionName, const std::string &optionValue)
      {
        convert_to_lower_case(optionName);

        if (optionName == "name") {
          parse_name(optionValue);
        }
        else if (optionName == "data") {
          parse_element_edge_pairs(optionValue);
        }
        else if (optionName == "split") {
          parse_split_type(optionValue);
        }
        else {
          std::ostringstream errmsg;
          errmsg << "Unrecognized edgeset option: " << optionName;
          m_errorHandler(errmsg);
        }
      }

      void parse_option_group(const std::string &option)
      {
        if (!option.empty()) {
          auto optionTokens = get_tokens(option, "=");

          if (optionTokens.size() != 2) {
            std::ostringstream errmsg;
            errmsg << "Unrecognized edgeset option: " << option;
            m_errorHandler(errmsg);
          }

          parse_option(optionTokens[0], optionTokens[1]);
        }
      }

      void parse_name(const std::string &data) { m_name = data; }

      void parse_element_edge_pairs(const std::string &data)
      {
        auto edgesetData = get_tokens(data, ",");

        if (edgesetData.size() % 2 != 0) {
          std::ostringstream errmsg;
          errmsg << "Unmatched element/ordinal pairs in edgeset data: " << data;
          m_errorHandler(errmsg);
        }

        for (unsigned i = 0; i < edgesetData.size(); i += 2) {
          EntityId elem = std::stoull(edgesetData[i]);
          int      edge = std::stoi(edgesetData[i + 1]);

          if (edge <= 0) {
            std::ostringstream errmsg;
            errmsg << "Invalid element/ordinal pair {" << edgesetData[i] << ","
                   << edgesetData[i + 1] << "}";
            m_errorHandler(errmsg);
          }

          m_elemEdgePairs.push_back(std::make_pair(elem, edge));
        }
      }

      void parse_split_type(std::string splitName)
      {
        convert_to_lower_case(splitName);

        if (splitName == "block") {
          m_splitType = ELEMENT_BLOCK;
        }
        else if (splitName == "topology") {
          m_splitType = TOPOLOGY;
        }
        else {
          std::ostringstream errmsg;
          errmsg << "Unrecognized edgeset split type: " << splitName;
          m_errorHandler(errmsg);
        }
      }

      std::vector<std::pair<EntityId, int>> m_elemEdgePairs{};
      std::string                           m_name{};
      SplitType                             m_splitType{TOPOLOGY};
      ErrorHandler                          m_errorHandler;
    };

  } // namespace text_mesh
} // namespace Iotm
