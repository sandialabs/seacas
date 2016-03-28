/*
 * Copyright (c) 2014, Sandia Corporation.
 * Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 * the U.S. Government retains certain rights in this software.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 * 
 *     * Neither the name of Sandia Corporation nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */
#ifndef IOCGNS_DECOMPOSITONDATA_H
#define IOCGNS_DECOMPOSITONDATA_H

#include <mpi.h>

#include <vector>
#include <string>
#include <unordered_map>

#include <cstddef>
#include <cstdint>


#include <Ioss_CodeTypes.h>
#include <Ioss_Field.h>
#include <Ioss_PropertyManager.h>

#include <cgnslib.h>

#if 0
#if !defined(NO_PARMETIS_SUPPORT)
#include <parmetis.h>
#endif
#endif

#undef MPICPP
#if !defined(NO_ZOLTAN_SUPPORT)
#include <zoltan_cpp.h>
#endif
namespace Ioss {
  class Field;
}
namespace Iocgns {

  class ZoneData
  {
  public:
    std::string m_name;
    size_t m_nodeOffset;
    size_t m_nodeCount;
    size_t m_elementOffset;
  };

  class BlockDecompositionData
  {
  public:
    BlockDecompositionData() :
      zone_(0), section_(0), fileSectionOffset(0),
      fileCount(0), iossCount(0), zoneNodeOffset(0),
      topologyType(CG_ElementTypeNull), nodesPerEntity(0), localIossOffset(0)
      {}

      const std::string &name() const {return name_;}
      int zone() const {return zone_;}
      int section() const {return section_;}
      
      size_t file_count() const {return fileCount;}
      size_t ioss_count() const {return iossCount;}

      std::string name_;
      int zone_;
      int section_;
      
      size_t fileSectionOffset; // In partial read, where start
      size_t fileCount;
      size_t iossCount;
      size_t zoneNodeOffset;

      CG_ElementType_t topologyType;
      int nodesPerEntity;

      // maps from file-block data to ioss-block data
      // The local_map.size() elements starting at localIossOffset are local.
      // ioss[localIossOffset+i] = file[local_map[i]];
      size_t localIossOffset;
      std::vector<int> localMap;

      // Maps from file-block data to export list.
      // export[i] = file[export_map[i]
      std::vector<int> exportMap;
      std::vector<int> exportCount;
      std::vector<int> exportIndex;


      // Maps from import data to ioss-block data.
      // ioss[import_map[i] = local_map[i];
      std::vector<int> importMap;
      std::vector<int> importCount;
      std::vector<int> importIndex;
  };

  class SetDecompositionData 
  {
  public:
    SetDecompositionData()
      : zone_(0), section_(0), fileCount(0), root_(0),
      parentBlockIndex(0)
      {}

      const std::string &name() const {return name_;}
      int zone() const {return zone_;}
      int section() const {return section_;}
      
      size_t file_count() const {return fileCount;}
      size_t ioss_count() const {return entitylist_map.size();}

      std::string name_;
      int zone_;
      int section_;
      
      size_t fileCount;
      int root_;  // Lowest number processor that has nodes for this nodest
      size_t parentBlockIndex;

      CG_ElementType_t topologyType;

      // contains global entity-list positions for all entities in this set on this processor. 
      std::vector<size_t> entitylist_map;
      std::vector<bool> hasEntities; // T/F if this set exists on processor p

  };

  class DecompositionDataBase
  {
  public:
    DecompositionDataBase(MPI_Comm comm) : comm_(comm),
      myProcessor(0), processorCount(0), spatialDimension(0), globalNodeCount(0),
      globalElementCount(0), elementCount(0), elementOffset(0), importPreLocalElemIndex(0),
      nodeCount(0), nodeOffset(0), importPreLocalNodeIndex(0)
      {}

      virtual ~DecompositionDataBase() {}
      virtual void decompose_model(int filePtr) = 0;
      virtual size_t ioss_node_count() const = 0;
      virtual size_t ioss_elem_count() const = 0;
      virtual int int_size() const = 0;

      virtual void get_node_coordinates(int filePtr, double *ioss_data, const Ioss::Field &field) const = 0;
      
      void get_block_connectivity(int filePtr, void *data, int blk_seq) const;

      template <typename T>
	void communicate_element_data(T *file_data, T *ioss_data, size_t comp_count) const;

      template <typename T>
	void communicate_node_data(T *file_data, T *ioss_data, size_t comp_count) const;

      void get_sideset_element_side(int filePtr, const SetDecompositionData &sset, void *data) const;

      MPI_Comm comm_;
      int myProcessor;
      int processorCount;

      size_t spatialDimension;
      size_t globalNodeCount;
      size_t globalElementCount;

      // Values for the file decomposition 
      size_t elementCount;
      size_t elementOffset;
      size_t importPreLocalElemIndex;

      size_t nodeCount;
      size_t nodeOffset;
      size_t importPreLocalNodeIndex;

      std::vector<double> centroids_;

      std::vector<ZoneData> zones_;
      std::vector<BlockDecompositionData> el_blocks;
      std::vector<SetDecompositionData> node_sets;
      std::vector<SetDecompositionData> side_sets;

      // Maps nodes shared between zones.
      // TODO: Currently each processor has same map; need to figure out how to reduce size
      std::unordered_map<cgsize_t,cgsize_t> zone_shared_map;
  };

  template <typename INT>
    class DecompositionData : public DecompositionDataBase
  {
  public:
    DecompositionData(const Ioss::PropertyManager &props, MPI_Comm communicator);
    ~DecompositionData() {}

    int int_size() const {return sizeof(INT);}

    void decompose_model(int filePtr);

    size_t ioss_node_count() const {return nodeGTL.size();}
    size_t ioss_elem_count() const {return localElementMap.size() + importElementMap.size();}

    template <typename T>
      void communicate_element_data(T *file_data, T *ioss_data, size_t comp_count) const;

    void communicate_set_data(INT *file_data, INT *ioss_data,
			      const SetDecompositionData &set, size_t comp_count) const;
      
    template <typename T>
      void communicate_node_data(T *file_data, T *ioss_data, size_t comp_count) const;

    template <typename T>
      void communicate_block_data(cgsize_t *file_data, T *ioss_data, size_t blk_seq, size_t comp_count) const;

    void get_block_connectivity(int filePtr, INT *data, int blk_seq) const;

    void get_sideset_element_side(int filePtr, const SetDecompositionData &sset, INT *data) const;
    
  private:
    void get_sideset_data(int filePtr);
    void generate_zone_shared_nodes(int filePtr, INT min_node, INT max_node);
    
#if !defined(NO_ZOLTAN_SUPPORT)
    void zoltan_decompose(const std::string &method);
#endif
    void simple_decompose(const std::string &method,
			  const std::vector<INT> &element_dist);
      
    bool i_own_node(size_t node) const; // T/F if node with global index node owned by this processors ioss-decomp.
    bool i_own_elem(size_t elem) const; // T/F if node with global index elem owned by this processors ioss-decomp.
    
    // global_index is 1-based index into global list of nodes [1..global_node_count]
    // return value is 1-based index into local list of nodes on this
    // processor (ioss-decomposition)
    size_t node_global_to_local(size_t global_index) const;
    size_t elem_global_to_local(size_t global_index) const;

    void build_global_to_local_elem_map();
    void get_element_block_communication();

    void generate_adjacency_list(int fileId, std::vector<INT> &pointer,
				 std::vector<INT> &adjacency);

    void calculate_element_centroids(int filePtr,
				     const std::vector<INT> &pointer,
				     const std::vector<INT> &adjacency,
				     const std::vector<INT> &node_dist);
#if !defined(NO_ZOLTAN_SUPPORT)
    void get_local_element_list(const ZOLTAN_ID_PTR &export_global_ids, size_t export_count);
#endif

    void get_shared_node_list();

    void get_local_node_list(const std::vector<INT> &pointer,
			     const std::vector<INT> &adjacency,
			     const std::vector<INT> &node_dist);

    void get_file_node_coordinates(int filePtr, int direction, double *ioss_data) const;
    void get_node_coordinates(int filePtr, double *ioss_data, const Ioss::Field &field) const;

    std::vector<INT> localElementMap;

    std::vector<INT> importElementMap;
    std::vector<INT> importElementCount;
    std::vector<INT> importElementIndex;

    std::vector<INT> exportElementMap;
    std::vector<INT> exportElementCount;
    std::vector<INT> exportElementIndex;

    std::vector<INT> nodeIndex;

    // Note that nodeGTL is a sorted vector.
    std::vector<INT> nodeGTL;  // Convert from global index to local index (1-based)
    std::map<INT,INT> elemGTL;  // Convert from global index to local index (1-based)

    std::vector<INT> exportNodeMap;
    std::vector<INT> exportNodeCount;
    std::vector<INT> exportNodeIndex;

    std::vector<INT> importNodeMap; // Where to put each imported nodes data in the list of all data...
    std::vector<INT> importNodeCount;
    std::vector<INT> importNodeIndex;

    std::vector<INT> localNodeMap;

    std::vector<INT> nodeCommMap; // node/processor pair of the
    // nodes I communicate with.  Stored node#,proc,node#,proc, ...

    // The global element at index 'I' (0-based) is on block B in the file decompositoin.
    // if fileBlockIndex[B] <= I && fileBlockIndex[B+1] < I
    std::vector<size_t> fileBlockIndex;

    Ioss::PropertyManager m_properties;
  };
}
#endif
