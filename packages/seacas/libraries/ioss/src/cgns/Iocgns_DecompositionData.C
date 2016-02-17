#include <cgns/Iocgns_DecompositionData.h>
#include <Ioss_CodeTypes.h>
#include <Ioss_Utils.h>
#include <Ioss_ParallelUtils.h>

#include <algorithm>

#include <assert.h>
#include <mpi.h>

#define DEBUG_OUTPUT 1
namespace {
  template <typename T>
  void uniquify(std::vector<T> &vec)
  {
    std::sort(vec.begin(), vec.end());
    vec.erase(std::unique(vec.begin(), vec.end()), vec.end());
    vec.shrink_to_fit();
  }

  template <typename T>
  void generate_index(std::vector<T> &index)
  {
    T sum = 0;
    for (size_t i=0; i < index.size()-1; i++) {
      T cnt = index[i];
      index[i] = sum;
      sum += cnt;
    }
    index[index.size()-1] = sum;
  }

  template <typename T>
  T find_index_location(T node, const std::vector<T> &index)
  {
    // 0-based node numbering
    // index[p] = first node (0-based) on processor p

#if 1
    // Assume data coherence.  I.e., a new search will be close to the
    // previous search.
    static size_t prev = 1;

    size_t nproc = index.size();
    if (prev < nproc && index[prev-1] <= node && index[prev] > node)
      return prev-1;

    for (size_t p = 1; p < nproc; p++) {
      if (index[p] > node) {
        prev = p;
        return p-1;
      }
    }

    std::cerr << nproc << ":\t" << node;
    for (auto ind : index) {
      std::cerr << "\t" << ind;
    }
    std::cerr << "\n";
    assert(1==0); // Cannot happen...
    return -1;
#else
    return std::distance(index.begin(), std::upper_bound(index.begin(), index.end(), node))-1;
#endif
  }

  template <typename INT> 
  void get_entity_dist(size_t proc_count, size_t my_proc, size_t entity_count,
                       std::vector<INT> &dist, size_t *offset, size_t *count)
  {
    size_t per_proc = entity_count / proc_count;
    size_t extra    = entity_count % proc_count;

    *count = per_proc + (my_proc < extra ? 1 : 0);

    if (my_proc < extra) {
      *offset = (per_proc+1) * my_proc;
    }
    else {
      *offset = (per_proc+1) * extra + per_proc * (my_proc - extra);
    }

    // This processors range of elements is
    // [element_offset..element_offset+element_count)

    // Fill in element_dist vector.  Range of elements on each processor...
    size_t sum = 0;
    for (size_t i=0; i < proc_count; i++) {
      dist[i] = sum;
      sum += per_proc;
      if (i < extra) sum++;
    }
    dist[proc_count] = sum;
  }
}

namespace Iocgns {
  template DecompositionData<int>::DecompositionData(const Ioss::PropertyManager &props,
						     MPI_Comm communicator);
  template DecompositionData<int64_t>::DecompositionData(const Ioss::PropertyManager &props,
							 MPI_Comm communicator);

  template <typename INT>
  DecompositionData<INT>::DecompositionData(const Ioss::PropertyManager &props,
					    MPI_Comm communicator)
    : DecompositionDataBase(communicator), m_properties(props)
  {
    MPI_Comm_rank(comm_, &myProcessor);
    MPI_Comm_size(comm_, &processorCount);
  }

  template <typename INT>
  bool DecompositionData<INT>::i_own_node(size_t global_index) const
  {
    // global_index is 1-based index into global list of nodes [1..global_node_count]
    return std::binary_search(nodeGTL.begin(), nodeGTL.end(), global_index);
  }

  template <typename INT>
  bool DecompositionData<INT>::i_own_elem(size_t global_index) const
  {
    // global_index is 1-based index into global list of nodes [1..global_node_count]
    return elemGTL.count(global_index) != 0;
  }

  template <typename INT>
  size_t DecompositionData<INT>::node_global_to_local(size_t global_index) const
  {
    // global_index is 1-based index into global list of nodes [1..global_node_count]
    // return value is 1-based index into local list of nodes on this
    // processor (ioss-decomposition)
    // Note that for 'int', equivalence and equality are the same, so
    // lower_bound is OK here (EffectiveSTL, Item 19)
    typename std::vector<INT>::const_iterator I = lower_bound(nodeGTL.begin(), nodeGTL.end(), global_index);
    assert(I != nodeGTL.end());
    return std::distance(nodeGTL.begin(), I)+1; // Convert to 1-based index.
  }

  template <typename INT>
  size_t DecompositionData<INT>::elem_global_to_local(size_t global_index) const
  {
    // global_index is 1-based index into global list of elements [1..global_node_count]
    // return value is 1-based index into local list of elements on this
    // processor (ioss-decomposition)
    typename std::map<INT,INT>::const_iterator I = elemGTL.find(global_index);
    assert(I != elemGTL.end());
    return I->second;
  }

  template <typename INT>
  void DecompositionData<INT>::decompose_model(int cgnsFilePtr)
  {
    // Initial decomposition is linear where processor #p contains
    // elements from (#p * #element/#proc) to (#p+1 * #element/#proc)

    // ========================================================================
    // Get the number of zones (element blocks) in the mesh...
    int num_zones = 0;
    int base = 1; // Only single base supported so far.
    
    cg_nzones(cgnsFilePtr, base, &num_zones);
    
    for (int zone=1; zone <= num_zones; zone++) {
      CG_ZoneType_t zone_type;
      cg_zone_type(cgnsFilePtr, base, zone, &zone_type);

      // See if all zones are "Unstructured" which is all we currently support...
      if (zone_type != CG_Unstructured) {
        std::ostringstream errmsg;
        errmsg << "ERROR: CGNS: Zone " << zone
	       << " is not of type Unstructured which is the only type currently supported";
	IOSS_ERROR(errmsg);
      }
      else {
	cgsize_t size[3];
	char zone_name[33];
	cg_zone_read(cgnsFilePtr, base, zone, zone_name, size);
	
	INT total_block_nodes = size[0];
	INT total_block_elem  = size[1];
	globalNodeCount += total_block_nodes;
	globalElementCount += total_block_elem;
      }
    }

    // Generate element_dist/node_dist --  size proc_count + 1
    // processor p contains all elements/nodes from X_dist[p] .. X_dist[p+1]
    std::vector<INT> element_dist(processorCount+1);
    std::vector<INT> node_dist(processorCount+1);

    get_entity_dist(processorCount, myProcessor, globalElementCount,
                    element_dist, &elementOffset, &elementCount);
    get_entity_dist(processorCount, myProcessor, globalNodeCount,
                    node_dist,    &nodeOffset,    &nodeCount);

#if DEBUG_OUTPUT
    std::cerr << "Processor " << myProcessor << " has "
	      << elementCount << " elements; offset = " << elementOffset << "\n";
    std::cerr << "Processor " << myProcessor << " has "
	      << nodeCount << " nodes; offset = " << nodeOffset << ".\n";
#endif

    std::vector<INT> pointer; // Index into adjacency, processor list for each element...
    std::vector<INT> adjacency; // Size is sum of element connectivity sizes 
    generate_adjacency_list(cgnsFilePtr, pointer, adjacency);

    std::string method = "LINEAR";

    if (m_properties.exists("DECOMPOSITION_METHOD")) {
      method = m_properties.get("DECOMPOSITION_METHOD").get_string();
      method = Ioss::Utils::uppercase(method);
    }

    if (method != "LINEAR") {
      if (myProcessor == 0) {
	std::ostringstream errmsg;
	errmsg << "ERROR: Invalid decomposition method specified: '" << method << "'\n"
	       << "       Valid methods: LINEAR"
	       << "\n";
	std::cerr << errmsg.str();
      }
      exit(EXIT_FAILURE);
    }

    if (myProcessor == 0)
      std::cerr << "\nUsing decomposition method '" << method << "' on "
		<< processorCount << " processors.\n\n";

    if (method == "LINEAR") {
      if (globalElementCount > 0) {
	simple_decompose(method, element_dist);
      }
    }

    std::sort(importElementMap.begin(), importElementMap.end());

    std::copy(importElementCount.begin(), importElementCount.end(), importElementIndex.begin());
    generate_index(importElementIndex);

    // Find the number of imported elements that precede the elements
    // that remain locally owned...
    importPreLocalElemIndex = 0;
    for (size_t i=0; i < importElementMap.size(); i++) {
      if ((size_t)importElementMap[i] >= elementOffset)
        break;
      importPreLocalElemIndex++;
    }

    // Determine size of this processors element blocks...
    get_element_block_communication();

    // Now need to determine the nodes that are on this processor,
    // both owned and shared...
    if (globalElementCount > 0) {
      get_local_node_list(pointer, adjacency, node_dist);
      get_shared_node_list();
    }

  }

  template <typename INT>
  void DecompositionData<INT>::simple_decompose(const std::string &method,
                                                const std::vector<INT> &element_dist)
  {
    if (method == "LINEAR") {
      // The "ioss_decomposition" is the same as the "file_decomposition"
      // Nothing is imported or exported, everything stays "local"

      size_t local = element_dist[myProcessor+1] - element_dist[myProcessor];
      assert(local == elementCount);
      localElementMap.resize(local);
      std::iota(localElementMap.begin(), localElementMap.end(), 0);

      // All values are 0
      exportElementCount.resize(processorCount+1);
      exportElementIndex.resize(processorCount+1);
      importElementCount.resize(processorCount+1);
      importElementIndex.resize(processorCount+1);
    }
  }

  template <typename INT>
  void DecompositionData<INT>::generate_adjacency_list(int cgnsFilePtr,
						       std::vector<INT> &pointer,
						       std::vector<INT> &adjacency)
  {
    int base = 1; // Only single base supported so far.

    // Range of elements currently handled by this processor [)
    size_t p_start = elementOffset;
    size_t p_end   = p_start + elementCount;

    assert(sizeof(INT) == sizeof(cgsize_t));
    size_t sum = 0; // Size of adjacency vector.
    size_t offset = 0;

    int num_zones = 0;
    INT zone_node_offset = 0;
    
    cg_nzones(cgnsFilePtr, base, &num_zones);
    for (int zone=1; zone <= num_zones; zone++) {
      cgsize_t size[3];
      char zone_name[33];
      cg_zone_read(cgnsFilePtr, base, zone, zone_name, size);

      INT total_elements = size[1];
      // NOTE: A Zone will have a single set of nodes, but can have
      //       multiple sections each with their own element type...
      //       Keep treating sections as element blocks until we
      //       have handled 'size[1]' number of elements; the remaining
      //       sections are then the boundary faces (?)
      int num_sections = 0;
      cg_nsections(cgnsFilePtr, base, zone, &num_sections);
	
      for (int is = 1; is <= num_sections && total_elements > 0; is++) {
	char section_name[33];
	CG_ElementType_t e_type;
	cgsize_t el_start = 0;
	cgsize_t el_end = 0;
	int num_bndry = 0;
	int parent_flag = 0;

	// Get the type of elements in this section...
	cg_section_read(cgnsFilePtr, base, zone, is,
			section_name, &e_type, &el_start, &el_end, &num_bndry, &parent_flag);

	INT num_entity = el_end - el_start + 1;

	if (parent_flag == 0 && total_elements > 0) {
	  total_elements -= num_entity;

	  // Range of elements in element block b [)
	  size_t b_start = offset;  // offset is index of first element in this block...
	  offset += num_entity;
	  size_t b_end   = offset;
	    
	  int element_nodes;
	  cg_npe(e_type, &element_nodes);

	  if (b_start < p_end && p_start < b_end) {
	    // Some of this blocks elements are on this processor...
	    size_t overlap = std::min(b_end, p_end) - std::max(b_start, p_start);
	    sum += overlap * element_nodes;
	  }

	  BlockDecompositionData block;
	  block.zone_ = zone;
	  block.section_ = is;
	  block.topologyType = e_type;
	  block.nodesPerEntity = element_nodes;
	  block.fileCount = num_entity;
	  block.zoneNodeOffset = zone_node_offset;

	  el_blocks.push_back(block);
	}
      }
      zone_node_offset += size[0];
    }
    int block_count = (int)el_blocks.size();

    // Get the global element block index list at this time also.
    // The global element at index 'I' (0-based) is on block B
    // if global_block_index[B] <= I && global_block_index[B+1] < I
    // allocate and TODO: Fill
    fileBlockIndex.reserve(block_count+1);
    for (auto block : el_blocks) {
      fileBlockIndex.push_back(block.file_count());
    }
    fileBlockIndex.push_back(0);
    generate_index(fileBlockIndex);
    
    // Make sure 'sum' can fit in INT...
    INT tmp_sum = (INT)sum;
    if ((size_t)tmp_sum != sum) {
      std::ostringstream errmsg;
      errmsg << "ERROR: The decomposition of this mesh requires 64-bit integers, but is being\n"
             << "       run with 32-bit integer code. Please rerun with the property INTEGER_SIZE_API\n"
             << "       set to 8. The details of how to do this vary with the code that is being run.\n"
             << "       Contact gdsjaar@sandia.gov for more details.\n";
      std::cerr << errmsg.str();
      exit(EXIT_FAILURE);
    }

    // Now, populate the vectors...
    pointer.reserve(elementCount+1);
    adjacency.reserve(sum);
    offset = 0;
    sum = 0; // Size of adjacency vector.

    for (int block=0; block < block_count; block++) {
      // Range of elements in element block b [)
      size_t b_start = offset;  // offset is index of first element in this block...
      offset += el_blocks[block].file_count();
      size_t b_end   = b_start + el_blocks[block].file_count();

      if (b_start < p_end && p_start < b_end) {
        // Some of this blocks elements are on this processor...
        size_t overlap = std::min(b_end, p_end) - std::max(b_start, p_start);
        size_t element_nodes = el_blocks[block].nodesPerEntity;
	int zone = el_blocks[block].zone_;
	int section = el_blocks[block].section_;

	// Get the connectivity (raw) for this portion of elements...
	std::vector<cgsize_t> connectivity(overlap*element_nodes);
	INT blk_start = std::max(b_start, p_start) - b_start + 1;
	INT blk_end   = blk_start + overlap -1;
#if DEBUG_OUTPUT
	std::cerr << "Processor " << myProcessor << " has "
		  << overlap << " elements on element block " << block << "\t("
		  << blk_start << " to " << blk_end << ")\n";
#endif
	cg_elements_partial_read(cgnsFilePtr, base, zone, section,
				 blk_start, blk_end,
				 TOPTR(connectivity), nullptr);
	size_t el = 0;
	INT zone_offset = el_blocks[block].zoneNodeOffset;
	
	for (size_t elem = 0; elem < overlap; elem++) {
	  pointer.push_back(adjacency.size());
	  for (size_t k=0; k < element_nodes; k++) {
	    INT node = connectivity[el++]-1 + zone_offset; // 0-based node
	    adjacency.push_back(node);
	  }
	}
	sum += overlap * element_nodes;
      }
    }
    pointer.push_back(adjacency.size());
  }

  template <typename INT>
  void DecompositionData<INT>::get_element_block_communication()
  {
    for (auto &block : el_blocks) {
      block.exportCount.resize(processorCount);
      block.exportIndex.resize(processorCount);
      block.importCount.resize(processorCount);
      block.importIndex.resize(processorCount);
    }

    // First iterate the local element indices and count number in
    // each block.
    size_t b = 0;
    for (auto loc_elem : localElementMap) {
      size_t elem = loc_elem + elementOffset;
      b = find_index_location(elem, fileBlockIndex);

      assert(elem >= fileBlockIndex[b] && elem < fileBlockIndex[b+1]);
      size_t off = std::max(fileBlockIndex[b], elementOffset);
      el_blocks[b].localMap.push_back(elem-off);
    }

    // Now iterate the imported element list...
    // Find number of imported elements that are less than the current local_map[0]
    b = 0;
    size_t proc = 0;
    std::vector<size_t> imp_index(el_blocks.size()); 
    for (size_t i=0; i < importElementMap.size(); i++) {
      size_t elem = importElementMap[i];
      while (i >= (size_t)importElementIndex[proc+1])
        proc++;

      b = find_index_location(elem-1, fileBlockIndex);
      size_t off = std::max(fileBlockIndex[b], elementOffset);

      if (!el_blocks[b].localMap.empty() && elem < el_blocks[b].localMap[0]+off) {
        el_blocks[b].localIossOffset++;
        el_blocks[b].importMap.push_back(imp_index[b]++);
      } else {
        el_blocks[b].importMap.push_back(el_blocks[b].localMap.size() + imp_index[b]++);
      }
      el_blocks[b].importCount[proc]++;
    }

    // Now for the exported data...
    proc = 0;
    b = 0;
    for (size_t i=0; i < exportElementMap.size(); i++) {
      size_t elem = exportElementMap[i];
      while (i >= (size_t)exportElementIndex[proc+1])
        proc++;

      b = find_index_location(elem-1, fileBlockIndex);

      size_t off = std::max(fileBlockIndex[b], elementOffset);
      el_blocks[b].exportMap.push_back(elem-off);
      el_blocks[b].exportCount[proc]++;
    }

    for (auto &block : el_blocks) {
      block.iossCount = block.localMap.size() + block.importMap.size();
      block.fileCount = block.localMap.size() + block.exportMap.size();
      std::copy(block.exportCount.begin(), block.exportCount.end(), block.exportIndex.begin());
      std::copy(block.importCount.begin(), block.importCount.end(), block.importIndex.begin());
      generate_index(block.exportIndex);
      generate_index(block.importIndex);
    }
  }

  template <typename INT>
  void DecompositionData<INT>::get_local_node_list(const std::vector<INT> &pointer,
                                                   const std::vector<INT> &adjacency,
                                                   const std::vector<INT> &node_dist)
  {
    // Get the connectivity of all imported elements...
    // First, determine how many nodes the exporting processors are
    // going to send me and how many nodes my exported elements
    // have...

    std::vector<INT> export_conn_size(processorCount);
    std::vector<INT> import_conn_size(processorCount);
    for (int p=0; p < processorCount; p++) {
      size_t el_begin = exportElementIndex[p];
      size_t el_end = exportElementIndex[p+1];
      for (size_t i=el_begin; i < el_end; i++) {
        INT elem = exportElementMap[i] - elementOffset;
        size_t nnpe = pointer[elem+1] - pointer[elem];
        export_conn_size[p] += nnpe;
      }
    }

    MPI_Alltoall(TOPTR(export_conn_size), 1, Ioss::mpi_type((INT)0),
                 TOPTR(import_conn_size), 1, Ioss::mpi_type((INT)0), comm_);

    // Now fill the vectors with the nodes ...
    size_t exp_size = std::accumulate(export_conn_size.begin(), export_conn_size.end(), 0);
    size_t imp_size = std::accumulate(import_conn_size.begin(), import_conn_size.end(), 0);
    std::vector<INT> export_conn;
    export_conn.reserve(exp_size);

    std::vector<INT> export_disp(processorCount);
    std::vector<INT> import_disp(processorCount);
    for (int p=1; p < processorCount; p++) {
      export_disp[p] = export_disp[p-1] + export_conn_size[p-1];
      import_disp[p] = import_disp[p-1] + import_conn_size[p-1];
    }

    for (int p=0; p < processorCount; p++) {
      size_t el_begin = exportElementIndex[p];
      size_t el_end = exportElementIndex[p+1];
      for (size_t i=el_begin; i < el_end; i++) {
        INT elem = exportElementMap[i] - elementOffset;
        for (INT n = pointer[elem]; n < pointer[elem+1]; n++) {
          export_conn.push_back(adjacency[n]);
        }
      }
    }

    // Count number of nodes on local elements...
    size_t node_sum = 0;
    for (auto elem : localElementMap) {
      node_sum += pointer[elem+1] - pointer[elem];
    }
    // Also holds imported nodes...
    node_sum += imp_size;

    std::vector<INT> nodes;

    {
      std::vector<INT> import_conn(imp_size);

      Ioss::MY_Alltoallv(export_conn, export_conn_size, export_disp,
			 import_conn, import_conn_size, import_disp, comm_);

      // Done with export_conn...
      std::vector<INT>().swap(export_conn);

      // Find list of unique nodes used by the elements on this
      // processor... adjacency list contains connectivity for local
      // elements and import_conn contains connectivity for imported
      // elements.

      // Nodes on Imported elements...
      nodes.reserve(node_sum);
      for (size_t i=0; i < import_conn.size(); i++) {
        nodes.push_back(import_conn[i]);
      }
    }    

    // Nodes on local elements...
    for (size_t i=0; i < localElementMap.size(); i++) {
      INT elem = localElementMap[i];
      for (INT n = pointer[elem]; n < pointer[elem+1]; n++) {
        nodes.push_back(adjacency[n]);
      }
    }

    // Now need to sort and uniquify 'nodes'
    uniquify(nodes);

    // Determine owning 'file' processor for each node...
    nodeIndex.resize(processorCount+1);

    for (size_t i=0; i < nodes.size(); i++) {
      INT owning_processor = find_index_location(nodes[i], node_dist);
      nodeIndex[owning_processor]++;
    }
    importNodeCount.resize(nodeIndex.size());
    std::copy(nodeIndex.begin(), nodeIndex.end(), importNodeCount.begin());
    exportNodeCount.resize(processorCount);
    generate_index(nodeIndex);

    // Tell other processors how many nodes I will be importing from
    // them...
    importNodeCount[myProcessor] = 0;
    MPI_Alltoall(TOPTR(importNodeCount), 1, Ioss::mpi_type((INT)0),
                 TOPTR(exportNodeCount), 1, Ioss::mpi_type((INT)0), comm_);

    size_t import_sum = std::accumulate(importNodeCount.begin(), importNodeCount.end(), 0);
    size_t export_sum = std::accumulate(exportNodeCount.begin(), exportNodeCount.end(), 0);

    std::vector<INT> import_nodes;
    import_nodes.reserve(import_sum);
    importNodeMap.reserve(import_sum);
    for (int p=0; p < processorCount; p++) {
      size_t beg = nodeIndex[p];
      size_t end = nodeIndex[p+1];

      if (p == myProcessor) {
        importPreLocalNodeIndex = beg;
        localNodeMap.reserve(end-beg);
        for (size_t n = beg; n < end; n++) {
          localNodeMap.push_back(nodes[n]);
        }
      } else {
        for (size_t n = beg; n < end; n++) {
          import_nodes.push_back(nodes[n]);
          importNodeMap.push_back(n);
        }
      }
    }
    assert(import_nodes.size() == import_sum);
    exportNodeMap.resize(export_sum);
    exportNodeIndex.resize(processorCount+1);
    std::copy(exportNodeCount.begin(), exportNodeCount.end(), exportNodeIndex.begin());
    generate_index(exportNodeIndex);

    // Now send the list of nodes that I need to import from each
    // processor...
    importNodeIndex.resize(importNodeCount.size());
    std::copy(importNodeCount.begin(), importNodeCount.end(), importNodeIndex.begin());
    generate_index(importNodeIndex);

    Ioss::MY_Alltoallv(import_nodes,  importNodeCount, importNodeIndex, 
		       exportNodeMap, exportNodeCount, exportNodeIndex, comm_);

    // Map that converts nodes from the global index (1-based) to a local-per-processor index (1-based)
    std::cerr << myProcessor << ":\tNode Count = " << nodes.size() << "\n";
    nodeGTL.swap(nodes);
    for (size_t i=0; i < nodeGTL.size(); i++) {
      nodeGTL[i]++; // convert from 0-based index to 1-based index
    }
  }

  template <typename INT>
  void DecompositionData<INT>::get_shared_node_list()
  {
    // Need a list of all "shared" nodes (nodes on more than one
    // processor) and the list of processors that they are on for the
    // ioss decomposition.
    //
    // * iterate all local nodes (those that are in both file and ioss decomposition)
    //   on this procesor and all exported nodes,
    // * put in a vector and sort on (id,proc).
    // * iterate and create a vector of all shared nodes and the
    //   processor they are on..
    size_t local_node_count = nodeIndex[myProcessor+1]-nodeIndex[myProcessor];
    std::vector<std::pair<INT,int> > node_proc_list;
    node_proc_list.reserve(local_node_count + exportNodeMap.size());

    for (auto local_node : localNodeMap) {
      node_proc_list.push_back(std::make_pair(local_node, myProcessor));
    }

    for (int p=0; p < processorCount; p++) {
      if (p == myProcessor)
        continue;
      size_t beg = exportNodeIndex[p];
      size_t end = exportNodeIndex[p+1];
      for (size_t i=beg; i < end; i++) {
        node_proc_list.push_back(std::make_pair(exportNodeMap[i], p));
      }
    }
    std::sort(node_proc_list.begin(), node_proc_list.end());

    std::vector<std::pair<INT,int> > shared_nodes;
    for (size_t i=0; i < node_proc_list.size(); i++) {
      INT node = node_proc_list[i].first;
      if (i+1 < node_proc_list.size() && node_proc_list[i+1].first == node) {
        shared_nodes.push_back(node_proc_list[i]);
      }

      while (i+1 < node_proc_list.size() && node_proc_list[i+1].first == node) {
        shared_nodes.push_back(node_proc_list[++i]);
      }
    }

    // The shared_nodes list contains all nodes that I know about that
    // are shared.

    // Determine the counts...
    std::vector<INT> send_comm_map_count(processorCount);
    for (size_t i=0; i < shared_nodes.size(); i++) {
      size_t beg = i;
      size_t end = ++i;
      while (i+1 < shared_nodes.size() && shared_nodes[beg].first == shared_nodes[i+1].first) {
        end = ++i;
      }
      for (size_t p=beg; p <= end; p++) {
        int proc = shared_nodes[p].second;
        for (size_t j = beg; j <= end; j++) {
          if (j == p)
            continue;
          assert(shared_nodes[p].first == shared_nodes[j].first);
          send_comm_map_count[proc] += 2;
        }
      }
    }

    // Determine total count... (including myProcessor for now just to
    // see whether it simplifies/complicates coding)
    std::vector<INT> send_comm_map_disp(processorCount+1);
    std::copy(send_comm_map_count.begin(), send_comm_map_count.end(), send_comm_map_disp.begin());
    generate_index(send_comm_map_disp);

    std::vector<INT> send_comm_map(send_comm_map_disp[processorCount]);
    std::vector<INT> nc_offset(processorCount);

#if DEBUG_OUTPUT
    std::cerr << "Processor " << myProcessor << " has "
	      << shared_nodes.size() << " shared nodes\n";
#endif
    for (size_t i=0; i < shared_nodes.size(); i++) {
      size_t beg = i;
      size_t end = ++i;
      while (i+1 < shared_nodes.size() && shared_nodes[beg].first == shared_nodes[i+1].first) {
        end = ++i;
      }
      for (size_t p=beg; p <= end; p++) {
        int proc = shared_nodes[p].second;
        for (size_t j = beg; j <= end; j++) {
          if (j == p)
            continue;
          assert(shared_nodes[p].first == shared_nodes[j].first);
          size_t location = send_comm_map_disp[proc] + nc_offset[proc];
          send_comm_map[location+0] = shared_nodes[j].first;
          send_comm_map[location+1] = shared_nodes[j].second;
          nc_offset[proc] += 2;
        }
      }
    }

    // Tell other processors how many nodes/procs I am sending them...
    std::vector<INT> recv_comm_map_count(processorCount);
    MPI_Alltoall(TOPTR(send_comm_map_count), 1, Ioss::mpi_type((INT)0),
                 TOPTR(recv_comm_map_count), 1, Ioss::mpi_type((INT)0), comm_);


    std::vector<INT> recv_comm_map_disp(recv_comm_map_count);
    generate_index(recv_comm_map_disp);
    nodeCommMap.resize(recv_comm_map_disp[processorCount-1] + recv_comm_map_count[processorCount-1]);
    Ioss::MY_Alltoallv(send_comm_map, send_comm_map_count, send_comm_map_disp, 
		       nodeCommMap, recv_comm_map_count, recv_comm_map_disp, comm_);

    // Map global 0-based index to local 1-based index.
    for (size_t i=0; i < nodeCommMap.size(); i+=2) {
      nodeCommMap[i] = node_global_to_local(nodeCommMap[i]+1);
    }
  }

}

