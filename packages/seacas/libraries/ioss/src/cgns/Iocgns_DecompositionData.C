#include <cgns/Iocgns_DecompositionData.h>
#include <Ioss_CodeTypes.h>
#include <Ioss_Utils.h>
#include <Ioss_ParallelUtils.h>

#include <algorithm>

#include <assert.h>
#include <mpi.h>

#define DEBUG_OUTPUT 1
namespace {
  const char *Version() {return "Iocgns_DecompositionData.C 2016/02/17";}

  void cgns_error(int cgnsid, int lineno, int /* processor */)
  {
    std::ostringstream errmsg;
    errmsg << "CGNS error '" << cg_get_error() << "' at line " << lineno
	   << " in file '" << Version()
	   << "' Please report to gdsjaar@sandia.gov if you need help.";
    if (cgnsid > 0) {
      cg_close(cgnsid);
    }
    IOSS_ERROR(errmsg);
  }

  // ZOLTAN Callback functions...

#if !defined(NO_ZOLTAN_SUPPORT)
  int zoltan_num_dim(void *data, int *ierr)
  {
    // Return dimensionality of coordinate data.
    Iocgns::DecompositionDataBase *zdata = (Iocgns::DecompositionDataBase *)(data);

    *ierr = ZOLTAN_OK;
    return zdata->spatialDimension;
  }

  int zoltan_num_obj(void *data, int *ierr)
  {
    // Return number of objects (element count) on this processor...
    Iocgns::DecompositionDataBase *zdata = (Iocgns::DecompositionDataBase *)(data);

    *ierr = ZOLTAN_OK;
    return zdata->elementCount;
  }

  void zoltan_obj_list(void *data, int ngid_ent, int nlid_ent,
                       ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
                       int wdim, float *wgts, int *ierr)
  {
    // Return list of object IDs, both local and global.
    Iocgns::DecompositionDataBase *zdata = (Iocgns::DecompositionDataBase *)(data);

    // At the time this is called, we don't have much information
    // These routines are the ones that are developing that
    // information... 
    size_t element_count  = zdata->elementCount;
    size_t element_offset = zdata->elementOffset;

    *ierr = ZOLTAN_OK;

    if (lids) {
      std::iota(lids, lids+element_count, 0);
    }

    if (wdim) {
      for (size_t i = 0; i < element_count; i++) {
        wgts[i] = 1.0;
      }
    }

    if (ngid_ent == 1) {
      for (size_t i = 0; i < element_count; i++) {
        gids[i] = element_offset + i;
      }
    } else if (ngid_ent == 2){
      int64_t* global_ids = (int64_t*)gids;
      for (size_t i = 0; i < element_count; i++) {
        global_ids[i] = element_offset + i;
      }
    } else {
      *ierr = ZOLTAN_FATAL;
    }
    return;
  }

  void zoltan_geom(void *data, int ngid_ent, int nlid_ent, int nobj,
                   ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
                   int ndim, double *geom, int *ierr)
  {
    // Return coordinates for objects.
    Iocgns::DecompositionDataBase *zdata = (Iocgns::DecompositionDataBase *)(data);

    std::copy(zdata->centroids_.begin(), zdata->centroids_.end(), &geom[0]);

    *ierr = ZOLTAN_OK;
    return;
  }
#endif

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
    // global_index is 1-based index into global list of elements [1..global_element_count]
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
    
    {
      cgsize_t cell_dimension = 0;
      cgsize_t phys_dimension = 0;
      char base_name[33];
      cg_base_read(cgnsFilePtr, base, base_name, &cell_dimension, &phys_dimension);
      spatialDimension = phys_dimension;
    }

    cg_nzones(cgnsFilePtr, base, &num_zones);
    zones_.resize(num_zones+1); // Use 1-based zones.
    
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

	zones_[zone].m_nodeCount = total_block_nodes;
	zones_[zone].m_nodeOffset = globalNodeCount;
	zones_[zone].m_name = zone_name;
	
	globalNodeCount += total_block_nodes;
	globalElementCount += total_block_elem;
      }
    }

    // Generate element_dist/node_dist --  size proc_count + 1
    // processor p contains all elements/nodes from X_dist[p] .. X_dist[p+1]
    std::vector<INT> element_dist = Ioss::get_entity_dist<INT>(processorCount, myProcessor, globalElementCount,
							       &elementOffset, &elementCount);

    // Does not yet take into account shared nodes at zone boundaries.
    std::vector<INT> node_dist = Ioss::get_entity_dist<INT>(processorCount, myProcessor, globalNodeCount,
							    &nodeOffset,    &nodeCount);

#if DEBUG_OUTPUT
    std::cerr << "Processor " << myProcessor << " has "
	      << elementCount << " elements; offset = " << elementOffset << "\n";
    std::cerr << "Processor " << myProcessor << " has "
	      << nodeCount << " nodes; offset = " << nodeOffset << ".\n";
#endif

    std::vector<INT> pointer; // Index into adjacency, processor list for each element...
    std::vector<INT> adjacency; // Size is sum of element connectivity sizes 
    generate_adjacency_list(cgnsFilePtr, pointer, adjacency);

    // Get min and max node used on this processor...
    auto min_max = std::minmax_element(adjacency.begin(), adjacency.end());
    INT min_node = *(min_max.first);
    INT max_node = *(min_max.second);
    generate_zone_shared_nodes(cgnsFilePtr, min_node, max_node);
    
    // Now iterate adjacency list and update any "zone_shared_node" nodes
    // with their "sharee"
    if (!zone_shared_map.empty()) {
      for (auto &node : adjacency) {
	auto alias = zone_shared_map.find(node);
	if (alias != zone_shared_map.end()) {
	  node = (*alias).second;
	}
      }
    }

    std::string method = "LINEAR";

    if (m_properties.exists("DECOMPOSITION_METHOD")) {
      method = m_properties.get("DECOMPOSITION_METHOD").get_string();
      method = Ioss::Utils::uppercase(method);
    }

    if (method != "LINEAR"
#if !defined(NO_ZOLTAN_SUPPORT)
	&& method != "BLOCK"
	&& method != "CYCLIC"
	&& method != "RANDOM"
	&& method != "RCB"
	&& method != "RIB"
	&& method != "HSFC"
#endif
#if !defined(NO_PARMETIS_SUPPORT)
        && method != "KWAY"
        && method != "GEOM_KWAY"
        && method != "KWAY_GEOM"
        && method != "METIS_SFC"
#endif
	) {
      if (myProcessor == 0) {
	std::ostringstream errmsg;
	errmsg << "ERROR: Invalid decomposition method specified: '" << method << "'\n"
	       << "       Valid methods: LINEAR"
#if !defined(NO_ZOLTAN_SUPPORT)
	       << ", BLOCK, CYCLIC, RANDOM, RCB, RIB, HSFC"
#endif
#if !defined(NO_PARMETIS_SUPPORT)
                 << ", KWAY, GEOM_KWAY, METIS_SFC"
#endif
	       << "\n";
	std::cerr << errmsg.str();
      }
      exit(EXIT_FAILURE);
    }

    if (myProcessor == 0)
      std::cerr << "\nUsing decomposition method '" << method << "' on "
		<< processorCount << " processors.\n\n";

    if (method == "RCB" ||
	method == "RIB" ||
	method == "HSFC" ||
	method == "GEOM_KWAY" ||
	method == "KWAY_GEOM" ||
	method == "METIS_SFC") {
      calculate_element_centroids(cgnsFilePtr, pointer, adjacency, node_dist);
    }

#if !defined(NO_PARMETIS_SUPPORT)
    if (method == "KWAY" ||
        method == "GEOM_KWAY" ||
        method == "KWAY_GEOM" ||
        method == "METIS_SFC") {
      metis_decompose(method, element_dist, pointer, adjacency);
    }
#endif
#if !defined(NO_ZOLTAN_SUPPORT)
    if (method == "RCB" ||
	method == "RIB" ||
	method == "HSFC" ||
	method == "BLOCK" ||
	method == "CYCLIC" ||
	method == "RANDOM") {
      zoltan_decompose(method);
    }
#endif

    if (method == "LINEAR") {
      if (globalElementCount > 0) {
	simple_decompose(method, element_dist);
      }
    }

    std::sort(importElementMap.begin(), importElementMap.end());

    std::copy(importElementCount.begin(), importElementCount.end(), importElementIndex.begin());
    Ioss::Utils::generate_index(importElementIndex);

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
  void DecompositionData<INT>::generate_zone_shared_nodes(int cgnsFilePtr, INT min_node, INT max_node)
  {
    // Begin of Zone-Shared node information

    // Modify adjacency list based on shared nodes between zones...
    // Need the map from "global" to "global-shared"
    // * This is not necessarily nodes only on my processor since connectivity can include
    //   nodes other than those I own.
    // * Potentially large number of shared nodes; practically small(?)
    
    // * Maintain hash map from old id to new (if any)
    // * TODO: Determine whether the node is used on this processor...

    int base = 1; // Only single base supported so far.

    // Donor zone is always lower numbered, so zone 1 has no donor zone. Start at zone 2.
    for (cgsize_t zone=2; zone < (cgsize_t)zones_.size(); zone++) {
	
      // Determine number of "shared" nodes (shared with other zones)
      int nconn = 0;
      cg_nconns(cgnsFilePtr, base, zone, &nconn);
      for (int i=0; i < nconn; i++) {
	char connectname[33];
	CG_GridLocation_t location;
	CG_GridConnectivityType_t connect_type;
	CG_PointSetType_t ptset_type;
	cgsize_t npnts = 0;
	char donorname[33];
	CG_ZoneType_t donor_zonetype;
	CG_PointSetType_t donor_ptset_type;
	CG_DataType_t donor_datatype;
	cgsize_t ndata_donor;
	  
	cg_conn_info(cgnsFilePtr, base, zone, i+1, connectname,
		     &location, &connect_type,
		     &ptset_type, &npnts, donorname,
		     &donor_zonetype, &donor_ptset_type,
		     &donor_datatype, &ndata_donor);

	if (connect_type != CG_Abutting1to1 ||
	    ptset_type != CG_PointList ||
	    donor_ptset_type != CG_PointListDonor) {
	  std::ostringstream errmsg;
	  errmsg << "ERROR: CGNS: Zone " << zone
		 << " adjacency data is not correct type. Require Abutting1to1 and PointList."
		 << connect_type << "\t" << ptset_type << "\t" << donor_ptset_type;
	  IOSS_ERROR(errmsg);
	}

	// Verify data consistency...
	if (npnts != ndata_donor) {
	  std::ostringstream errmsg;
	  errmsg << "ERROR: CGNS: Zone " << zone
		 << " point count (" << npnts << ") does not match donor point count (" << ndata_donor << ").";
	  IOSS_ERROR(errmsg);
	}

	// Get number of nodes shared with other "previous" zones...
	// A "previous" zone will have a lower zone number this this zone...
	std::string dz_name(donorname);
	int dz = 1;
	for ( ; dz < zone; dz++) {
	  if (zones_[dz].m_name == dz_name) 
	    break;
	}

	if (dz != zone) {
	  std::cout << "Zone " << zone << " shares " << npnts << " nodes with " << donorname << "\n";
	  
	  std::vector<cgsize_t> points(npnts);
	  std::vector<cgsize_t> donors(npnts);

	  cg_conn_read(cgnsFilePtr, base, zone, i+1, TOPTR(points),
		       donor_datatype, TOPTR(donors));
	    
	  for (int j = 0; j < npnts; j++) {
	    cgsize_t point = points[j] - 1 + zones_[zone].m_nodeOffset;
	    if (point >= min_node && point <= max_node) {
	      cgsize_t donor = donors[j] - 1 + zones_[dz].m_nodeOffset;

	      // See if 'donor' is mapped to a different node already
	      auto donor_map = zone_shared_map.find(donor);
	      if (donor_map != zone_shared_map.end()) {
		donor = (*donor_map).second;
	      }
	      assert(zone_shared_map.find(point) == zone_shared_map.end());
	      zone_shared_map.insert({point, donor});
	    }
	  }
	}
      }
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

#if !defined(NO_PARMETIS_SUPPORT)
  template <typename INT>
  void DecompositionData<INT>::metis_decompose(const std::string &method,
                                               const std::vector<INT> &element_dist,
                                               const std::vector<INT> &pointer,
                                               const std::vector<INT> &adjacency)
  {
    std::vector<idx_t> elem_partition(elementCount);

    // Determine whether sizeof(INT) matches sizeof(idx_t).
    // If not, decide how to proceed...
    if (sizeof(INT) == sizeof(idx_t)) {
      internal_metis_decompose(method, (idx_t*)TOPTR(element_dist), (idx_t*)TOPTR(pointer), (idx_t*)TOPTR(adjacency), TOPTR(elem_partition));
    } 

    // Now know that they don't match... Are we widening or narrowing...
    else if (sizeof(idx_t) > sizeof(INT)) {
      assert(sizeof(idx_t) == 8);
      // ... Widening; just create new wider arrays
      std::vector<idx_t> dist_cv(element_dist.begin(), element_dist.end());
      std::vector<idx_t> pointer_cv(pointer.begin(), pointer.end());
      std::vector<idx_t> adjacency_cv(adjacency.begin(), adjacency.end());
      internal_metis_decompose(method, TOPTR(dist_cv), TOPTR(pointer_cv), TOPTR(adjacency_cv), TOPTR(elem_partition));
    }

    else if (sizeof(idx_t) < sizeof(INT)) {
      // ... Narrowing.  See if data range (#elements and/or #nodes) fits in 32-bit idx_t
      // Can determine this by checking the pointer[
      assert(sizeof(idx_t) == 4);
      if (globalElementCount >= INT_MAX || globalNodeCount >= INT_MAX || pointer[elementCount] >= INT_MAX) {
        // Can't narrow...
        std::ostringstream errmsg;
        errmsg << "ERROR: The metis/parmetis libraries being used with this application only support\n"
               << "       32-bit integers, but the mesh being decomposed requires 64-bit integers.\n"
               << "       You must either choose a different, non-metis decomposition method, or\n"
               << "       rebuild your metis/parmetis libraries with 64-bit integer support.\n"
               << "       Contact gdsjaar@sandia.gov for more details.\n";
        std::cerr << errmsg.str();
        exit(EXIT_FAILURE);
      } else {
        // Should be able to narrow...
        std::vector<idx_t> dist_cv(element_dist.begin(), element_dist.end());
        std::vector<idx_t> pointer_cv(pointer.begin(), pointer.end());
        std::vector<idx_t> adjacency_cv(adjacency.begin(), adjacency.end());
        internal_metis_decompose(method, TOPTR(dist_cv), TOPTR(pointer_cv), TOPTR(adjacency_cv), TOPTR(elem_partition));
      }
    }
    // ------------------------------------------------------------------------
    // Done with metis functions...

    // Determine how many elements I send to the other processors...
    // and how many remain local (on this processor)
    exportElementCount.resize(processorCount+1);
    for (size_t i=0; i < elem_partition.size(); i++) {
      exportElementCount[elem_partition[i]]++;
    }

    size_t local = exportElementCount[myProcessor];
    localElementMap.reserve(local);
    for (size_t i=0; i < elem_partition.size(); i++) {
      if (elem_partition[i] == myProcessor) {
        localElementMap.push_back(i);
      }
    }

    // Zero out the local element count so local elements aren't communicated.
    exportElementCount[myProcessor] = 0;

    importElementCount.resize(processorCount+1);
    MPI_Alltoall(TOPTR(exportElementCount), 1, Ioss::mpi_type((INT)0),
                 TOPTR(importElementCount), 1, Ioss::mpi_type((INT)0), comm_);

    // Now fill the vectors with the elements ...
    size_t exp_size = std::accumulate(exportElementCount.begin(), exportElementCount.end(), 0);

    exportElementMap.resize(exp_size);
    exportElementIndex.resize(processorCount+1);
    std::copy(exportElementCount.begin(), exportElementCount.end(), exportElementIndex.begin());
    Ioss::Utils::generate_index(exportElementIndex);

    {
      std::vector<INT> tmp_disp(exportElementIndex);
      for (size_t i=0; i < elem_partition.size(); i++) {
        if (elem_partition[i] != myProcessor) {
          exportElementMap[tmp_disp[elem_partition[i]]++] = elementOffset+i;
        }
      }
    }
    std::vector<idx_t>().swap(elem_partition);

    size_t imp_size = std::accumulate(importElementCount.begin(), importElementCount.end(), 0);
    importElementMap.resize(imp_size);
    importElementIndex.resize(processorCount+1);
    std::copy(importElementCount.begin(), importElementCount.end(), importElementIndex.begin());
    Ioss::Utils::generate_index(importElementIndex);

    Ioss::MY_Alltoallv(exportElementMap, exportElementCount, exportElementIndex, 
                       importElementMap, importElementCount, importElementIndex, comm_);

#if DEBUG_OUTPUT
    std::cerr << "Processor " << myProcessor << ":\t"
              << elementCount-exp_size << " local, "
              << imp_size             << " imported and "
              << exp_size            << " exported elements\n";
#endif
  }

  template <typename INT>
  void DecompositionData<INT>::internal_metis_decompose(const std::string &method,
                                                        idx_t *element_dist,
                                                        idx_t *pointer,
                                                        idx_t *adjacency,
                                                        idx_t *elem_partition)
  {
    idx_t wgt_flag = 0; // No weights
    idx_t *elm_wgt = nullptr;
    idx_t ncon = 1;
    idx_t num_flag = 0; // Use C-based numbering
    idx_t common_nodes = get_common_node_count(el_blocks, comm_);

    idx_t nparts = processorCount;
    idx_t ndims = spatialDimension;
    std::vector<real_t> tp_wgts(ncon*nparts, 1.0/nparts);

    std::vector<real_t> ub_vec(ncon, 1.01);

    idx_t edge_cuts = 0;

    std::vector<idx_t> options(3);
    options[0] = 1; // Use my values instead of default
    options[1] = 0; // PARMETIS_DBGLVL_TIME; 
    options[2] = 1234567; // Random number seed

    if (method == "KWAY") {
      int rc = ParMETIS_V3_PartMeshKway(element_dist, pointer, adjacency,
                                        elm_wgt, &wgt_flag, &num_flag, &ncon, &common_nodes, &nparts,
                                        TOPTR(tp_wgts), TOPTR(ub_vec), TOPTR(options), &edge_cuts, elem_partition,
                                        &comm_);
#if DEBUG_OUTPUT
      std::cerr << "Edge Cuts = " << edge_cuts << "\n";
#endif
      if (rc != METIS_OK) {
        std::ostringstream errmsg;
        errmsg << "ERROR: Problem during call to ParMETIS_V3_PartMeshKWay decomposition\n";
        std::cerr << errmsg.str();
        exit(EXIT_FAILURE);
      }
    }
    else if (method == "GEOM_KWAY" || method == "KWAY_GEOM") {

      idx_t *dual_xadj = nullptr;
      idx_t *dual_adjacency = nullptr;
      int rc = ParMETIS_V3_Mesh2Dual(element_dist, pointer, adjacency,
                                     &num_flag, &common_nodes, &dual_xadj, &dual_adjacency, &comm_);

      if (rc != METIS_OK) {
        std::ostringstream errmsg;
        errmsg << "ERROR: Problem during call to ParMETIS_V3_Mesh2Dual graph conversion\n";
        std::cerr << errmsg.str();
        exit(EXIT_FAILURE);
      }

      static_assert(sizeof(double) == sizeof(real_t),
                    "Parmetis real_t size must match double size");

      rc = ParMETIS_V3_PartGeomKway(element_dist, dual_xadj, dual_adjacency,
                                    elm_wgt, elm_wgt, &wgt_flag, &num_flag, &ndims, (real_t*)TOPTR(centroids_), &ncon, &nparts,
                                    TOPTR(tp_wgts), TOPTR(ub_vec), TOPTR(options), &edge_cuts, elem_partition, &comm_);

#if DEBUG_OUTPUT
      std::cerr << "Edge Cuts = " << edge_cuts << "\n";
#endif
      METIS_Free(dual_xadj);
      METIS_Free(dual_adjacency);

      if (rc != METIS_OK) {
        std::ostringstream errmsg;
        errmsg << "ERROR: Problem during call to ParMETIS_V3_PartGeomKWay decomposition\n";
        std::cerr << errmsg.str();
        exit(EXIT_FAILURE);
      }
    }
    else if (method == "METIS_SFC") {
      static_assert(sizeof(double) == sizeof(real_t),
                    "Parmetis real_t size must match double size");

      int rc = ParMETIS_V3_PartGeom(element_dist, &ndims, (real_t*)TOPTR(centroids_), elem_partition, &comm_);

      if (rc != METIS_OK) {
        std::ostringstream errmsg;
        errmsg << "ERROR: Problem during call to ParMETIS_V3_PartGeom decomposition\n";
        std::cerr << errmsg.str();
        exit(EXIT_FAILURE);
      }
    }
  }
#endif

#if !defined(NO_ZOLTAN_SUPPORT)
  template <typename INT>
  void DecompositionData<INT>::zoltan_decompose(const std::string &method)
  {
    float version = 0.0;
    Zoltan_Initialize(0, nullptr, &version);

    Zoltan zz(comm_);

    // Register Zoltan Callback functions...
    zz.Set_Num_Obj_Fn(zoltan_num_obj, this);
    zz.Set_Obj_List_Fn(zoltan_obj_list, this);
    zz.Set_Num_Geom_Fn(zoltan_num_dim, this);
    zz.Set_Geom_Multi_Fn(zoltan_geom, this);

    // Set Zoltan parameters
    std::string num_proc = Ioss::Utils::to_string(processorCount);
    zz.Set_Param("DEBUG_LEVEL", "0");
    zz.Set_Param("NUM_GLOBAL_PARTS", num_proc);

    int num_global = sizeof(INT)/sizeof(int);
    zz.Set_Param("NUM_GID_ENTRIES", Ioss::Utils::to_string(num_global));
    zz.Set_Param("NUM_LID_ENTRIES", "0");
    zz.Set_Param("LB_METHOD", method);
    zz.Set_Param("REMAP", "0");
    zz.Set_Param("RETURN_LISTS", "ALL");

    int changes = 0;
    int num_local  = 0;
    int num_import = 1;
    int  num_export = 1;
    ZOLTAN_ID_PTR import_global_ids = nullptr;
    ZOLTAN_ID_PTR import_local_ids  = nullptr;
    ZOLTAN_ID_PTR export_global_ids = nullptr;
    ZOLTAN_ID_PTR export_local_ids  = nullptr;
    int *import_procs   = nullptr;
    int *import_to_part = nullptr;
    int *export_procs   = nullptr;
    int *export_to_part = nullptr;

    num_local  = 1;

    // TODO: Check return value for error.
    zz.LB_Partition(changes, num_global, num_local,
		    num_import, import_global_ids, import_local_ids, import_procs, import_to_part,
		    num_export, export_global_ids, export_local_ids, export_procs, export_to_part);

#if DEBUG_OUTPUT
    std::cerr << "Processor " << myProcessor << ":\t"
	      << elementCount-num_export << " local, "
	      << num_import                  << " imported and "
	      << num_export                  << " exported elements\n";
#endif

    // Don't need centroid data anymore... Free up space
    std::vector<double>().swap(centroids_);

    // Find all elements that remain locally owned...
    get_local_element_list(export_global_ids, num_export);

    // Build exportElementMap and importElementMap...
    importElementMap.reserve(num_import);
    importElementIndex.resize(processorCount+1);
    importElementCount.resize(processorCount+1);

    if (num_global == 1) {
      std::vector<std::pair<int,int> > export_map;
      export_map.reserve(num_export);
      for (int i=0; i < num_export; i++) {
	export_map.push_back(std::make_pair(export_procs[i],export_global_ids[i]));
      }

      std::sort(export_map.begin(), export_map.end());
      exportElementMap.reserve(num_export);
      exportElementIndex.resize(processorCount+1);
      exportElementCount.resize(processorCount+1);
      for (int i=0; i < num_export; i++) {
	exportElementMap.push_back(export_map[i].second);
	exportElementCount[export_map[i].first]++;
      }

      for (int i=0; i < num_import; i++) {
	importElementMap.push_back(import_global_ids[i]);
	importElementCount[import_procs[i]]++;
      }
    } else {
      std::vector<std::pair<int,int64_t> > export_map;
      export_map.reserve(num_export);
      int64_t *export_glob = (int64_t*)export_global_ids;
      for (int i=0; i < num_export; i++) {
	export_map.push_back(std::make_pair(export_procs[i],export_glob[i]));
      }

      std::sort(export_map.begin(), export_map.end());
      exportElementMap.reserve(num_export);
      exportElementIndex.resize(processorCount+1);
      exportElementCount.resize(processorCount+1);
      for (int i=0; i < num_export; i++) {
	exportElementMap.push_back(export_map[i].second);
	exportElementCount[export_map[i].first]++;
      }

      int64_t *import_glob = (int64_t*)import_global_ids;
      for (int i=0; i < num_import; i++) {
	importElementMap.push_back(import_glob[i]);
	importElementCount[import_procs[i]]++;
      }
    }

    std::copy(exportElementCount.begin(), exportElementCount.end(), exportElementIndex.begin());
    Ioss::Utils::generate_index(exportElementIndex);

    zz.LB_Free_Part(&import_global_ids, &import_local_ids, &import_procs, &import_to_part);
    zz.LB_Free_Part(&export_global_ids, &export_local_ids, &export_procs, &export_to_part);
  }
#endif

#if !defined(NO_ZOLTAN_SUPPORT)
  template <typename INT>
  void DecompositionData<INT>::get_local_element_list(const ZOLTAN_ID_PTR &export_global_ids, size_t export_count)
  {
    std::vector<size_t> elements(elementCount);

    size_t global_id_size = sizeof(INT)/sizeof(int);

    if (global_id_size == 1) {
      for (size_t i=0; i < export_count; i++) {
	// flag all elements to be exported...
	size_t elem = export_global_ids[i];
	elements[elem-elementOffset] = 1;
      }
    } else {
      assert(global_id_size == 2);
      int64_t *export_glob = (int64_t*)export_global_ids;

      for (size_t i=0; i < export_count; i++) {
	// flag all elements to be exported...
	size_t elem = export_glob[i];
	elements[elem-elementOffset] = 1;
      }
    }

    localElementMap.reserve(elementCount - export_count);
    for (size_t i=0; i < elementCount; i++) {
      if (elements[i] == 0) {
	localElementMap.push_back(i);
      }
    }
  }
#endif

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
	  block.name_ = zone_name;
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
    Ioss::Utils::generate_index(fileBlockIndex);
    
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
	el_blocks[block].fileCount = overlap;
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
	el_blocks[block].fileSectionOffset = blk_start;
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
      else {
	el_blocks[block].fileCount = 0;
      }
    }
    pointer.push_back(adjacency.size());
  }

  template <typename INT>
  void DecompositionData<INT>::calculate_element_centroids(int cgnsFilePtr,
							   const std::vector<INT> &pointer,
							   const std::vector<INT> &adjacency,
							   const std::vector<INT> &node_dist)
  {
    // recv_count is the number of nodes that I need to recv from the other processors
    // send_count is the number of nodes that I need to send to the other processors
    std::vector<INT> recv_count(processorCount);
    std::vector<INT> send_count(processorCount);

    std::vector<int> owner; // Size is sum of element connectivity sizes (same as adjacency list)
    owner.reserve(adjacency.size());

    for (size_t i=0; i < adjacency.size(); i++) {
      INT node = adjacency[i];
      INT owning_processor = Ioss::Utils::find_index_location(node, node_dist);
      owner.push_back(owning_processor);
      recv_count[owning_processor]++;
    }

    // Zero out myProcessor entry in recv_count and sum the
    // remainder...
    recv_count[myProcessor] = 0;

    // Tell each processor how many nodes worth of data to send to
    // every other processor...
    MPI_Alltoall(TOPTR(recv_count), 1, Ioss::mpi_type((INT)0),
		 TOPTR(send_count), 1, Ioss::mpi_type((INT)0), comm_);

    send_count[myProcessor] = 0;

    std::vector<INT> recv_disp(processorCount);
    std::vector<INT> send_disp(processorCount);
    size_t sums = 0;
    size_t sumr = 0;
    for (int p=0; p < processorCount; p++) {
      recv_disp[p] = sumr;
      sumr += recv_count[p];

      send_disp[p] = sums;
      sums += send_count[p];
    }

#if DEBUG_OUTPUT
    std::cerr << "Processor " << myProcessor << " communicates "
	      << sumr << " nodes from and " << sums << " nodes to other processors\n";
#endif
    // Build the list telling the other processors which of their nodes I will need data from...
    std::vector<INT> node_comm_recv(sumr);
    std::vector<INT> node_comm_send(sums);
    {
      std::vector<INT> recv_tmp(processorCount);
      for (size_t i=0; i < owner.size(); i++) {
	int proc = owner[i];
	if (proc != myProcessor) {
	  INT node = adjacency[i];
	  size_t position = recv_disp[proc] + recv_tmp[proc]++;
	  node_comm_recv[position] = node;
	}
      }
    }

    Ioss::MY_Alltoallv(node_comm_recv, recv_count, recv_disp, 
		       node_comm_send, send_count, send_disp, comm_);

    // At this point, 'node_comm_send' contains the list of nodes that I need to provide
    // coordinate data for.

    // DEBUG: == Check that all nodes in node_comm_send are in the range
    //           nodeOffset..nodeOffset+nodeCount
    for (size_t i=0; i < node_comm_send.size(); i++) {
      assert((size_t)node_comm_send[i] >= nodeOffset &&
	     (size_t)node_comm_send[i] <  nodeOffset+nodeCount);
    }

    // Get my coordinate data using direct cgns calls
    std::vector<double> x(nodeCount);
    std::vector<double> y;
    std::vector<double> z;

    get_file_node_coordinates(cgnsFilePtr, 0, TOPTR(x));
    if (spatialDimension > 1) {
      y.resize(nodeCount);
      get_file_node_coordinates(cgnsFilePtr, 1, TOPTR(y));
    }
    if (spatialDimension > 2) {
      z.resize(nodeCount);
      get_file_node_coordinates(cgnsFilePtr, 2, TOPTR(z));
    }

    // The total vector size I need to send data in is node_comm_send.size()*3
    std::vector<double> coord_send;
    coord_send.reserve(node_comm_send.size() * spatialDimension);
    std::vector<double> coord_recv(node_comm_recv.size() * spatialDimension);
    for (size_t i=0; i < node_comm_send.size(); i++) {
      size_t node = node_comm_send[i] - nodeOffset;
      coord_send.push_back(x[node]);
      if (spatialDimension > 1)
	coord_send.push_back(y[node]);
      if (spatialDimension > 2) 
	coord_send.push_back(z[node]);
    }
    assert(coord_send.size() == node_comm_send.size() * spatialDimension);
    
    // Send the coordinate data back to the processors that requested it...
    for (int i=0; i < processorCount; i++) {
      send_count[i] *= spatialDimension;
      recv_count[i] *= spatialDimension;
      send_disp[i]  *= spatialDimension;
      recv_disp[i]  *= spatialDimension;
    }

    Ioss::MY_Alltoallv(coord_send, send_count, send_disp, 
		       coord_recv, recv_count, recv_disp, comm_);

    // Don't need coord_send data anymore ... clean out the vector.
    std::vector<double>().swap(coord_send);

    // Should have all needed coordinate data at this time.
    // Some in x,y,z vectors and some in coord_recv vector.

    // Note that in the current data structure, adjacency contains the
    // connectivity for all elements on this processor. 'owner' is a
    // parallel datastructure containing the owning processor for that
    // node.  If it is off-processor, then its coordinates will be
    // stored in coord_recv in processor order, but will be hit in the
    // correct order... The 'pointer' array tells the number of nodes
    // per element...

    // Calculate the centroid into the DecompositionData structure 'centroids'
    centroids_.reserve(elementCount*spatialDimension);
    std::vector<INT> recv_tmp(processorCount);

    for (size_t i=0; i < elementCount; i++) {
      size_t nnpe = pointer[i+1] - pointer[i];
      double cx = 0.0;
      double cy = 0.0;
      double cz = 0.0;
      for (INT jj = pointer[i]; jj < pointer[i+1]; jj++) {
	INT node = adjacency[jj];
	INT proc = owner[jj];
	if (proc == myProcessor) {
	  cx += x[node-nodeOffset];
	  if (spatialDimension > 1)
	    cy += y[node-nodeOffset];
	  if (spatialDimension > 2)
	    cz += z[node-nodeOffset];
	} else {
	  INT coffset = recv_disp[proc] + recv_tmp[proc];  recv_tmp[proc] += spatialDimension;
	  cx += coord_recv[coffset+0];
	  if (spatialDimension > 1)
	    cy += coord_recv[coffset+1];
	  if (spatialDimension > 2)
	    cz += coord_recv[coffset+2];
	}
      }
      centroids_.push_back(cx / nnpe);
      if (spatialDimension > 1)
	centroids_.push_back(cy / nnpe);
      if (spatialDimension > 2)
	centroids_.push_back(cz / nnpe);
    }
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
      b = Ioss::Utils::find_index_location(elem, fileBlockIndex);

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

      b = Ioss::Utils::find_index_location(elem, fileBlockIndex);
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

      b = Ioss::Utils::find_index_location(elem, fileBlockIndex);

      size_t off = std::max(fileBlockIndex[b], elementOffset);
      el_blocks[b].exportMap.push_back(elem-off);
      el_blocks[b].exportCount[proc]++;
    }

    for (auto &block : el_blocks) {
      block.iossCount = block.localMap.size() + block.importMap.size();
      std::copy(block.exportCount.begin(), block.exportCount.end(), block.exportIndex.begin());
      std::copy(block.importCount.begin(), block.importCount.end(), block.importIndex.begin());
      Ioss::Utils::generate_index(block.exportIndex);
      Ioss::Utils::generate_index(block.importIndex);
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

    // Now need to sort and Ioss::Utils::uniquify 'nodes'
    Ioss::Utils::uniquify(nodes);

    // Determine owning 'file' processor for each node...
    nodeIndex.resize(processorCount+1);

    for (size_t i=0; i < nodes.size(); i++) {
      INT owning_processor = Ioss::Utils::find_index_location(nodes[i], node_dist);
      nodeIndex[owning_processor]++;
    }
    importNodeCount.resize(nodeIndex.size());
    std::copy(nodeIndex.begin(), nodeIndex.end(), importNodeCount.begin());
    exportNodeCount.resize(processorCount);
    Ioss::Utils::generate_index(nodeIndex);

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
    Ioss::Utils::generate_index(exportNodeIndex);

    // Now send the list of nodes that I need to import from each
    // processor...
    importNodeIndex.resize(importNodeCount.size());
    std::copy(importNodeCount.begin(), importNodeCount.end(), importNodeIndex.begin());
    Ioss::Utils::generate_index(importNodeIndex);

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
    Ioss::Utils::generate_index(send_comm_map_disp);

    std::vector<INT> send_comm_map(send_comm_map_disp[processorCount]);
    std::vector<INT> nc_offset(processorCount);

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
    Ioss::Utils::generate_index(recv_comm_map_disp);
    nodeCommMap.resize(recv_comm_map_disp[processorCount-1] + recv_comm_map_count[processorCount-1]);
    Ioss::MY_Alltoallv(send_comm_map, send_comm_map_count, send_comm_map_disp, 
		       nodeCommMap, recv_comm_map_count, recv_comm_map_disp, comm_);

    // Map global 0-based index to local 1-based index.
    for (size_t i=0; i < nodeCommMap.size(); i+=2) {
      nodeCommMap[i] = node_global_to_local(nodeCommMap[i]+1);
    }
#if DEBUG_OUTPUT
    std::cerr << "Processor " << myProcessor << " has "
	      << nodeCommMap.size() << " shared nodes\n";
#endif

  }

  template <typename INT>
  void DecompositionData<INT>::get_file_node_coordinates(int cgnsFilePtr, int direction, double *data) const
  {
    const std::string coord_name[] = {"CoordinateX", "CoordinateY", "CoordinateZ"};

    int base = 1; // Only single base supported so far.
    cgsize_t beg = 0;
    cgsize_t end = 0;
    cgsize_t offset = 0;
    cgsize_t node_count = nodeCount;
    cgsize_t node_offset = nodeOffset;
    
    int num_zones = (int)zones_.size()-1;
    for (int zone=1; zone <= num_zones; zone++) {
      end += zones_[zone].m_nodeCount;

      if (end > node_offset && beg <= node_offset+node_count) {
	cgsize_t start  = std::max(node_offset, beg);
	cgsize_t finish = std::min(end, node_offset+node_count);
	if (finish > start) {
	  cgsize_t count = finish-start;

	  
	  // Now adjust start for 1-based node numbering and the start of this zone...
	  start = start - beg + 1;
	  finish = finish - beg;
	  std::cerr << myProcessor << ": reading " << count << " nodes from zone " << zone
		    << " starting at " << start
		    << " with an offset of " << offset
		    << " ending at " << finish << "\n";

	  int ierr = cg_coord_read(cgnsFilePtr, base, zone,
				   coord_name[direction].c_str(), CG_RealDouble,
				   &start, &finish, &data[offset]);
	  if (ierr < 0) {
	    cgns_error(cgnsFilePtr, __LINE__, myProcessor);
	  }
	  offset += count;
	}
      }
      beg = end;
    }
  }

  template <typename INT>
  void DecompositionData<INT>::get_node_coordinates(int cgnsFilePtr, double *ioss_data, const Ioss::Field &field) const
  {
    std::vector<double> tmp(nodeCount);
    if (field.get_name() == "mesh_model_coordinates_x") {
      get_file_node_coordinates(cgnsFilePtr, 0, TOPTR(tmp));
      communicate_node_data(TOPTR(tmp), ioss_data, 1);
    }

    else if (field.get_name() == "mesh_model_coordinates_y") {
      get_file_node_coordinates(cgnsFilePtr, 1, TOPTR(tmp));
      communicate_node_data(TOPTR(tmp), ioss_data, 1);
    }

    else if (field.get_name() == "mesh_model_coordinates_z") {
      get_file_node_coordinates(cgnsFilePtr, 2, TOPTR(tmp));
      communicate_node_data(TOPTR(tmp), ioss_data, 1);
    }

    else if (field.get_name() == "mesh_model_coordinates") {
      // Data required by upper classes store x0, y0, z0, ... xn,
      // yn, zn. Data stored in cgns file is x0, ..., xn, y0,
      // ..., yn, z0, ..., zn so we have to allocate some scratch
      // memory to read in the data and then map into supplied
      // 'data'

      std::vector<double> ioss_tmp(ioss_node_count());

      // This implementation trades off extra communication for
      // reduced memory overhead.
      // * This method uses 'ioss_node_count' extra memory; 3
      // reads; and 3 communicate_node_data calls.
      //
      // * Other method uses 6*ioss_node_count extra memory; 3 reads;
      // and 1 communicate_node_data call.
      //
      for (size_t d = 0; d < spatialDimension; d++) {
	get_file_node_coordinates(cgnsFilePtr, d, TOPTR(tmp));
        communicate_node_data(TOPTR(tmp), TOPTR(ioss_tmp), 1);

        size_t index = d;
        for (size_t i=0; i < ioss_node_count(); i++) {
          ioss_data[index] = ioss_tmp[i];
          index += spatialDimension;
        }
      }
    }
  }

  // The following function is used if reading all element data on a
  // processor instead of just an element blocks worth...
  template void DecompositionData<int>::communicate_element_data(int *file_data, int *ioss_data, size_t comp_count) const;
  template void DecompositionData<int64_t>::communicate_element_data(int64_t *file_data, int64_t *ioss_data, size_t comp_count) const;
  template void DecompositionData<int>::communicate_element_data(double *file_data, double *ioss_data, size_t comp_count) const;
  template void DecompositionData<int64_t>::communicate_element_data(double *file_data, double *ioss_data, size_t comp_count) const;

  template <typename INT> template <typename T>
  void DecompositionData<INT>::communicate_element_data(T *file_data, T *ioss_data, size_t comp_count) const
  {
    // Transfer the file-decomposition based data in 'file_data' to
    // the ioss-decomposition based data in 'ioss_data'
    std::vector<T> export_data(exportElementMap.size() * comp_count);
    std::vector<T> import_data(importElementMap.size() * comp_count);

    if (comp_count == 1) {
      for (size_t i=0; i < exportElementMap.size(); i++) {
        size_t index = exportElementMap[i] - elementOffset;
        export_data[i] = file_data[index];
      }

      // Transfer all local data from file_data to ioss_data...
      for (size_t i=0; i < localElementMap.size(); i++) {
        size_t index = localElementMap[i];
        ioss_data[importPreLocalElemIndex+i] = file_data[index];
      }

      // Get my imported data and send my exported data...
      Ioss::MY_Alltoallv(export_data, exportElementCount, exportElementIndex, 
			 import_data, importElementCount, importElementIndex, comm_);

      // Copy the imported data into ioss_data...
      // Some comes before the local data...
      for (size_t i=0; i < importPreLocalElemIndex; i++) {
        ioss_data[i] = import_data[i];
      }

      // Some comes after the local data...
      size_t offset = importPreLocalElemIndex + localElementMap.size();
      for (size_t i=0; i < importElementMap.size() - importPreLocalElemIndex; i++) {
        ioss_data[offset+i] = import_data[importPreLocalElemIndex+i];
      }
    } else {
      for (size_t i=0; i < exportElementMap.size(); i++) {
        size_t index = exportElementMap[i] - elementOffset;
        for (size_t j=0; j < comp_count; j++) {
          export_data[comp_count*i+j] = file_data[comp_count*index+j];
        }
      }

      // Transfer all local data from file_data to ioss_data...
      for (size_t i=0; i < localElementMap.size(); i++) {
        size_t index = localElementMap[i];
        for (size_t j=0; j < comp_count; j++) {
          ioss_data[comp_count*(importPreLocalElemIndex+i)+j] = file_data[comp_count*index+j];
        }
      }

      std::vector<INT> export_count(exportElementCount.begin(), exportElementCount.end());
      std::vector<INT> export_disp(exportElementIndex.begin(), exportElementIndex.end());
      std::vector<INT> import_count(importElementCount.begin(), importElementCount.end());
      std::vector<INT> import_disp(importElementIndex.begin(), importElementIndex.end());

      for (int i=0; i < processorCount; i++) {
        export_count[i] *= comp_count;
        export_disp[i]  *= comp_count;
        import_count[i] *= comp_count;
        import_disp[i]  *= comp_count;
      }

      // Get my imported data and send my exported data...
      Ioss::MY_Alltoallv(export_data, export_count, export_disp, 
			 import_data, import_count, import_disp, comm_);

      // Copy the imported data into ioss_data...
      // Some comes before the local data...
      for (size_t i=0; i < importPreLocalElemIndex; i++) {
        for (size_t j=0; j < comp_count; j++) {
          ioss_data[comp_count * i + j] = import_data[comp_count * i + j];
        }
      }

      // Some comes after the local data...
      size_t offset = importPreLocalElemIndex + localElementMap.size();
      for (size_t i=0; i < importElementMap.size() - importPreLocalElemIndex; i++) {
        for (size_t j=0; j < comp_count; j++) {
          ioss_data[comp_count*(offset+i) + j] = import_data[comp_count*(importPreLocalElemIndex+i)+j];
        }
      }
    }
  }

  template void DecompositionData<int>::get_block_connectivity(int cgnsFilePtr, int *data, int blk_seq) const;
  template void DecompositionData<int64_t>::get_block_connectivity(int cgnsFilePtr, int64_t *data, int blk_seq) const;

  template <typename INT>
  void DecompositionData<INT>::get_block_connectivity(int cgnsFilePtr, INT *data, int blk_seq) const
  {
    auto &blk = el_blocks[blk_seq];
    std::vector<cgsize_t> file_conn(blk.file_count() * blk.nodesPerEntity);
    int base = 1;
    cg_elements_partial_read(cgnsFilePtr, base, blk.zone(), blk.section(),
			     blk.fileSectionOffset, blk.fileSectionOffset + blk.file_count() - 1,
			     TOPTR(file_conn), nullptr);
    // Map from zone-local node numbers to global implicit
    for (auto &node : file_conn) {
      node += blk.zoneNodeOffset;
    }

    if (!zone_shared_map.empty()) {
      for (auto &node : file_conn) {
	auto alias = zone_shared_map.find(node-1);
	if (alias != zone_shared_map.end()) {
	  node = (*alias).second+1;
	}
      }
    }

    communicate_block_data(TOPTR(file_conn), data, blk_seq, blk.nodesPerEntity);
  }

  template void DecompositionData<int64_t>::communicate_block_data(cgsize_t *file_data,   int64_t *ioss_data, size_t blk_seq, size_t comp_count) const;
  template void DecompositionData<int>::communicate_block_data(cgsize_t *file_data,    int *ioss_data,  size_t blk_seq, size_t comp_count) const;
  template <typename INT> template <typename T>
  void DecompositionData<INT>::communicate_block_data(cgsize_t *file_data, T *ioss_data, size_t blk_seq, size_t comp_count) const
  {
    BlockDecompositionData blk = el_blocks[blk_seq];

    std::vector<T> exports;
    exports.reserve(comp_count * blk.exportMap.size());
    std::vector<T> imports(comp_count * blk.importMap.size());

    if (comp_count == 1) {
      for (size_t i=0; i < blk.exportMap.size(); i++) {
        exports.push_back(file_data[blk.exportMap[i]]);
      }

      std::vector<int> export_count(blk.exportCount.begin(), blk.exportCount.end());
      std::vector<int> export_disp(blk.exportIndex.begin(), blk.exportIndex.end());
      std::vector<int> import_count(blk.importCount.begin(), blk.importCount.end());
      std::vector<int> import_disp(blk.importIndex.begin(), blk.importIndex.end());

      for (int i=0; i < processorCount; i++) {
        export_count[i] *= sizeof(T);
        export_disp[i]  *= sizeof(T);
        import_count[i] *= sizeof(T);
        import_disp[i]  *= sizeof(T);
      }

      // Get my imported data and send my exported data...
      Ioss::MY_Alltoallv(exports, blk.exportCount, blk.exportIndex, 
			 imports, blk.importCount, blk.importIndex, comm_);

      // Map local and imported data to ioss_data.
      for (size_t i=0; i < blk.localMap.size(); i++) {
        ioss_data[i+blk.localIossOffset] = file_data[blk.localMap[i]];
      }

      for (size_t i=0; i < blk.importMap.size(); i++) {
        ioss_data[blk.importMap[i]] = imports[i];
      }
    } else {
      for (size_t i=0; i < blk.exportMap.size(); i++) {
        for (size_t j=0; j < comp_count; j++) {
          exports.push_back(file_data[blk.exportMap[i]*comp_count + j]);
        }
      }

      std::vector<int> export_count(blk.exportCount.begin(), blk.exportCount.end());
      std::vector<int> export_disp(blk.exportIndex.begin(), blk.exportIndex.end());
      std::vector<int> import_count(blk.importCount.begin(), blk.importCount.end());
      std::vector<int> import_disp(blk.importIndex.begin(), blk.importIndex.end());

      for (int i=0; i < processorCount; i++) {
        export_count[i] *= comp_count;
        export_disp[i]  *= comp_count;
        import_count[i] *= comp_count;
        import_disp[i]  *= comp_count;
      }

      // Get my imported data and send my exported data...
      Ioss::MY_Alltoallv(exports, export_count, export_disp, 
			 imports, import_count, import_disp, comm_);

      // Map local and imported data to ioss_data.
      for (size_t i=0; i < blk.localMap.size(); i++) {
        for (size_t j=0; j < comp_count; j++) {
          ioss_data[(i+blk.localIossOffset)*comp_count+j] = file_data[blk.localMap[i]*comp_count+j];
        }
      }

      for (size_t i=0; i < blk.importMap.size(); i++) {
        for (size_t j=0; j < comp_count; j++) {
          ioss_data[blk.importMap[i]*comp_count+j] = imports[i*comp_count+j];
        }
      }
    }
  }

  template void DecompositionData<int>::communicate_node_data(int *file_data, int *ioss_data, size_t comp_count) const;
  template void DecompositionData<int>::communicate_node_data(double *file_data, double *ioss_data, size_t comp_count) const;
  template void DecompositionData<int64_t>::communicate_node_data(int64_t *file_data, int64_t *ioss_data, size_t comp_count) const;
  template void DecompositionData<int64_t>::communicate_node_data(double *file_data, double *ioss_data, size_t comp_count) const;

  template <typename INT> template <typename T>
  void DecompositionData<INT>::communicate_node_data(T *file_data, T *ioss_data, size_t comp_count) const
  {
    // Transfer the file-decomposition based data in 'file_data' to
    // the ioss-decomposition based data in 'ioss_data'
    std::vector<T> export_data(exportNodeMap.size() * comp_count);
    std::vector<T> import_data(importNodeMap.size() * comp_count);

    if (comp_count == 1) {
      for (size_t i=0; i < exportNodeMap.size(); i++) {
        size_t index = exportNodeMap[i] - nodeOffset;
        assert(index < nodeCount);
        export_data[i] = file_data[index];
      }

      // Transfer all local data from file_data to ioss_data...
      for (size_t i=0; i < localNodeMap.size(); i++) {
        size_t index = localNodeMap[i] - nodeOffset;
        assert(index < nodeCount);
        ioss_data[importPreLocalNodeIndex+i] = file_data[index];
      }

      // Get my imported data and send my exported data...
      Ioss::MY_Alltoallv(export_data, exportNodeCount, exportNodeIndex,
                         import_data, importNodeCount, importNodeIndex, comm_);

      // Copy the imported data into ioss_data...
      for (size_t i=0; i < importNodeMap.size(); i++) {
        size_t index = importNodeMap[i];
        assert(index < ioss_node_count());
        ioss_data[index] = import_data[i];
      }

    } else { // Comp_count > 1
      for (size_t i=0; i < exportNodeMap.size(); i++) {
        size_t index = exportNodeMap[i] - nodeOffset;
        assert(index < nodeCount);
        for (size_t j=0; j < comp_count; j++) {
          export_data[comp_count*i+j] = file_data[comp_count*index+j];
        }
      }

      // Transfer all local data from file_data to ioss_data...
      for (size_t i=0; i < localNodeMap.size(); i++) {
        size_t index = localNodeMap[i] - nodeOffset;
        assert(index < nodeCount);
        for (size_t j=0; j < comp_count; j++) {
          ioss_data[comp_count*(importPreLocalNodeIndex+i)+j] = file_data[comp_count*index+j];
        }
      }

      std::vector<INT> export_count(exportNodeCount.begin(), exportNodeCount.end());
      std::vector<INT> export_disp(exportNodeIndex.begin(), exportNodeIndex.end());
      std::vector<INT> import_count(importNodeCount.begin(), importNodeCount.end());
      std::vector<INT> import_disp(importNodeIndex.begin(), importNodeIndex.end());

      for (int i=0; i < processorCount; i++) {
        export_count[i] *= comp_count;
        export_disp[i]  *= comp_count;
        import_count[i] *= comp_count;
        import_disp[i]  *= comp_count;
      }

      // Get my imported data and send my exported data...
      Ioss::MY_Alltoallv(export_data, export_count, export_disp, 
                         import_data, import_count, import_disp, comm_);

      // Copy the imported data into ioss_data...
      for (size_t i=0; i < importNodeMap.size(); i++) {
        size_t index = importNodeMap[i];
        assert(index < ioss_node_count());
        for (size_t j=0; j < comp_count; j++) {
          ioss_data[comp_count*index+j] = import_data[comp_count*i+j];
        }
      }
    }
  }

  template void DecompositionDataBase::communicate_node_data(int *file_data, int *ioss_data, size_t comp_count) const;
  template void DecompositionDataBase::communicate_node_data(int64_t *file_data, int64_t *ioss_data, size_t comp_count) const;
  template void DecompositionDataBase::communicate_node_data(double *file_data, double *ioss_data, size_t comp_count) const;

  template <typename T>
  void DecompositionDataBase::communicate_node_data(T *file_data, T *ioss_data, size_t comp_count) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int>*>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      this32->communicate_node_data(file_data, ioss_data, comp_count);
    } else {
      const DecompositionData<int64_t> *this64 = dynamic_cast<const DecompositionData<int64_t>*>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      this64->communicate_node_data(file_data, ioss_data, comp_count);
    }
  }

  template void DecompositionDataBase::communicate_element_data(int *file_data, int *ioss_data, size_t comp_count) const;
  template void DecompositionDataBase::communicate_element_data(int64_t *file_data, int64_t *ioss_data, size_t comp_count) const;
  template void DecompositionDataBase::communicate_element_data(double *file_data, double *ioss_data, size_t comp_count) const;

  template <typename T>
  void DecompositionDataBase::communicate_element_data(T *file_data, T *ioss_data, size_t comp_count) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int>*>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      this32->communicate_element_data(file_data, ioss_data, comp_count);
    } else {
      const DecompositionData<int64_t> *this64 = dynamic_cast<const DecompositionData<int64_t>*>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      this64->communicate_element_data(file_data, ioss_data, comp_count);
    }
  }

  void DecompositionDataBase::get_block_connectivity(int cgnsFilePtr, void *data, int blk_seq) const
  {
    if (int_size() == sizeof(int)) {
      const DecompositionData<int> *this32 = dynamic_cast<const DecompositionData<int>*>(this);
      Ioss::Utils::check_dynamic_cast(this32);
      this32->get_block_connectivity(cgnsFilePtr, (int*)data, blk_seq);
    } else {
      const DecompositionData<int64_t> *this64 = dynamic_cast<const DecompositionData<int64_t>*>(this);
      Ioss::Utils::check_dynamic_cast(this64);
      this64->get_block_connectivity(cgnsFilePtr,  (int64_t*)data, blk_seq);
    }
  }

}
