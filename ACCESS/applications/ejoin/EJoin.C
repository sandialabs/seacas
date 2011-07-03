// Copyright(C) 2010 Sandia Corporation.
// 
// Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
// the U.S. Government retains certain rights in this software.
//         
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
// 
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
// 
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Sandia Corporation nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#include <iostream>
#include <iomanip>
#include <iterator>
#include <set>
#include <vector>
#include <algorithm>
#include <numeric>
#include <map>
#include <string>
#include <exception>

#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <cfloat>
#include <cmath>
#include <limits>

#include <cstring>
#include <ctime>
#include <sys/times.h>
#include <ctype.h>
#include <sys/utsname.h>

#include "adler.h"
#include "smart_assert.h"
#include "to_string.h"

#include <Ionit_Initializer.h>
#include <Ioss_SubSystem.h>
#include <Ioss_Transform.h>

#include "SystemInterface.h"
#include "match_xyz.h"
#include "Vector3.h"

namespace {
  void define_global_fields(Ioss::Region &output_region, RegionVector &part_mesh);
  void define_nodal_fields(Ioss::Region &output_region, RegionVector &part_mesh);
  void define_element_fields(Ioss::Region &output_region, RegionVector &part_mesh);
  void define_nset_fields(Ioss::Region &output_region, RegionVector &part_mesh);
  void define_eset_fields(Ioss::Region &output_region, RegionVector &part_mesh);
  void define_fset_fields(Ioss::Region &output_region, RegionVector &part_mesh);

  void output_nodeblock(Ioss::Region &output_region, RegionVector &part_mesh,
			const Map &local_node_map, Map &global_node_map);
  void output_elementblock(Ioss::Region &output_region, RegionVector &part_mesh, const Map &local_node_map);
  void output_nodeset(Ioss::Region &output_region, RegionVector &part_mesh, const Map &local_node_map);
  void output_edgeset(Ioss::Region &output_region, RegionVector &part_mesh);
  void output_faceset(Ioss::Region &output_region, RegionVector &part_mesh);
  void output_transient_state(Ioss::Region &output_region, RegionVector &part_mesh,
			      double time, const Map &local_node_map);

  template <typename T>
  bool approx_equal(T v1, T v2)
  {
#if 0
    static const T tolerance = 100.0 * std::numeric_limits<T>::epsilon();
    return std::fabs(v1 - v2) <= std::fabs(v1+v2)*tolerance;
#else
    return (float)v1 == (float)v2;
#endif
  }
}

extern void add_to_log(const char *name, int elapsed);
extern double ejoin_timer();

#include <exodusII.h>

namespace {
  void put_mesh_summary(const Ioss::Region& mesh);
  void transfer_elementblock(Ioss::Region &region, Ioss::Region &output_region, bool debug);
  void transfer_nodesets(Ioss::Region &region, Ioss::Region &output_region, bool debug);
  void transfer_facesets(Ioss::Region &region, Ioss::Region &output_region, bool debug);
  void transfer_edgesets(Ioss::Region &region, Ioss::Region &output_region, bool debug);
  void transfer_fields(Ioss::GroupingEntity *ige,
                       Ioss::GroupingEntity *oge,
                       Ioss::Field::RoleType role,
                       const std::string &prefix = "");

  void transfer_field_data(Ioss::GroupingEntity *ige,
                           Ioss::GroupingEntity *oge,
                           Ioss::Field::RoleType role,
                           const std::string &prefix = "",
                           bool transfer_connectivity = true);

  void transfer_field_data_internal(Ioss::GroupingEntity *ige,
                                    Ioss::GroupingEntity *oge,
                                    const std::string &field_name);


  std::string time_stamp(const std::string &format);
}

std::string tsFormat = "[%H:%M:%S] ";

// prototypes

int ejoin(Excn::SystemInterface &interface, std::vector<Ioss::Region*> &part_mesh);

namespace {
void build_reverse_node_map(Ioss::Region &output_region,
			    RegionVector &part_mesh,
			    Map &global_node_map,
			    Map &local_node_map);

#if 0
void build_reverse_element_map(Ioss::Region *output_region,
			       RegionVector &part_mesh,
			       GlobalElemMap &global_element_map);
#endif

template <typename T>
void uniqify(std::vector<T> &map) {
  std::sort(map.begin(), map.end());
  map.erase(std::unique(map.begin(), map.end()), map.end());
  // shrink-to-fit...
  std::vector<T>(map).swap(map);
}
}

unsigned int debug_level = 0;

using namespace Excn;

int main(int argc, char* argv[])
{
#if defined(__LIBCATAMOUNT__)
  setlinebuf(stderr);
#endif
  try {
  time_t begin_time = time(NULL);
  SystemInterface::show_version();
  Ioss::Init::Initializer io;

  SystemInterface interface;
  bool ok = interface.parse_options(argc, argv);

  if (!ok) {
    std::cerr << "\nERROR: Problems parsing command line arguments.\n\n";
    exit(EXIT_FAILURE);
  }

  debug_level = interface.debug();

  if (debug_level & 64)
    ex_opts(EX_VERBOSE|EX_DEBUG);
  else
    ex_opts(0);

  int error = 0;

  std::vector<Ioss::Region*> part_mesh(interface.inputFiles_.size());
  for (size_t p=0; p < interface.inputFiles_.size(); p++) {
    Ioss::DatabaseIO *dbi = Ioss::IOFactory::create("exodusII", interface.inputFiles_[p],
						    Ioss::READ_RESTART, (MPI_Comm)MPI_COMM_WORLD);
    if (dbi == NULL || !dbi->ok(true))
      std::exit(EXIT_FAILURE);

    dbi->set_surface_split_type(Ioss::SPLIT_BY_DONT_SPLIT);
    
    // Generate a name for the region based on the part number...
    std::string name = "p" + to_string(p+1);
    // NOTE: region owns database pointer at this time...
    part_mesh[p] = new Ioss::Region(dbi, name);

    Vector3 offset = interface.offset();
    if (p > 0 && (offset.x != 0.0 || offset.y != 0.0 || offset.z != 0.0)) {
      Ioss::NodeBlock *nb = part_mesh[p]->get_node_blocks()[0];
      Ioss::Field coord = nb->get_field("mesh_model_coordinates");
      Ioss::Transform* transform = Iotr::Factory::create("offset3D");
      assert(transform != NULL);
      std::vector<double> values(3);
      values[0] = offset.x * p; values[1] = offset.y * p; values[2] = offset.z * p;
      transform->set_properties("offset", values);
      coord.add_transform(transform);
      nb->field_erase("mesh_model_coordinates");
      nb->field_add(coord);
    }
  }

  ejoin(interface, part_mesh);
  
  for (size_t p=0; p < interface.inputFiles_.size(); p++) {
    delete part_mesh[p];
  }
  
  time_t end_time = time(NULL);
  add_to_log(argv[0], (int)(end_time-begin_time));
  return (error);
  }
  catch (std::exception &e) {
    std::cerr << "ERROR: Standard exception: " << e.what() << std::endl;
  }
}

int ejoin(SystemInterface &interface, std::vector<Ioss::Region*> &part_mesh)
{
  size_t part_count = interface.inputFiles_.size();
  SMART_ASSERT(part_count == part_mesh.size());
  
  Ioss::DatabaseIO *dbo = Ioss::IOFactory::create("exodusII", interface.outputName_, Ioss::WRITE_RESTART,
						  (MPI_Comm)MPI_COMM_WORLD);
  if (dbo == NULL || !dbo->ok(true)) {
    std::exit(EXIT_FAILURE);
  }
  
  // NOTE: 'output_region' owns 'dbo' pointer at this time
  Ioss::Region output_region(dbo, "ejoin_output_region");

  output_region.begin_mode(Ioss::STATE_DEFINE_MODEL);
  
  if (debug_level & 1)
    std::cerr << time_stamp(tsFormat);

  int node_offset = 0;
  int element_offset = 0;
  for (size_t p = 0; p < part_count; p++) {
    part_mesh[p]->property_add(Ioss::Property("node_offset", node_offset));
    part_mesh[p]->property_add(Ioss::Property("element_offset", element_offset));
    int local_node_count = part_mesh[p]->get_property("node_count").get_int();
    int local_elem_count = part_mesh[p]->get_property("element_count").get_int();
    node_offset += local_node_count;
    element_offset += local_elem_count;
  }

  int node_count = node_offset; // Sum of nodes in part meshes.
  Map local_node_map(node_count);
  Map global_node_map;
  
  // Need a map from local node to global node.
  // Position in the map is part_mesh.offset+local_node_position
  // Return value is position in the global node list.
  // If no mapping, then the list is simply 0..number_of_global_nodes
  // where number_of_global_nodes is the sump of the node counts in the
  // individual files.
  if (interface.match_node_ids()) {
    build_reverse_node_map(output_region, part_mesh, global_node_map, local_node_map);
  } else if (interface.match_xyz()) {
    match_xyz(part_mesh, interface.tolerance(), local_node_map, global_node_map);
  } else {
    global_node_map.resize(local_node_map.size());
    for (size_t i=0; i < local_node_map.size(); i++) {
      local_node_map[i] = i;
      global_node_map[i] = i+1;
    }
  }
	     
  node_count = global_node_map.size();
  size_t merged = local_node_map.size() - global_node_map.size();
  if (merged > 0) {
    std::cerr << "*** " << merged << " Nodes were merged.\n";
  }

  // Verify nodemap...
  std::vector<int> glob(node_count);
  for (size_t i=0; i<local_node_map.size(); i++) {
    glob[local_node_map[i]] = 1;
  }
  for (size_t i=0; i<glob.size(); i++) {
    SMART_ASSERT(glob[i] == 1);
  }
  // Transfer some common data...
  output_region.property_add(part_mesh[0]->get_property("title"));
  output_region.property_add(part_mesh[0]->get_property("spatial_dimension"));
  int spatial_dimension = part_mesh[0]->get_property("spatial_dimension").get_int();

  // Define a node block...  
  std::string block_name = "nodeblock_1";
  Ioss::NodeBlock *block = new Ioss::NodeBlock(output_region.get_database(), block_name,
					       node_count, spatial_dimension);
  block->property_add(Ioss::Property("id", 1));
  
  output_region.add(block);

  // Add element blocks, nodesets, sidesets (facesets, edgesets)
  for (size_t p = 0; p < part_count; p++) {
    transfer_elementblock(*part_mesh[p], output_region, false);
    if (!interface.omit_nodesets()) {
      transfer_nodesets(*part_mesh[p], output_region, false);
    }
    if (!interface.omit_sidesets()) {
      transfer_facesets(*part_mesh[p], output_region, false);
      transfer_edgesets(*part_mesh[p], output_region, false);
    }
  }
    
  output_region.end_mode(Ioss::STATE_DEFINE_MODEL);

  output_region.begin_mode(Ioss::STATE_MODEL);

  output_nodeblock(output_region, part_mesh, local_node_map, global_node_map);
  output_elementblock(output_region, part_mesh, local_node_map);
  if (!interface.omit_nodesets())
    output_nodeset(output_region, part_mesh, local_node_map);
  if (!interface.omit_sidesets()) {
    output_edgeset(output_region, part_mesh);
    output_faceset(output_region, part_mesh);
  }

  output_region.end_mode(Ioss::STATE_MODEL);
  
  // ####################TRANSIENT DATA SECTION###########################
  // ***********************************************************************
  // 9. Get Variable Information and names

  if (debug_level & 1)
    std::cerr << time_stamp(tsFormat);

  output_region.begin_mode(Ioss::STATE_DEFINE_TRANSIENT);
  
  define_global_fields(output_region, part_mesh);
  define_nodal_fields(output_region, part_mesh);
  define_element_fields(output_region, part_mesh);
  if (!interface.omit_nodesets())
    define_nset_fields(output_region, part_mesh);
  if (!interface.omit_sidesets()) {
    define_fset_fields(output_region, part_mesh);
    define_eset_fields(output_region, part_mesh);
  }

  output_region.end_mode(Ioss::STATE_DEFINE_TRANSIENT);

  // Get database times...
  // Different parts can have either no times or the times must match
  std::vector<double> global_times;
  
  for (size_t p = 0; p < part_count; p++) {
    int nts = part_mesh[p]->get_property("state_count").get_int();
    std::vector<double> times(nts);
    // If multiple databases have timesteps, they must all match.
    for (int i=0; i<nts; i++) {
      times[i] = part_mesh[p]->get_state_time(i+1);
    }

    if (nts > 0) {
      if (global_times.empty()) {
	std::copy(times.begin(), times.end(), std::back_inserter(global_times));
      } else {
	if (global_times.size() != times.size()) {
	  std::cerr << "Time step sizes must match.";
	  SMART_ASSERT(global_times.size() == times.size())(global_times.size())(times.size())(p);
	  exit(EXIT_FAILURE);
	}

	for (int i=0; i < global_times.size(); i++) {
	  if (!approx_equal(global_times[i], times[i])) {
	    std::cerr << "Time step " << i << " in part " << p+1
		      << " does not match time steps in previous part(s): previous: "
		      << global_times[i] << ", current: " << times[i] << "\n";
	    exit(EXIT_FAILURE);
	  }
	}
      }
    }
  }

  output_region.begin_mode(Ioss::STATE_TRANSIENT);
  for (size_t step=0; step < global_times.size(); step++) {
    int ostep = output_region.add_state(global_times[step]);
    output_region.begin_state(ostep);
    output_transient_state(output_region, part_mesh, global_times[step], local_node_map);
    output_region.end_state(ostep);
  }
  output_region.end_mode(Ioss::STATE_TRANSIENT);
  
  /*************************************************************************/
  // EXIT program
  if (debug_level & 1)
    std::cerr << time_stamp(tsFormat);
  put_mesh_summary(output_region);
  std::cerr << "******* END *******\n";
  return(0);
}

namespace {
  void transfer_elementblock(Ioss::Region &region, Ioss::Region &output_region, bool debug)
  {
    static int used_blocks = 0;
    std::string prefix = region.name();
    
    Ioss::ElementBlockContainer ebs = region.get_element_blocks();
    Ioss::ElementBlockContainer::const_iterator i = ebs.begin();
    int total_elements = 0;
    while (i != ebs.end()) {
      std::string name      = prefix + "_" + (*i)->name();
      if (output_region.get_element_block(name) != NULL) {
	std::cerr << "ERROR: Duplicate element blocks named '" << name << "'\n";
	exit(EXIT_FAILURE);
      }
      if (debug) std::cerr << name << ", ";
      std::string type      = (*i)->get_property("topology_type").get_string();
      int    num_elem  = (*i)->get_property("entity_count").get_int();
      int    num_attrib= (*i)->get_property("attribute_count").get_int();
      total_elements += num_elem;

      if (num_elem > 0) {
	Ioss::ElementBlock *eb = new Ioss::ElementBlock(output_region.get_database(), name, type,
							num_elem, num_attrib);
	eb->property_add(Ioss::Property("original_block_order", used_blocks++));
	output_region.add(eb);
	transfer_fields(*i, eb, Ioss::Field::ATTRIBUTE);
      }

      ++i;
    }
  }

  void transfer_facesets(Ioss::Region &region, Ioss::Region &output_region, bool debug)
  {
    std::string prefix = region.name();

    Ioss::FaceSetContainer      fss = region.get_facesets();
    Ioss::FaceSetContainer::const_iterator i = fss.begin();
    int total_faces = 0;
    while (i != fss.end()) {
      std::string name      = prefix + "_" + (*i)->name();
      if (output_region.get_faceset(name) != NULL) {
	std::cerr << "ERROR: Duplicate face sets named '" << name << "'\n";
	exit(EXIT_FAILURE);
      }
      if (debug) std::cerr << name << ", ";
      Ioss::FaceSet *surf = new Ioss::FaceSet(output_region.get_database(), name);

      Ioss::FaceBlockContainer fbs = (*i)->get_face_blocks();
      Ioss::FaceBlockContainer::const_iterator j = fbs.begin();
      while (j != fbs.end()) {
	std::string fbname    = prefix + "_" + (*j)->name();
	if (debug) std::cerr << fbname << ", ";
	std::string fbtype    = (*j)->get_property("topology_type").get_string();
	std::string partype   = (*j)->get_property("parent_topology_type").get_string();
	int    num_face  = (*j)->get_property("entity_count").get_int();
	total_faces += num_face;

	Ioss::FaceBlock *block = new Ioss::FaceBlock(output_region.get_database(), fbname, fbtype,
						     partype, num_face);
	surf->add(block);
	//	transfer_fields(*j, block, Ioss::Field::MESH);
	//	transfer_fields(*j, block, Ioss::Field::ATTRIBUTE);
	++j;
      }
      //      transfer_fields(*i, surf, Ioss::Field::MESH);
      //      transfer_fields(*i, surf, Ioss::Field::ATTRIBUTE);
      output_region.add(surf);
      ++i;
    }
  }

  void transfer_edgesets(Ioss::Region &region, Ioss::Region &output_region, bool debug)
  {
    std::string prefix = region.name();

    Ioss::EdgeSetContainer ess = region.get_edgesets();
    Ioss::EdgeSetContainer::const_iterator i = ess.begin();
    int total_edges = 0;
    while (i != ess.end()) {
      std::string name = prefix + "_" + (*i)->name();
      if (output_region.get_edgeset(name) != NULL) {
	std::cerr << "ERROR: Duplicate edge sets named '" << name << "'\n";
	exit(EXIT_FAILURE);
      }
      if (debug) std::cerr << name << ", ";
      Ioss::EdgeSet *surf = new Ioss::EdgeSet(output_region.get_database(), name);

      Ioss::EdgeBlockContainer ebs = (*i)->get_edge_blocks();
      Ioss::EdgeBlockContainer::const_iterator j = ebs.begin();
      while (j != ebs.end()) {
	std::string ebname = prefix + "_" + (*j)->name();
	if (debug) std::cerr << ebname << ", ";
	std::string ebtype    = (*j)->get_property("topology_type").get_string();
	std::string partype   = (*j)->get_property("parent_topology_type").get_string();
	int    num_edge  = (*j)->get_property("entity_count").get_int();
	total_edges += num_edge;

	Ioss::EdgeBlock *block = new Ioss::EdgeBlock(output_region.get_database(), ebname, ebtype,
						     partype, num_edge);
	surf->add(block);
	//	transfer_fields(*j, block, Ioss::Field::MESH);
	//	transfer_fields(*j, block, Ioss::Field::ATTRIBUTE);
	++j;
      }
      //      transfer_fields(*i, surf, Ioss::Field::MESH);
      //      transfer_fields(*i, surf, Ioss::Field::ATTRIBUTE);
      output_region.add(surf);
      ++i;
    }
  }

  void transfer_nodesets(Ioss::Region &region, Ioss::Region &output_region, bool debug)
  {
    std::string prefix = region.name();

    Ioss::NodeSetContainer      nss = region.get_nodesets();
    Ioss::NodeSetContainer::const_iterator i = nss.begin();
    while (i != nss.end()) {
      std::string name = prefix + "_" + (*i)->name();
      if (output_region.get_nodeset(name) != NULL) {
	std::cerr << "ERROR: Duplicate node sets named '" << name << "'\n";
	exit(EXIT_FAILURE);
      }
      if (debug) std::cerr << name << ", ";
      int    count     = (*i)->get_property("entity_count").get_int();
      Ioss::NodeSet *ns = new Ioss::NodeSet(output_region.get_database(), name, count);
      output_region.add(ns);
      //      transfer_fields(*i, ns, Ioss::Field::MESH);
      //      transfer_fields(*i, ns, Ioss::Field::ATTRIBUTE);
      ++i;
    }
  }

#if 0
  void build_reverse_element_map(std::vector<Mesh> &part_mesh,
				 std::vector<std::vector<Block> > &blocks,
				 std::vector<Block> &glob_blocks,
				 Mesh *global, size_t part_count,
				 GlobalElemMap &global_element_map)
  {
    int error = 0;
   
   // Global element map and count.

    size_t offset = 0;
    for (size_t p = 0; p < part_count; p++) {
      ExodusFile id(p);
      error += ex_get_elem_num_map(id, &global_element_numbers[p][0]);
      std::copy(global_element_numbers[p].begin(), global_element_numbers[p].end(),
		&global_element_map[offset]);
      offset += part_mesh[p].count(ELEM);
    }

    uniqify(global_element_map);
    global->count(ELEM) = global_element_map.size();

    // Needed in case we create new ids for duplicated elements.
    

    // We check whether the element map is contiguous, but it doesn't
    // really help in all cases since the congtiguous elements could
    // be spread haphazardly among element blocks.  (elements 1, 2, 4
    // could be in block 1; element 3, 5 in block 2). 
    
   // Create the map that maps from a local part element to the
    // global map. This combines the mapping local part element to
    // 'global id' and then 'global id' to global position. The
    // mapping is now a direct lookup instead of a lookup followed by
    // a reverse map.
    //

    // We iterate through each element block a part at a time to
    // determine which elements are in each block. Note that an
    // element will possibly exist in multiple parts, so we need to do
    // the sort/uniqify/shrink on each element block.  The operations
    // for each element block will be:
    // 1. get elements for the block for each part into a vector.
    // 2. sort/uniqify/shrink that vector and copy into the
    // 'global_element_map' in the positions following the previous block.
    // 
    // This is similar to what was done above in a global sense,
    // except it is done an element block at a time. The only need for
    // doing them both is that it lets us detect whether an element id
    // was reused and appears in both blocks.  Currently, just error
    // out if that happens...  In the future, change the id to an
    // unused value and continue....

    size_t tot_size = 0;
    std::vector<GlobalElemMap> global_element_numbers(part_count);
    for (size_t p = 0; p < part_count; p++) {
      ExodusFile id(p);
      global_element_numbers[p].resize(part_mesh[p].count(ELEM));
      IntVector ids(part_mesh[p].count(ELEM));
      error += ex_get_elem_num_map(id, &ids[0]);
      for (int i=0; i < part_mesh[p].count(ELEM); i++) {
	global_element_numbers[p][i] = std::make_pair(ids[i], size_t(0));
      }
      tot_size += part_mesh[p].count(ELEM);
    }
    global_element_map.resize(tot_size);

    size_t goffset = 0;
    for (size_t b = 0; b < glob_blocks.size(); b++) {
      size_t block_size = 0;
      for (size_t p = 0; p < part_count; p++) {
	block_size += blocks[p][b].entity_count();
      }
      GlobalElemMap block_element_map(block_size);
      
      size_t poffset = 0;
      for (size_t p = 0; p < part_count; p++) {
	ExodusFile id(p);

	// Get connectivity array for this element block...
	size_t element_count = blocks[p][b].entity_count();
	size_t nodes_per_elem = blocks[p][b].nodesPerElement;
	size_t maximum_nodes = element_count * nodes_per_elem;
	
	IntVector local_linkage(maximum_nodes);

	int bid = blocks[p][b].id;
	error = ex_get_conn(id, EX_ELEM_BLOCK, bid, &local_linkage[0], 0, 0);
	if (error < 0) {
	  std::cerr << "ERROR: Cannot get element block connectivity for block "
		    << bid << " on part " << p << ".\n";
	}

	// Have element global ids for all elements in this block,
	// and connectivity.  Can now create out "eleminfo" for these elements.
	size_t con_offset = 0;
	size_t boffset = blocks[p][b].offset_;
	for (int i=0; i < element_count; i++) {
	  size_t adler_crc = adler(0, &local_linkage[con_offset], nodes_per_elem * sizeof(int));
	  global_element_numbers[p][boffset+i].second = adler_crc;
	  block_element_map[poffset+i] = global_element_numbers[p][boffset+i];
	  con_offset += nodes_per_elem;
	}
	poffset += element_count;
      }

      // Sort, uniqify, shrink 'block_element_map' and the result
      // then contains the list of elements in this block...
      uniqify(block_element_map);
      
      size_t block_total_num_elements = block_element_map.size();
      glob_blocks[b].elementCount = block_total_num_elements;
      glob_blocks[b].offset_ = goffset;
      
      // Copy into the global_element_map...
      std::copy(block_element_map.begin(),
		block_element_map.end(),
		&global_element_map[goffset]);
      goffset += block_total_num_elements;
    }
      
    global->elementCount = goffset;
    global_element_map.resize(goffset);

    size_t max_id = global_element_map[global->elementCount-1].first;
    bool is_contiguous = max_id == (int)global_element_map.size();
    //std::cerr  << "Element id map " << (is_contiguous ? "is" : "is not") << " contiguous.\n";

    // The global_element_map may or may not be globally sorted; however, each
    // block is sorted, so if we do the iteration by blocks, we can
    // use equal_range instead of doing global searches...
    for (size_t b = 0; b < glob_blocks.size(); b++) {
      GElemMapIter gm_begin = global_element_map.begin() + glob_blocks[b].offset_;
      GElemMapIter gm_end   = gm_begin + glob_blocks[b].elementCount;
      GElemMapIter cur_pos  = gm_begin;
      for (size_t p = 0; p < part_count; p++) {
	size_t element_count = blocks[p][b].entity_count();
	size_t boffset = blocks[p][b].offset_;
	for (size_t i = 0; i < element_count; i++) {
	  std::pair<int, size_t> global_element = global_element_numbers[p][boffset+i];
	  
	  if (cur_pos == gm_end || *cur_pos != global_element) {
	    std::pair<GElemMapIter, GElemMapIter> iter =
	      std::equal_range(gm_begin, gm_end, global_element);
	    SMART_ASSERT(iter.first != iter.second);
	    cur_pos = iter.first;
	  }
	  size_t element_value = cur_pos - gm_begin;
	  part_mesh[p].localElementToGlobal[i+boffset] = element_value + glob_blocks[b].offset_;
	  ++cur_pos;
	}				
      }
    }

    // Update the element ids to give a unique, non-repeating set.  If
    // contiguous, then there is nothing to do.  If not contiguous,
    // then need to determine if there are any repeats (id reuse) and
    // if so, generate a new id for the repeated uses.  Note that
    // there is a possibility that elements in two or more element
    // blocks will have the same element id, so we generate a vector
    // containing a pair<id, position_in_global_element_map>, sort it,
    // and then use it to detect duplicates and map them to a new id.

    if (!is_contiguous) {
      std::vector<std::pair<int, int> > id_pos(global_element_map.size());
      for (int i=0; i < global->elementCount; i++) {
	id_pos[i].first  = global_element_map[i].first;
	id_pos[i].second = i;
      }
      std::sort(id_pos.begin(), id_pos.end());
      
      max_id = id_pos[id_pos.size()-1].first;
      // Check again for contiguous ids since we now have a sorted list...
      is_contiguous = max_id == (int)global_element_map.size();
      
      if (!is_contiguous) {
	bool repeat_found = false;
	int id_last = id_pos[0].first;
	SMART_ASSERT(id_last == global_element_map[id_pos[0].second].first);
	
	for (int i=1; i < global->elementCount; i++) {
	  if (id_pos[i].first == id_last) {
	    global_element_map[id_pos[i].second].first = ++max_id;
	    repeat_found = true;
	  } else {
	    id_last = id_pos[i].first;
	  }
	}
	if (repeat_found) {
	  std::cerr  << "Duplicate element ids were found. Their ids have been renumbered to remove duplicates.\n";
	}
      }
    }
  }
#endif
  void build_reverse_node_map(Ioss::Region &global,
			      RegionVector &part_mesh,
			      Map &global_node_map,
			      Map &local_node_map)
  {
    // Instead of using <set> and <map>, consider using a sorted vector...
    // Append all local node maps to the global node map.
    // Sort the global node map
    // Remove duplicates.
    // Position within map is now the map...
    // When building the local-part node to global id, use binary_search...

    size_t part_count = part_mesh.size();

    // Global node map and count.
    std::vector<std::vector<int> > global_nodes(part_count);

    size_t tot_size = 0;
    for (size_t p = 0; p < part_count; p++) {
      Ioss::NodeBlock *nb = part_mesh[p]->get_node_blocks()[0];
      int loc_size = nb->get_property("entity_count").get_int();
      tot_size += loc_size;
      global_nodes[p].resize(loc_size);
    }
    global_node_map.resize(tot_size);

    size_t offset = 0;
    for (size_t p = 0; p < part_count; p++) {
      Ioss::NodeBlock *nb = part_mesh[p]->get_node_blocks()[0];
      nb->get_field_data("ids", global_nodes[p]);
      std::copy(global_nodes[p].begin(), global_nodes[p].end(), &global_node_map[offset]);
      offset += global_nodes[p].size();
    }

    // Now, sort the global_node_map array and remove duplicates...
    uniqify(global_node_map);

    size_t output_node_count = global_node_map.size();

    // See whether the node numbers are contiguous.  If so, we can map
    // the nodes back to their original location. Since the nodes are
    // sorted and there are no duplicates, we just need to see if the id
    // at global_node_map.size() == global_node_map.size();
    int max_id = global_node_map[output_node_count-1];

    bool is_contiguous = max_id == output_node_count;
    std::cerr  << "Node map " << (is_contiguous ? "is" : "is not") << " contiguous.\n";

    // Create the map that maps from a local part node to the
    // global map. This combines the mapping local part node to
    // 'global id' and then 'global id' to global position. The
    // mapping is now a direct lookup instead of a lookup followed by
    // a reverse map.
    Map::iterator cur_pos = global_node_map.begin();
    for (size_t p = 0; p < part_count; p++) {
      int offset = part_mesh[p]->get_property("node_offset").get_int();
      size_t node_count = global_nodes[p].size();
      for (size_t i = 0; i < node_count; i++) {
	int global_node = global_nodes[p][i];

	if (cur_pos == global_node_map.end() || *cur_pos != global_node) {
	  std::pair<Map::iterator, Map::iterator> iter = std::equal_range(global_node_map.begin(),
									  global_node_map.end(),
									  global_node);
	  if (iter.first == iter.second) {
	    int n = global_node;
	    std::cerr << n << "\n";
	    SMART_ASSERT(iter.first != iter.second);
	  }
	  cur_pos = iter.first;
	}
	size_t nodal_value = cur_pos - global_node_map.begin();
	local_node_map[offset+i] = nodal_value;
	++cur_pos;
      }				
    }
    
    // Update the nodal ids to give a unique, non-repeating set.  If contiguous, then
    // there is nothing to do.  If not contiguous, then need to determine if there are any
    // repeats (id reuse) and if so, generate a new id for the repeated uses.
    if (!is_contiguous) {
      bool repeat_found = false;
      int id_last = global_node_map[0];
      for (int i=1; i < output_node_count; i++) {
	if (global_node_map[i] == id_last) {
	  global_node_map[i] = ++max_id;
	  repeat_found = true;
	} else {
	  id_last = global_node_map[i];
	}
      }
      if (repeat_found) {
	std::cerr  << "Duplicate node ids were found. Their ids have been renumbered to remove duplicates.\n";
      }
    }
  }

  void put_mesh_summary(const Ioss::Region& mesh)
  {
    // Write out Mesh info
    std::cout << " Number of coordinates per node       =" << std::setw(9)
	      << mesh.get_property("spatial_dimension").get_int() << "\n";
    std::cout << " Number of nodes                      =" << std::setw(9)
	      << mesh.get_property("node_count").get_int() << "\n";
    std::cout << " Number of elements                   =" << std::setw(9)
	      << mesh.get_property("element_count").get_int() << "\n";
    std::cout << " Number of element blocks             =" << std::setw(9)
	      << mesh.get_property("element_block_count").get_int() << "\n";
    std::cout << " Number of nodal point sets           =" << std::setw(9)
	      << mesh.get_property("node_set_count").get_int() << "\n";
    std::cout << " Number of element side sets          =" << std::setw(9)
	      << mesh.get_property("face_set_count").get_int() +
                 mesh.get_property("edge_set_count").get_int() << "\n\n";
  }

  std::string time_stamp(const std::string &format)
  {
    if (format == "") {
      return std::string("");
    } else {
      const int length=256;
      static char time_string[length];

      time_t calendar_time = time(NULL);
      struct tm *local_time = localtime(&calendar_time);

      int error = strftime(time_string, length, format.c_str(), local_time);
      if (error != 0) {
        time_string[length-1] = (char)NULL;
        return std::string(time_string);
      } else {
        return std::string("[ERROR]");
      }
    }
  }

  template <typename T>
  void map_element_vars(int loffset, int goffset, int entity_count, 
			std::vector<T> &values,
			std::vector<T> &global_values,
			int *part_loc_elem_to_global)
  {
    // copy values to master element value information
    T* local_values = &values[0];
    for (int j = 0; j < entity_count; j++) {
      int global_block_pos = part_loc_elem_to_global[(j + loffset)] - goffset;
      global_values[global_block_pos] = local_values[j];
    }
  }

  template <typename T>
  void map_sideset_vars(int loffset, int entity_count, 
			std::vector<T> &values,
			std::vector<T> &global_values)
  {
    // copy values to master sideset value information
    T* local_values = &values[0];
    for (int j = 0; j < entity_count; j++) {
      global_values[j+loffset] = local_values[j];
    }
  }

  template <typename T, typename U>
  void map_nodeset_vars(U &, int, int, std::vector<T> &, std::vector<T> &)
  {
    SMART_ASSERT(1==0 && "Internal Error!");
  }
}

namespace {
  void output_edgeset(Ioss::Region &output_region, RegionVector &part_mesh)
  {
    Ioss::EdgeSetContainer os = output_region.get_edgesets();
    Ioss::EdgeSetContainer::const_iterator I = os.begin();

    Ioss::EdgeBlockContainer out_eb;
    // Put all output edge blocks in the same list...
    while (I != os.end()) {
      Ioss::EdgeBlockContainer obs = (*I++)->get_edge_blocks();
      std::copy(obs.begin(), obs.end(), std::back_inserter(out_eb));
    }
    
    // Assuming (with checks) that the output edge blocks will be
    // iterated in same order as input edge blocks...
    Ioss::EdgeBlockContainer::const_iterator II = out_eb.begin();

    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      int element_offset = part_mesh[p]->get_property("element_offset").get_int();

      Ioss::EdgeSetContainer is = part_mesh[p]->get_edgesets();
      Ioss::EdgeSetContainer::const_iterator J = is.begin();
      while (J != is.end()) {
	Ioss::EdgeBlockContainer ebs = (*J)->get_edge_blocks();
	Ioss::EdgeBlockContainer::const_iterator JJ = ebs.begin();

	while (JJ != ebs.end()) {
	  SMART_ASSERT(part_mesh[p]->name() + "_" + (*JJ)->name() == (*II)->name());
	  SMART_ASSERT((*JJ)->get_property("entity_count").get_int() == (*II)->get_property("entity_count").get_int());
	  std::vector<int> elem_face_list;
	  (*JJ)->get_field_data("element_side", elem_face_list);

	  if (element_offset > 0) {
	    for (size_t i=0; i < elem_face_list.size(); i+=2) { // just get the elem part of the pair...
	      elem_face_list[i] += element_offset;
	    }
	  }
	  (*II)->put_field_data("element_side", elem_face_list);
	  ++JJ; ++II;
	}
	++J;
      }
    }
  }

  void output_nodeblock(Ioss::Region &output_region, RegionVector &part_mesh, const Map &local_node_map,
			Map &global_node_map)
  {
    Ioss::NodeBlock *onb = output_region.get_node_blocks()[0];
    SMART_ASSERT(onb != NULL);

    onb->put_field_data("ids", global_node_map);

    int spatial_dimension = output_region.get_property("spatial_dimension").get_int();
    std::vector<double> coord(global_node_map.size()*spatial_dimension);
    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::NodeBlock *nb = part_mesh[p]->get_node_blocks()[0];
      SMART_ASSERT(nb != NULL);
      std::vector<double> coordinates;
      nb->get_field_data("mesh_model_coordinates", coordinates);
      int node_count = nb->get_property("entity_count").get_int();
      int offset = part_mesh[p]->get_property("node_offset").get_int();
      for (size_t i=0; i < node_count; i++) {
	int glob_pos = local_node_map[i+offset];
	coord[glob_pos*spatial_dimension + 0] = coordinates[i*spatial_dimension + 0];
	coord[glob_pos*spatial_dimension + 1] = coordinates[i*spatial_dimension + 1];
	coord[glob_pos*spatial_dimension + 2] = coordinates[i*spatial_dimension + 2];
      }
    }
    onb->put_field_data("mesh_model_coordinates", coord);
  }

  void output_elementblock(Ioss::Region &output_region, RegionVector &part_mesh,
			   const Map &local_node_map)
  {
    Ioss::ElementBlockContainer ebs = output_region.get_element_blocks();

    size_t block_count = ebs.size();
    int element_count = output_region.get_property("element_count").get_int();
    std::vector<int> ids(element_count);
    for (int i=0; i < element_count; i++) {
      ids[i] = i+1;
    }

    int element_offset = 0;
    Ioss::ElementBlockContainer::const_iterator I = ebs.begin();
    while (I != ebs.end()) {
      (*I)->put_field_data("ids", &ids[element_offset], ids.size()*sizeof(int));
      element_offset += (*I)->get_property("entity_count").get_int();
      ++I;
    }

    SMART_ASSERT(element_offset == element_count);

    // Connectivity...
    I = ebs.begin();
    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::ElementBlockContainer iebs = part_mesh[p]->get_element_blocks();
      Ioss::ElementBlockContainer::const_iterator J = iebs.begin();
      int node_offset = part_mesh[p]->get_property("node_offset").get_int();
      
      while (J != iebs.end()) {
	Ioss::ElementBlock *ieb = *J;
	std::string name = part_mesh[p]->name() + "_" + ieb->name();
	Ioss::ElementBlock *oeb = output_region.get_element_block(name);
	if (oeb != NULL) {
	  std::vector<int> connectivity;
	  ieb->get_field_data("connectivity", connectivity);
	
	  SMART_ASSERT(ieb->get_property("entity_count").get_int() == oeb->get_property("entity_count").get_int());
	  for (size_t i=0; i < connectivity.size(); i++) {

	    // connectivity is in part-global node ids.
	    // loc_node = the position of part-global node 'connectivity[i]' in the local [0..num_node)
	    // local_node_map[node_offset+loc_node] gives the position of this node in the global list
	    int loc_node = part_mesh[p]->node_global_to_local(connectivity[i], true)-1;
	    SMART_ASSERT(node_offset+loc_node < local_node_map.size());
	    connectivity[i] = local_node_map[node_offset+loc_node]+1;
	  }
	  oeb->put_field_data("connectivity", connectivity);
	  transfer_field_data(ieb, oeb, Ioss::Field::ATTRIBUTE);
	}
	++J;
      }
    }
  }

  void output_nodeset(Ioss::Region &output_region, RegionVector &part_mesh, const Map &local_node_map)
  {
    Ioss::NodeSetContainer ons = output_region.get_nodesets();
    Ioss::NodeSetContainer::const_iterator I = ons.begin();
    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      int node_offset = part_mesh[p]->get_property("node_offset").get_int();
      Ioss::NodeSetContainer ins = part_mesh[p]->get_nodesets();
      Ioss::NodeSetContainer::const_iterator J = ins.begin();
      while (J != ins.end()) {
	std::vector<int> nodelist;
	(*J)->get_field_data("ids", nodelist);
	
	SMART_ASSERT(part_mesh[p]->name() + "_" + (*J)->name() == (*I)->name())((*J)->name())((*I)->name());
	SMART_ASSERT((*J)->get_property("entity_count").get_int() == (*I)->get_property("entity_count").get_int());
	// This needs to make sure that the nodelist comes back as local id (1..numnodes)
	for (size_t i=0; i < nodelist.size(); i++) {
	  int loc_node = part_mesh[p]->node_global_to_local(nodelist[i], true)-1;
	  nodelist[i] = local_node_map[node_offset+loc_node]+1;
	}
	(*I)->put_field_data("ids", nodelist);
	++J; ++I;
      }
    }
  }
  
  void output_faceset(Ioss::Region &output_region, RegionVector &part_mesh)
  {
    Ioss::FaceSetContainer os = output_region.get_facesets();
    Ioss::FaceSetContainer::const_iterator I = os.begin();

    Ioss::FaceBlockContainer out_eb;
    // Put all output face blocks in the same list...
    while (I != os.end()) {
      Ioss::FaceBlockContainer obs = (*I++)->get_face_blocks();
      std::copy(obs.begin(), obs.end(), std::back_inserter(out_eb));
    }
    
    // Assuming (with checks) that the output face blocks will be
    // iterated in same order as input face blocks...
    Ioss::FaceBlockContainer::const_iterator II = out_eb.begin();

    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      int element_offset = part_mesh[p]->get_property("element_offset").get_int();

      Ioss::FaceSetContainer is = part_mesh[p]->get_facesets();
      Ioss::FaceSetContainer::const_iterator J = is.begin();
      while (J != is.end()) {
	Ioss::FaceBlockContainer ebs = (*J)->get_face_blocks();
	Ioss::FaceBlockContainer::const_iterator JJ = ebs.begin();

	while (JJ != ebs.end()) {
	  SMART_ASSERT(part_mesh[p]->name() + "_" + (*JJ)->name() == (*II)->name())((*JJ)->name())((*II)->name());
	  SMART_ASSERT((*JJ)->get_property("entity_count").get_int() == (*II)->get_property("entity_count").get_int());
	  std::vector<int> elem_face_list;
	  (*JJ)->get_field_data("element_side", elem_face_list);

	  if (element_offset > 0) {
	    for (size_t i=0; i < elem_face_list.size(); i+=2) { // just get the elem part of the pair...
	      elem_face_list[i] += element_offset;
	    }
	  }
	  (*II)->put_field_data("element_side", elem_face_list);
	  ++JJ; ++II;
	}
	++J;
      }
    }
  }

  void output_globals(Ioss::Region &output_region, RegionVector &part_mesh,
		      double time, const IntVector &steps)
  {
    size_t part_count = part_mesh.size();
    for (size_t p=0; p<part_count; p++) {
      Ioss::NameList fields;
      part_mesh[p]->field_describe(Ioss::Field::TRANSIENT, &fields);
      Ioss::NameList::const_iterator IF;
      for (IF = fields.begin(); IF != fields.end(); ++IF) {
	std::vector<double> data;
	part_mesh[p]->get_field_data(*IF, data);
	output_region.put_field_data(*IF, data);
      }
    }
  }

  void output_nodal(Ioss::Region &output_region, RegionVector &part_mesh,
		    double time, const IntVector &steps, const Map &local_node_map)
  {
    size_t part_count = part_mesh.size();

    Ioss::NodeBlock *onb = output_region.get_node_blocks()[0];
    SMART_ASSERT(onb != NULL);
    int node_count = onb->get_property("entity_count").get_int();

    Ioss::NameList fields;
    onb->field_describe(Ioss::Field::TRANSIENT, &fields);
    Ioss::NameList::const_iterator IF;
    for (IF = fields.begin(); IF != fields.end(); ++IF) {
      int comp_count = onb->get_field(*IF).raw_storage()->component_count();
      std::vector<double> data(node_count*comp_count);
      for (size_t p=0; p<part_count; p++) {
	int offset = part_mesh[p]->get_property("node_offset").get_int();
	Ioss::NodeBlock *nb = part_mesh[p]->get_node_blocks()[0];
	SMART_ASSERT(nb != NULL);
	if (nb->field_exists(*IF)) {
	  SMART_ASSERT(comp_count == nb->get_field(*IF).raw_storage()->component_count());
	  std::vector<double> loc_data;
	  nb->get_field_data(*IF, loc_data);
	  int nc = nb->get_property("entity_count").get_int();
	  SMART_ASSERT(loc_data.size() == nc * comp_count);
	  for (size_t i=0; i < nc; i++) {
	    int glob_pos = local_node_map[offset+i];
	    for (size_t j=0; j<comp_count; j++) {
	      data[glob_pos*comp_count+j] = loc_data[i*comp_count+j];
	    }
	  }
	}
      }
      onb->put_field_data(*IF, data);
    }
  }

  void output_element(Ioss::Region &output_region, RegionVector &part_mesh,
		      double time, const IntVector &steps)
  {
    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::ElementBlockContainer iebs = part_mesh[p]->get_element_blocks();
      Ioss::ElementBlockContainer::const_iterator J = iebs.begin();
      while (J != iebs.end()) {
	Ioss::ElementBlock *ieb = *J;
	std::string name = part_mesh[p]->name() + "_" + ieb->name();

	Ioss::ElementBlock *oeb = output_region.get_element_block(name);
	if (oeb != NULL) {
	  Ioss::NameList fields;
	  ieb->field_describe(Ioss::Field::TRANSIENT, &fields);
	  Ioss::NameList::const_iterator IF;
	  for (IF = fields.begin(); IF != fields.end(); ++IF) {
	    if (oeb->field_exists(*IF)) {
	      transfer_field_data_internal(ieb, oeb, *IF);
	    }
	  }
	}
	++J;
      }
    }
  }

  void output_nset(Ioss::Region &output_region, RegionVector &part_mesh,
		   double time, const IntVector &steps)
  {
    Ioss::NodeSetContainer ons = output_region.get_nodesets();
    if (ons.empty())
      return;
    Ioss::NodeSetContainer::const_iterator I = ons.begin();

    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::NodeSetContainer ins = part_mesh[p]->get_nodesets();
      Ioss::NodeSetContainer::const_iterator J = ins.begin();
      while (J != ins.end()) {
	SMART_ASSERT(part_mesh[p]->name() + "_" + (*J)->name() == (*I)->name());
	Ioss::NameList fields;
	(*J)->field_describe(Ioss::Field::TRANSIENT, &fields);
	Ioss::NameList::const_iterator IF;
	for (IF = fields.begin(); IF != fields.end(); ++IF) {
	  if ((*I)->field_exists(*IF)) {
	    transfer_field_data_internal(*J, *I, *IF);
	  }
	}
	++J; ++I;
      }
    }
  }

  void output_eset(Ioss::Region &output_region, RegionVector &part_mesh,
		   double time, const IntVector &steps)
  {
    Ioss::EdgeSetContainer os = output_region.get_edgesets();
    if (os.empty())
      return;
    Ioss::EdgeSetContainer::const_iterator I = os.begin();

    Ioss::EdgeBlockContainer out_eb;
    // Put all output edge blocks in the same list...
    while (I != os.end()) {
      Ioss::EdgeBlockContainer obs = (*I++)->get_edge_blocks();
      std::copy(obs.begin(), obs.end(), std::back_inserter(out_eb));
    }
    
    // Assuming (with checks) that the output edge blocks will be
    // iterated in same order as input edge blocks...
    Ioss::EdgeBlockContainer::const_iterator II = out_eb.begin();

    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::EdgeSetContainer is = part_mesh[p]->get_edgesets();
      Ioss::EdgeSetContainer::const_iterator J = is.begin();
      while (J != is.end()) {
	Ioss::EdgeBlockContainer ebs = (*J)->get_edge_blocks();
	Ioss::EdgeBlockContainer::const_iterator JJ = ebs.begin();

	while (JJ != ebs.end()) {
	  SMART_ASSERT(part_mesh[p]->name() + "_" + (*JJ)->name() == (*II)->name());
	  Ioss::NameList fields;
	  (*JJ)->field_describe(Ioss::Field::TRANSIENT, &fields);
	  Ioss::NameList::const_iterator IF;
	  for (IF = fields.begin(); IF != fields.end(); ++IF) {
	    if ((*II)->field_exists(*IF)) {
	      transfer_field_data_internal(*JJ, *II, *IF);
	    }
	  }
	  ++JJ; ++II;
	}
	++J;
      }
    }
  }

  void output_fset(Ioss::Region &output_region, RegionVector &part_mesh,
		   double time, const IntVector &steps)
  {
    Ioss::FaceSetContainer os = output_region.get_facesets();
    if (os.empty())
      return;
    Ioss::FaceSetContainer::const_iterator I = os.begin();

    Ioss::FaceBlockContainer out_eb;
    // Put all output face blocks in the same list...
    while (I != os.end()) {
      Ioss::FaceBlockContainer obs = (*I++)->get_face_blocks();
      std::copy(obs.begin(), obs.end(), std::back_inserter(out_eb));
    }
    
    // Assuming (with checks) that the output face blocks will be
    // iterated in same order as input face blocks...
    Ioss::FaceBlockContainer::const_iterator II = out_eb.begin();

    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::FaceSetContainer is = part_mesh[p]->get_facesets();
      Ioss::FaceSetContainer::const_iterator J = is.begin();
      while (J != is.end()) {
	Ioss::FaceBlockContainer ebs = (*J)->get_face_blocks();
	Ioss::FaceBlockContainer::const_iterator JJ = ebs.begin();

	while (JJ != ebs.end()) {
	  SMART_ASSERT(part_mesh[p]->name() + "_" + (*JJ)->name() == (*II)->name());
	  Ioss::NameList fields;
	  (*JJ)->field_describe(Ioss::Field::TRANSIENT, &fields);
	  Ioss::NameList::const_iterator IF;
	  for (IF = fields.begin(); IF != fields.end(); ++IF) {
	    if ((*II)->field_exists(*IF)) {
	      transfer_field_data_internal(*JJ, *II, *IF);
	    }
	  }
	  ++JJ; ++II;
	}
	++J;
      }
    }
  }

  void output_transient_state(Ioss::Region &output_region, RegionVector &part_mesh, double time,
			      const Map &local_node_map)
  {
    // Determine which state on each input mesh corresponds to 'time'
    std::vector<int> steps(part_mesh.size());
    for (size_t p=0; p < part_mesh.size(); p++) {
      size_t nts = part_mesh[p]->get_property("state_count").get_int();
      steps[p] = 0;
      for (size_t i=0; i < nts; i++) {
	if (approx_equal(part_mesh[p]->get_state_time(i+1), time)) {
	  steps[p] = i+1;
	  part_mesh[p]->begin_state(steps[p]);
	  break;
	}
      }
    }

    output_globals(output_region, part_mesh, time, steps);
    output_nodal(output_region, part_mesh, time, steps, local_node_map);
    output_element(output_region, part_mesh, time, steps);
    output_nset(output_region, part_mesh, time, steps);
    output_eset(output_region, part_mesh, time, steps);
    output_fset(output_region, part_mesh, time, steps);

    for (size_t p=0; p < part_mesh.size(); p++) {
      if (steps[p] != 0)
	part_mesh[p]->end_state(steps[p]);
    }
  }

  void transfer_field_data(Ioss::GroupingEntity *ige,
                           Ioss::GroupingEntity *oge,
                           Ioss::Field::RoleType role,
                           const std::string &prefix,
                           bool transfer_connectivity)
  {
    // Iterate through the TRANSIENT-role fields of the input
    // database and transfer to output database.
    Ioss::NameList state_fields;
    Ioss::NameList::const_iterator IF;
    ige->field_describe(role, &state_fields);

    // Complication here is that if the 'role' is 'Ioss::Field::MESH',
    // then the 'ids' field must be transferred first...
    if (role == Ioss::Field::MESH) {
      for (IF = state_fields.begin(); IF != state_fields.end(); ++IF) {
        std::string field_name = *IF;
        assert(oge->field_exists(field_name));
        if (field_name == "ids") {
          transfer_field_data_internal(ige, oge, field_name);
          break;
        }
      }
    }

    for (IF = state_fields.begin(); IF != state_fields.end(); ++IF) {
      std::string field_name = *IF;
      // All of the 'Ioss::EntityBlock' derived classes have a
      // 'connectivity' field, but it is only interesting on the
      // Ioss::ElementBlock class. On the other classes, it just
      // generates overhead...
      if (!transfer_connectivity && field_name == "connectivity")
        continue;


      if (field_name != "ids" &&
          (prefix.length() == 0 || std::strncmp(prefix.c_str(), field_name.c_str(), prefix.length()) == 0)) {
        assert(oge->field_exists(field_name));
        transfer_field_data_internal(ige, oge, field_name);
      }
    }
  }

  void transfer_field_data_internal(Ioss::GroupingEntity *ige,
				    Ioss::GroupingEntity *oge,
				    const std::string &field_name)
  {

    size_t isize = ige->get_field(field_name).get_size();
    assert (isize == oge->get_field(field_name).get_size());

    std::vector<double> data;
    ige->get_field_data(field_name, data);
    oge->put_field_data(field_name, data);
  }

  void define_global_fields(Ioss::Region &output_region, RegionVector &part_mesh)
  {
    size_t part_count = part_mesh.size();
    for (int p=0; p<part_count; p++) {
      Ioss::NameList fields;
      part_mesh[p]->field_describe(Ioss::Field::TRANSIENT, &fields);
      Ioss::NameList::const_iterator IF;
      for (IF = fields.begin(); IF != fields.end(); ++IF) {
	Ioss::Field field = part_mesh[p]->get_field(*IF);
	output_region.field_add(field);
      }
    }
  }

  void define_nodal_fields(Ioss::Region &output_region, RegionVector &part_mesh)
  {
    Ioss::NodeBlock *onb = output_region.get_node_blocks()[0];
    SMART_ASSERT(onb != NULL);
    int node_count = onb->get_property("entity_count").get_int();
    size_t part_count = part_mesh.size();
    for (int p=0; p<part_count; p++) {
      Ioss::NodeBlock *nb = part_mesh[p]->get_node_blocks()[0];
      Ioss::NameList fields;
      SMART_ASSERT(nb != NULL);
      nb->field_describe(Ioss::Field::TRANSIENT, &fields);
      Ioss::NameList::const_iterator IF;
      for (IF = fields.begin(); IF != fields.end(); ++IF) {
	Ioss::Field field = nb->get_field(*IF);
	field.reset_count(node_count);
	onb->field_add(field);
      }
    }
  }

  void define_element_fields(Ioss::Region &output_region, RegionVector &part_mesh)
  {
    // Element Block Fields...
    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::ElementBlockContainer iebs = part_mesh[p]->get_element_blocks();
      Ioss::ElementBlockContainer::const_iterator J = iebs.begin();
      while (J != iebs.end()) {
	std::string name = part_mesh[p]->name() + "_" + (*J)->name();
	Ioss::ElementBlock *oeb = output_region.get_element_block(name);
	if (oeb != NULL) {
	  Ioss::NameList fields;
	  (*J)->field_describe(Ioss::Field::TRANSIENT, &fields);
	  Ioss::NameList::const_iterator IF;
	  for (IF = fields.begin(); IF != fields.end(); ++IF) {
	    Ioss::Field field = (*J)->get_field(*IF);
	    oeb->field_add(field);
	  }
	}
	++J;
      }
    }
  }

  void define_nset_fields(Ioss::Region &output_region, RegionVector &part_mesh)
  {
    // Nodeset fields...
    Ioss::NodeSetContainer ons = output_region.get_nodesets();
    Ioss::NodeSetContainer::const_iterator I = ons.begin();
    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::NodeSetContainer ins = part_mesh[p]->get_nodesets();
      Ioss::NodeSetContainer::const_iterator J = ins.begin();
      while (J != ins.end()) {
	SMART_ASSERT(part_mesh[p]->name() + "_" + (*J)->name() == (*I)->name());
	Ioss::NameList fields;
	(*J)->field_describe(Ioss::Field::TRANSIENT, &fields);
	Ioss::NameList::const_iterator IF;
	for (IF = fields.begin(); IF != fields.end(); ++IF) {
	  Ioss::Field field = (*J)->get_field(*IF);
	  (*I)->field_add(field);
	}
	++J; ++I;
      }
    }
  }

  void define_eset_fields(Ioss::Region &output_region, RegionVector &part_mesh)
  {
    Ioss::EdgeSetContainer os = output_region.get_edgesets();
    Ioss::EdgeSetContainer::const_iterator I = os.begin();

    Ioss::EdgeBlockContainer out_eb;
    // Put all output edge blocks in the same list...
    while (I != os.end()) {
      Ioss::EdgeBlockContainer obs = (*I++)->get_edge_blocks();
      std::copy(obs.begin(), obs.end(), std::back_inserter(out_eb));
    }
    
    // Assuming (with checks) that the output edge blocks will be
    // iterated in same order as input edge blocks...
    Ioss::EdgeBlockContainer::const_iterator II = out_eb.begin();

    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::EdgeSetContainer is = part_mesh[p]->get_edgesets();
      Ioss::EdgeSetContainer::const_iterator J = is.begin();
      while (J != is.end()) {
	Ioss::EdgeBlockContainer ebs = (*J)->get_edge_blocks();
	Ioss::EdgeBlockContainer::const_iterator JJ = ebs.begin();

	while (JJ != ebs.end()) {
	  SMART_ASSERT(part_mesh[p]->name() + "_" + (*JJ)->name() == (*II)->name());
	  Ioss::NameList fields;
	  (*JJ)->field_describe(Ioss::Field::TRANSIENT, &fields);
	  Ioss::NameList::const_iterator IF;
	  for (IF = fields.begin(); IF != fields.end(); ++IF) {
	    Ioss::Field field = (*JJ)->get_field(*IF);
	    (*II)->field_add(field);
	  }
	  ++JJ; ++II;
	}
	++J;
      }
    }
  }

  void define_fset_fields(Ioss::Region &output_region, RegionVector &part_mesh)
  {
    Ioss::FaceSetContainer os = output_region.get_facesets();
    Ioss::FaceSetContainer::const_iterator I = os.begin();

    Ioss::FaceBlockContainer out_eb;
    // Put all output face blocks in the same list...
    while (I != os.end()) {
      Ioss::FaceBlockContainer obs = (*I++)->get_face_blocks();
      std::copy(obs.begin(), obs.end(), std::back_inserter(out_eb));
    }
    
    // Assuming (with checks) that the output face blocks will be
    // iterated in same order as input face blocks...
    Ioss::FaceBlockContainer::const_iterator II = out_eb.begin();

    size_t part_count = part_mesh.size();
    for (size_t p = 0; p < part_count; p++) {
      Ioss::FaceSetContainer is = part_mesh[p]->get_facesets();
      Ioss::FaceSetContainer::const_iterator J = is.begin();
      while (J != is.end()) {
	Ioss::FaceBlockContainer ebs = (*J)->get_face_blocks();
	Ioss::FaceBlockContainer::const_iterator JJ = ebs.begin();

	while (JJ != ebs.end()) {
	  SMART_ASSERT(part_mesh[p]->name() + "_" + (*JJ)->name() == (*II)->name());
	  Ioss::NameList fields;
	  (*JJ)->field_describe(Ioss::Field::TRANSIENT, &fields);
	  Ioss::NameList::const_iterator IF;
	  for (IF = fields.begin(); IF != fields.end(); ++IF) {
	    Ioss::Field field = (*JJ)->get_field(*IF);
	    (*II)->field_add(field);
	  }
	  ++JJ; ++II;
	}
	++J;
      }
    }
  }

   void transfer_fields(Ioss::GroupingEntity *ige,
                       Ioss::GroupingEntity *oge,
                       Ioss::Field::RoleType role,
                       const std::string &prefix)
  {
    // Check for transient fields...
    Ioss::NameList fields;
    ige->field_describe(role, &fields);

    // Iterate through results fields and transfer to output
    // database...  If a prefix is specified, only transfer fields
    // whose names begin with the prefix
    Ioss::NameList::const_iterator IF;
    for (IF = fields.begin(); IF != fields.end(); ++IF) {
      std::string field_name = *IF;
      if (field_name != "ids" && !oge->field_exists(field_name) &&
          (prefix.length() == 0 || std::strncmp(prefix.c_str(), field_name.c_str(), prefix.length()) == 0)) {
        // If the field does not already exist, add it to the output node block
        Ioss::Field field = ige->get_field(field_name);
        oge->field_add(field);
      }
    }
  }
}
