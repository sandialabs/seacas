// CGNS Assumptions:
// * All boundary conditions are listed as Family nodes at the "top" level.
// * Unstructured mesh only
// * Single element block per zone.
// * Serial for now.
// * Single Base.
// * ZoneGridConnectivity is 1to1 with point lists

// Copyright(C) 2015
// Sandia Corporation. Under the terms of Contract
// DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
// certain rights in this software.
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

#include <Ioss_CodeTypes.h>
#include <Ioss_Utils.h>
#include <assert.h>
#include <cgns/Iocgns_ParallelDatabaseIO.h>
#include <stddef.h>
#include <sys/select.h>
#include <time.h>
#include <numeric>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cgnslib.h>

#if !defined(CGNSLIB_H)
#error "Could not include cgnslib.h"
#endif

#include "Ioss_DBUsage.h"
#include "Ioss_DatabaseIO.h"
#include "Ioss_EntityType.h"
#include "Ioss_NodeBlock.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_SideBlock.h"
#include "Ioss_SideSet.h"

#include "Ioss_Field.h"
#include "Ioss_IOFactory.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"
#include "Ioss_Region.h"
#include "Ioss_State.h"
#include "Ioss_Utils.h"
#include "Ioss_VariableType.h"

namespace {
  const char *Version() {return "Iocgns_ParallelDatabaseIO.C 2016/01/28";}

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

  template <typename INT>
  void map_cgns_face_to_ioss(const Ioss::ElementTopology *parent_topo, size_t num_to_get, INT *idata)
  {
    // The {topo}_map[] arrays map from CGNS face# to IOSS face#.
    // See http://cgns.github.io/CGNS_docs_current/sids/conv.html#unstructgrid
    // NOTE: '0' for first entry is to account for 1-based face numbering.

    switch (parent_topo->shape())
      {
      case Ioss::ElementShape::HEX:
	static int hex_map[] = {0, 5, 1, 2, 3, 4, 6};
	for (size_t i=0; i < num_to_get; i++) {
	  idata[2*i+1] = hex_map[idata[2*i+1]];
	}
	break;

      case Ioss::ElementShape::TET:
	static int tet_map[] = {0, 4, 1, 2, 3};
	for (size_t i=0; i < num_to_get; i++) {
	  idata[2*i+1] = tet_map[idata[2*i+1]];
	}
	break;

      case Ioss::ElementShape::PYRAMID:
	static int pyr_map[] = {0, 5, 1, 2, 3, 4};
	for (size_t i=0; i < num_to_get; i++) {
	  idata[2*i+1] = pyr_map[idata[2*i+1]];
	}
	break;

      case Ioss::ElementShape::WEDGE:
#if 0
	static int wed_map[] = {0, 1, 2, 3, 4, 5}; // Same
	// Not needed -- maps 1 to 1
	for (size_t i=0; i < num_to_get; i++) {
	  idata[2*i+1] = wed_map[idata[2*i+1]];
	}
#endif
	break;
      default:
	;
      }
  }

  std::string map_cgns_to_topology_type(CG_ElementType_t type)
  {
    std::string topology = "unknown";
    switch (type)
      {
      case CG_NODE:
	topology = "tetra4"; break;
      case CG_BAR_2:
	topology = "bar2"; break;
      case CG_BAR_3:
	topology = "bar3"; break;
      case CG_TRI_3:
	topology = "tri3"; break;
      case CG_TRI_6:
	topology = "tri6"; break;
      case CG_QUAD_4:
	topology = "quad4"; break;
      case CG_QUAD_8:
	topology = "quad8"; break;
      case CG_QUAD_9:
	topology = "quad9"; break;
      case CG_TETRA_4:
	topology = "tetra4"; break;
      case CG_TETRA_10:
	topology = "tetra10"; break;
      case CG_PYRA_5:
	topology = "pyramid5"; break;
      case CG_PYRA_13:
	topology = "pyramid13"; break;
      case CG_PYRA_14:
	topology = "pyramid14"; break;
      case CG_PENTA_6:
	topology = "wedge6"; break;
      case CG_PENTA_15:
	topology = "wedge15"; break;
      case CG_PENTA_18:
	topology = "wedge18"; break;
      case CG_HEXA_8:
	topology = "hex8"; break;
      case CG_HEXA_20:
	topology = "hex20"; break;
      case CG_HEXA_27:
	topology = "hex27"; break;
      default:
	std::cerr << "WARNING: Found topology of type "
		  << cg_ElementTypeName(type)
		  << " which is not currently supported.\n";
	topology = "unknown";
      }
    return topology;
  }
}
namespace Iocgns {

  ParallelDatabaseIO::ParallelDatabaseIO(Ioss::Region *region, const std::string& filename,
			 Ioss::DatabaseUsage db_usage,
			 MPI_Comm communicator,
			 const Ioss::PropertyManager &props) :
    Ioss::DatabaseIO(region, filename, db_usage, communicator, props),
    cgnsFilePtr(-1), nodeCount(0), elementCount(0)
  {
    dbState = Ioss::STATE_UNKNOWN;

    if (!is_input()) {
      std::ostringstream errmsg;
      errmsg << "ERROR: IOSS CGNS Currently only supports reading, not writing.\n";
      IOSS_ERROR(errmsg);
    }

    std::cout << "CGNS ParallelDatabaseIO using " << CG_SIZEOF_SIZE << "-bit integers.\n";
    if (CG_SIZEOF_SIZE == 64) {
      set_int_byte_size_api(Ioss::USE_INT64_API);
    }

    openDatabase();
  }

  void ParallelDatabaseIO::openDatabase() const
  {
    if (cgnsFilePtr < 0) {
      if (is_input()) {
        int ierr = cg_open(get_filename().c_str(), CG_MODE_READ, &cgnsFilePtr);
	if (ierr != CG_OK) {
	  // NOTE: Code will not continue past this call...
	  std::ostringstream errmsg;
	  errmsg << "ERROR: Problem opening file '" << get_filename() << "' for read access.";
	  IOSS_ERROR(errmsg);
	}
      }
    }
    assert(cgnsFilePtr >= 0);
  }

  void ParallelDatabaseIO::closeDatabase() const
  {
    if (cgnsFilePtr != -1) {
      cg_close(cgnsFilePtr);
    }
    cgnsFilePtr = -1;
  }

  int64_t ParallelDatabaseIO::node_global_to_local(int64_t global, bool must_exist) const
  {
    return global;
  }

  int64_t ParallelDatabaseIO::element_global_to_local(int64_t global) const
  {
    return global;
  }

  void ParallelDatabaseIO::read_meta_data()
  {
    openDatabase();
    
    // Determine the number of bases in the grid.
    // Currently only handle 1.
    cgsize_t n_bases = 0;
    cg_nbases(cgnsFilePtr, &n_bases);
    if (n_bases != 1) {
      std::ostringstream errmsg;
      errmsg << "CGNS: Too many bases; only support files with a single bases at this time";
      IOSS_ERROR(errmsg);
    }

    if (int_byte_size_api() == 8) {
      decomp = std::unique_ptr<DecompositionDataBase>(new DecompositionData<int64_t>(properties, util().communicator()));
    } else {
      decomp = std::unique_ptr<DecompositionDataBase>(new DecompositionData<int>(properties, util().communicator()));
    }
    assert(decomp != nullptr);
    decomp->decompose_model(cgnsFilePtr);

    nodeCount = decomp->ioss_node_count();
    elementCount = decomp->ioss_elem_count();

    // ========================================================================
    // Get the number of families in the mesh...
    // Will treat these as sidesets if they are of the type "FamilyBC_t"
    cgsize_t base = 1;
    cgsize_t num_families = 0;
    cg_nfamilies(cgnsFilePtr, base, &num_families);
    for (cgsize_t family=1; family <= num_families; family++) {
      char name[33];
      cgsize_t num_bc = 0;
      cgsize_t num_geo = 0;
      cg_family_read(cgnsFilePtr, base, family, name, &num_bc, &num_geo);
      std::cout << "Family " << family << " named " << name
		<< " has " << num_bc << " BC, and "
		<< num_geo << " geometry references\n";
      if (num_bc > 0) {
	// Create a sideset...
	std::string ss_name(name);
	Ioss::SideSet *ss = new Ioss::SideSet(this, ss_name);
	get_region()->add(ss);
      }
    }

    // ========================================================================
    // Get the number of zones (element blocks) in the mesh...
    int i = 0;
    for (auto &block : decomp->el_blocks) {
      std::string element_topo = map_cgns_to_topology_type(block.topologyType);
      std::cout << "Added block " << block.name()
		<< ": CGNS topology = '" << cg_ElementTypeName(block.topologyType)
		<< "', IOSS topology = '" << element_topo
		<< "' with " << block.ioss_count() << " elements\n";

      auto *eblock = new Ioss::ElementBlock(this, block.name(), element_topo, block.ioss_count());
      eblock->property_add(Ioss::Property("base", base));
      eblock->property_add(Ioss::Property("zone", block.zone()));
      eblock->property_add(Ioss::Property("section", block.section()));
      eblock->property_add(Ioss::Property("original_block_order", i++));
      get_region()->add(eblock);
    }

    Ioss::NodeBlock *nblock = new Ioss::NodeBlock(this, "nodeblock_1", decomp->ioss_node_count(), 3);
    nblock->property_add(Ioss::Property("base", base));
    get_region()->add(nblock);
  }

  bool ParallelDatabaseIO::begin(Ioss::State /* state */)
  {
    return true;
  }

  bool   ParallelDatabaseIO::end(Ioss::State /* state */)
  {
    return true;
  }

  bool ParallelDatabaseIO::begin_state(Ioss::Region *region, int /* state */, double time )
  {
    return true;
  }

  bool   ParallelDatabaseIO::end_state(Ioss::Region */* region */, int /* state */, double /* time */)
  {
    return true;
  }

  const Ioss::Map& ParallelDatabaseIO::get_map(entity_type type) const
  {
    switch (type) {
    case entity_type::NODE:
      {
        size_t offset = decomp->nodeOffset;
        size_t count  = decomp->nodeCount;
        return get_map(nodeMap, nodeCount, offset, count, entity_type::NODE);
      }
    case entity_type::ELEM:
      {
        size_t offset = decomp->elementOffset;
        size_t count  = decomp->elementCount;
        return get_map(elemMap, elementCount, offset, count, entity_type::ELEM);
      }

    default:
      std::ostringstream errmsg;
      errmsg << "INTERNAL ERROR: Invalid map type. "
             << "Something is wrong in the Iocgns::ParallelDatabaseIO::get_map() function. "
             << "Please report.\n";
      IOSS_ERROR(errmsg);
    }      
  }

  const Ioss::Map& ParallelDatabaseIO::get_map(Ioss::Map &entity_map,
                                       int64_t entityCount,
                                       int64_t file_offset, int64_t file_count,
                                       entity_type type) const
                                       
  {
    // Allocate space for node number map and read it in...
    // Can be called multiple times, allocate 1 time only
    if (entity_map.map.empty()) {
      entity_map.map.resize(entityCount+1);

      if (is_input()) {
        Ioss::MapContainer file_data(file_count);

	// For cgns, my file_data is just nodes from file_offset to file_offset+file_count
	std::iota(file_data.begin(), file_data.end(), file_offset+1);

	if (type == entity_type::NODE)
	  decomp->communicate_node_data(TOPTR(file_data), &entity_map.map[1], 1);
	else if (type == entity_type::ELEM)
	  decomp->communicate_element_data(TOPTR(file_data), &entity_map.map[1], 1);
	
        // Check for sequential node map.
        // If not, build the reverse G2L node map...
        entity_map.map[0] = -1;
        for (int64_t i=1; i < entityCount+1; i++) {
          if (i != entity_map.map[i]) {
            entity_map.map[0] = 1;
            break;
          }
        }

        entity_map.build_reverse_map();

      } else {
        // Output database; entity_map.map not set yet... Build a default map.
        for (int64_t i=1; i < entityCount+1; i++) {
          entity_map.map[i] = i;
        }
        // Sequential map
        entity_map.map[0] = -1;
      }
    }
    return entity_map;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::Region* /* reg */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::NodeBlock* nb, const Ioss::Field& field,
					 void *data, size_t data_size) const
  {
    size_t num_to_get = field.verify(data_size);

    Ioss::Field::RoleType role = field.get_role();
    if (role == Ioss::Field::MESH) {
      if (field.get_name() == "mesh_model_coordinates_x" ||
	  field.get_name() == "mesh_model_coordinates_y" ||
	  field.get_name() == "mesh_model_coordinates_z" ||
	  field.get_name() == "mesh_model_coordinates") {
	decomp->get_node_coordinates(cgnsFilePtr, (double*)data, field);
      }

      else if (field.get_name() == "ids") {
	// Map the local ids in this node block
	// (1...node_count) to global node ids.
	get_map(entity_type::NODE).map_implicit_data(data, field, num_to_get, 0);
      }
      else {
	num_to_get = Ioss::Utils::field_warning(nb, field, "input");
      }
      return num_to_get;
    }
    return -1;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::EdgeBlock* /* nb */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::FaceBlock* /* nb */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::ElementBlock* eb,
					 const Ioss::Field& field,
					 void *data, size_t data_size) const
  {
    size_t num_to_get = field.verify(data_size);
    Ioss::Field::RoleType role = field.get_role();

    if (role == Ioss::Field::MESH) {
      // Handle the MESH fields required for a CGNS file model.
      // (The 'genesis' portion)

      if (field.get_name() == "connectivity_raw" || field.get_name() == "connectivity") {

	// The connectivity is stored in a 1D array.
	// The element_node index varies fastet
	int order = eb->get_property("original_block_order").get_int();
	decomp->get_block_connectivity(cgnsFilePtr, data, order);

#if 0
	int element_nodes = eb->topology()->number_nodes();
	assert(field.raw_storage()->component_count() == element_nodes);
	get_map(entity_type::NODE).map_data(data, field, num_to_get*element_nodes);
#endif
      }
      else if (field.get_name() == "ids" || field.get_name() == "implicit_ids") {
	// Map the local ids in this node block
	// (1...node_count) to global node ids.
	get_map(entity_type::ELEM).map_implicit_data(data, field, num_to_get, eb->get_offset());
      }
      else {
	num_to_get = Ioss::Utils::field_warning(eb, field, "input");
      }
    }
    else {
      num_to_get = Ioss::Utils::field_warning(eb, field, "unknown");
    }
    return num_to_get;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::NodeSet* /* ns */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::EdgeSet* /* ns */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::FaceSet* /* ns */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::ElementSet* /* ns */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::SideBlock* sb, const Ioss::Field& field,
					 void *data , size_t data_size) const
  {
    cgsize_t base = sb->get_property("base").get_int();
    cgsize_t zone = sb->get_property("zone").get_int();
    cgsize_t sect = sb->get_property("section").get_int();
    
    ssize_t num_to_get = field.verify(data_size);
    if (num_to_get > 0) {
      int64_t entity_count = sb->get_property("entity_count").get_int();
      if (num_to_get != entity_count) {
	std::ostringstream errmsg;
	errmsg << "ERROR: Partial field input not yet implemented for side blocks";
	IOSS_ERROR(errmsg);
      }
    }

    Ioss::Field::RoleType role = field.get_role();
    if (role == Ioss::Field::MESH) {
      if (field.get_name() == "element_side_raw" || field.get_name() == "element_side") {

	// TODO? Possibly rewrite using cgi_read_int_data so can skip reading element connectivity
	int nodes_per_face = sb->topology()->number_nodes();
	std::vector<cgsize_t> elements(nodes_per_face*num_to_get); // Not needed, but can't skip

	// We get:
	// *  num_to_get parent elements,
	// *  num_to_get zeros (other parent element for face, but on boundary so 0)
	// *  num_to_get face_on_element
	// *  num_to_get zeros (face on other parent element)
	std::vector<cgsize_t> parent(4 * num_to_get);

	int ierr = cg_elements_read(cgnsFilePtr, base, zone, sect,
				    TOPTR(elements), TOPTR(parent));
	if (ierr < 0) {
	  cgns_error(cgnsFilePtr, __LINE__, myProcessor);
	}

	size_t offset = m_zoneOffset[zone];
	if (field.get_type() == Ioss::Field::INT32) {
	  int *idata = (int*)data;
	  size_t j = 0;
	  for (ssize_t i=0; i < num_to_get; i++) {
	    idata[j++] = parent[num_to_get*0 + i]+offset;  // Element
	    idata[j++] = parent[num_to_get*2 + i];
	    assert(parent[num_to_get*1+i] == 0);
	    assert(parent[num_to_get*3+i] == 0);
	  }
	  // Adjust face numbers to IOSS convention instead of CGNS convention...
	  map_cgns_face_to_ioss(sb->parent_element_topology(), num_to_get, idata);
	}
	else {
	  int64_t *idata = (int64_t*)data;
	  size_t j = 0;
	  for (ssize_t i=0; i < num_to_get; i++) {
	    idata[j++] = parent[num_to_get*0 + i]+offset; // Element
	    idata[j++] = parent[num_to_get*2 + i];
	    assert(parent[num_to_get*1+i] == 0);
	    assert(parent[num_to_get*3+i] == 0);
	  }
	  // Adjust face numbers to IOSS convention instead of CGNS convention...
	  map_cgns_face_to_ioss(sb->parent_element_topology(), num_to_get, idata);
	}


      }
    }
    return -1;
  }

  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::SideSet* /* fs */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::get_field_internal(const Ioss::CommSet* /* cs */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }

  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::Region* region, const Ioss::Field& field,
					 void *data, size_t data_size) const
  {
    return -1;
  }

  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::ElementBlock* /* eb */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::FaceBlock* /* nb */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::EdgeBlock* /* nb */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::NodeBlock* /* nb */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }

  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::NodeSet* /* ns */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::EdgeSet* /* ns */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::FaceSet* /* ns */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::ElementSet* /* ns */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::SideBlock* /* fb */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::SideSet* /* fs */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }
  int64_t ParallelDatabaseIO::put_field_internal(const Ioss::CommSet* /* cs */, const Ioss::Field& /* field */,
					 void */* data */, size_t /* data_size */) const
  {
    return -1;
  }

  unsigned ParallelDatabaseIO::entity_field_support() const
  {
    return Ioss::REGION;
  }
}

