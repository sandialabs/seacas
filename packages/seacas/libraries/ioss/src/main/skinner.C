// Copyright(C) 1999-2010
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
#include <Ioss_SurfaceSplit.h>
#include <Ioss_Utils.h>
#include <Ionit_Initializer.h>
#include <stddef.h>
#include <stdlib.h>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <string>
#include <utility>
#include <vector>
#include <chrono>
#include <array>
#include <algorithm>
#include <functional>
#include <unordered_set>
#include <random>

#if !defined(NO_EXODUS_SUPPORT)
#include <exodusII.h>
#endif

#include "Ioss_CommSet.h"
#include "Ioss_CoordinateFrame.h"
#include "Ioss_DBUsage.h"
#include "Ioss_DatabaseIO.h"
#include "Ioss_EdgeBlock.h"
#include "Ioss_EdgeSet.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ElementSet.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_FaceBlock.h"
#include "Ioss_FaceSet.h"
#include "Ioss_Field.h"
#include "Ioss_GroupingEntity.h"
#include "Ioss_IOFactory.h"
#include "Ioss_NodeBlock.h"
#include "Ioss_NodeSet.h"
#include "Ioss_Property.h"
#include "Ioss_Region.h"
#include "Ioss_SideBlock.h"
#include "Ioss_SideSet.h"
#include "Ioss_VariableType.h"
#include "Ioss_ParallelUtils.h"

#include <assert.h>

#include "skinner_interface.h"

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#ifndef NO_XDMF_SUPPORT
#include <xdmf/Ioxf_Initializer.h>
#endif

#define OUTPUT std::cout

// ========================================================================

namespace {

  template <typename T>
  void generate_index(std::vector<T> &index)
  {
    T sum = 0;
    for (size_t i=0; i < index.size(); i++) {
      T cnt = index[i];
      index[i] = sum;
      sum += cnt;
    }
  }

  class Face
  {
  public:
    Face() : id_(0), elementCount_(0) {}
    Face(size_t id, const std::array<size_t,4> &conn)
      : id_(id), elementCount_(0),
        sharedWithProc_(-1), connectivity_(conn)
    {}
    
    void add_element(size_t element_id) const
    {
      assert(elementCount_ < 2);
      element[elementCount_++] = element_id;
    }
    
    size_t id_;
    mutable size_t element[2];
    mutable size_t elementCount_;
    mutable int sharedWithProc_;
    std::array<size_t,4> connectivity_;
  };

  struct FaceHash
  {
    size_t operator()(const Face &face) const
    {
      return face.id_;
    }
  };

  struct FaceEqual
  {
    bool operator()(const Face &left, const Face &right) const
    {
      if (left.id_ != right.id_) return false;
      // Hash (id_) is equal
      // Check whether same vertices (can be in different order)
      for (auto lvert : left.connectivity_) {
        if (std::find(right.connectivity_.begin(),
                      right.connectivity_.end(),
                      lvert) == right.connectivity_.end()) {
          // Not found, therefore not the same.
          return false;
        }
      }
      return true;
    }
  };

  template <typename INT> 
  void generate_faces(Ioss::Region &region, Skinner::Interface& interface, INT /*dummy*/);
  void skinner(Skinner::Interface& interface);

  size_t id_rand(size_t id)
  {
#if 0
    std::ranlux48 rng;
    //    std::mt19937_64 rng;
    rng.seed(id);
    return rng();
#else
    return id;
#endif
  }

}
// ========================================================================

namespace {
  std::string codename;
  std::string version = "0.6";
}

int main(int argc, char *argv[])
{
  int my_rank = 0;
#ifdef HAVE_MPI
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
#endif
  
  Skinner::Interface interface;
  interface.parse_options(argc, argv);
  
  std::string in_type = "exodusII";

  codename = argv[0];
  size_t ind = codename.find_last_of("/", codename.size());
  if (ind != std::string::npos)
    codename = codename.substr(ind+1, codename.size());

  Ioss::Init::Initializer io;

  if (my_rank == 0) {
    OUTPUT << "Input:    '" << interface.input_filename()  << "', Type: " << interface.input_type()  << '\n';
    OUTPUT << "Output:   '" << interface.output_filename()  << "', Type: " << interface.output_type()  << '\n';
  }
  skinner(interface);

  if (my_rank == 0) {
    OUTPUT << "\n" << codename << " execution successful.\n";
  }
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
  return EXIT_SUCCESS;
}

namespace {
  void skinner(Skinner::Interface& interface)
  {
    std::string inpfile = interface.input_filename();
    std::string input_type = interface.input_type();
    
    Ioss::PropertyManager properties;
    if (interface.ints_64_bit()) {
      properties.add(Ioss::Property("INTEGER_SIZE_DB",  8));
      properties.add(Ioss::Property("INTEGER_SIZE_API", 8));
    }

    if (interface.debug)
      properties.add(Ioss::Property("LOGGING", 1));

    if (!interface.decomp_method.empty()) {
      properties.add(Ioss::Property("DECOMPOSITION_METHOD", interface.decomp_method));
    }
    //========================================================================
    // INPUT ...
    // NOTE: The "READ_RESTART" mode ensures that the node and element ids will be mapped.
    //========================================================================
    Ioss::DatabaseIO *dbi = Ioss::IOFactory::create(input_type, inpfile, Ioss::READ_RESTART,
                                                    (MPI_Comm)MPI_COMM_WORLD, properties);
    if (dbi == NULL || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    if (interface.ints_64_bit())
      dbi->set_int_byte_size_api(Ioss::USE_INT64_API);
    
    // NOTE: 'region' owns 'db' pointer at this time...
    Ioss::Region region(dbi, "region_1");

    if (interface.ints_64_bit()) {
      generate_faces(region, interface, (int64_t)0);
    }
    else {
      generate_faces(region, interface, (int)0);
    }
  }


  void create_face(std::unordered_set<Face,FaceHash,FaceEqual> &faces,
                   size_t id, std::array<size_t,4> &conn,
                   size_t element)
  {
    Face face(id, conn);
    auto face_iter = faces.insert(face);

    (*(face_iter.first)).add_element(element);
  }

  template <typename INT>
  void resolve_parallel_faces(Ioss::Region &region,
			      std::unordered_set<Face,FaceHash,FaceEqual> &faces,
			      const std::vector<size_t> &hash_ids,
			      INT /*dummy*/)
{
#ifdef HAVE_MPI
    size_t my_rank = region.get_database()->parallel_rank();
    size_t proc_count = region.get_database()->util().parallel_size();

    if (proc_count > 1) {
      // If parallel, resolve faces on processor boundaries.
      // For each boundary face, need to check whether all of the nodes on
      // the face are shared with the same processor.  If so, then that face
      // is *possibly* shared with that processor.
      //
      // With the current continuum element only restriction, then a face
      // can only be shared with one other processor...

      // get nodal communication data CommSet...
      Ioss::CommSet  *css = region.get_commset("commset_node");

      std::vector<std::pair<INT,INT>> proc_entity;
      {
        // entity_processor consists of node,proc, node,proc, entries.
        std::vector<INT> entity_processor;
        css->get_field_data("entity_processor_raw", entity_processor);

        proc_entity.reserve(entity_processor.size()/2);
        for (size_t i = 0; i < entity_processor.size(); i+= 2) {
          // Converts from 1-based to 0-based local nodes.
          proc_entity.push_back(std::make_pair(entity_processor[i+1], entity_processor[i]-1));
        }
      }
      
      // 'id_span' gives index into proc_entity for all nodes.
      // 'id_span[local_node_id] .. id_span[local_node_id+1]' gives
      // the location in 'proc_entity' of the sharing information
      // for node 'local_node_id'
      std::vector<size_t> id_span(hash_ids.size()+1);
      for (size_t i = 0; i < proc_entity.size(); i++) {
        INT node = proc_entity[i].second;
        assert(node >= 0 && node < id_span.size()-1);
        id_span[node]++;
      }
      generate_index(id_span);
      
      // Each boundary face ...
      // .. See if all of its nodes are shared with same processor.
      //  .. Iterate face nodes
      //  .. Determine shared proc.
      //  .. (for now, use a map of <proc,count>
      //   .. if potentially shared with 'proc', then count == num_nodes_face
      
      std::vector<INT> potential_count(proc_count);
      for (auto& face : faces) {
        if (face.elementCount_ == 1) {
          // On 'boundary' -- try to determine whether on processor or exterior boundary
          std::map<int,int> shared_nodes;
          int face_node_count = 0;
          for (auto &gnode : face.connectivity_) {
            if (gnode > 0) {
              auto node = region.get_database()->node_global_to_local(gnode, true) - 1;
              face_node_count++;
              size_t begin = id_span[node];
              size_t end   = id_span[node+1];
              for (size_t j=begin; j < end; j++) {
                assert(proc_entity[j].second == node);
                int proc = proc_entity[j].first;
                shared_nodes[proc]++;
              }
            }
          }
          for (auto &proc_count : shared_nodes) {
            if (proc_count.second == face_node_count) {
              potential_count[proc_count.first]++;
            }
          }
        }
      }

      std::vector<INT> potential_offset(potential_count.begin(), potential_count.end());
      generate_index(potential_offset);

      size_t potential = potential_offset[proc_count-1]+potential_count[proc_count-1];
      std::vector<int64_t> potential_faces(6*potential);

      for (auto& face : faces) {
        if (face.elementCount_ == 1) {
          // On 'boundary' -- try to determine whether on processor or exterior boundary
          std::map<int,int> shared_nodes;
          int face_node_count = 0;
          for (auto &gnode : face.connectivity_) {
            if (gnode > 0) {
              auto node = region.get_database()->node_global_to_local(gnode, true) - 1;
              face_node_count++;
              size_t begin = id_span[node];
              size_t end   = id_span[node+1];
              for (size_t j=begin; j < end; j++) {
                assert(proc_entity[j].second == node);
                int proc = proc_entity[j].first;
                shared_nodes[proc]++;
              }
            }
          }
          for (auto &proc_count : shared_nodes) {
            if (proc_count.second == face_node_count) {
	      size_t offset = potential_offset[proc_count.first];
	      potential_faces[6*offset+0] = face.id_;
	      potential_faces[6*offset+1] = face.connectivity_[0];
	      potential_faces[6*offset+2] = face.connectivity_[1];
	      potential_faces[6*offset+3] = face.connectivity_[2];
	      potential_faces[6*offset+4] = face.connectivity_[3];
	      potential_faces[6*offset+5] = face.element[0];
	      assert(face.elementCount_ == 1);
	      potential_offset[proc_count.first]++;
            }
          }
        }
      }

      // Regenerate potential_offset since it was modified above...
      std::copy(potential_count.begin(), potential_count.end(), potential_offset.begin());
      generate_index(potential_offset);

      // Now need to send to the other processors... 
      // For now, use all-to-all; optimization is just send to processors with data...
      std::vector<INT> check_count(proc_count); // SIZE_T
      MPI_Alltoall(TOPTR(potential_count), 1, Ioss::mpi_type((INT)0),
                   TOPTR(check_count),     1, Ioss::mpi_type((INT)0),
                   region.get_database()->util().communicator());

      const int values_per_face = 6;
      auto sum = std::accumulate(check_count.begin(), check_count.end(), 0);
      std::vector<int64_t> check_faces(values_per_face*sum);

      std::vector<INT> check_offset(check_count.begin(), check_count.end());
      generate_index(check_offset);
      
      // Need to adjust counts and offsets to account for sending 6 values per face...
      for (size_t i=0; i < proc_count; i++) {
        potential_count[i] *= values_per_face;
        potential_offset[i] *= values_per_face;
        check_count[i] *= values_per_face;
        check_offset[i] *= values_per_face;
      }
      
      Ioss::MY_Alltoallv(potential_faces, potential_count, potential_offset,
			 check_faces,     check_count,     check_offset,
			 region.get_database()->util().communicator());

      // Now iterate the check_faces and see if any of them match one
      // of this processors faces...  If so, then mark as shared and
      // add the element...
      for (size_t i=0; i < check_faces.size(); i+= values_per_face) {
        size_t id = check_faces[i+0];
        std::array<size_t,4> conn;
        conn[0] = check_faces[i+1];
        conn[1] = check_faces[i+2];
        conn[2] = check_faces[i+3];
        conn[3] = check_faces[i+4];
        size_t element = check_faces[i+5];
        Face face(id, conn);
        auto face_iter = faces.find(face);
        if (face_iter != faces.end()) {
          // we have a match... This is a shared interior face
          (*face_iter).add_element(element);

          int proc = 0;
          for (int j=0; j < check_count.size(); j++) {
            if (check_count[j] > 0 && check_offset[j] == i) {
              break;
            }
            proc++;
          }
          (*face_iter).sharedWithProc_ = proc;
        }
      }
      
    }
#endif
}
  template <typename INT>
  void generate_faces(Ioss::Region &region, Skinner::Interface& interface, INT /*dummy*/)
  {
    Ioss::NodeBlock *nb = region.get_node_blocks()[0];

    std::vector<INT>  ids;
    nb->get_field_data("ids", ids);

    // Convert ids into hashed-ids
    auto starth = std::chrono::steady_clock::now();
    std::vector<size_t> hash_ids;
    hash_ids.reserve(ids.size());
    for (auto id : ids) {
      hash_ids.push_back(id_rand(id));
    }
    auto endh =  std::chrono::steady_clock::now();

    size_t numel = region.get_property("element_count").get_int();

    std::unordered_set<Face,FaceHash,FaceEqual> faces(3.3*numel);

    Ioss::ElementBlockContainer ebs = region.get_element_blocks();
    Ioss::ElementBlockContainer::const_iterator i = ebs.begin();
    while (i != ebs.end()) {

      const Ioss::ElementTopology *topo = (*i)->topology();
      std::vector<INT> connectivity;
      (*i)->get_field_data("connectivity_raw", connectivity);

      std::vector<INT> elem_ids;
      (*i)->get_field_data("ids", elem_ids);
      
      int num_face_per_elem = topo->number_faces();
      assert(num_face_per_elem <= 6);
      std::array<Ioss::IntVector,6> face_conn;
      std::array<int,6> face_count;
      for (int face = 0; face < num_face_per_elem; face++) {
        face_conn[face] = topo->face_connectivity(face+1);
        face_count[face] = topo->number_edges_face(face+1);
      }
      
      int num_node_per_elem = topo->number_nodes();
      size_t num_elem = (*i)->get_property("entity_count").get_int();

      // NOTE: By using number_edges_face instead of number_nodes_face,
      //       we get the essential topological shape (QUAD vs TRI)
      //       which is all that is needed here.
      for (size_t elem = 0, offset = 0; elem < num_elem; elem++, offset += num_node_per_elem) {
        for (int face = 0; face < num_face_per_elem; face++) {
          size_t id = 0;
          assert(face_count[face] <= 4);
          std::array<size_t,4> conn = {0,0,0,0};
          for (size_t j = 0; j < face_count[face]; j++) {
            size_t fnode = offset + face_conn[face][j];
            size_t gnode = connectivity[fnode];
            conn[j] = ids[gnode-1];
            id += hash_ids[gnode-1];
          }
          create_face(faces, id, conn, elem_ids[elem]);
        }
      }
      ++i;
    }
    
    auto endf = std::chrono::steady_clock::now();

    resolve_parallel_faces(region, faces, hash_ids, (INT)0);
    
    auto endp = std::chrono::steady_clock::now();

    auto diffh = endh - starth;
    auto difff = endf - endh;
    auto diffp = endp - endf;

    std::cout << "Node ID hash time:   \t" << std::chrono::duration<double, std::milli> (diffh).count() << " ms\t"
              << std::chrono::duration<double, std::micro> (diffh).count()/hash_ids.size() << " us/node\n";
    std::cout << "Face generation time:\t" << std::chrono::duration<double, std::milli> (difff).count() << " ms\t"
              << faces.size()/std::chrono::duration<double> (difff).count() << " faces/second.\n";
    std::cout << "Parallel time:       \t" << std::chrono::duration<double, std::milli> (diffp).count() << " ms\t"
	      << faces.size()/std::chrono::duration<double> (diffp).count() << " faces/second.\n";
    std::cout << "Total time:          \t" << std::chrono::duration<double, std::milli> (endp-starth).count() << " ms\n\n";
    // Faces have been generated at this point.
    // Categorize (boundary/interior)
    size_t interior = 0;
    size_t boundary = 0;
    size_t error = 0;
    size_t pboundary = 0;

    for (auto& face : faces) {
      if (face.elementCount_ == 2) {
        interior++;
        if (face.sharedWithProc_ != -1)
          pboundary++;
      }
      else if (face.elementCount_ == 1)
        boundary++;
      else
        error++;
    }

    // Get vector of all boundary faces which will be output as the skin...
    std::vector<Face> boundary_faces;
    boundary_faces.reserve(boundary);
    for (auto& face : faces) {
      if (face.elementCount_ == 1) {
	boundary_faces.push_back(face);
      }
    }

    // Iterate the boundary faces and determine which nodes are referenced...
    std::vector<int> ref_nodes(ids.size());
    size_t quad = 0;
    size_t tri  = 0;
    for (const auto& face : boundary_faces) {
      size_t face_node_count = 0;
      for (auto &gnode : face.connectivity_) {
	if (gnode > 0) {
	  face_node_count++;
	  auto node = region.get_database()->node_global_to_local(gnode, true) - 1;
	  ref_nodes[node] = 1;
	}
      }
      if (face_node_count == 3)
	tri++;
      else if (face_node_count == 4)
	quad++;
    }

    size_t ref_count = std::accumulate(ref_nodes.begin(), ref_nodes.end(), 0);
    std::cout << "Unique nodes in boundary = " << ref_count << "\n";

    // Map ids from total mesh down to skin mesh...
    // Also get coordinates...
    std::vector<double> coord_in;
    nb->get_field_data("mesh_model_coordinates", coord_in);
    
    std::vector<INT> ref_ids(ref_count);
    std::vector<double> coord_out(3*ref_count);
    
    size_t j = 0;
    for (size_t i=0; i < ref_nodes.size(); i++) {
      if (ref_nodes[i] == 1) {
	coord_out[3*j+0] = coord_in[3*i+0];
	coord_out[3*j+1] = coord_in[3*i+1];
	coord_out[3*j+2] = coord_in[3*i+2];
	ref_ids[j++] = ids[i];
      }
    }
    
    // Create output file...
    Ioss::PropertyManager properties;
    if (interface.ints_64_bit()) {
      properties.add(Ioss::Property("INTEGER_SIZE_DB",  8));
      properties.add(Ioss::Property("INTEGER_SIZE_API", 8));
    }

    if (interface.compression_level > 0 || interface.shuffle) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf4"));
      properties.add(Ioss::Property("COMPRESSION_LEVEL", interface.compression_level));
      properties.add(Ioss::Property("COMPRESSION_SHUFFLE", interface.shuffle));
    }
      
    if (interface.compose_output != "none") {
      properties.add(Ioss::Property("COMPOSE_RESULTS", "YES"));
      properties.add(Ioss::Property("COMPOSE_RESTART", "YES"));
      if (interface.compose_output != "default")
	properties.add(Ioss::Property("PARALLEL_IO_MODE", interface.compose_output));
    }

    if (interface.netcdf4) {
      properties.add(Ioss::Property("FILE_TYPE", "netcdf4"));
    }
    
    std::string file = interface.output_filename();
    std::string type = interface.output_type();
    Ioss::DatabaseIO *dbo = Ioss::IOFactory::create(type, file,
						    Ioss::WRITE_RESTART, (MPI_Comm)MPI_COMM_WORLD,
						    properties);
    if (dbo == NULL || !dbo->ok(true)) {
      std::exit(EXIT_FAILURE);
    }


    // NOTE: 'output_region' owns 'dbo' pointer at this time
    Ioss::Region output_region(dbo, "skin");
    output_region.begin_mode(Ioss::STATE_DEFINE_MODEL);

    Ioss::NodeBlock *nbo = new Ioss::NodeBlock(output_region.get_database(), "nodeblock_1", ref_count, 3);
    output_region.add(nbo);
    
    Ioss::ElementBlock *quadblock = NULL;
    Ioss::ElementBlock *triblock = NULL;
    if (quad > 0) {
      quadblock = new Ioss::ElementBlock(output_region.get_database(), "quad", "shell", quad);
      output_region.add(quadblock);
    }
      
    if (tri > 0) {
      triblock = new Ioss::ElementBlock(output_region.get_database(), "tri", "trishell", tri);
      output_region.add(triblock);
    }
      
    output_region.end_mode(Ioss::STATE_DEFINE_MODEL);
      
    output_region.begin_mode(Ioss::STATE_MODEL);
    nbo->put_field_data("ids", ref_ids);
    nbo->put_field_data("mesh_model_coordinates", coord_out);
    
    std::vector<INT> quad_conn;
    std::vector<INT> quad_ids;
    std::vector<INT> tri_conn;
    std::vector<INT> tri_ids;
    quad_conn.reserve(4*quad);
    quad_ids.reserve(quad);
    tri_conn.reserve(3*tri);
    tri_ids.reserve(tri);

    bool use_face_ids = !interface.ignoreFaceIds_;
    INT fid = 1;
    for (auto& face : boundary_faces) {
      if (use_face_ids) {
	fid = face.id_;
	if (fid < 0) fid = -fid;
      } else {
	fid++;
      }

      if (face.connectivity_[3] != 0) {
	for (int i=0; i < 4; i++) {
	  quad_conn.push_back(face.connectivity_[i]);
	}
	quad_ids.push_back(fid);
      }
      else {
	for (int i=0; i < 3; i++) {
	  tri_conn.push_back(face.connectivity_[i]);
	}
	tri_ids.push_back(fid);
      }
    }

    if (quad > 0) {
      quadblock->put_field_data("ids", quad_ids);
      quadblock->put_field_data("connectivity", quad_conn);
    }
    if (tri > 0) {
      triblock->put_field_data("ids", tri_ids);
      triblock->put_field_data("connectivity", tri_conn);
    }

    output_region.end_mode(Ioss::STATE_MODEL);

#ifdef HAVE_MPI
    Ioss::Int64Vector counts(3), global(3);
    counts[0] = interior;
    counts[1] = boundary;
    counts[2] = pboundary;
    region.get_database()->util().global_count(counts, global);
    interior = global[0];
    boundary = global[1];
    pboundary= global[2];
#endif

    size_t my_rank = region.get_database()->parallel_rank();
    if (my_rank == 0) {
      OUTPUT << "Face count = " << interior+boundary-pboundary/2
             << "\tInterior = " << interior-pboundary/2
             << "\tBoundary = " << boundary
             << "\tShared   = " << pboundary
             << "\tError = " << error << "\n";
      OUTPUT << "Buckets = " << faces.bucket_count() << "\n";
      OUTPUT << "Load = " << faces.load_factor() << "\n";
      OUTPUT << "Faces/Element ratio = " << (double)faces.size() / numel << "\n";
    }
  }

}
