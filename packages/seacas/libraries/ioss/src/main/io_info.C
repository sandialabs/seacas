
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

#include <assert.h>

#include "info_interface.h"

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#ifndef NO_XDMF_SUPPORT
#include <xdmf/Ioxf_Initializer.h>
#endif

#define OUTPUT std::cout

// ========================================================================

namespace {

  MPI_Datatype mpi_type(double /*dummy*/)  {return MPI_DOUBLE;}
  MPI_Datatype mpi_type(int /*dummy*/)     {return MPI_INT;}
  MPI_Datatype mpi_type(int64_t /*dummy*/) {return MPI_LONG_LONG_INT;}
  MPI_Datatype mpi_type(size_t /*dummy*/)  {assert(sizeof(size_t) == sizeof(int64_t)); return MPI_LONG_LONG_INT;}

  int power_2(int count)
  {
    // Return the power of two which is equal to or greater than 'count'
    // count = 15 -> returns 16
    // count = 16 -> returns 16
    // count = 17 -> returns 32

    // Use brute force...
    int pow2 = 1;
    while (pow2 < count) {
      pow2 *= 2;
    }
    return pow2;
  }

  template <typename T>
  int MY_Alltoallv64(std::vector<T> &sendbuf, const std::vector<int64_t> &sendcounts, const std::vector<int64_t> &senddisp,
                     std::vector<T> &recvbuf, const std::vector<int64_t> &recvcounts, const std::vector<int64_t> &recvdisp, MPI_Comm  comm)
  {
    int processor_count = 0;
    int my_processor = 0;
    MPI_Comm_size(comm, &processor_count);
    MPI_Comm_rank(comm, &my_processor);

    // Verify that all 'counts' can fit in an integer. Symmetric
    // communication, so recvcounts are sendcounts on another processor.
    for (int i=0; i < processor_count; i++) {
      int snd_cnt = (int)sendcounts[i];
      if ((int64_t)snd_cnt != sendcounts[i]) {
        std::ostringstream errmsg;
        errmsg << "ERROR: The number of items that must be communicated via MPI calls from\n"
               << "       processor " << my_processor << " to processor " << i << " is " << sendcounts[i]
               << "\n       which exceeds the storage capacity of the integers used by MPI functions.\n";
        std::cerr << errmsg.str();
        exit(EXIT_FAILURE);
      }
    }

    size_t pow_2=power_2(processor_count);

    for(size_t i=1; i < pow_2; i++) {
      MPI_Status status;

      int tag = 24713;
      size_t exchange_proc = i ^ my_processor;
      if(exchange_proc < (size_t)processor_count){
        int snd_cnt = (int)sendcounts[exchange_proc]; // Converts from int64_t to int as needed by mpi
        int rcv_cnt = (int)recvcounts[exchange_proc];
        if ((size_t)my_processor < exchange_proc) {
          MPI_Send(&sendbuf[senddisp[exchange_proc]], snd_cnt, mpi_type(T(0)), exchange_proc, tag, comm);
          MPI_Recv(&recvbuf[recvdisp[exchange_proc]], rcv_cnt, mpi_type(T(0)), exchange_proc, tag, comm, &status);
        }
        else {
          MPI_Recv(&recvbuf[recvdisp[exchange_proc]], rcv_cnt, mpi_type(T(0)), exchange_proc, tag, comm, &status);
          MPI_Send(&sendbuf[senddisp[exchange_proc]], snd_cnt, mpi_type(T(0)), exchange_proc, tag, comm);
        }
      }
    }

    // Take care of this processor's data movement...
    std::copy(&sendbuf[senddisp[my_processor]],
              &sendbuf[senddisp[my_processor]+sendcounts[my_processor]],
              &recvbuf[recvdisp[my_processor]]);
    return 0;
  }

  template <typename T>
  int MY_Alltoallv(std::vector<T> &sendbuf, const std::vector<int64_t> &sendcnts, const std::vector<int64_t> &senddisp, 
                   std::vector<T> &recvbuf, const std::vector<int64_t> &recvcnts, const std::vector<int64_t> &recvdisp, MPI_Comm comm)
  {
    // Wrapper to handle case where send/recv counts and displacements are 64-bit integers.
    // Two cases:
    // 1) They are of type 64-bit integers, but only storing data in the 32-bit integer range.
    //    -- if (sendcnts[#proc-1] + senddisp[#proc-1] < 2^31, then we are ok
    // 2) They are of type 64-bit integers, and storing data in the 64-bit integer range.
    //    -- call special alltoallv which does point-to-point sends
    int processor_count = 0;
    MPI_Comm_size(comm, &processor_count);
    size_t max_comm = sendcnts[processor_count-1] + senddisp[processor_count-1];
    size_t one = 1;
    if (max_comm < one<<31) {
      // count and displacement data in range, need to copy to integer vector.
      std::vector<int> send_cnt(sendcnts.begin(), sendcnts.end());
      std::vector<int> send_dis(senddisp.begin(), senddisp.end());
      std::vector<int> recv_cnt(recvcnts.begin(), recvcnts.end());
      std::vector<int> recv_dis(recvdisp.begin(), recvdisp.end());
      return MPI_Alltoallv(TOPTR(sendbuf), (int*)TOPTR(send_cnt), (int*)TOPTR(send_dis), mpi_type(T(0)),
                           TOPTR(recvbuf), (int*)TOPTR(recv_cnt), (int*)TOPTR(recv_dis), mpi_type(T(0)), comm);
    }
    else {
      // Same as if each processor sent a message to every other process with:
      //     MPI_Send(sendbuf+senddisp[i]*sizeof(sendtype),sendcnts[i], sendtype, i, tag, comm);
      // And received a message from each processor with a call to:
      //     MPI_Recv(recvbuf+recvdisp[i]*sizeof(recvtype),recvcnts[i], recvtype, i, tag, comm);
      return MY_Alltoallv64(sendbuf, sendcnts, senddisp, recvbuf, recvcnts, recvdisp, comm);

    }
  }

  template <typename T>
  int MY_Alltoallv(std::vector<T> &sendbuf, const std::vector<int> &sendcnts, const std::vector<int> &senddisp, 
                   std::vector<T> &recvbuf, const std::vector<int> &recvcnts, const std::vector<int> &recvdisp,
                   MPI_Comm comm)
  {
    return MPI_Alltoallv(TOPTR(sendbuf), (int*)TOPTR(sendcnts), (int*)TOPTR(senddisp), mpi_type(T(0)),
                         TOPTR(recvbuf), (int*)TOPTR(recvcnts), (int*)TOPTR(recvdisp), mpi_type(T(0)), comm);
  }

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

  // Data space shared by most field input/output routines...
  std::vector<char> data;

  template <typename INT> 
  void generate_faces(Ioss::Region &region, INT /*dummy*/);
  void info_nodeblock(Ioss::Region &region, const Info::Interface &interface, bool summary);
  void info_edgeblock(Ioss::Region &region, bool summary);
  void info_faceblock(Ioss::Region &region, bool summary);
  void info_elementblock(Ioss::Region &region, const Info::Interface &interface, bool summary);

  void info_nodesets(Ioss::Region &region, bool summary);
  void info_edgesets(Ioss::Region &region, bool summary);
  void info_facesets(Ioss::Region &region, bool summary);
  void info_elementsets(Ioss::Region &region, bool summary);

  void info_sidesets(Ioss::Region &region, const Info::Interface &interface, bool summary);
  void info_commsets(Ioss::Region &region, bool summary);
  void info_coordinate_frames(Ioss::Region &region, bool summary);

  void info_aliases(Ioss::Region &region, Ioss::GroupingEntity *ige, bool nl_pre, bool nl_post);
  
  void info_fields(Ioss::GroupingEntity *ige,
                   Ioss::Field::RoleType role,
                   const std::string &header);

  void info_properties(Ioss::GroupingEntity *ige);

  void file_info(Info::Interface& interface);
  void group_info(Info::Interface& interface);

  std::string name(Ioss::GroupingEntity *entity) {
    return entity->type_string() + " '" + entity->name() + "'";
  }

  int64_t id(Ioss::GroupingEntity *entity) {
    int64_t id = -1;
    if (entity->property_exists("id")) {
      id = entity->get_property("id").get_int();
    }
    return id;
  }

  size_t id_rand(size_t id)
  {
#if 0
    std::mt19937_64 rng;
    rng.seed(id);
    return rng();
#else
    return id;
#endif
  }

}
void hex_volume(Ioss::ElementBlock *block, const std::vector<double> &coordinates);

// ========================================================================

namespace {
  std::string codename;
  std::string version = "1.0";
}

int main(int argc, char *argv[])
{
#ifdef HAVE_MPI
  MPI_Init(&argc, &argv);
#endif
  
  Info::Interface interface;
  interface.parse_options(argc, argv);
  
  std::string in_type = "exodusII";

  codename = argv[0];
  size_t ind = codename.find_last_of("/", codename.size());
  if (ind != std::string::npos)
    codename = codename.substr(ind+1, codename.size());

  Ioss::Init::Initializer io;
#ifndef NO_XDMF_SUPPORT
  Ioxf::Initializer ioxf;
#endif

  OUTPUT << "Input:    '" << interface.filename()  << "', Type: " << interface.type()  << '\n';
  OUTPUT << '\n';

  if (interface.list_groups()) {
    group_info(interface);
  }
  else {
    file_info(interface);
  }

  OUTPUT << "\n" << codename << " execution successful.\n";
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
  return EXIT_SUCCESS;
}

namespace {
  void element_volume(Ioss::Region &region)
  {
    std::vector<double> coordinates;
    Ioss::NodeBlock *nb = region.get_node_blocks()[0];
    nb->get_field_data("mesh_model_coordinates", coordinates);

    Ioss::ElementBlockContainer ebs = region.get_element_blocks();
    Ioss::ElementBlockContainer::const_iterator i = ebs.begin();
    while (i != ebs.end()) {
      if ((*i)->get_property("topology_type").get_string() == "hex8") {
        hex_volume(*i, coordinates);
      }
      ++i;
    }
  }

  int print_groups(int exoid, std::string prefix)
  {
#if !defined(NO_EXODUS_SUPPORT)
    int idum;
    float rdum;
    char group_name[33];
    // Print name of this group...
    ex_inquire(exoid, EX_INQ_GROUP_NAME, &idum, &rdum, group_name);
    OUTPUT << prefix << group_name << '\n';
    
    int num_children = ex_inquire_int(exoid, EX_INQ_NUM_CHILD_GROUPS);
    std::vector<int> children(num_children);
    ex_get_group_ids(exoid, NULL, TOPTR(children));
    prefix += '\t';
    for (int i=0; i < num_children; i++) {
      print_groups(children[i], prefix);
    }
#endif
    return 0;
  }

  void group_info(Info::Interface& interface)
  {
#if !defined(NO_EXODUS_SUPPORT)
    // Assume exodusII...
    std::string inpfile = interface.filename();
    float vers = 0.0;
    int CPU_word_size = 0;
    int IO_word_size = 0;

    int exoid = ex_open (inpfile.c_str(),
                         EX_READ, &CPU_word_size, &IO_word_size, &vers);

    print_groups(exoid,"");
#endif
  }

  void file_info(Info::Interface& interface)
  {
    std::string inpfile = interface.filename();
    std::string input_type = interface.type();
    
    //========================================================================
    // INPUT ...
    // NOTE: The "READ_RESTART" mode ensures that the node and element ids will be mapped.
    //========================================================================
    Ioss::DatabaseIO *dbi = Ioss::IOFactory::create(input_type, inpfile, Ioss::READ_RESTART,
                                                    (MPI_Comm)MPI_COMM_WORLD);
    if (dbi == NULL || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    if (interface.use_generic_names()) {
      dbi->set_use_generic_canonical_name(true);
    }
  
    dbi->set_surface_split_type(Ioss::int_to_surface_split(interface.surface_split_scheme()));
    dbi->set_field_separator(interface.field_suffix_separator());
    if (interface.ints_64_bit())
      dbi->set_int_byte_size_api(Ioss::USE_INT64_API);
    
    if (!interface.groupname().empty()) {
      bool success = dbi->open_group(interface.groupname());
      if (!success) {
        OUTPUT << "ERROR: Unable to open group '" << interface.groupname()
               << "' in file '" << inpfile << "\n";
        return;
      }
    }

    // NOTE: 'region' owns 'db' pointer at this time...
    Ioss::Region region(dbi, "region_1");

    // Get all properties of input database...
    bool summary = true;
    info_properties(&region);
    info_nodeblock(region,    interface, summary);
    info_edgeblock(region,    summary);
    info_faceblock(region,    summary);
    info_elementblock(region, interface, summary);

    
    info_nodesets(region,     summary);
    info_edgesets(region,     summary);
    info_facesets(region,     summary);
    info_elementsets(region,  summary);

    info_sidesets(region,     interface, summary);
    info_commsets(region,     summary);
    info_coordinate_frames(region, summary);
    if (region.property_exists("state_count") && region.get_property("state_count").get_int() > 0) {
      std::pair<int, double> state_time_max = region.get_max_time();
      std::pair<int, double> state_time_min = region.get_min_time();
      OUTPUT << " Number of time steps on database     =" << std::setw(12)
             << region.get_property("state_count").get_int() << "\n"
             << "    Minimum time = " << state_time_min.second << " at step " << state_time_min.first << "\n"
             << "    Maximum time = " << state_time_max.second << " at step " << state_time_max.first << "\n\n";
    }

    if (!interface.summary()) {
      summary = false;
      info_properties(&region);
      info_nodeblock(region,    interface, summary);
      info_edgeblock(region,    summary);
      info_faceblock(region,    summary);
      info_elementblock(region, interface, summary);
      
      info_nodesets(region,     summary);
      info_edgesets(region,     summary);
      info_facesets(region,     summary);
      info_elementsets(region,  summary);
      
      info_sidesets(region,     interface, summary);
      info_commsets(region,     summary);
      info_coordinate_frames(region, summary);
    }


    if (interface.compute_volume()) {
      element_volume(region);
    }

    if (interface.create_faces()) {
      if (interface.ints_64_bit()) {
        generate_faces(region, (int64_t)0);
      }
      else {
        generate_faces(region, (int)0);
      }
    }
  }


  void info_nodeblock(Ioss::Region &region, const Info::Interface &interface, bool summary)
  {
    Ioss::NodeBlockContainer    nbs = region.get_node_blocks();
    Ioss::NodeBlockContainer::const_iterator i = nbs.begin();
    int64_t total_num_nodes = 0;
    if (summary) {
      int64_t    degree    = 0;
      while (i != nbs.end()) {
        int64_t    num_nodes = (*i)->get_property("entity_count").get_int();
        total_num_nodes += num_nodes;
        degree    = (*i)->get_property("component_degree").get_int();
        ++i;
      }
      OUTPUT << " Number of spatial dimensions =" << std::setw(12) << degree << "\n";
      OUTPUT << " Number of nodeblocks         =" << std::setw(12) << nbs.size() << "\t";
      OUTPUT << " Number of nodes            =" << std::setw(12) << total_num_nodes << "\n";
    } else {
      while (i != nbs.end()) {
        int64_t    num_nodes = (*i)->get_property("entity_count").get_int();
        int64_t    num_attrib= (*i)->get_property("attribute_count").get_int();
        OUTPUT << '\n' << name(*i) 
               << std::setw(12) << num_nodes << " nodes, "
               << std::setw(3) << num_attrib << " attributes.\n";
        if (interface.check_node_status()) {
          std::vector<char> node_status;
          std::vector<int64_t>  ids;
          (*i)->get_field_data("node_connectivity_status", node_status);
          (*i)->get_field_data("ids", ids);
          bool header = false;
          for (size_t j=0; j < node_status.size(); j++) {
            if (node_status[j] == 0) {
              if (!header) {
                header = true;
                OUTPUT << "\tUnconnected nodes: " << ids[j];
              } else {
                OUTPUT << ", " << ids[j];
              }
            }
          }
          if (header)
            OUTPUT << "\n";
        }
        info_aliases(region, *i, false, true);
        info_fields(*i, Ioss::Field::ATTRIBUTE, "\tAttributes: ");
        info_fields(*i, Ioss::Field::TRANSIENT, "\tTransient: ");
        ++i;
      }
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
  void generate_faces(Ioss::Region &region, INT /*dummy*/)
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

    size_t my_rank = region.get_database()->parallel_rank();
    size_t proc_count = region.get_database()->util().parallel_size();

#ifdef HAVE_MPI
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
      INT proc_last = 0;
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
      
      std::vector<int> potential_count(proc_count); // SIZE_T
      std::vector<int>    shared_with(faces.size(),my_rank);

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

      std::vector<int> potential_offset(potential_count.begin(), potential_count.end());
      generate_index(potential_offset);

      size_t potential = potential_offset[proc_count-1]+potential_count[proc_count-1];
      std::vector<size_t> potential_faces(6*potential);

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
      std::vector<int> check_count(proc_count); // SIZE_T
      MPI_Alltoall(TOPTR(potential_count), 1, mpi_type((int)0),
                   TOPTR(check_count),     1, mpi_type((int)0),
                   region.get_database()->util().communicator());

      const int values_per_face = 6;
      auto sum = std::accumulate(check_count.begin(), check_count.end(), 0);
      std::vector<size_t> check_faces(values_per_face*sum);

      std::vector<int> check_offset(check_count.begin(), check_count.end());
      generate_index(check_offset);
      
      // Need to adjust counts and offsets to account for sending 6 values per face...
      for (size_t i=0; i < proc_count; i++) {
        potential_count[i] *= values_per_face;
        potential_offset[i] *= values_per_face;
        check_count[i] *= values_per_face;
        check_offset[i] *= values_per_face;
      }
      
      MY_Alltoallv(potential_faces, potential_count, potential_offset,
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
    auto endp = std::chrono::steady_clock::now();

    auto diffh = endh - starth;
    auto difff = endf - endh;
    auto diffp = endp - endf;

    std::cout << "Node ID hash time:   \t" << std::chrono::duration<double, std::milli> (diffh).count() << " ms\t"
              << std::chrono::duration<double, std::micro> (diffh).count()/hash_ids.size() << " us/node\n";
    std::cout << "Face generation time:\t" << std::chrono::duration<double, std::milli> (difff).count() << " ms\t"
              << faces.size()/std::chrono::duration<double> (difff).count() << " faces/second.\n";
    if (proc_count > 1) {
      std::cout << "Parallel time:       \t" << std::chrono::duration<double, std::milli> (diffp).count() << " ms\t"
		<< faces.size()/std::chrono::duration<double> (diffp).count() << " faces/second.\n";
    }
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

    if (my_rank == 0) {
      OUTPUT << "Face count = " << interior+boundary-pboundary/2
             << "\tInterior = " << interior-pboundary/2
             << "\tBoundary = " << boundary
             << "\tShared   = " << pboundary
             << "\tError = " << error << "\n";
      OUTPUT << "Buckets = " << faces.bucket_count() << "\n";
      OUTPUT << "Load = " << faces.load_factor() << "\n";
    }
  }

  void info_elementblock(Ioss::Region &region, const Info::Interface &interface, bool summary)
  {
    Ioss::ElementBlockContainer ebs = region.get_element_blocks();
    Ioss::ElementBlockContainer::const_iterator i = ebs.begin();
    int64_t total_elements = 0;
    while (i != ebs.end()) {
      int64_t    num_elem  = (*i)->get_property("entity_count").get_int();
      total_elements += num_elem;

      if (!summary) {
        std::string type      = (*i)->get_property("topology_type").get_string();
        int64_t    num_attrib= (*i)->get_property("attribute_count").get_int();
        OUTPUT << '\n' << name(*i)
               << " id: " << std::setw(6) << id(*i)
               << ", topology: " << std::setw(10) << type << ", "
               << std::setw(12) << num_elem << " elements, "
               << std::setw(3) << num_attrib << " attributes.";

        info_aliases(region, *i, true, false);
        info_fields(*i, Ioss::Field::ATTRIBUTE, "\n\tAttributes: ");

        if (interface.adjacencies()) {
          std::vector<std::string> blocks;
          (*i)->get_block_adjacencies(blocks);
          OUTPUT << "\n\tAdjacent to  " << blocks.size() << " element block(s):\t";
          std::vector<std::string>::iterator b = blocks.begin();
          while (b != blocks.end()) {
            OUTPUT << *b++ << "  ";
          }
        }
        info_fields(*i, Ioss::Field::TRANSIENT, "\n\tTransient:  ");
        OUTPUT << "\n";

        if (interface.compute_bbox()) {
          Ioss::AxisAlignedBoundingBox bbox = (*i)->get_bounding_box();
          OUTPUT << "\tBounding Box: Minimum X,Y,Z = "
                 << std::setw(12) << std::setprecision(4) << std::scientific
                 << bbox.xmin << "\t" << bbox.ymin << "\t" << bbox.zmin << "\n"
                 << "\t              Maximum X,Y,Z = "
                 << std::setw(12) << std::setprecision(4) << std::scientific
                 << bbox.xmax << "\t" << bbox.ymax << "\t" << bbox.zmax << "\n";
        }
      }
      ++i;
    }
    if (summary) {
      OUTPUT << " Number of element blocks     =" << std::setw(12) << ebs.size() << "\t";
      OUTPUT << " Number of elements         =" << std::setw(12) << total_elements << "\n";
    }

  }

  void info_edgeblock(Ioss::Region &region, bool summary)
  {
    Ioss::EdgeBlockContainer ebs = region.get_edge_blocks();
    Ioss::EdgeBlockContainer::const_iterator i = ebs.begin();
    int64_t total_edges = 0;
    while (i != ebs.end()) {
      int64_t    num_edge  = (*i)->get_property("entity_count").get_int();
      total_edges += num_edge;

      if (!summary) {
        std::string type      = (*i)->get_property("topology_type").get_string();
        int64_t    num_attrib= (*i)->get_property("attribute_count").get_int();
        OUTPUT << '\n' << name(*i)
               << " id: " << std::setw(6) << id(*i)
               << ", topology: " << std::setw(10) << type << ", "
               << std::setw(12) << num_edge << " edges, "
               << std::setw(3) << num_attrib << " attributes.\n";

        info_aliases(region, *i, false, true);
        info_fields(*i, Ioss::Field::ATTRIBUTE, "\tAttributes: ");

#if 0
        std::vector<std::string> blocks;
        (*i)->get_block_adjacencies(blocks);
        OUTPUT << "\tAdjacent to  " << blocks.size() << " edge block(s):\t";
        std::vector<std::string>::iterator b = blocks.begin();
        while (b != blocks.end()) {
          OUTPUT << *b++ << "  ";
        }
#endif
        info_fields(*i, Ioss::Field::TRANSIENT, "\n\tTransient:  ");
        OUTPUT << "\n";
      }

      ++i;
    }
    if (summary) {
      OUTPUT << " Number of edge blocks        =" << std::setw(12) << ebs.size() << "\t";
      OUTPUT << " Number of edges            =" << std::setw(12) << total_edges << "\n";
    }

  }

  void info_faceblock(Ioss::Region &region, bool summary)
  {
    Ioss::FaceBlockContainer ebs = region.get_face_blocks();
    Ioss::FaceBlockContainer::const_iterator i = ebs.begin();
    int64_t total_faces = 0;
    while (i != ebs.end()) {
      int64_t    num_face  = (*i)->get_property("entity_count").get_int();
      total_faces += num_face;

      if (!summary) {
        std::string type      = (*i)->get_property("topology_type").get_string();
        int64_t    num_attrib= (*i)->get_property("attribute_count").get_int();
        OUTPUT << '\n' << name(*i)
               << " id: " << std::setw(6) << id(*i)
               << ", topology: " << std::setw(10) << type << ", "
               << std::setw(12) << num_face << " faces, "
               << std::setw(3) << num_attrib << " attributes.\n";

        info_aliases(region, *i, false, true);
        info_fields(*i, Ioss::Field::ATTRIBUTE, "\tAttributes: ");

#if 0
        std::vector<std::string> blocks;
        (*i)->get_block_adjacencies(blocks);
        OUTPUT << "\tAdjacent to  " << blocks.size() << " face block(s):\t";
        std::vector<std::string>::iterator b = blocks.begin();
        while (b != blocks.end()) {
          OUTPUT << *b++ << "  ";
        }
#endif
        info_fields(*i, Ioss::Field::TRANSIENT, "\n\tTransient:  ");
        OUTPUT << "\n";
      }

      ++i;
    }
    if (summary) {
      OUTPUT << " Number of face blocks        =" << std::setw(12) << ebs.size() << "\t";
      OUTPUT << " Number of faces            =" << std::setw(12) << total_faces << "\n";
    }

  }

  void info_sidesets(Ioss::Region &region, const Info::Interface &interface, bool summary)
  {
    Ioss::SideSetContainer      fss = region.get_sidesets();
    Ioss::SideSetContainer::const_iterator i = fss.begin();
    int64_t total_sides = 0;
    while (i != fss.end()) {
      if (!summary) {
        OUTPUT << '\n' << name(*i) << " id: " << std::setw(6)<< id(*i) << ":";
        info_aliases(region, *i, true, false);
        if (interface.adjacencies()) {
          std::vector<std::string> blocks;
          (*i)->block_membership(blocks);
          OUTPUT << "\n\tTouches " << blocks.size() << " element block(s):\t";
          std::vector<std::string>::iterator b = blocks.begin();
          while (b != blocks.end()) {
            OUTPUT << *b++ << "  ";
          }
          OUTPUT << "\n";
        }
      }
      if (!summary) {
        OUTPUT << "\n\tContains: \n";
      }
      
      Ioss::SideBlockContainer fbs = (*i)->get_side_blocks();
      Ioss::SideBlockContainer::const_iterator j = fbs.begin();
      while (j != fbs.end()) {
        int64_t    num_side  = (*j)->get_property("entity_count").get_int();
        if (!summary) {
          std::string fbtype    = (*j)->get_property("topology_type").get_string();
          std::string partype   = (*j)->get_property("parent_topology_type").get_string();
          OUTPUT << "\t\t"
                 << name(*j) << ", "
                 << num_side << " " << fbtype << " sides"
                 << ", parent topology: " << partype 
                 << "\n";
          if (interface.adjacencies()) {
            std::vector<std::string> blocks;
            (*j)->block_membership(blocks);
            OUTPUT << "\t\t\tTouches " << blocks.size() << " element block(s):\t";
            std::vector<std::string>::iterator b = blocks.begin();
            while (b != blocks.end()) {
              OUTPUT << *b++ << "  ";
            }
            OUTPUT << "\n";
          }
          OUTPUT << "\n";
          info_fields(*j, Ioss::Field::ATTRIBUTE, "\t\tAttributes: ");
          info_fields(*j, Ioss::Field::TRANSIENT, "\t\tTransient:  ");
        }
        total_sides += num_side;
        ++j;
      }
      ++i;
    }

    if (summary) {
      OUTPUT << " Number of side sets          =" << std::setw(12) << fss.size() << "\t";
      OUTPUT << " Number of element sides    =" << std::setw(12) << total_sides << "\n";
    }
  }
  
  void info_nodesets(Ioss::Region &region, bool summary)
  {
    Ioss::NodeSetContainer      nss = region.get_nodesets();
    Ioss::NodeSetContainer::const_iterator i = nss.begin();
    int64_t total_nodes = 0;
    while (i != nss.end()) {
      int64_t    count     = (*i)->get_property("entity_count").get_int();
      int64_t    num_attrib= (*i)->get_property("attribute_count").get_int();
      int64_t    num_dist  = (*i)->get_property("distribution_factor_count").get_int();
      if (!summary) {
        OUTPUT << '\n' << name(*i) << " id: " << std::setw(6) << id(*i)   << ", "
               << std::setw(8) << count << " nodes" 
               << std::setw(3) << num_attrib << " attributes"
               << std::setw(8) << num_dist << " distribution factors.\n";
        info_aliases(region, *i, false, true);
        info_fields(*i, Ioss::Field::ATTRIBUTE, "\tAttributes: ");
        info_fields(*i, Ioss::Field::TRANSIENT, "\tTransient:  ");
      }
      total_nodes += count;
      ++i;
    }
    if (summary) {
      OUTPUT << " Number of nodal point sets   =" << std::setw(12) << nss.size() << "\t";
      OUTPUT << " Length of node list        =" << std::setw(12) << total_nodes << "\n";
    }
  }

  void info_edgesets(Ioss::Region &region, bool summary)
  {
    Ioss::EdgeSetContainer      nss = region.get_edgesets();
    Ioss::EdgeSetContainer::const_iterator i = nss.begin();
    int64_t total_edges = 0;
    while (i != nss.end()) {
      int64_t    count     = (*i)->get_property("entity_count").get_int();
      int64_t    num_attrib= (*i)->get_property("attribute_count").get_int();
      if (!summary) {
        OUTPUT << '\n' << name(*i)
               << " id: " << std::setw(6) << id(*i)   << ", "
               << std::setw(8) << count << " edges"
               << std::setw(3) << num_attrib << " attributes.\n";
        info_aliases(region, *i, false, true);
        info_fields(*i, Ioss::Field::ATTRIBUTE, "\tAttributes: ");
        info_fields(*i, Ioss::Field::TRANSIENT, "\tTransient:  ");
      }
      total_edges += count;
      ++i;
    }
    if (summary) {
      OUTPUT << " Number of edge sets          =" << std::setw(12) << nss.size() << "\t";
      OUTPUT << " Length of edge list        =" << std::setw(12) << total_edges << "\n";
    }
  }

  void info_facesets(Ioss::Region &region, bool summary)
  {
    Ioss::FaceSetContainer      nss = region.get_facesets();
    Ioss::FaceSetContainer::const_iterator i = nss.begin();
    int64_t total_faces = 0;
    while (i != nss.end()) {
      int64_t    count     = (*i)->get_property("entity_count").get_int();
      int64_t    num_attrib= (*i)->get_property("attribute_count").get_int();
      if (!summary) {
        OUTPUT << '\n' << name(*i)
               << " id: " << std::setw(6) << id(*i)   << ", "
               << std::setw(8) << count << " faces"
               << std::setw(3) << num_attrib << " attributes.\n";
        info_aliases(region, *i, false, true);
        info_fields(*i, Ioss::Field::ATTRIBUTE, "\tAttributes: ");
        info_fields(*i, Ioss::Field::TRANSIENT, "\tTransient:  ");
      }
      total_faces += count;
      ++i;
    }
    if (summary) {
      OUTPUT << " Number of face sets          =" << std::setw(12) << nss.size() << "\t";
      OUTPUT << " Length of face list        =" << std::setw(12) << total_faces << "\n";
    }
  }

  void info_elementsets(Ioss::Region &region, bool summary)
  {
    Ioss::ElementSetContainer      nss = region.get_elementsets();
    Ioss::ElementSetContainer::const_iterator i = nss.begin();
    int64_t total_elements = 0;
    while (i != nss.end()) {
      int64_t    count     = (*i)->get_property("entity_count").get_int();
      if (!summary) {
        OUTPUT << '\n' << name(*i)
               << " id: " << std::setw(6) << id(*i)   << ", "
               << std::setw(8) << count << " elements" << "\n";
        info_aliases(region, *i, false, true);
        info_fields(*i, Ioss::Field::ATTRIBUTE, "\tAttributes: ");
        info_fields(*i, Ioss::Field::TRANSIENT, "\tTransient:  ");
      }
      total_elements += count;
      ++i;
    }
    if (summary) {
      OUTPUT << " Number of element sets       =" << std::setw(12) << nss.size() << "\t";
      OUTPUT << " Length of element list     =" << std::setw(12) << total_elements << "\n";
    }
  }

  void info_commsets(Ioss::Region &region, bool summary)
  {
    Ioss::CommSetContainer      css = region.get_commsets();
    Ioss::CommSetContainer::const_iterator i = css.begin();
    while (i != css.end()) {
      std::string type      = (*i)->get_property("entity_type").get_string();
      ++i;
    }
    OUTPUT << '\n';
  }

  void info_coordinate_frames(Ioss::Region &region, bool summary)
  {
    Ioss::CoordinateFrameContainer      cf = region.get_coordinate_frames();
    Ioss::CoordinateFrameContainer::const_iterator i = cf.begin();

    while (i != cf.end()) {
      if (!summary) {
        const double *origin = (*i).origin();
        const double *a3pt = (*i).axis_3_point();
        const double *p13pt = (*i).plane_1_3_point();
        
        OUTPUT << '\n' << "Coordinate Frame id: " << std::setw(6) << (*i).id()
               << ", type tag '" << (*i).tag() << "'\n"
               << "\tOrigin:          " << origin[0] << "\t" << origin[1] << "\t" << origin[2] << "\n"
               << "\tAxis 3 Point:    " << a3pt[0] << "\t" << a3pt[1] << "\t" << a3pt[2] << "\n"
               << "\tPlane 1-3 Point: " << p13pt[0] << "\t" << p13pt[1] << "\t" << p13pt[2] << "\n";
      }
      ++i;
    }
    if (summary) {
      OUTPUT << " Number of coordinate frames  =" << std::setw(12) << cf.size() << "\n";
    }
  }

  void info_aliases(Ioss::Region &region, Ioss::GroupingEntity *ige, bool nl_pre, bool nl_post)
  {
    std::vector<std::string> aliases;
    if (region.get_aliases(ige->name(), aliases) > 0) {
      if (nl_pre)
        OUTPUT << "\n";
      OUTPUT << "\tAliases: ";
      for (size_t i=0; i < aliases.size(); i++) {
        if (i > 0)
          OUTPUT << ", ";
        OUTPUT << aliases[i];
      }
      if (nl_post)
        OUTPUT << "\n";
    }
  }

  void info_fields(Ioss::GroupingEntity *ige,
                   Ioss::Field::RoleType role,
                   const std::string &header)
  {
    Ioss::NameList fields;
    ige->field_describe(role, &fields);

    if (fields.empty())
      return;
    
    if (!header.empty()) {
      OUTPUT << header;
    }
    // Iterate through results fields and transfer to output
    // database...  
    Ioss::NameList::const_iterator IF;
    for (IF = fields.begin(); IF != fields.end(); ++IF) {
      std::string field_name = *IF;

      const Ioss::VariableType *var_type = ige->get_field(field_name).raw_storage();
      int comp_count = var_type->component_count();
      OUTPUT << std::setw(16) << field_name << ":" << comp_count << " ";
    }
    if (!header.empty()) {
      OUTPUT << "\n";
    }
  }

  void info_properties(Ioss::GroupingEntity *ige)
  {
#if 0
    Ioss::NameList names;
    ige->property_describe(&names);

    // Iterate through properties and transfer to output database...
    Ioss::NameList::const_iterator I;
    for (I = names.begin(); I != names.end(); ++I) {
      OUTPUT << *I << ", ";
    }
#endif
  }

}
