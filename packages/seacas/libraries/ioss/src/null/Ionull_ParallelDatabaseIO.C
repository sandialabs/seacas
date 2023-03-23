// ISSUES:
// 1. Does not handle unconnected nodes (not connected to any element)
//
// 2. SideSet distribution factors are klugy and may not fully work in
//    strange cases
//
//
// Copyright(C) 1999-2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_CodeTypes.h>
#include <null/Ionull_ParallelDatabaseIO.h>
#if defined SEACAS_HAS_MPI
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cfloat>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <fmt/ostream.h>
#include <functional>
#include <iostream>
#include <limits>
#include <map>
#include <numeric>
#include <set>
#include <string>
#include <tokenize.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif
#include <utility>
#include <vector>

#include <null/Ionull_DecompositionData.h>
#include <null/Ionull_Internals.h>
#include <null/Ionull_Utils.h>

#include <Ioss_Assembly.h>
#include <Ioss_Blob.h>
#include <Ioss_CommSet.h>
#include <Ioss_CoordinateFrame.h>
#include <Ioss_DBUsage.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_EdgeBlock.h>
#include <Ioss_EdgeSet.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_ElementSet.h>
#include <Ioss_ElementTopology.h>
#include <Ioss_EntityBlock.h>
#include <Ioss_EntitySet.h>
#include <Ioss_EntityType.h>
#include <Ioss_FaceBlock.h>
#include <Ioss_FaceSet.h>
#include <Ioss_Field.h>
#include <Ioss_FileInfo.h>
#include <Ioss_GroupingEntity.h>
#include <Ioss_Map.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_NodeSet.h>
#include <Ioss_ParallelUtils.h>
#include <Ioss_Property.h>
#include <Ioss_Region.h>
#include <Ioss_SideBlock.h>
#include <Ioss_SideSet.h>
#include <Ioss_State.h>
#include <Ioss_SurfaceSplit.h>
#include <Ioss_Utils.h>
#include <Ioss_VariableType.h>

#include <Ioss_FileInfo.h>
#undef MPICPP

// ========================================================================
// Static internal helper functions
// ========================================================================
namespace {
  const size_t max_line_length = MAX_LINE_LENGTH;

  const std::string SEP() { return std::string("@"); } // Separator for attribute offset storage
  const std::array<std::string, 2> complex_suffix{".re", ".im"};

  void check_node_owning_processor_data(const Ioss::IntVector &nop, size_t file_node_count)
  {
    // Verify that the nop (NodeOwningProcessor) vector is not empty and is of the correct size.
    // This vector specifies which rank owns each node on this rank
    // Throws error if problem, otherwise returns quietly.
    if (file_node_count == 0) {
      return;
    }
    if (nop.empty()) {
      std::ostringstream errmsg;
      fmt::print(errmsg, "ERROR: The use of the 'compose' output option requires the definition of "
                         "the 'owning_processor'"
                         " field prior to the output of nodal data.  This field has not yet been "
                         "defined so output is not possible."
                         " For more information, contact gdsjaar@sandia.gov.\n");
      IOSS_ERROR(errmsg);
    }
    else if (nop.size() < file_node_count) {
      std::ostringstream errmsg;
      fmt::print(errmsg,
                 "ERROR: The 'owning_processor' data was defined, but it is not the correct size."
                 "  Its size is {}, but it must be at least this size {}."
                 " For more information, contact gdsjaar@sandia.gov.\n",
                 nop.size(), file_node_count);
      IOSS_ERROR(errmsg);
    }
  }
  template <typename T>
  void compute_internal_border_maps(T *entities, T *internal, size_t count, size_t entity_count)
  {
    for (size_t ij = 0; ij < count; ij++) {
      internal[ij] = 1;
    }
    for (size_t J = 0; J < entity_count; J++) {
      internal[entities[J] - 1] = 0;
    }

    size_t b = 0;
    for (size_t ij = 0; ij < count; ij++) {
      if (internal[ij] == 0) {
        entities[b++] = ij + 1;
      }
    }

    size_t k = 0;
    for (size_t ij = 0; ij < count; ij++) {
      if (internal[ij] == 1) {
        internal[k++] = ij + 1;
      }
    }
  }

  template <typename INT>
  void map_nodeset_id_data(const Ioss::IntVector &owning_processor, Ioss::Int64Vector &owned_nodes,
                           int this_processor, const INT *ids, size_t ids_size,
                           std::vector<INT> &file_data)
  {
    // Determine which nodes in this nodeset are owned by this processor.
    // Save this mapping in the "owned_nodes" vector for use in
    // mapping nodeset field data (df, transient, attributes, ...)
    for (size_t i = 0; i < ids_size; i++) {
      INT node = ids[i];
      if (owning_processor[node - 1] == this_processor) {
        file_data.push_back(ids[i]);
        owned_nodes.push_back(i);
      }
    }
  }

  template <typename T, typename U>
  void map_nodeset_data(Ioss::Int64Vector &owned_nodes, const T *data, std::vector<U> &file_data,
                        size_t offset = 0, size_t stride = 1)
  {
    // Pull out the locally owned nodeset data
    for (auto owned_node : owned_nodes) {
      file_data.push_back(data[stride * owned_node + offset]);
    }
  }

  template <typename T>
  void extract_data(std::vector<double> &local_data, T *data, size_t num_entity, size_t offset,
                    size_t comp_count)
  {
    local_data.resize(num_entity);
    if (comp_count == 1 && offset == 0) {
      for (size_t j = 0; j < num_entity; j++) {
        local_data[j] = data[j];
      }
    }
    else {
      for (size_t j = 0; j < num_entity; j++) {
        local_data[j] = data[offset];
        offset += comp_count;
      }
    }
  }

  // Ideally, there should only be a single data type for in and out
  // data, but in the node id map mapping, we have an int64_t coming
  // in and either an int or int64_t going out...
  template <typename T, typename U>
  void filter_owned_nodes(const Ioss::IntVector &owning_processor, int this_processor,
                          const T *data, std::vector<U> &file_data, size_t offset = 0,
                          size_t stride = 1)
  {
    size_t index = offset;
    for (auto op : owning_processor) {
      if (op == this_processor) {
        file_data.push_back(data[index]);
      }
      index += stride;
    }
  }

  // This version can be used *if* the input and output types are the same *and* the
  // input `data` can be modified / overwritten.
  template <typename T>
  void filter_owned_nodes(const Ioss::IntVector &owning_processor, int this_processor, T *data)
  {
    size_t index = 0;
    size_t entry = 0;
    for (auto op : owning_processor) {
      if (op == this_processor) {
        data[entry++] = data[index];
      }
      index++;
    }
  }

  template <typename INT>
  void map_local_to_global_implicit(INT *data, size_t count,
                                    const std::vector<int64_t> &global_implicit_map)
  {
    for (size_t i = 0; i < count; i++) {
      data[i] = global_implicit_map[data[i] - 1];
    }
  }

  void update_processor_offset_property(Ioss::Region *region, const Ionull::Mesh &mesh)
  {
    const Ioss::NodeBlockContainer &node_blocks = region->get_node_blocks();
    if (!node_blocks.empty()) {
      node_blocks[0]->property_add(
          Ioss::Property("_processor_offset", mesh.nodeblocks[0].procOffset));
    }
    const Ioss::EdgeBlockContainer &edge_blocks = region->get_edge_blocks();
    for (size_t i = 0; i < edge_blocks.size(); i++) {
      edge_blocks[i]->property_add(
          Ioss::Property("_processor_offset", mesh.edgeblocks[i].procOffset));
    }
    const Ioss::FaceBlockContainer &face_blocks = region->get_face_blocks();
    for (size_t i = 0; i < face_blocks.size(); i++) {
      face_blocks[i]->property_add(
          Ioss::Property("_processor_offset", mesh.faceblocks[i].procOffset));
    }

    int64_t                            offset         = 0; // Offset into global element map...
    const Ioss::ElementBlockContainer &element_blocks = region->get_element_blocks();
    for (size_t i = 0; i < element_blocks.size(); i++) {
      element_blocks[i]->property_add(Ioss::Property("global_map_offset", offset));
      offset += mesh.elemblocks[i].entityCount;
      element_blocks[i]->property_add(
          Ioss::Property("_processor_offset", mesh.elemblocks[i].procOffset));
    }

    const Ioss::NodeSetContainer &nodesets = region->get_nodesets();
    for (size_t i = 0; i < nodesets.size(); i++) {
      nodesets[i]->property_add(Ioss::Property("_processor_offset", mesh.nodesets[i].procOffset));
    }
    const Ioss::EdgeSetContainer &edgesets = region->get_edgesets();
    for (size_t i = 0; i < edgesets.size(); i++) {
      edgesets[i]->property_add(Ioss::Property("_processor_offset", mesh.edgesets[i].procOffset));
    }
    const Ioss::FaceSetContainer &facesets = region->get_facesets();
    for (size_t i = 0; i < facesets.size(); i++) {
      facesets[i]->property_add(Ioss::Property("_processor_offset", mesh.facesets[i].procOffset));
    }
    const Ioss::ElementSetContainer &elementsets = region->get_elementsets();
    for (size_t i = 0; i < facesets.size(); i++) {
      elementsets[i]->property_add(
          Ioss::Property("_processor_offset", mesh.elemsets[i].procOffset));
    }

    const Ioss::SideSetContainer &ssets = region->get_sidesets();
    for (size_t i = 0; i < ssets.size(); i++) {
      ssets[i]->property_add(Ioss::Property("_processor_offset", mesh.sidesets[i].procOffset));
      ssets[i]->property_add(Ioss::Property("processor_df_offset", mesh.sidesets[i].dfProcOffset));

      // Propagate down to owned sideblocks...
      const Ioss::SideBlockContainer &side_blocks = ssets[i]->get_side_blocks();
      for (auto &block : side_blocks) {
        block->property_add(Ioss::Property("_processor_offset", mesh.sidesets[i].procOffset));
        block->property_add(Ioss::Property("processor_df_offset", mesh.sidesets[i].dfProcOffset));
      }
    }
    const auto &blobs = region->get_blobs();
    for (size_t i = 0; i < blobs.size(); i++) {
      blobs[i]->property_add(Ioss::Property("_processor_offset", mesh.blobs[i].procOffset));
    }
  }
} // namespace

namespace Ionull {
  ParallelDatabaseIO::ParallelDatabaseIO(Ioss::Region *region, const std::string &filename,
                                         Ioss::DatabaseUsage db_usage, Ioss_MPI_Comm communicator,
                                         const Ioss::PropertyManager &props)
      : Ionull::BaseDatabaseIO(region, filename, db_usage, communicator, props)
  {
    usingParallelIO = true;
    if (!is_parallel_consistent()) {
      std::ostringstream errmsg;
      fmt::print(errmsg,
                 "ERROR: Parallel IO cannot be used in an application that is not guaranteeing "
                 "parallel consistent calls of the get and put field data functions.\n"
                 "The application created this database with a 'false' setting for the "
                 "isParallelConsistent property.");
      IOSS_ERROR(errmsg);
    }

    if (!is_input()) {
      // Check whether appending to or modifying existing file...
      if (open_create_behavior() == Ioss::DB_APPEND ||
          open_create_behavior() == Ioss::DB_APPEND_GROUP ||
          open_create_behavior() == Ioss::DB_MODIFY) {
        // Append to file if it already exists -- See if the file exists.
        auto file  = Ioss::FileInfo(get_filename());
        fileExists = file.exists();
        if (fileExists && myProcessor == 0) {
          fmt::print(Ioss::WarnOut(),
                     "Appending to existing database in parallel single-file "
                     "output mode is a new capability; please check results carefully. File '{}'",
                     get_filename());
        }
      }
    }
  }

  ParallelDatabaseIO::~ParallelDatabaseIO() = default;

  void ParallelDatabaseIO::release_memory__()
  {
    free_file_pointer();
    nodeMap.release_memory();
    edgeMap.release_memory();
    faceMap.release_memory();
    elemMap.release_memory();
    Ioss::Utils::clear(nodeOwningProcessor);
    Ioss::Utils::clear(nodeGlobalImplicitMap);
    Ioss::Utils::clear(elemGlobalImplicitMap);
    nodeGlobalImplicitMapDefined = false;
    elemGlobalImplicitMapDefined = false;
    nodesetOwnedNodes.clear();
    try {
      decomp.reset();
    }
    catch (...) {
    }
  }

  bool ParallelDatabaseIO::check_valid_file_ptr(bool write_message, std::string *error_msg,
                                                int *bad_count, bool abort_if_error) const
  {
    // Check for valid exodus_file_ptr (valid >= 0; invalid < 0)
    assert(isParallel);
    int global_file_ptr = util().global_minmax(m_nullFilePtr, Ioss::ParallelUtils::DO_MIN);

    if (global_file_ptr < 0) {
      if (write_message || error_msg != nullptr || bad_count != nullptr) {
        Ioss::IntVector status;
        util().all_gather(m_nullFilePtr, status);

        std::string open_create = is_input() ? "open input" : "create output";
        if (write_message || error_msg != nullptr) {
          std::vector<size_t> procs;
          for (int i = 0; i < util().parallel_size(); i++) {
            if (status[i] < 0) {
              procs.push_back(i);
            }
          }
          std::string error_list = Ioss::Utils::format_id_list(procs, "--");
          // See which processors could not open/create the file...
          std::ostringstream errmsg;
          fmt::print(errmsg, "ERROR: Unable to {} exodus database file '{}' on processors:\n\t{}",
                     open_create, get_filename(), error_list);
          fmt::print(errmsg, "\n");
          if (error_msg != nullptr) {
            *error_msg = errmsg.str();
          }
          if (write_message && myProcessor == 0) {
            fmt::print(Ioss::OUTPUT(), "{}", errmsg.str());
          }
        }
        if (bad_count != nullptr) {
          *bad_count = std::count_if(status.begin(), status.end(), [](int i) { return i < 0; });
        }
        if (abort_if_error) {
          std::ostringstream errmsg;
          fmt::print(errmsg, "ERROR: Cannot {} file '{}'", open_create, get_filename());
          IOSS_ERROR(errmsg);
        }
      }
      return false;
    }
    return true;
  }

  bool ParallelDatabaseIO::open_input_file(bool write_message, std::string *error_msg,
                                           int *bad_count, bool abort_if_error) const
  {
    int   cpu_word_size = sizeof(double);
    int   io_word_size  = 0;
    float version;

    int mode = exodusMode;
    if (int_byte_size_api() == 8) {
      mode |= EX_ALL_INT64_API;
    }

#if defined EX_DISKLESS
    // Experimental -- in memory read by netcdf library
    if (properties.exists("MEMORY_READ")) {
      mode |= EX_DISKLESS;
    }
#endif

    MPI_Info    info     = MPI_INFO_NULL;
    std::string filename = get_filename();

    // See bug description in thread at
    // https://www.open-mpi.org/community/lists/users/2015/01/26167.php and
    // https://prod.sandia.gov/sierra-trac/ticket/14679
    // Kluge is to set cwd to pathname, open file, then set cwd back to original.
    //
    // Since several different mpi implementations are based on the
    // mpich code which introduced this bug, it has been difficult to
    // create an ifdef'd version of the fix which is only applied to the
    // buggy mpiio code.  Therefore, we always do chdir call.  Maybe in several
    // years, we can remove this code and everything will work...

#if !defined(__IOSS_WINDOWS__)
    Ioss::FileInfo file(filename);
    std::string    path = file.pathname();
    filename            = file.tailname();
    char *current_cwd   = getcwd(nullptr, 0);
    if (!path.empty()) {
      auto success = chdir(path.c_str());
      if (success == -1) {
        if (write_message || error_msg != nullptr) {
          std::ostringstream errmsg;
          fmt::print(errmsg,
                     "ERROR: Directory '{}' does not exist.  Error in filename specification.",
                     path);
          fmt::print(errmsg, "\n");
          if (error_msg != nullptr) {
            *error_msg = errmsg.str();
          }
          if (write_message && myProcessor == 0) {
            fmt::print(Ioss::OUTPUT(), "{}", errmsg.str());
          }
          if (bad_count != nullptr) {
            *bad_count = 1;
          }
          if (abort_if_error) {
            IOSS_ERROR(errmsg);
          }
        }
        return false;
      }
    }
#endif

    bool do_timer = false;
    Ioss::Utils::check_set_bool_property(properties, "IOSS_TIME_FILE_OPEN_CLOSE", do_timer);
    double t_begin = (do_timer ? Ioss::Utils::timer() : 0);

    int app_opt_val = ex_opts(EX_VERBOSE);
    m_nullFilePtr   = ex_open_par(filename.c_str(), EX_READ | mode, &cpu_word_size, &io_word_size,
                                  &version, util().communicator(), info);

    if (do_timer) {
      double t_end    = Ioss::Utils::timer();
      double duration = util().global_minmax(t_end - t_begin, Ioss::ParallelUtils::DO_MAX);
      if (myProcessor == 0) {
        fmt::print(Ioss::DebugOut(), "File Open Time = {}\n", duration);
      }
    }

#if !defined(__IOSS_WINDOWS__)
    if (!path.empty()) {
      chdir(current_cwd);
    }
    std::free(current_cwd);
#endif

    bool is_ok = check_valid_file_ptr(write_message, error_msg, bad_count, abort_if_error);

    if (is_ok) {
      finalize_file_open();
    }
    ex_opts(app_opt_val); // Reset back to what it was.
    return is_ok;
  }

  bool ParallelDatabaseIO::handle_output_file(bool write_message, std::string *error_msg,
                                              int *bad_count, bool overwrite,
                                              bool abort_if_error) const
  {
    // If 'overwrite' is false, we do not want to overwrite or clobber
    // the output file if it already exists since the app might be
    // reading the restart data from this file and then later
    // clobbering it and then writing restart data to the same
    // file. So, for output, we first check whether the file exists
    // and if it it and is writable, assume that we can later create a
    // new or append to existing file.

    // if 'overwrite' is true, then clobber/append

    if (!overwrite) {
      // check if file exists and is writeable. If so, return true.
      // Only need to check on processor 0
      int int_is_ok = 0;
      if (myProcessor == 0) {
        Ioss::FileInfo file(get_filename());
        int_is_ok = file.exists() && file.is_writable() ? 1 : 0;
      }
      util().broadcast(int_is_ok);

      if (int_is_ok == 1) {
        // Note that at this point, we cannot totally guarantee that
        // we will be able to create the file when needed, but we have
        // a pretty good chance.  We can't guarantee creation without
        // creating and the app (or calling function) doesn't want us to overwrite...
        return true;
      }
      // File doesn't exist, so fall through and try to
      // create file since we won't be overwriting anything...
    }

    int   cpu_word_size = sizeof(double);
    int   io_word_size  = 0;
    float version;

    int mode = exodusMode;
    if (int_byte_size_api() == 8) {
      mode |= EX_ALL_INT64_API;
    }

#if defined EX_DISKLESS
    // Experimental -- in memory write by netcdf library
    if (properties.exists("MEMORY_WRITE")) {
      mode |= EX_DISKLESS;
    }
#endif

    MPI_Info info        = MPI_INFO_NULL;
    int      app_opt_val = ex_opts(EX_VERBOSE);
    Ioss::DatabaseIO::openDatabase__();

    std::string filename = get_dwname();

    Ioss::FileInfo file(filename);
#if !defined(__IOSS_WINDOWS__)
    std::string path  = file.pathname();
    filename          = file.tailname();
    char *current_cwd = getcwd(nullptr, 0);
    chdir(path.c_str());
#endif

    bool do_timer = false;
    Ioss::Utils::check_set_bool_property(properties, "IOSS_TIME_FILE_OPEN_CLOSE", do_timer);
    double t_begin = (do_timer ? Ioss::Utils::timer() : 0);

    if (fileExists) {
      m_nullFilePtr = ex_open_par(filename.c_str(), EX_WRITE | mode, &cpu_word_size, &io_word_size,
                                  &version, util().communicator(), info);
    }
    else {
      // If the first write for this file, create it...
      if (int_byte_size_api() == 8) {
        // Check whether client actually wants 4-byte output on db
        // - If they specified INTEGER_SIZE_DB and the size isn't 8,
        //   then don't change mode and use the default 4-byte output.
        if (properties.exists("INTEGER_SIZE_DB")) {
          if (properties.get("INTEGER_SIZE_DB").get_int() == 8) {
            mode |= EX_ALL_INT64_DB;
          }
        }
        else {
          mode |= EX_ALL_INT64_DB;
        }
      }

      // Check whether we are on a NFS filesyste -- composed output is sometimes slow/hangs
      // on NFS
      if (myProcessor == 0) {
        if (file.is_nfs()) {
          fmt::print(
              Ioss::WarnOut(),
              "The database file: '{}'.\n"
              "\tis being written to an NFS filesystem. Some NFS filesystems have difficulty\n"
              "\twith parallel I/O (specifically writes). If you experience slow I/O,\n"
              "\ttry `export OMPI_MCA_fs_ufs_lock_algorithm=1` prior to running or\n"
              "\tnon-composed output or a different filesystem.\n",
              filename);
        }
      }
      m_nullFilePtr = ex_create_par(filename.c_str(), mode, &cpu_word_size, &dbRealWordSize,
                                    util().communicator(), info);
    }

    if (do_timer) {
      double      t_end       = Ioss::Utils::timer();
      double      duration    = util().global_minmax(t_end - t_begin, Ioss::ParallelUtils::DO_MAX);
      std::string open_create = fileExists ? "Open" : "Create";
      if (myProcessor == 0) {
        fmt::print(Ioss::DebugOut(), "File {} Time = {}\n", open_create, duration);
      }
    }

#if !defined(__IOSS_WINDOWS__)
    chdir(current_cwd);
    std::free(current_cwd);
#endif

    bool is_ok = check_valid_file_ptr(write_message, error_msg, bad_count, abort_if_error);

    if (is_ok) {
      ex_set_max_name_length(m_nullFilePtr, maximumNameLength);

      // Check properties handled post-create/open...
      if (properties.exists("COMPRESSION_METHOD")) {
        auto method                    = properties.get("COMPRESSION_METHOD").get_string();
        method                         = Ioss::Utils::lowercase(method);
        ex_compression_type exo_method = EX_COMPRESS_ZLIB;
        if (method == "zlib" || method == "libz" || method == "gzip") {
          exo_method = EX_COMPRESS_ZLIB;
        }
        else if (method == "szip") {
#if !defined(NC_HAS_SZIP_WRITE)
#define NC_HAS_SZIP_WRITE 0
#endif
#if NC_HAS_SZIP_WRITE
          exo_method = EX_COMPRESS_SZIP;
#else
          if (myProcessor == 0) {
            fmt::print(Ioss::WarnOut(), "The NetCDF library does not have SZip compression enabled."
                                        " 'zlib' will be used instead.\n\n");
          }
#endif
        }
        else {
          if (myProcessor == 0) {
            fmt::print(Ioss::WarnOut(),
                       "Unrecognized compression method specified: '{}'."
                       " 'zlib' will be used instead.\n\n",
                       method);
          }
        }
        ex_set_option(m_nullFilePtr, EX_OPT_COMPRESSION_TYPE, exo_method);
      }
      if (properties.exists("COMPRESSION_LEVEL")) {
        int comp_level = properties.get("COMPRESSION_LEVEL").get_int();
        ex_set_option(m_nullFilePtr, EX_OPT_COMPRESSION_LEVEL, comp_level);
      }
      if (properties.exists("COMPRESSION_SHUFFLE")) {
        int shuffle = properties.get("COMPRESSION_SHUFFLE").get_int();
        ex_set_option(m_nullFilePtr, EX_OPT_COMPRESSION_SHUFFLE, shuffle);
      }
    }
    ex_opts(app_opt_val); // Reset back to what it was.
    return is_ok;
  }

  int ParallelDatabaseIO::get_file_pointer() const
  {
    return Ionull::BaseDatabaseIO::get_file_pointer();
  }

  int ParallelDatabaseIO::free_file_pointer() const
  {
    int flag;
    MPI_Initialized(&flag);
    if (flag == 0) {
      std::ostringstream errmsg;
      fmt::print(errmsg, "ERROR: MPI is not initialized.");
      IOSS_ERROR(errmsg);
    }

    // Make sure all file pointers are valid...
    int fp_min = util().global_minmax(m_nullFilePtr, Ioss::ParallelUtils::DO_MIN);
    int fp_max = util().global_minmax(m_nullFilePtr, Ioss::ParallelUtils::DO_MAX);
    if (fp_min != fp_max && fp_min < 0) {
      std::ostringstream errmsg;
      fmt::print(errmsg, "ERROR: Inconsistent file pointer values.");
      IOSS_ERROR(errmsg);
    }
    return Ionull::BaseDatabaseIO::free_file_pointer();
  }

  void ParallelDatabaseIO::read_meta_data__()
  {
    int exoid = get_file_pointer(); // get_file_pointer() must be called first.

    // APPENDING:
    // If parallel (single file, not fpp), we have assumptions
    // that the writing process (ranks, mesh, decomp, vars) is the
    // same for the original run that created this database and
    // for this run which is appending to the database so the
    // defining of the output database should be the same except
    // we don't write anything since it is already there.  We do
    // need the number of steps though...
    if (open_create_behavior() == Ioss::DB_APPEND || dbUsage == Ioss::QUERY_TIMESTEPS_ONLY) {
      get_step_times__();
      return;
    }

    if (int_byte_size_api() == 8) {
      decomp = std::make_unique<DecompositionData<int64_t>>(properties, util().communicator());
    }
    else {
      decomp = std::make_unique<DecompositionData<int>>(properties, util().communicator());
    }
    assert(decomp != nullptr);
    decomp->decompose_model(exoid);

    read_region();
    get_elemblocks();

    get_step_times__();

    get_nodeblocks();
    get_edgeblocks();
    get_faceblocks();

    check_side_topology();

    get_nodesets();
    get_sidesets();
#if 0
    get_edgesets();
    get_facesets();
    get_elemsets();
#endif

    get_commsets();

    // Add assemblies now that all entities should be defined... consistent across processors
    // (metadata)
    get_assemblies();

    get_blobs();

    handle_groups();

    add_region_fields();

    if (!is_input() && open_create_behavior() == Ioss::DB_APPEND) {
      get_map(EX_NODE_BLOCK);
      get_map(EX_ELEM_BLOCK);
    }
  }
} // namespace Ionull

int64_t ParallelDatabaseIO::write_attribute_field(const Ioss::Field          &field,
                                                  const Ioss::GroupingEntity *ge, void *data) const
{
  std::string att_name   = ge->name() + SEP() + field.get_name();
  int64_t     num_entity = ge->entity_count();
  int64_t     offset     = field.get_index();

  assert(offset > 0);
  assert(offset - 1 + field.get_component_count(Ioss::Field::InOut::OUTPUT) <=
         ge->get_property("attribute_count").get_int());

  size_t  proc_offset = ge->get_optional_property("_processor_offset", 0);
  int64_t file_count  = ge->get_optional_property("locally_owned_count", num_entity);

  Ioss::Field::BasicType ioss_type = field.get_type();
  assert(ioss_type == Ioss::Field::REAL || ioss_type == Ioss::Field::INTEGER ||
         ioss_type == Ioss::Field::INT64);

  if (ioss_type == Ioss::Field::INT64) {
    Ioss::Utils::check_int_to_real_overflow(field, (int64_t *)data, num_entity);
  }

  int comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);

  ex_entity_type type = Ionull::map_exodus_type(ge->type());
  if (type == EX_NODAL) {
    for (int i = 0; i < comp_count; i++) {
      std::vector<double> file_data;
      file_data.reserve(file_count);
      check_node_owning_processor_data(nodeOwningProcessor, file_count);
      if (ioss_type == Ioss::Field::REAL) {
        filter_owned_nodes(nodeOwningProcessor, myProcessor, static_cast<double *>(data), file_data,
                           i, comp_count);
      }
      else if (ioss_type == Ioss::Field::INTEGER) {
        filter_owned_nodes(nodeOwningProcessor, myProcessor, static_cast<int *>(data), file_data, i,
                           comp_count);
      }
      else if (ioss_type == Ioss::Field::INT64) {
        filter_owned_nodes(nodeOwningProcessor, myProcessor, static_cast<int64_t *>(data),
                           file_data, i, comp_count);
      }
    }
  }
  else if (type == EX_NODE_SET) {
    for (int i = 0; i < comp_count; i++) {
      std::vector<double> file_data;
      file_data.reserve(file_count);
      if (ioss_type == Ioss::Field::REAL) {
        map_nodeset_data(nodesetOwnedNodes[ge], static_cast<double *>(data), file_data, i,
                         comp_count);
      }
      else if (ioss_type == Ioss::Field::INTEGER) {
        map_nodeset_data(nodesetOwnedNodes[ge], static_cast<int *>(data), file_data, i, comp_count);
      }
      else if (ioss_type == Ioss::Field::INT64) {
        map_nodeset_data(nodesetOwnedNodes[ge], static_cast<int64_t *>(data), file_data, i,
                         comp_count);
      }
    }
  }
  else {
    assert(file_count == num_entity);
    std::vector<double> file_data(file_count);
    for (int i = 0; i < comp_count; i++) {
      if (ioss_type == Ioss::Field::REAL) {
        extract_data(file_data, static_cast<double *>(data), num_entity, i, comp_count);
      }
      else if (ioss_type == Ioss::Field::INTEGER) {
        extract_data(file_data, static_cast<int *>(data), num_entity, i, comp_count);
      }
      else if (ioss_type == Ioss::Field::INT64) {
        extract_data(file_data, static_cast<int64_t *>(data), num_entity, i, comp_count);
      }
    }
  }
  return num_entity;
}

int64_t ParallelDatabaseIO::read_attribute_field(const Ioss::Field          &field,
                                                 const Ioss::GroupingEntity *ge, void *data) const
{
  int64_t num_entity = ge->entity_count();

  int attribute_count = ge->get_property("attribute_count").get_int();

  Ioss::Field::BasicType ioss_type = field.get_type();
  if (ioss_type == Ioss::Field::INTEGER || ioss_type == Ioss::Field::INT64) {
    std::ostringstream errmsg;
    fmt::print(errmsg, "INTERNAL ERROR: Integer attribute fields are not yet handled for read. "
                       "Please report.\n");
    IOSS_ERROR(errmsg);
  }

  std::string    att_name = ge->name() + SEP() + field.get_name();
  ex_entity_type type     = Ionull::map_exodus_type(ge->type());
  int64_t        offset   = field.get_index();
  assert(offset - 1 + field.get_component_count(Ioss::Field::InOut::INPUT) <= attribute_count);
  if (offset == 1 && field.get_component_count(Ioss::Field::InOut::INPUT) == attribute_count) {
    // Read all attributes in one big chunk...
    int ierr = decomp->get_attr(get_file_pointer(), type, id, attribute_count,
                                static_cast<double *>(data));
    if (ierr < 0) {
      Ionull::throw_error(get_file_pointer(), __LINE__, __func__, __FILE__);
    }
  }
  else {
    // Read a subset of the attributes.  If scalar, read one;
    // if higher-order (vector3d, ..) read each component and
    // put into correct location...
    if (field.get_component_count(Ioss::Field::InOut::INPUT) == 1) {
      int ierr =
          decomp->get_one_attr(get_file_pointer(), type, id, offset, static_cast<double *>(data));
      if (ierr < 0) {
        Ionull::throw_error(get_file_pointer(), __LINE__, __func__, __FILE__);
      }
    }
    else {
      // Multi-component...
      // Need a local memory space to read data into and
      // then push that into the user-supplied data block...
      std::vector<double> local_data(num_entity);
      int                 comp_count = field.get_component_count(Ioss::Field::InOut::INPUT);
      double             *rdata      = static_cast<double *>(data);
      for (int i = 0; i < comp_count; i++) {
        int ierr =
            decomp->get_one_attr(get_file_pointer(), type, id, offset + i, local_data.data());
        if (ierr < 0) {
          Ionull::throw_error(get_file_pointer(), __LINE__, __func__, __FILE__);
        }

        size_t k = i;
        for (int64_t j = 0; j < num_entity; j++) {
          rdata[k] = local_data[j];
          k += comp_count;
        }
      }
    }
  }
  return num_entity;
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::Region *reg, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  return Ionull::BaseDatabaseIO::put_field_internal(reg, field, data, data_size);
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::NodeBlock *nb, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  size_t num_to_get = field.verify(data_size);

  size_t proc_offset = nb->get_optional_property("_processor_offset", 0);
  size_t file_count  = nb->get_optional_property("locally_owned_count", num_to_get);

  Ioss::Field::RoleType role = field.get_role();

  if (role == Ioss::Field::MESH) {
    if (field.get_name() == "owning_processor") {
      // Set the nodeOwningProcessor vector for all nodes on this processor.
      // Value is the processor that owns the node.

      // NOTE: The owning_processor field is always int32
      nodeOwningProcessor.reserve(num_to_get);
      int *owned = (int *)data;
      for (size_t i = 0; i < num_to_get; i++) {
        nodeOwningProcessor.push_back(owned[i]);
      }

      // Now create the "implicit local" to "implicit global"
      // map which maps data from its local implicit position
      // to its implicit (1..num_global_node) position in the
      // global file.  This is needed for the global-to-local
      // mapping of element connectivity and nodeset nodelists.
      create_implicit_global_map();
    }

    else if (field.get_name() == "mesh_model_coordinates_x") {
      double             *rdata = static_cast<double *>(data);
      std::vector<double> file_data;
      file_data.reserve(file_count);
      check_node_owning_processor_data(nodeOwningProcessor, file_count);
      filter_owned_nodes(nodeOwningProcessor, myProcessor, rdata, file_data);
    }

    else if (field.get_name() == "mesh_model_coordinates_y") {
      double             *rdata = static_cast<double *>(data);
      std::vector<double> file_data;
      file_data.reserve(file_count);
      check_node_owning_processor_data(nodeOwningProcessor, file_count);
      filter_owned_nodes(nodeOwningProcessor, myProcessor, rdata, file_data);
    }

    else if (field.get_name() == "mesh_model_coordinates_z") {
      double             *rdata = static_cast<double *>(data);
      std::vector<double> file_data;
      file_data.reserve(file_count);
      check_node_owning_processor_data(nodeOwningProcessor, file_count);
      filter_owned_nodes(nodeOwningProcessor, myProcessor, rdata, file_data);
    }

    else if (field.get_name() == "mesh_model_coordinates") {
      // Data required by upper classes store x0, y0, z0, ... xn, yn, zn
      // Data stored in exodusII file is x0, ..., xn, y0, ..., yn, z0, ..., zn
      // so we have to allocate some scratch memory to read in the data
      // and then map into supplied 'data'
      std::vector<double> x;
      std::vector<double> y;
      std::vector<double> z;

      x.reserve(file_count > 0 ? file_count : 1);
      if (spatialDimension > 1) {
        y.reserve(file_count > 0 ? file_count : 1);
      }
      if (spatialDimension == 3) {
        z.reserve(file_count > 0 ? file_count : 1);
      }

      // Cast 'data' to correct size -- double
      double *rdata = static_cast<double *>(data);
      check_node_owning_processor_data(nodeOwningProcessor, file_count);
      filter_owned_nodes(nodeOwningProcessor, myProcessor, rdata, x, 0, spatialDimension);
      if (spatialDimension > 1) {
        filter_owned_nodes(nodeOwningProcessor, myProcessor, rdata, y, 1, spatialDimension);
      }
      if (spatialDimension == 3) {
        filter_owned_nodes(nodeOwningProcessor, myProcessor, rdata, z, 2, spatialDimension);
      }
    }
    else if (field.get_name() == "ids") {
      // The ids coming in are the global ids; their position is the
      // local id -1 (That is, data[0] contains the global id of local
      // node 1)

      // Another 'const-cast' since we are modifying the database just
      // for efficiency; which the client does not see...
      handle_node_ids(data, num_to_get, proc_offset, file_count);
    }
    else if (field.get_name() == "connectivity") {
      // Do nothing, just handles an idiosyncrasy of the GroupingEntity
    }
    else if (field.get_name() == "connectivity_raw") {
      // Do nothing, just handles an idiosyncrasy of the GroupingEntity
    }
    else if (field.get_name() == "node_connectivity_status") {
      // Do nothing, input only field.
    }
    else if (field.get_name() == "implicit_ids") {
      // Do nothing, input only field.
    }
    else {
      return Ioss::Utils::field_warning(nb, field, "mesh output");
    }
  }
  else if (role == Ioss::Field::TRANSIENT) {
    // Check if the specified field exists on this node block.
    // Note that 'higher-order' storage types (e.g. SYM_TENSOR)
    // exist on the database as scalars with the appropriate
    // extensions.

    // Transfer each component of the variable into 'data' and then
    // output.  Need temporary storage area of size 'number of
    // nodes in this block.
    write_nodal_transient_field(field, nb, num_to_get, data);
  }
  else if (role == Ioss::Field::REDUCTION) {
    store_reduction_field(field, nb, data);
  }
  return num_to_get;
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::Blob *blob, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  {
    Ioss::SerializeIO serializeIO__(this);

    size_t num_to_get = field.verify(data_size);
    if (num_to_get > 0) {

      Ioss::Field::RoleType role = field.get_role();

      if (role == Ioss::Field::MESH) {
        if (field.get_name() == "ids") {
          // The ids coming in are the global ids; their position is the
          // local id -1 (That is, data[0] contains the global id of local
          // node 1)
          //          handle_node_ids(data, num_to_get);
        }
        else if (field.get_name() == "connectivity") {
          // Do nothing, just handles an idiosyncrasy of the GroupingEntity
        }
        else if (field.get_name() == "connectivity_raw") {
          // Do nothing, just handles an idiosyncrasy of the GroupingEntity
        }
        else if (field.get_name() == "node_connectivity_status") {
          // Do nothing, input only field.
        }
        else if (field.get_name() == "implicit_ids") {
          // Do nothing, input only field.
        }
        else {
          return Ioss::Utils::field_warning(blob, field, "mesh output");
        }
      }
      else if (role == Ioss::Field::TRANSIENT) {
        // Check if the specified field exists on this node block.
        // Note that 'higher-order' storage types (e.g. SYM_TENSOR)
        // exist on the database as scalars with the appropriate
        // extensions.

        // Transfer each component of the variable into 'data' and then
        // output.  Need temporary storage area of size 'number of
        // nodes in this block.
        write_entity_transient_field(field, blob, num_to_get, data);
      }
      else if (role == Ioss::Field::REDUCTION) {
        store_reduction_field(field, blob, data);
      }
      else if (role == Ioss::Field::ATTRIBUTE) {
        num_to_get = write_attribute_field(field, blob, data);
      }
    }
    return num_to_get;
  }
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::Assembly *assembly,
                                               const Ioss::Field &field, void *data,
                                               size_t data_size) const
{
  {
    Ioss::SerializeIO serializeIO__(this);

    size_t num_to_get = field.verify(data_size);
    if (num_to_get > 0) {

      Ioss::Field::RoleType role = field.get_role();

      if (role == Ioss::Field::MESH) {
        if (field.get_name() == "ids") {
          // The ids coming in are the global ids; their position is the
          // local id -1 (That is, data[0] contains the global id of local
          // node 1)
          //          handle_node_ids(data, num_to_get);
        }
        else if (field.get_name() == "connectivity") {
          // Do nothing, just handles an idiosyncrasy of the GroupingEntity
        }
        else if (field.get_name() == "connectivity_raw") {
          // Do nothing, just handles an idiosyncrasy of the GroupingEntity
        }
        else if (field.get_name() == "node_connectivity_status") {
          // Do nothing, input only field.
        }
        else if (field.get_name() == "implicit_ids") {
          // Do nothing, input only field.
        }
        else {
          return Ioss::Utils::field_warning(assembly, field, "mesh output");
        }
      }
      else if (role == Ioss::Field::TRANSIENT) {
        // Check if the specified field exists on this node block.
        // Note that 'higher-order' storage types (e.g. SYM_TENSOR)
        // exist on the database as scalars with the appropriate
        // extensions.

        // Transfer each component of the variable into 'data' and then
        // output.  Need temporary storage area of size 'number of
        // nodes in this block.
        write_entity_transient_field(field, assembly, num_to_get, data);
      }
      else if (role == Ioss::Field::REDUCTION) {
        store_reduction_field(field, assembly, data);
      }
      else if (role == Ioss::Field::ATTRIBUTE) {
        num_to_get = write_attribute_field(field, assembly, data);
      }
    }
    return num_to_get;
  }
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::ElementBlock *eb,
                                               const Ioss::Field &field, void *data,
                                               size_t data_size) const
{
  size_t num_to_get = field.verify(data_size);

  int ierr = 0;

  // Get the element block id and element count
  int64_t               my_element_count = eb->entity_count();
  Ioss::Field::RoleType role             = field.get_role();

  auto proc_offset = eb->get_optional_property("_processor_offset", 0);
  auto file_count  = eb->get_optional_property("locally_owned_count", num_to_get);

  if (role == Ioss::Field::MESH) {
    // Handle the MESH fields required for an ExodusII file model.
    // (The 'genesis' portion)
    if (field.get_name() == "connectivity") {
      // Map element connectivity from global node id to local node id.
      int element_nodes = eb->topology()->number_nodes();

      // Maps global to local
      nodeMap.reverse_map_data(data, field, num_to_get * element_nodes);

      // Maps local to "global_implicit"
      if (int_byte_size_api() == 4) {
        map_local_to_global_implicit(reinterpret_cast<int *>(data), num_to_get * element_nodes,
                                     nodeGlobalImplicitMap);
      }
      else {
        map_local_to_global_implicit(reinterpret_cast<int64_t *>(data), num_to_get * element_nodes,
                                     nodeGlobalImplicitMap);
      }
    }
    else if (field.get_name() == "connectivity_edge") {
      // Map element connectivity from global edge id to local edge id.
      int element_edges = field.get_component_count(Ioss::Field::InOut::OUTPUT);
      edgeMap.reverse_map_data(data, field, num_to_get * element_edges);
    }
    else if (field.get_name() == "connectivity_face") {
      // Map element connectivity from global face id to local face id.
      int element_faces = field.get_component_count(Ioss::Field::InOut::OUTPUT);
      faceMap.reverse_map_data(data, field, num_to_get * element_faces);
    }
    else if (field.get_name() == "connectivity_raw") {
      // Element connectivity is already in local node id, map local to "global_implicit"
      int element_nodes = eb->topology()->number_nodes();
      if (int_byte_size_api() == 4) {
        map_local_to_global_implicit(reinterpret_cast<int *>(data), num_to_get * element_nodes,
                                     nodeGlobalImplicitMap);
      }
      else {
        map_local_to_global_implicit(reinterpret_cast<int64_t *>(data), num_to_get * element_nodes,
                                     nodeGlobalImplicitMap);
      }
    }
    else if (field.get_name() == "ids") {
      size_t glob_map_offset = eb->get_property("global_map_offset").get_int();
      handle_element_ids(eb, data, num_to_get, glob_map_offset + proc_offset, file_count);
    }
    else if (field.get_name() == "implicit_ids") {
      // Do nothing, input only field.
    }
  }
  else if (role == Ioss::Field::MAP) {
    int comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);
    for (int comp = 0; comp < comp_count; comp++) {
      std::vector<char> component(my_element_count * int_byte_size_api());

      if (int_byte_size_api() == 4) {
        int *data32 = reinterpret_cast<int *>(data);
        int *comp32 = reinterpret_cast<int *>(component.data());

        int index = comp;
        for (int64_t i = 0; i < my_element_count; i++) {
          comp32[i] = data32[index];
          index += comp_count;
        }
      }
      else {
        int64_t *data64 = reinterpret_cast<int64_t *>(data);
        int64_t *comp64 = reinterpret_cast<int64_t *>(component.data());

        int index = comp;
        for (int64_t i = 0; i < my_element_count; i++) {
          comp64[i] = data64[index];
          index += comp_count;
        }
      }
      auto eb_offset =
          eb->get_offset(); // Offset of beginning of the element block elements for this block
      int index =
          -1 * (field.get_index() + comp); // Negative since specifying index, not id to exodus API.
    }
  }
  else if (role == Ioss::Field::ATTRIBUTE) {
    num_to_get = write_attribute_field(field, eb, data);
  }
  else if (role == Ioss::Field::TRANSIENT) {
    // Check if the specified field exists on this element block.
    // Note that 'higher-order' storage types (e.g. SYM_TENSOR)
    // exist on the database as scalars with the appropriate
    // extensions.

    // Transfer each component of the variable into 'data' and then
    // output.  Need temporary storage area of size 'number of
    // elements in this block.
    auto global_entity_count = eb->get_property("global_entity_count").get_int();
    if (global_entity_count > 0) {
      write_entity_transient_field(field, eb, my_element_count, data);
    }
  }
  else if (role == Ioss::Field::REDUCTION) {
    store_reduction_field(field, eb, data);
  }
  return num_to_get;
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::FaceBlock *eb, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  size_t num_to_get = field.verify(data_size);

  int ierr = 0;

  // Get the face block id and face count
  int64_t               my_face_count = eb->entity_count();
  Ioss::Field::RoleType role          = field.get_role();

  if (role == Ioss::Field::MESH) {
    // Handle the MESH fields required for an ExodusII file model.
    // (The 'genesis' portion)
    if (field.get_name() == "connectivity") {
      if (my_face_count > 0) {
        // Map face connectivity from global node id to local node id.
        int face_nodes = eb->topology()->number_nodes();
        nodeMap.reverse_map_data(data, field, num_to_get * face_nodes);
      }
    }
    else if (field.get_name() == "connectivity_edge") {
      if (my_face_count > 0) {
        // Map face connectivity from global edge id to local edge id.
        // Do it in 'data' ...
        int face_edges = field.get_component_count(Ioss::Field::InOut::OUTPUT);
        edgeMap.reverse_map_data(data, field, num_to_get * face_edges);
      }
    }
    else if (field.get_name() == "connectivity_raw") {
      // Do nothing, input only field.
    }
    else if (field.get_name() == "ids") {
      handle_face_ids(eb, data, num_to_get);
    }
    else {
      num_to_get = Ioss::Utils::field_warning(eb, field, "mesh output");
    }
  }
  else if (role == Ioss::Field::ATTRIBUTE) {
    num_to_get = write_attribute_field(field, eb, data);
  }
  else if (role == Ioss::Field::TRANSIENT) {
    // Check if the specified field exists on this face block.
    // Note that 'higher-order' storage types (e.g. SYM_TENSOR)
    // exist on the database as scalars with the appropriate
    // extensions.

    // Transfer each component of the variable into 'data' and then
    // output.  Need temporary storage area of size 'number of
    // faces in this block.
    write_entity_transient_field(field, eb, my_face_count, data);
  }
  else if (role == Ioss::Field::REDUCTION) {
    store_reduction_field(field, eb, data);
  }
  return num_to_get;
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::EdgeBlock *eb, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  size_t num_to_get = field.verify(data_size);

  int ierr = 0;

  // Get the edge block id and edge count
  int64_t               my_edge_count = eb->entity_count();
  Ioss::Field::RoleType role          = field.get_role();

  if (role == Ioss::Field::MESH) {
    // Handle the MESH fields required for an ExodusII file model. (The 'genesis' portion)
    if (field.get_name() == "connectivity") {
      if (my_edge_count > 0) {
        // Map edge connectivity from global node id to local node id.
        int edge_nodes = eb->topology()->number_nodes();
        nodeMap.reverse_map_data(data, field, num_to_get * edge_nodes);
      }
    }
    else if (field.get_name() == "connectivity_raw") {
      // Do nothing, input only field.
    }
    else if (field.get_name() == "ids") {
      handle_edge_ids(eb, data, num_to_get);
    }
    else {
      num_to_get = Ioss::Utils::field_warning(eb, field, "mesh output");
    }
  }
  else if (role == Ioss::Field::ATTRIBUTE) {
    num_to_get = write_attribute_field(field, eb, data);
  }
  else if (role == Ioss::Field::TRANSIENT) {
    // Check if the specified field exists on this edge block.
    // Note that 'higher-order' storage types (e.g. SYM_TENSOR)
    // exist on the database as scalars with the appropriate
    // extensions.

    // Transfer each component of the variable into 'data' and then
    // output.  Need temporary storage area of size 'number of
    // edges in this block.
    write_entity_transient_field(field, eb, my_edge_count, data);
  }
  else if (role == Ioss::Field::REDUCTION) {
    store_reduction_field(field, eb, data);
  }
  return num_to_get;
}

int64_t ParallelDatabaseIO::handle_node_ids(void *ids, int64_t num_to_get, size_t /* offset */,
                                            size_t /*count*/) const
{
  /*!
   * There are two modes we need to support in this routine:
   * 1. Initial definition of node map (local->global) and
   * nodeMap.reverse (global->local).
   * 2. Redefinition of node map via 'reordering' of the original
   * map when the nodes on this processor are the same, but their
   * order is changed (or count because of ghosting)
   *
   * So, there will be two maps the 'nodeMap.map' map is a 'direct lookup'
   * map which maps current local position to global id and the
   * 'nodeMap.reverse' is an associative lookup which maps the
   * global id to 'original local'.  There is also a
   * 'nodeMap.reorder' which is direct lookup and maps current local
   * position to original local.

   * The ids coming in are the global ids; their position is the
   * "local id-1" (That is, data[0] contains the global id of local
   * node 1 in this node block).
   *
   * int local_position = nodeMap.reverse[NodeMap[i+1]]
   * (the nodeMap.map and nodeMap.reverse are 1-based)
   *
   * To determine which map to update on a call to this function, we
   * use the following hueristics:
   * -- If the database state is 'STATE_MODEL:', then update the
   *    'nodeMap.reverse' and 'nodeMap.map'
   *
   * -- If the database state is not STATE_MODEL, then leave the
   *    'nodeMap.reverse' and 'nodeMap.map' alone since they correspond to the
   *    information already written to the database. [May want to add a
   *    STATE_REDEFINE_MODEL]
   *
   * -- In both cases, update the nodeMap.reorder
   *
   * NOTE: The mapping is done on TRANSIENT fields only; MODEL fields
   *       should be in the original order...
   */
  nodeMap.set_size(num_to_get);

  bool in_define = (dbState == Ioss::STATE_MODEL) || (dbState == Ioss::STATE_DEFINE_MODEL);
  if (int_byte_size_api() == 4) {
    nodeMap.set_map(static_cast<int *>(ids), num_to_get, 0, in_define);
  }
  else {
    nodeMap.set_map(static_cast<int64_t *>(ids), num_to_get, 0, in_define);
  }

  nodeMap.set_defined(true);
  return num_to_get;
}

int64_t ParallelDatabaseIO::handle_element_ids(const Ioss::ElementBlock *eb, void *ids,
                                               size_t num_to_get, size_t offset, size_t count) const
{
  if (dbState == Ioss::STATE_MODEL) {
    if (elemGlobalImplicitMap.empty()) {
      elemGlobalImplicitMap.resize(elementCount);
    }
    // Build the implicit_global map used to map an elements
    // local-implicit position to the global-implicit
    // position. Primarily used for sideset elements.  'count'
    // Elements starting at 'eb_offset' map to the global implicit
    // position of 'offset'
    int64_t eb_offset = eb->get_offset();
    for (size_t i = 0; i < count; i++) {
      elemGlobalImplicitMap[eb_offset + i] = offset + i + 1;
    }
    elemGlobalImplicitMapDefined = true;
  }

  elemMap.set_size(elementCount);
  return handle_block_ids(eb, EX_ELEM_MAP, elemMap, ids, num_to_get, offset);
}

int64_t ParallelDatabaseIO::handle_face_ids(const Ioss::FaceBlock *eb, void *ids,
                                            size_t num_to_get) const
{
  faceMap.set_size(faceCount);
  return handle_block_ids(eb, EX_FACE_MAP, faceMap, ids, num_to_get, 0);
}

int64_t ParallelDatabaseIO::handle_edge_ids(const Ioss::EdgeBlock *eb, void *ids,
                                            size_t num_to_get) const
{
  edgeMap.set_size(edgeCount);
  return handle_block_ids(eb, EX_EDGE_MAP, edgeMap, ids, num_to_get, 0);
}

void ParallelDatabaseIO::write_nodal_transient_field(const Ioss::Field     &field,
                                                     const Ioss::NodeBlock *nb, int64_t count,
                                                     void *variables) const
{
  Ioss::Field::BasicType ioss_type = field.get_type();
  assert(ioss_type == Ioss::Field::REAL || ioss_type == Ioss::Field::INTEGER ||
         ioss_type == Ioss::Field::INT64 || ioss_type == Ioss::Field::COMPLEX);

  if (ioss_type == Ioss::Field::INT64) {
    Ioss::Utils::check_int_to_real_overflow(field, (int64_t *)variables, count);
  }

  // Note that if the field's basic type is COMPLEX, then each component of
  // the VariableType is a complex variable consisting of a real and
  // imaginary part.  Since exodus cannot handle complex variables,
  // we have to output a (real and imaginary) X (number of
  // components) fields. For example, if V is a 3d vector of complex
  // data, the data in the 'variables' array are v_x, v.im_x, v_y,
  // v.im_y, v_z, v.im_z which need to be output in six separate
  // exodus fields.  These fields were already defined in
  // "write_results_metadata".

  std::vector<double> temp(count);

  int step = get_current_state();
  step     = get_database_step(step);

  // get number of components, cycle through each component
  // and add suffix to base 'field_name'.  Look up index
  // of this name in 'm_variables[EX_NODE_BLOCK]' map
  int comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);

  int re_im = 1;
  if (ioss_type == Ioss::Field::COMPLEX) {
    re_im = 2;
  }
  for (int complex_comp = 0; complex_comp < re_im; complex_comp++) {
    std::string field_name = field.get_name();
    if (re_im == 2) {
      field_name += complex_suffix[complex_comp];
    }

    for (int i = 0; i < comp_count; i++) {
      std::string var_name = get_component_name(field, Ioss::Field::InOut::OUTPUT, i + 1);

      auto var_iter = m_variables[EX_NODE_BLOCK].find(var_name);
      if (var_iter == m_variables[EX_NODE_BLOCK].end()) {
        std::ostringstream errmsg;
        fmt::print(errmsg, "ERROR: Could not find nodal variable '{}'\n", var_name);
        IOSS_ERROR(errmsg);
      }

      int var_index = var_iter->second;

      size_t begin_offset = (re_im * i) + complex_comp;
      size_t stride       = re_im * comp_count;
      size_t num_out      = 0;

      if (ioss_type == Ioss::Field::REAL || ioss_type == Ioss::Field::COMPLEX) {
        num_out = nodeMap.map_field_to_db_scalar_order(static_cast<double *>(variables), temp,
                                                       begin_offset, count, stride, 0);
      }
      else if (ioss_type == Ioss::Field::INTEGER) {
        num_out = nodeMap.map_field_to_db_scalar_order(static_cast<int *>(variables), temp,
                                                       begin_offset, count, stride, 0);
      }
      else if (ioss_type == Ioss::Field::INT64) {
        num_out = nodeMap.map_field_to_db_scalar_order(static_cast<int64_t *>(variables), temp,
                                                       begin_offset, count, stride, 0);
      }

      if (num_out != static_cast<size_t>(nodeCount)) {
        std::ostringstream errmsg;
        fmt::print(errmsg,
                   "ERROR: Problem outputting nodal variable '{}' with index = {} to file '{}' on "
                   "processor {}\n"
                   "\tShould have output {} values, but instead only output {} values.\n",
                   var_name, var_index, get_filename(), myProcessor, fmt::group_digits(nodeCount),
                   fmt::group_digits(num_out));
        IOSS_ERROR(errmsg);
      }

      // Write the variable...
      size_t proc_offset = nb->get_optional_property("_processor_offset", 0);
      size_t file_count  = nb->get_optional_property("locally_owned_count", num_out);

      check_node_owning_processor_data(nodeOwningProcessor, file_count);
      filter_owned_nodes(nodeOwningProcessor, myProcessor, temp.data());
    }
  }
}

void ParallelDatabaseIO::write_entity_transient_field(const Ioss::Field          &field,
                                                      const Ioss::GroupingEntity *ge, int64_t count,
                                                      void *variables) const
{
  static Ioss::Map    non_element_map; // Used as an empty map for ge->type() != element block.
  std::vector<double> temp(count);

  int step = get_current_state();
  step     = get_database_step(step);

  Ioss::Map *map       = nullptr;
  int64_t    eb_offset = 0;
  if (ge->type() == Ioss::ELEMENTBLOCK) {
    const Ioss::ElementBlock *elb = dynamic_cast<const Ioss::ElementBlock *>(ge);
    Ioss::Utils::check_dynamic_cast(elb);
    eb_offset = elb->get_offset();
    map       = &elemMap;
  }
  else {
    map = &non_element_map;
  }

  Ioss::Field::BasicType ioss_type = field.get_type();
  assert(ioss_type == Ioss::Field::REAL || ioss_type == Ioss::Field::INTEGER ||
         ioss_type == Ioss::Field::INT64 || ioss_type == Ioss::Field::COMPLEX);

  if (ioss_type == Ioss::Field::INT64) {
    Ioss::Utils::check_int_to_real_overflow(field, (int64_t *)variables, count);
  }

  // Note that if the field's basic type is COMPLEX, then each component of
  // the VariableType is a complex variable consisting of a real and
  // imaginary part.  Since exodus cannot handle complex variables,
  // we have to output a (real and imaginary) X (number of
  // components) fields. For example, if V is a 3d vector of complex
  // data, the data in the 'variables' array are v_x, v.im_x, v_y,
  // v.im_y, v_z, v.im_z which need to be output in six separate
  // exodus fields.  These fields were already defined in
  // "write_results_metadata".

  // get number of components, cycle through each component
  // and add suffix to base 'field_name'.  Look up index
  // of this name in 'm_variables[type]' map
  int            comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);
  ex_entity_type type       = Ionull::map_exodus_type(ge->type());

  int re_im = 1;
  if (ioss_type == Ioss::Field::COMPLEX) {
    re_im = 2;
  }
  for (int complex_comp = 0; complex_comp < re_im; complex_comp++) {
    std::string field_name = field.get_name();
    if (re_im == 2) {
      field_name += complex_suffix[complex_comp];
    }

    for (int i = 0; i < comp_count; i++) {
      std::string var_name = get_component_name(field, Ioss::Field::InOut::OUTPUT, i + 1);

      auto var_iter = m_variables[type].find(var_name);
      if (var_iter == m_variables[type].end()) {
        std::ostringstream errmsg;
        fmt::print(errmsg, "ERROR: Could not find field '{}'\n", var_name);
        IOSS_ERROR(errmsg);
      }
      int var_index = var_iter->second;
      assert(var_index > 0);

      // var is a [count,comp,re_im] array;  re_im = 1(real) or 2(complex)
      // beg_offset = (re_im*i)+complex_comp
      // number_values = count
      // stride = re_im*comp_count
      int64_t begin_offset = (re_im * i) + complex_comp;
      int64_t stride       = re_im * comp_count;

      if (ioss_type == Ioss::Field::REAL || ioss_type == Ioss::Field::COMPLEX) {
        map->map_field_to_db_scalar_order(static_cast<double *>(variables), temp, begin_offset,
                                          count, stride, eb_offset);
      }
      else if (ioss_type == Ioss::Field::INTEGER) {
        map->map_field_to_db_scalar_order(static_cast<int *>(variables), temp, begin_offset, count,
                                          stride, eb_offset);
      }
      else if (ioss_type == Ioss::Field::INT64) {
        map->map_field_to_db_scalar_order(static_cast<int64_t *>(variables), temp, begin_offset,
                                          count, stride, eb_offset);
      }
    }
  }
}

int64_t ParallelDatabaseIO::put_Xset_field_internal(const Ioss::EntitySet *ns,
                                                    const Ioss::Field &field, void *data,
                                                    size_t data_size) const
{
  size_t entity_count = ns->entity_count();
  size_t num_to_get   = field.verify(data_size);

  Ioss::Field::RoleType role = field.get_role();

  if (role == Ioss::Field::MESH) {

    void                *out_data = data;
    std::vector<int>     i32data;
    std::vector<int64_t> i64data;
    std::vector<double>  dbldata;

    size_t proc_offset = ns->get_optional_property("_processor_offset", 0);
    size_t file_count  = ns->get_optional_property("locally_owned_count", num_to_get);

    ex_entity_type type = Ionull::map_exodus_type(ns->type());
    if (field.get_name() == "ids" || field.get_name() == "ids_raw") {
      // Map node id from global node id to local node id.
      // Do it in 'data' ...

      if (field.get_name() == "ids") {
        nodeMap.reverse_map_data(data, field, num_to_get);
      }

      if (type == EX_NODE_SET) {
        nodesetOwnedNodes[ns].reserve(file_count);
        if (int_byte_size_api() == 4) {
          i32data.reserve(file_count);
          check_node_owning_processor_data(nodeOwningProcessor, file_count);
          map_nodeset_id_data(nodeOwningProcessor, nodesetOwnedNodes[ns], myProcessor,
                              reinterpret_cast<int *>(data), num_to_get, i32data);
          assert(i32data.size() == file_count);
          // Maps local to "global_implicit"
          map_local_to_global_implicit(i32data.data(), file_count, nodeGlobalImplicitMap);
          out_data = i32data.data();
        }
        else {
          i64data.reserve(file_count);
          check_node_owning_processor_data(nodeOwningProcessor, file_count);
          map_nodeset_id_data(nodeOwningProcessor, nodesetOwnedNodes[ns], myProcessor,
                              reinterpret_cast<int64_t *>(data), num_to_get, i64data);
          assert(i64data.size() == file_count);
          map_local_to_global_implicit(i64data.data(), file_count, nodeGlobalImplicitMap);
          out_data = i64data.data();
        }
      }
    }
    else if (field.get_name() == "orientation") {
    }
    else if (field.get_name() == "distribution_factors") {
    }
    else {
      num_to_get = Ioss::Utils::field_warning(ns, field, "output");
    }
  }
  else if (role == Ioss::Field::TRANSIENT) {
    // Check if the specified field exists on this element block.
    // Note that 'higher-order' storage types (e.g. SYM_TENSOR)
    // exist on the database as scalars with the appropriate
    // extensions.

    // Transfer each component of the variable into 'data' and then
    // output.  Need temporary storage area of size 'number of
    // elements in this block.
    write_entity_transient_field(field, ns, entity_count, data);
  }
  else if (role == Ioss::Field::ATTRIBUTE) {
    num_to_get = write_attribute_field(field, ns, data);
  }
  else if (role == Ioss::Field::REDUCTION) {
    store_reduction_field(field, ns, data);
  }
  return num_to_get;
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::NodeSet *ns, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  return put_Xset_field_internal(ns, field, data, data_size);
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::EdgeSet *ns, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  return put_Xset_field_internal(ns, field, data, data_size);
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::FaceSet *ns, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  return put_Xset_field_internal(ns, field, data, data_size);
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::ElementSet *ns, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  return put_Xset_field_internal(ns, field, data, data_size);
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::SideSet *ss, const Ioss::Field &field,
                                               void * /* data */, size_t data_size) const
{
  size_t num_to_get = field.verify(data_size);
  if (field.get_name() == "ids") {
    // Do nothing, just handles an idiosyncrasy of the GroupingEntity
  }
  else {
    num_to_get = Ioss::Utils::field_warning(ss, field, "output");
  }
  return num_to_get;
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::CommSet * /*cs*/,
                                               const Ioss::Field &field, void * /*data*/,
                                               size_t             data_size) const
{
  size_t num_to_get = field.verify(data_size);
  return num_to_get;
}

int64_t ParallelDatabaseIO::put_field_internal(const Ioss::SideBlock *sb, const Ioss::Field &field,
                                               void *data, size_t data_size) const
{
  size_t num_to_get = field.verify(data_size);

  size_t entity_count = sb->entity_count();
  size_t offset       = sb->get_property("set_offset").get_int();

  Ioss::Field::RoleType role = field.get_role();

  if (role == Ioss::Field::MESH) {
    if (field.get_name() == "side_ids" && sb->name() == "universal_sideset") {
      // The side ids are being stored as the distribution factor
      // field on the universal sideset.  There should be no other
      // side sets that request this field...  (Eventually,
      // create an id field to store this info.

      // Need to convert 'ints' to 'double' for storage on mesh...
      // FIX 64
      if (field.get_type() == Ioss::Field::INTEGER) {
        int                *ids = static_cast<int *>(data);
        std::vector<double> real_ids(num_to_get);
        for (size_t i = 0; i < num_to_get; i++) {
          real_ids[i] = static_cast<double>(ids[i]);
        }
      }
      else {
        int64_t            *ids = static_cast<int64_t *>(data);
        std::vector<double> real_ids(num_to_get);
        for (size_t i = 0; i < num_to_get; i++) {
          real_ids[i] = static_cast<double>(ids[i]);
        }
      }
    }

    else if (field.get_name() == "side_ids") {
    }

    else if (field.get_name() == "ids") {
      // =============================================================
      // NOTE: Code is currently commented out since we have
      // redundant ways of getting the data (element/side) out to
      // the database.  The 'ids' field method relies on a numbering
      // kluge, so for now trying the 'element_side' field...
      // =============================================================
    }

    else if (field.get_name() == "distribution_factors") {
      int    ierr;
      size_t df_offset      = sb->get_property("set_df_offset").get_int();
      size_t proc_df_offset = sb->get_property("processor_df_offset").get_int();
      size_t df_count       = sb->get_property("distribution_factor_count").get_int();
    }
    else if (field.get_name() == "element_side") {
      // In exodusII, the 'side block' is stored as a sideset.  A
      // sideset has a list of elements and a corresponding local
      // element side (1-based)

      // The 'data' passed into the function is stored as a
      // 2D vector e0,f0,e1,f1,... (e=element, f=side)

      // To avoid overwriting the passed in data, we allocate
      // two arrays to store the data for this sideset.

      // The element_id passed in is the global id; we need to
      // output the local id.

      // Allocate space for local side number and element numbers
      // numbers.
      // See if edges or faces...
      size_t side_offset = Ioss::Utils::get_side_offset(sb);

      size_t index = 0;

      size_t proc_offset = sb->get_optional_property("_processor_offset", 0);

      if (field.get_type() == Ioss::Field::INTEGER) {
        Ioss::IntVector element(num_to_get);
        Ioss::IntVector side(num_to_get);
        int            *el_side = reinterpret_cast<int *>(data);

        for (size_t i = 0; i < num_to_get; i++) {
          element[i] = elemMap.global_to_local(el_side[index++]);
          side[i]    = el_side[index++] + side_offset;
        }

        map_local_to_global_implicit(element.data(), num_to_get, elemGlobalImplicitMap);
      }
      else {
        Ioss::Int64Vector element(num_to_get);
        Ioss::Int64Vector side(num_to_get);
        int64_t          *el_side = reinterpret_cast<int64_t *>(data);

        for (size_t i = 0; i < num_to_get; i++) {
          element[i] = elemMap.global_to_local(el_side[index++]);
          side[i]    = el_side[index++] + side_offset;
        }

        map_local_to_global_implicit(element.data(), num_to_get, elemGlobalImplicitMap);
      }
    }
    else if (field.get_name() == "element_side_raw") {
      // In exodusII, the 'side block' is stored as a sideset.  A
      // sideset has a list of elements and a corresponding local
      // element side (1-based)

      // The 'data' passed into the function is stored as a
      // 2D vector e0,f0,e1,f1,... (e=element, f=side)

      // To avoid overwriting the passed in data, we allocate
      // two arrays to store the data for this sideset.

      // The element_id passed in is the local id.

      // See if edges or faces...
      size_t side_offset = Ioss::Utils::get_side_offset(sb);

      size_t index = 0;
      if (field.get_type() == Ioss::Field::INTEGER) {
        Ioss::IntVector element(num_to_get);
        Ioss::IntVector side(num_to_get);
        int            *el_side = reinterpret_cast<int *>(data);

        for (size_t i = 0; i < num_to_get; i++) {
          element[i] = el_side[index++];
          side[i]    = el_side[index++] + side_offset;
        }
      }
      else {
        Ioss::Int64Vector element(num_to_get);
        Ioss::Int64Vector side(num_to_get);
        int64_t          *el_side = reinterpret_cast<int64_t *>(data);

        for (size_t i = 0; i < num_to_get; i++) {
          element[i] = el_side[index++];
          side[i]    = el_side[index++] + side_offset;
        }
      }
    }
    else if (field.get_name() == "connectivity") {
      // Do nothing, just handles an idiosyncrasy of the GroupingEntity
    }
    else if (field.get_name() == "connectivity_raw") {
      // Do nothing, just handles an idiosyncrasy of the GroupingEntity
    }
    else {
      num_to_get = Ioss::Utils::field_warning(sb, field, "output");
    }
  }
  else if (role == Ioss::Field::TRANSIENT) {
    // Check if the specified field exists on this block.
    // Note that 'higher-order' storage types (e.g. SYM_TENSOR)
    // exist on the database as scalars with the appropriate
    // extensions.

    // Transfer each component of the variable into 'data' and then
    // output.  Need temporary storage area of size 'number of
    // entities in this block.
    write_entity_transient_field(field, sb, entity_count, data);
  }
  else if (role == Ioss::Field::ATTRIBUTE) {
    num_to_get = write_attribute_field(field, sb, data);
  }
  else if (role == Ioss::Field::REDUCTION) {
    store_reduction_field(field, sb, data);
  }
  return num_to_get;
}

void ParallelDatabaseIO::write_meta_data(Ioss::IfDatabaseExistsBehavior behavior)
{
  Ioss::Region *region = get_region();
  common_write_meta_data(behavior);

  char the_title[max_line_length + 1];

  // Title...
  if (region->property_exists("title")) {
    std::string title_str = region->get_property("title").get_string();
    Ioss::Utils::copy_string(the_title, title_str);
  }
  else {
    Ioss::Utils::copy_string(the_title, "IOSS Default Output Title");
  }

  bool         file_per_processor = false;
  Ionull::Mesh mesh(spatialDimension, the_title, util(), file_per_processor);
  mesh.populate(region);

  if (behavior != Ioss::DB_APPEND && behavior != Ioss::DB_MODIFY) {
    bool omit_qa = false;
    Ioss::Utils::check_set_bool_property(properties, "OMIT_QA_RECORDS", omit_qa);
    if (!omit_qa) {
      put_qa();
    }

    bool omit_info = false;
    Ioss::Utils::check_set_bool_property(properties, "OMIT_INFO_RECORDS", omit_info);
    if (!omit_info) {
      put_info();
    }

    // Write the metadata to the exodusII file...
    Ionull::Internals data(get_file_pointer(), maximumNameLength, util());
    mesh.comm.outputNemesis = false;

    int ierr = data.write_meta_data(mesh);

    if (ierr < 0) {
      Ionull::throw_error(get_file_pointer(), __LINE__, __func__, __FILE__);
    }
  }

  metaDataWritten = true;

  // Set the processor offset property. Specifies where in the global list, the data from this
  // processor begins...
  update_processor_offset_property(region, mesh);

  if (behavior != Ioss::DB_APPEND && behavior != Ioss::DB_MODIFY) {
    output_node_map();
    output_other_meta_data();
  }
}

void ParallelDatabaseIO::create_implicit_global_map() const
{
  // If the node is locally owned, then its position is basically
  // determined by removing all shared nodes from the list and
  // then compressing the list. This location plus the proc_offset
  // gives its location in the global-implicit file.
  //
  // Do this over in the DecompositionData class since it has
  // several utilities in place for MPI communication.

  DecompositionData<int64_t> compose(Ioss::PropertyManager(), util().communicator());
  int64_t                    locally_owned_count = 0;
  int64_t                    processor_offset    = 0;
  compose.create_implicit_global_map(nodeOwningProcessor, nodeGlobalImplicitMap, nodeMap,
                                     &locally_owned_count, &processor_offset);

  nodeGlobalImplicitMapDefined                = true;
  const Ioss::NodeBlockContainer &node_blocks = get_region()->get_node_blocks();
  if (!node_blocks[0]->property_exists("locally_owned_count")) {
    node_blocks[0]->property_add(Ioss::Property("locally_owned_count", locally_owned_count));
  }
  if (!node_blocks[0]->property_exists("_processor_offset")) {
    node_blocks[0]->property_add(Ioss::Property("_processor_offset", processor_offset));
  }

  output_node_map();
}

void ParallelDatabaseIO::output_node_map() const
{
  // Write the partial nodemap to the database...  This is called
  // two times -- once from create_implicit_global_map() and once
  // from write_meta_data().  It will only output the map if
  // the metadata has been written to the output database AND if
  // the nodeMap.map and nodeGlobalImplicitMap are defined.

  if (metaDataWritten) {
    const Ioss::NodeBlockContainer &node_blocks = get_region()->get_node_blocks();
    if (node_blocks.empty()) {
      return;
    }
    assert(node_blocks[0]->property_exists("_processor_offset"));
    assert(node_blocks[0]->property_exists("locally_owned_count"));
    size_t processor_offset    = node_blocks[0]->get_property("_processor_offset").get_int();
    size_t locally_owned_count = node_blocks[0]->get_property("locally_owned_count").get_int();

    if (nodeMap.defined() && nodeGlobalImplicitMapDefined) {
      std::vector<int64_t> file_ids;
      file_ids.reserve(locally_owned_count);
      check_node_owning_processor_data(nodeOwningProcessor, locally_owned_count);
      filter_owned_nodes(nodeOwningProcessor, myProcessor, &nodeMap.map()[1], file_ids);
    }
  }
}

void ParallelDatabaseIO::check_valid_values() const
{
  std::vector<int64_t> counts{nodeCount, elementCount, m_groupCount[EX_ELEM_BLOCK]};
  std::vector<int64_t> all_counts;
  util().all_gather(counts, all_counts);
  // Get minimum value in `all_counts`. If >0, then don't need to check further...
  auto min_val = *std::min_element(all_counts.begin(), all_counts.end());

  if (myProcessor == 0) {
    size_t proc_count = all_counts.size() / 3;

    if (min_val < 0) {
      static std::array<std::string, 3> label{"node", "element", "element block"};
      // Error on one or more of the counts...
      for (size_t j = 0; j < 3; j++) {
        std::vector<size_t> bad_proc;
        for (size_t i = 0; i < proc_count; i++) {
          if (all_counts[3 * i + j] < 0) {
            bad_proc.push_back(i);
          }
        }

        if (!bad_proc.empty()) {
          std::ostringstream errmsg;
          fmt::print(errmsg, "ERROR: Negative {} count on {} processor{}:\n\t{}\n\n", label[j],
                     bad_proc.size(), bad_proc.size() > 1 ? "s" : "",
                     Ioss::Utils::format_id_list(bad_proc, ":"));
          IOSS_ERROR(errmsg);
        }
      }
    }

    // Now check for warning (count == 0)
    if (min_val <= 0) {
      static std::array<std::string, 3> label{"nodes or elements", "elements", "element blocks"};
      // Possible warning on one or more of the counts...
      // Note that it is possible to have nodes on a processor with no elements,
      // but not possible to have elements if no nodes...
      for (size_t j = 0; j < 3; j++) {
        std::vector<size_t> bad_proc;
        for (size_t i = 0; i < proc_count; i++) {
          if (all_counts[3 * i + j] == 0) {
            bad_proc.push_back(i);
          }
        }

        if (!bad_proc.empty()) {
          fmt::print(Ioss::WarnOut(), "No {} on processor{}:\n\t{}\n\n", label[j],
                     bad_proc.size() > 1 ? "s" : "", Ioss::Utils::format_id_list(bad_proc, ":"));
          if (j == 0) {
            break;
          }
        }
      }
    }
  }
  else { // All other processors; need to abort if negative count
    if (min_val < 0) {
      std::ostringstream errmsg;
      IOSS_ERROR(errmsg);
    }
  }
}
} // namespace Ionull
#else
IOSS_MAYBE_UNUSED const char ioss_exodus_parallel_database_unused_symbol_dummy = '\0';
#endif
