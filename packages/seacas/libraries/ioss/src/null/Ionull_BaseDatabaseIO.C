// Copyright(C) 1999-2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_CodeTypes.h>
#include <Ioss_ElementTopology.h>
#include <Ioss_FileInfo.h>
#include <Ioss_IOFactory.h>
#include <Ioss_ParallelUtils.h>
#include <Ioss_SurfaceSplit.h>
#include <Ioss_Utils.h>
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cfloat>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <fmt/ostream.h>
#include <functional>
#include <iostream>
#include <map>
#include <null/Ionull_BaseDatabaseIO.h>
#include <null/Ionull_Utils.h>
#include <set>
#include <string>
#include <tokenize.h>
#include <utility>
#include <vector>

#include "Ioss_Assembly.h"
#include "Ioss_Blob.h"
#include "Ioss_CommSet.h"
#include "Ioss_CoordinateFrame.h"
#include "Ioss_DBUsage.h"
#include "Ioss_DatabaseIO.h"
#include "Ioss_EdgeBlock.h"
#include "Ioss_EdgeSet.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ElementSet.h"
#include "Ioss_EntityBlock.h"
#include "Ioss_EntitySet.h"
#include "Ioss_EntityType.h"
#include "Ioss_FaceBlock.h"
#include "Ioss_FaceSet.h"
#include "Ioss_Field.h"
#include "Ioss_GroupingEntity.h"
#include "Ioss_Map.h"
#include "Ioss_NodeBlock.h"
#include "Ioss_NodeSet.h"
#include "Ioss_Property.h"
#include "Ioss_Region.h"
#include "Ioss_SideBlock.h"
#include "Ioss_SideSet.h"
#include "Ioss_SmartAssert.h"
#include "Ioss_State.h"
#include "Ioss_VariableType.h"

#include "Ionull_Utils.h"

// Transitioning from treating global variables as Ioss::Field::TRANSIENT
// to Ioss::Field::REDUCTION.  To get the old behavior, define the value
// below to '1'.
#define GLOBALS_ARE_TRANSIENT 0

// ========================================================================
// Static internal helper functions
// ========================================================================
namespace {
  static bool sixty_four_bit_message_output = false;

  const size_t max_line_length = MAX_LINE_LENGTH;

  void check_attribute_index_order(Ioss::GroupingEntity *block);

  void insert_sort_and_unique(const std::vector<std::string> &src, std::vector<std::string> &dest);

} // namespace

namespace Ionull {
  BaseDatabaseIO::BaseDatabaseIO(Ioss::Region *region, const std::string &filename,
                                 Ioss::DatabaseUsage db_usage, Ioss_MPI_Comm communicator,
                                 const Ioss::PropertyManager &props)
      : Ioss::DatabaseIO(region, filename, db_usage, communicator, props)
  {
    // A history file is only written on processor 0...
    if (db_usage == Ioss::WRITE_HISTORY) {
      isParallel = false;
    }

    timeLastFlush = time(nullptr);
    dbState       = Ioss::STATE_UNKNOWN;
  }

  void BaseDatabaseIO::set_int_byte_size_api(Ioss::DataSize size) const
  {
    dbIntSizeAPI = size; // mutable
  }

  // Returns byte size of integers stored on the database...
  int BaseDatabaseIO::int_byte_size_db() const { return 8; }

  // common
  BaseDatabaseIO::~BaseDatabaseIO()
  {
    try {
      free_file_pointer();
    }
    catch (...) {
    }
  }

  // common
  unsigned BaseDatabaseIO::entity_field_support() const
  {
    return Ioss::NODEBLOCK | Ioss::EDGEBLOCK | Ioss::FACEBLOCK | Ioss::ELEMENTBLOCK |
           Ioss::NODESET | Ioss::EDGESET | Ioss::FACESET | Ioss::ELEMENTSET | Ioss::SIDESET |
           Ioss::SIDEBLOCK | Ioss::REGION | Ioss::SUPERELEMENT;
  }

  // common
  int BaseDatabaseIO::get_file_pointer() const { return m_nullFilePtr; }

  int BaseDatabaseIO::free_file_pointer() const
  {
    if (m_nullFilePtr != -1) {
      bool do_timer = false;
      if (isParallel) {
        Ioss::Utils::check_set_bool_property(properties, "IOSS_TIME_FILE_OPEN_CLOSE", do_timer);
      }
      double t_begin = (do_timer ? Ioss::Utils::timer() : 0);

      closeDW();
      if (do_timer && isParallel) {
        double t_end    = Ioss::Utils::timer();
        double duration = util().global_minmax(t_end - t_begin, Ioss::ParallelUtils::DO_MAX);
        if (myProcessor == 0) {
          fmt::print(Ioss::DebugOut(), "File Close Time = {}\n", duration);
        }
      }
    }
    m_nullFilePtr = -1;

    return m_nullFilePtr;
  }

  bool BaseDatabaseIO::ok__(bool write_message, std::string *error_message, int *bad_count) const
  {
    // For input, we try to open the existing file.

    // For output, we do not want to overwrite or clobber the output
    // file if it already exists since the app might be reading the restart
    // data from this file and then later clobbering it and then writing
    // restart data to the same file. So, for output, we first check
    // whether the file exists and if it it and is writable, assume
    // that we can later create a new or append to existing file.

    // Returns the number of processors on which this file is *NOT* ok in 'bad_count' if not null.
    // Will return 'true' only if file ok on all processors.
    return true;
  }

  void BaseDatabaseIO::finalize_file_open() const {}

  // common
  void BaseDatabaseIO::put_qa()
  {
    struct qa_element
    {
      char *qa_record[1][4];
    };

    size_t num_qa_records = qaRecords.size() / 4;

    if (using_parallel_io() && myProcessor != 0) {}
    else {
      auto *qa = new qa_element[num_qa_records + 1];
      for (size_t i = 0; i < num_qa_records + 1; i++) {
        for (int j = 0; j < 4; j++) {
          qa[i].qa_record[0][j] = new char[MAX_STR_LENGTH + 1];
        }
      }

      {
        int j = 0;
        for (size_t i = 0; i < num_qa_records; i++) {
          Ioss::Utils::copy_string(qa[i].qa_record[0][0], qaRecords[j++], MAX_STR_LENGTH + 1);
          Ioss::Utils::copy_string(qa[i].qa_record[0][1], qaRecords[j++], MAX_STR_LENGTH + 1);
          Ioss::Utils::copy_string(qa[i].qa_record[0][2], qaRecords[j++], MAX_STR_LENGTH + 1);
          Ioss::Utils::copy_string(qa[i].qa_record[0][3], qaRecords[j++], MAX_STR_LENGTH + 1);
        }
      }

      Ioss::Utils::time_and_date(qa[num_qa_records].qa_record[0][3],
                                 qa[num_qa_records].qa_record[0][2], MAX_STR_LENGTH);

      std::string codename = "unknown";
      std::string version  = "unknown";

      if (get_region()->property_exists("code_name")) {
        codename = get_region()->get_property("code_name").get_string();
      }
      if (get_region()->property_exists("code_version")) {
        version = get_region()->get_property("code_version").get_string();
      }

      Ioss::Utils::copy_string(qa[num_qa_records].qa_record[0][0], codename, MAX_STR_LENGTH + 1);
      Ioss::Utils::copy_string(qa[num_qa_records].qa_record[0][1], version, MAX_STR_LENGTH + 1);

      for (size_t i = 0; i < num_qa_records + 1; i++) {
        for (int j = 0; j < 4; j++) {
          delete[] qa[i].qa_record[0][j];
        }
      }
      delete[] qa;
    }
  }

  // common
  void BaseDatabaseIO::put_info()
  {
    int    total_lines = 0;
    char **info        = nullptr;

    if (!using_parallel_io() || myProcessor == 0) {
      // dump info records, include the product_registry
      // See if the input file was specified as a property on the database...
      std::vector<std::string> input_lines;
      if (get_region()->property_exists("input_file_name")) {
        std::string filename = get_region()->get_property("input_file_name").get_string();
        // Determine size of input file so can embed it in info records...
        Ioss::Utils::input_file(filename, &input_lines, max_line_length);
      }

      // Get configuration information for IOSS library.
      // Split into strings and remove empty lines...
      std::string config = Ioss::IOFactory::show_configuration();
      std::replace(std::begin(config), std::end(config), '\t', ' ');
      auto lines = Ioss::tokenize(config, "\n");
      lines.erase(std::remove_if(lines.begin(), lines.end(),
                                 [](const std::string &line) { return line == ""; }),
                  lines.end());

      // See if the client added any "information_records"
      size_t info_rec_size = informationRecords.size();
      size_t in_lines      = input_lines.size();
      size_t qa_lines      = 1; // Platform info
      size_t config_lines  = lines.size();

      total_lines = in_lines + qa_lines + info_rec_size + config_lines;

      // 'total_lines' pointers to char buffers
      info = Ioss::Utils::get_name_array(total_lines, max_line_length);

      int i = 0;
      Ioss::Utils::copy_string(info[i++], Ioss::Utils::platform_information(), max_line_length + 1);

      // Copy input file lines into 'info' array...
      for (size_t j = 0; j < input_lines.size(); j++, i++) {
        Ioss::Utils::copy_string(info[i], input_lines[j], max_line_length + 1);
      }

      // Copy "information_records" property data ...
      for (size_t j = 0; j < informationRecords.size(); j++, i++) {
        Ioss::Utils::copy_string(info[i], informationRecords[j], max_line_length + 1);
      }

      for (size_t j = 0; j < lines.size(); j++, i++) {
        Ioss::Utils::copy_string(info[i], lines[j], max_line_length + 1);
      }
    }

    if (using_parallel_io()) {
      util().broadcast(total_lines);
    }

    int ierr = 0;
    if (!using_parallel_io() || myProcessor == 0) {
      Ioss::Utils::delete_name_array(info, total_lines);
    }
    else {
    }
    if (ierr < 0) {
      Ionull::throw_error(get_file_pointer(), __LINE__, __func__, __FILE__);
    }
  }

  // common
  int BaseDatabaseIO::get_current_state() const
  {
    int step = get_region()->get_current_state();

    if (step <= 0) {
      std::ostringstream errmsg;
      fmt::print(errmsg,
                 "ERROR: No currently active state.  The calling code must call "
                 "Ioss::Region::begin_state(int step)\n"
                 "       to set the database timestep from which to read the transient data.\n"
                 "       [{}]\n",
                 get_filename());
      IOSS_ERROR(errmsg);
    }
    return step;
  }

  size_t BaseDatabaseIO::handle_block_ids(const Ioss::EntityBlock *eb, ex_entity_type map_type,
                                          Ioss::Map &entity_map, void *ids, size_t num_to_get,
                                          size_t offset) const
  {
    /*!
     * NOTE: "element" is generic for "element", "face", or "edge"
     *
     * There are two modes we need to support in this routine:
     * 1. Initial definition of element map (local->global) and
     * elemMap.reverse (global->local).
     * 2. Redefinition of element map via 'reordering' of the original
     * map when the elements on this processor are the same, but their
     * order is changed.
     *
     * So, there will be two maps the 'elemMap.map' map is a 'direct lookup'
     * map which maps current local position to global id and the
     * 'elemMap.reverse' is an associative lookup which maps the
     * global id to 'original local'.  There is also a
     * 'elemMap.reorder' which is direct lookup and maps current local
     * position to original local.

     * The ids coming in are the global ids; their position is the
     * local id -1 (That is, data[0] contains the global id of local
     * element 1 in this element block).  The 'model-local' id is
     * given by eb_offset + 1 + position:
     *
     * int local_position = elemMap.reverse[ElementMap[i+1]]
     * (the elemMap.map and elemMap.reverse are 1-based)
     *
     * But, this assumes 1..numel elements are being output at the same
     * time; we are actually outputting a blocks worth of elements at a
     * time, so we need to consider the block offsets.
     * So... local-in-block position 'i' is index 'eb_offset+i' in
     * 'elemMap.map' and the 'local_position' within the element
     * blocks data arrays is 'local_position-eb_offset'.  With this, the
     * position within the data array of this element block is:
     *
     * int eb_position =
     * elemMap.reverse[elemMap.map[eb_offset+i+1]]-eb_offset-1
     *
     * To determine which map to update on a call to this function, we
     * use the following hueristics:
     * -- If the database state is 'Ioss::STATE_MODEL:', then update the
     *    'elemMap.reverse'.
     * -- If the database state is not Ioss::STATE_MODEL, then leave
     *    the 'elemMap.reverse' alone since it corresponds to the
     *    information already written to the database. [May want to add
     *    a Ioss::STATE_REDEFINE_MODEL]
     * -- Always update elemMap.map to match the passed in 'ids'
     *    array.
     *
     * NOTE: the maps are built an element block at a time...
     * NOTE: The mapping is done on TRANSIENT fields only; MODEL fields
     *       should be in the original order...
     */

    // Overwrite this portion of the 'elemMap.map', but keep other
    // parts as they were.  We are adding elements starting at position
    // 'eb_offset+offset' and ending at
    // 'eb_offset+offset+num_to_get'. If the entire block is being
    // processed, this reduces to the range 'eb_offset..eb_offset+my_element_count'

    bool    in_define = (dbState == Ioss::STATE_MODEL) || (dbState == Ioss::STATE_DEFINE_MODEL);
    int64_t eb_offset = eb->get_offset();
    if (int_byte_size_api() == 4) {
      entity_map.set_map(static_cast<int *>(ids), num_to_get, eb_offset, in_define);
    }
    else {
      entity_map.set_map(static_cast<int64_t *>(ids), num_to_get, eb_offset, in_define);
    }

    // Now, if the state is Ioss::STATE_MODEL, output this portion of
    // the entity number map...
    if (in_define) {}
    return num_to_get;
  }

  // common
  int64_t BaseDatabaseIO::put_field_internal(const Ioss::Region * /* region */,
                                             const Ioss::Field &field, void *data,
                                             size_t data_size) const
  {
    // For now, assume that all TRANSIENT fields on a region
    // are REDUCTION fields (1 value).  We need to gather these
    // and output them all at one time.  The storage location is a
    // 'globalVariables' array
    {
      Ioss::Field::RoleType role       = field.get_role();
      size_t                num_to_get = field.verify(data_size);

      if ((role == Ioss::Field::TRANSIENT || role == Ioss::Field::REDUCTION) && num_to_get == 1) {
        store_reduction_field(field, get_region(), data);
      }
      else if (num_to_get != 1) {
        // There should have been a warning/error message printed to the
        // log file earlier for this, so we won't print anything else
        // here since it would be printed for each and every timestep....
        ;
      }
      else {
        std::ostringstream errmsg;
        fmt::print(
            errmsg,
            "ERROR: The variable named '{}' is of the wrong type. A region variable must be of type"
            " TRANSIENT or REDUCTION.\n"
            "This is probably an internal error; please notify gdsjaar@sandia.gov",
            field.get_name());
        IOSS_ERROR(errmsg);
      }
      return num_to_get;
    }
  }

  namespace {
    // common
    template <typename T>
    void generate_block_truth_table(const VariableNameMap &variables, Ioss::IntVector &truth_table,
                                    std::vector<T *> &blocks, char field_suffix_separator)
    {
      size_t block_count = blocks.size();
      size_t var_count   = variables.size();

      if (var_count == 0 || block_count == 0) {
        return;
      }

      truth_table.resize(block_count * var_count);

      // Fill in the truth table.  It is conceptually a two-dimensional array of
      // the form 'array[num_blocks][num_element_var]'.  In C++,
      // the values for the first block are first, followed by
      // next block, ...
      size_t offset = 0;
      for (const auto &block : blocks) {
        // Get names of all transient and reduction fields...
        Ioss::NameList results_fields = block->field_describe(Ioss::Field::TRANSIENT);
        block->field_describe(Ioss::Field::REDUCTION, &results_fields);

        for (const auto &fn : results_fields) {
          Ioss::Field            field     = block->get_field(fn);
          Ioss::Field::BasicType ioss_type = field.get_type();

          int re_im = 1;
          if (ioss_type == Ioss::Field::COMPLEX) {
            re_im = 2;
          }
          for (int complex_comp = 0; complex_comp < re_im; complex_comp++) {
            for (int i = 1; i <= field.get_component_count(Ioss::Field::InOut::INPUT); i++) {
              std::string var_string =
                  field.get_component_name(i, Ioss::Field::InOut::INPUT, field_suffix_separator);
              // Find position of 'var_string' in 'variables'
              auto VN = variables.find(var_string);
              if (VN != variables.end()) {
                // Index '(*VN).second' is 1-based...
                truth_table[offset + (*VN).second - 1] = 1;
              }
            }
          }
        }
        offset += var_count;
      }
      assert(offset == var_count * block_count);
    }
  } // namespace
  // common
  void BaseDatabaseIO::store_reduction_field(const Ioss::Field          &field,
                                             const Ioss::GroupingEntity *ge, void *variables) const
  {
  }

  // common
  void BaseDatabaseIO::write_reduction_fields() const {}

  // common
  bool BaseDatabaseIO::begin__(Ioss::State state)
  {
    dbState = state;
    return true;
  }

  // common
  bool BaseDatabaseIO::end__(Ioss::State state)
  {
    // Transitioning out of state 'state'
    assert(state == dbState);
    switch (state) {
    case Ioss::STATE_DEFINE_MODEL:
      if (!is_input()) {
        write_meta_data(open_create_behavior());
      }
      break;
    case Ioss::STATE_DEFINE_TRANSIENT:
      if (!is_input()) {
        write_results_metadata(true, open_create_behavior());
      }
      break;
    default: // ignore everything else...
      break;
    }

    {
      if (!is_input()) {}
      dbState = Ioss::STATE_UNKNOWN;
    }

    return true;
  }

  bool BaseDatabaseIO::begin_state__(int state, double time) { return true; }

  // common
  bool BaseDatabaseIO::end_state__(int state, double time)
  {
    if (!is_input()) {
      write_reduction_fields();
      time /= timeScaleFactor;
      finalize_write(state, time);
      if (minimizeOpenFiles) {
        free_file_pointer();
      }
    }
    return true;
  }

  // common
  void BaseDatabaseIO::write_results_metadata(bool                           gather_data,
                                              Ioss::IfDatabaseExistsBehavior behavior)
  {
  }

  // common
  template <typename T>
  void BaseDatabaseIO::internal_gather_results_metadata(ex_entity_type   type,
                                                        std::vector<T *> entities)
  {
  }

  // common
  int BaseDatabaseIO::gather_names(VariableNameMap &variables, const Ioss::GroupingEntity *ge,
                                   int index, bool reduction)
  {
    return 0;
  }

  // common
  void BaseDatabaseIO::output_results_names(ex_entity_type type, VariableNameMap &variables,
                                            bool reduction) const
  {
    bool lowercase_names =
        (properties.exists("VARIABLE_NAME_CASE") &&
         Ioss::Utils::lowercase(properties.get("VARIABLE_NAME_CASE").get_string()) == "lower");
    bool uppercase_names =
        (properties.exists("VARIABLE_NAME_CASE") &&
         Ioss::Utils::lowercase(properties.get("VARIABLE_NAME_CASE").get_string()) == "upper");

    size_t var_count = variables.size();

    if (var_count > 0) {
      size_t name_length = 0;
      // Push into a char** array...
      std::vector<char *>      var_names(var_count);
      std::vector<std::string> variable_names(var_count);
      for (const auto &variable : variables) {
        size_t index = variable.second;
        assert(index > 0 && index <= var_count);
        variable_names[index - 1] = variable.first;
        if (uppercase_names) {
          variable_names[index - 1] = Ioss::Utils::uppercase(variable_names[index - 1]);
        }
        else if (lowercase_names) {
          variable_names[index - 1] = Ioss::Utils::lowercase(variable_names[index - 1]);
        }
        var_names[index - 1] = const_cast<char *>(variable_names[index - 1].c_str());
        size_t name_len      = variable_names[index - 1].length();
        name_length          = name_len > name_length ? name_len : name_length;
      }

      // Should handle this automatically, but by the time we get to defining transient fields, we
      // have already created the output database and populated the set/block names. At this point,
      // it is too late to change the size of the names stored on the output database... (I think...
      // try changing DIM_STR_NAME value and see if works...)
      if (name_length > static_cast<size_t>(maximumNameLength)) {
        if (myProcessor == 0) {
          fmt::print(Ioss::WarnOut(),
                     "There are variables names whose name length ({0}) exceeds the current "
                     "maximum name length ({1})\n         set for this database ({2}).\n"
                     "         You should either reduce the length of the variable name, or "
                     "set the 'MAXIMUM_NAME_LENGTH' property\n"
                     "         to at least {0}.\n         Contact gdsjaar@sandia.gov for more "
                     "information.\n\n",
                     name_length, maximumNameLength, get_filename());
        }
      }
    }
  }

  // common
  // Handle special output time requests -- primarily restart (cycle, overwrite)
  // Given the global region step, return the step on the database...
  int BaseDatabaseIO::get_database_step(int global_step) const
  {
    if (get_file_per_state()) {
      return 1;
    }

    assert(overlayCount >= 0 && cycleCount >= 0);
    if (overlayCount == 0 && cycleCount == 0) {
      return global_step;
    }

    int local_step = global_step - 1;
    local_step /= (overlayCount + 1);
    if (cycleCount > 0) {
      local_step %= cycleCount;
    }
    return local_step + 1;
  }

  // common
  void BaseDatabaseIO::flush_database__() const {}

  void BaseDatabaseIO::finalize_write(int state, double sim_time)
  {
    // Attempt to ensure that all data written up to this point has
    // actually made it out to disk.  We also write a special attribute
    // to the file to indicate that the current timestep should be
    // complete on the disk.
    // The attribute is a GLOBAL attribute named "last_written_time"
    // which is a double value which can be compared to the values in
    // the time array to make sure they match.  If they don't, then
    // hopefully the "last_written_time" is smaller than the time
    // array value and indicates that the last step is corrupt.

    // Flush the files buffer to disk...
    // If a history file, then only flush if there is more
    // than 10 seconds since the last flush to avoid
    // the flush eating up cpu time for small fast jobs...
    // NOTE: If decide to do this on all files, need to sync across
    // processors to make sure they all flush at same time.

    // GDS: 2011/03/30 -- Use for all non-parallel files, but shorten
    // time for non history files.  Assume that can afford to lose ~10
    // seconds worth of data...  (Flush was taking long time on some
    // /scratch filesystems at SNL for short regression tests with
    // lots of steps)
    // GDS: 2011/07/27 -- shorten from 90 to 10.  Developers running
    // small jobs were not able to view output until job
    // finished. Hopefully the netcdf no-fsync fix along with this fix
    // results in negligible impact on runtime with more syncs.

    // Need to be able to handle a flushInterval == 1 to force flush
    // every time step even in a serial run.
    // The default setting for flushInterval is 1, but in the past,
    // it was not checked for serial runs.  Now, set the default to -1
    // and if that is the value and serial, then do the time-based
    // check; otherwise, use flushInterval setting...

    bool do_flush = true;
    if (flushInterval == 1) {
      do_flush = true;
    }
    else if (flushInterval == 0) {
      do_flush = false;
    }
    else if (dbUsage == Ioss::WRITE_HISTORY || !isParallel) {
      assert(myProcessor == 0);
      time_t cur_time = time(nullptr);
      if (cur_time - timeLastFlush >= 10) {
        timeLastFlush = cur_time;
        do_flush      = true;
      }
      else {
        do_flush = false;
      }
    }

    if (!do_flush && flushInterval > 0) {
      if (state % flushInterval == 0) {
        do_flush = true;
      }
    }

    if (do_flush) {
      flush_database__();
    }
  }

  void BaseDatabaseIO::common_write_meta_data(Ioss::IfDatabaseExistsBehavior behavior)
  {
    Ioss::Region *region = get_region();

    // Verify that exodus supports the mesh_type...
    if (region->mesh_type() != Ioss::MeshType::UNSTRUCTURED) {
      std::ostringstream errmsg;
      fmt::print(errmsg,
                 "ERROR: The mesh type is '{}' which Exodus does not support.\n"
                 "       Only 'Unstructured' is supported at this time.\n",
                 region->mesh_type_string());
      IOSS_ERROR(errmsg);
    }

    const Ioss::NodeBlockContainer &node_blocks = region->get_node_blocks();
    assert(node_blocks.size() <= 1);
    if (!node_blocks.empty()) {
      nodeCount        = node_blocks[0]->entity_count();
      spatialDimension = node_blocks[0]->get_property("component_degree").get_int();
    }
    else {
      spatialDimension = 1;
    }

    // Assemblies --
    {
      const auto &assemblies = region->get_assemblies();
      if (behavior != Ioss::DB_MODIFY) {
        // Set ids of all entities that have "id" property...
      }
      m_groupCount[EX_ASSEMBLY] = assemblies.size();
    }

    // Blobs --
    {
      const auto &blobs = region->get_blobs();
      // Set ids of all entities that have "id" property...
      m_groupCount[EX_BLOB] = blobs.size();
    }

    // Edge Blocks --
    {
      const Ioss::EdgeBlockContainer &edge_blocks = region->get_edge_blocks();
      assert(Ioss::Utils::check_block_order(edge_blocks));
      // Set ids of all entities that have "id" property...
      if (behavior != Ioss::DB_MODIFY) {

        edgeCount = 0;
        for (const auto &edge_block : edge_blocks) {
          edgeCount += edge_block->entity_count();
        }
      }
      m_groupCount[EX_EDGE_BLOCK] = edge_blocks.size();
    }

    // Face Blocks --
    {
      const Ioss::FaceBlockContainer &face_blocks = region->get_face_blocks();
      assert(Ioss::Utils::check_block_order(face_blocks));
      // Set ids of all entities that have "id" property...
      if (behavior != Ioss::DB_MODIFY) {
        faceCount = 0;
        for (auto &face_block : face_blocks) {
          faceCount += face_block->entity_count();
          // Set ids of all entities that do not have "id" property...
        }
      }
      m_groupCount[EX_FACE_BLOCK] = face_blocks.size();
    }

    // Element Blocks --
    {
      const Ioss::ElementBlockContainer &element_blocks = region->get_element_blocks();
      assert(Ioss::Utils::check_block_order(element_blocks));
      elementCount = 0;
      Ioss::Int64Vector element_counts;
      element_counts.reserve(element_blocks.size());
      for (const auto &element_block : element_blocks) {
        elementCount += element_block->entity_count();
        element_counts.push_back(element_block->entity_count());
      }
      m_groupCount[EX_ELEM_BLOCK] = element_blocks.size();

      if (isParallel) {
        // Set "global_entity_count" property on all blocks.
        // Used to skip output on "globally" empty blocks.
        Ioss::Int64Vector global_counts(element_counts.size());
        util().global_count(element_counts, global_counts);
        size_t idx = 0;
        for (const auto &element_block : element_blocks) {
          element_block->property_add(Ioss::Property("global_entity_count", global_counts[idx++]));
        }
      }
    }

    // NodeSets ...
    {
      const Ioss::NodeSetContainer &nodesets = region->get_nodesets();
      m_groupCount[EX_NODE_SET]              = nodesets.size();
    }

    // EdgeSets ...
    {
      const Ioss::EdgeSetContainer &edgesets = region->get_edgesets();
      m_groupCount[EX_EDGE_SET]              = edgesets.size();
    }

    // FaceSets ...
    {
      const Ioss::FaceSetContainer &facesets = region->get_facesets();
      m_groupCount[EX_FACE_SET]              = facesets.size();
    }

    // ElementSets ...
    {
      const Ioss::ElementSetContainer &elementsets = region->get_elementsets();
      m_groupCount[EX_ELEM_SET]                    = elementsets.size();
    }

    // SideSets ...
    {
      const Ioss::SideSetContainer &ssets = region->get_sidesets();
      // Get entity counts for all face sets... Create SideSets.
      for (const auto &set : ssets) {
        int64_t id           = set->get_property("id").get_int();
        int64_t entity_count = 0;
        int64_t df_count     = 0;

        const Ioss::SideBlockContainer &side_blocks = set->get_side_blocks();
        for (const auto &block : side_blocks) {
          // Add  "*_offset" properties to specify at what offset
          // the data for this block appears in the containing set.
          auto *new_block = const_cast<Ioss::SideBlock *>(block);
          new_block->property_add(Ioss::Property("set_offset", entity_count));
          new_block->property_add(Ioss::Property("set_df_offset", df_count));

          // If combining sideblocks into sidesets on output, then
          // the id of the sideblock must be the same as the sideset
          // id.
          new_block->property_update("id", id);
          new_block->property_update("guid", util().generate_guid(id));

          entity_count += block->entity_count();
          df_count += block->get_property("distribution_factor_count").get_int();
        }
        auto *new_entity = const_cast<Ioss::SideSet *>(set);
        new_entity->property_add(Ioss::Property("entity_count", entity_count));
        new_entity->property_add(Ioss::Property("distribution_factor_count", df_count));
      }
      m_groupCount[EX_SIDE_SET] = ssets.size();
    }
  }

  void BaseDatabaseIO::output_other_meta_data()
  {
    // Write "reduction" attributes...
    std::vector<Ioss::Region *> regions;
    regions.push_back(get_region());
    Ionull::write_reduction_attributes(get_file_pointer(), regions);
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_nodesets());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_nodesets());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_edgesets());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_facesets());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_elementsets());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_node_blocks());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_edge_blocks());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_face_blocks());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_element_blocks());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_assemblies());
    Ionull::write_reduction_attributes(get_file_pointer(), get_region()->get_blobs());

    // Write coordinate names...
    if (!get_region()->get_node_blocks().empty()) {
      char const *labels[3];
      labels[0] = "x";
      labels[1] = "y";
      labels[2] = "z";
    }

    // Determine number of node, element maps (client-specified)
    // Set the index/order of the maps for later output.
    // Note that some fields have more than a single component and each component maps to a
    // different map
    size_t node_map_cnt = 0;
    if (get_region()->get_property("node_block_count").get_int() > 0) {
      auto *node_block      = get_region()->get_node_blocks()[0];
      auto  node_map_fields = node_block->field_describe(Ioss::Field::MAP);
      for (const auto &field_name : node_map_fields) {
        const auto &field = node_block->get_fieldref(field_name);
        if (field.get_index() == 0) {
          field.set_index(node_map_cnt + 1);
        }
        node_map_cnt += field.get_component_count(Ioss::Field::InOut::OUTPUT);
      }
    }

    Ioss::NameList elem_map_fields;
    const auto    &blocks = get_region()->get_element_blocks();
    for (const auto &block : blocks) {
      block->field_describe(Ioss::Field::MAP, &elem_map_fields);
    }

    Ioss::Utils::uniquify(elem_map_fields);

    // Now need to set the map index on any element map fields...
    // Note that not all blocks will potentially have all maps...
    size_t elem_map_cnt = 0;
    for (const auto &field_name : elem_map_fields) {
      int comp_count = 0;
      for (const auto &block : blocks) {
        if (block->field_exists(field_name)) {
          auto &field = block->get_fieldref(field_name);
          if (field.get_index() == 0) {
            field.set_index(elem_map_cnt + 1);
          }
          // Assumes all maps of a type have same component count
          comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);
        }
      }
      elem_map_cnt += comp_count;
    }

    if (node_map_cnt > 0) {
      char **names = Ioss::Utils::get_name_array(node_map_cnt, maximumNameLength);
      auto  *node_block =
          get_region()->get_node_blocks()[0]; // If there are node_maps, then there is a node_block
      auto node_map_fields = node_block->field_describe(Ioss::Field::MAP);
      for (const auto &field_name : node_map_fields) {
        const auto &field           = node_block->get_fieldref(field_name);
        int         component_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);
        if (component_count == 1) {
          Ioss::Utils::copy_string(names[field.get_index() - 1], field_name, maximumNameLength + 1);
        }
        else {
          for (int i = 0; i < component_count; i++) {
            auto name = fmt::format("{}:{}", field_name, i + 1);
            Ioss::Utils::copy_string(names[field.get_index() + i - 1], name, maximumNameLength + 1);
          }
        }
      }
      Ioss::Utils::delete_name_array(names, node_map_cnt);
    }

    if (elem_map_cnt > 0) {
      char **names = Ioss::Utils::get_name_array(elem_map_cnt, maximumNameLength);
      for (const auto &field_name : elem_map_fields) {
        // Now, we need to find an element block that has this field...
        for (const auto &block : blocks) {
          if (block->field_exists(field_name)) {
            const auto &field           = block->get_fieldref(field_name);
            int         component_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);
            if (component_count == 1) {
              Ioss::Utils::copy_string(names[field.get_index() - 1], field_name,
                                       maximumNameLength + 1);
            }
            else {
              for (int i = 0; i < component_count; i++) {
                auto name = fmt::format("{}:{}", field_name, i + 1);
                if (field_name == "skin") {
                  name = i == 0 ? "skin:parent_element_id" : "skin:parent_element_side_number";
                }
                else if (field_name == "chain") {
                  name = i == 0 ? "chain:root_element_id" : "chain:depth_from_root";
                }
                Ioss::Utils::copy_string(names[field.get_index() + i - 1], name,
                                         maximumNameLength + 1);
              }
            }
            break;
          }
        }
      }
      Ioss::Utils::delete_name_array(names, elem_map_cnt);
    }

    // Write coordinate frame data...
    write_coordinate_frames(get_file_pointer(), get_region()->get_coordinate_frames());
  }
} // namespace Ionull

namespace {
  // common
  void check_attribute_index_order(Ioss::GroupingEntity *block)
  {
    int attribute_count = block->get_property("attribute_count").get_int();
    if (attribute_count == 0) {
      return;
    }
    int component_sum = 0;

    std::vector<int> attributes(attribute_count + 1);

    // Get the attribute fields...
    Ioss::NameList results_fields = block->field_describe(Ioss::Field::ATTRIBUTE);

    bool all_attributes_indexed  = true;
    bool some_attributes_indexed = false;

    for (const auto &field_name : results_fields) {
      const Ioss::Field &field = block->get_fieldref(field_name);

      if (field_name == "attribute") {
        field.set_index(1);
        if (results_fields.size() == 1) {
          return;
        }
        continue;
      }

      int field_offset = field.get_index();
      if (field_offset == 0) {
        all_attributes_indexed = false;
      }
      else {
        some_attributes_indexed = true;
      }

      int comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);
      component_sum += comp_count;

      if (field_offset == 0) {
        continue;
      }

      if (field_offset + comp_count - 1 > attribute_count) {
        std::ostringstream errmsg;
        fmt::print(
            errmsg,
            "INTERNAL ERROR: For block '{}', attribute '{}', the indexing is incorrect.\n"
            "Something is wrong in the Ionull::BaseDatabaseIO class, function {}. Please report.\n",
            block->name(), field_name, __func__);
        IOSS_ERROR(errmsg);
      }

      for (int i = field_offset; i < field_offset + comp_count; i++) {
        if (attributes[i] != 0) {
          std::ostringstream errmsg;
          fmt::print(
              errmsg,
              "INTERNAL ERROR: For block '{}', attribute '{}', indexes into the same location as a "
              "previous attribute.\n"
              "Something is wrong in the Ionull::BaseDatabaseIO class, function {}. Please "
              "report.\n",
              block->name(), field_name, __func__);
          IOSS_ERROR(errmsg);
        }
        attributes[i] = 1;
      }
    }

    if (component_sum > attribute_count) {
      std::ostringstream errmsg;
      fmt::print(
          errmsg,
          "INTERNAL ERROR: Block '{}' is supposed to have {} attributes, but {} attributes "
          "were counted.\n"
          "Something is wrong in the Ionull::BaseDatabaseIO class, function {}. Please report.\n",
          block->name(), attribute_count, component_sum, __func__);
      IOSS_ERROR(errmsg);
    }

    // Take care of the easy cases first...
    if (all_attributes_indexed) {
      // Check that all attributes are defined.  This should have
      // caught above in the duplicate index check.
      for (int i = 1; i <= attribute_count; i++) {
        if (attributes[i] == 0) {
          std::ostringstream errmsg;
          fmt::print(errmsg,
                     "INTERNAL ERROR: Block '{}' has an incomplete set of attributes.\n"
                     "Something is wrong in the Ionull::BaseDatabaseIO class, function {}. Please "
                     "report.\n",
                     block->name(), __func__);
          IOSS_ERROR(errmsg);
        }
      }
      return;
    }

    if (!some_attributes_indexed) {
      // Index was not set for any of the attributes; set them all...
      size_t offset = 1;
      for (const auto &field_name : results_fields) {
        const Ioss::Field &field = block->get_fieldref(field_name);

        if (field_name == "attribute") {
          field.set_index(1);
          continue;
        }

        int comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);

        assert(field.get_index() == 0);
        field.set_index(offset);
        offset += comp_count;
      }
      assert((int)offset == attribute_count + 1);
      return;
    }

    // At this point, we have a partially indexed set of attributes.  Some have an index and some
    // don't
    // The easy case is if the missing indices are at the end of the list...
    assert(!all_attributes_indexed && some_attributes_indexed);
    int last_defined = 0;
    for (int i = 1; i < attribute_count + 1; i++) {
      if (attributes[i] != 0) {
        last_defined = i;
      }
    }
    int first_undefined = attribute_count;
    for (int i = attribute_count; i > 0; i--) {
      if (attributes[i] == 0) {
        first_undefined = i;
      }
    }
    if (last_defined < first_undefined) {
      for (const auto &field_name : results_fields) {
        const Ioss::Field &field = block->get_fieldref(field_name);

        if (field_name == "attribute") {
          field.set_index(1);
          continue;
        }

        if (field.get_index() == 0) {
          field.set_index(first_undefined);
          int comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);
          first_undefined += comp_count;
        }
      }
      assert(first_undefined == attribute_count + 1);
      return;
    }

    // Take the easy way out... Just reindex all attributes.
    size_t offset = 1;
    for (const auto &field_name : results_fields) {
      const Ioss::Field &field = block->get_fieldref(field_name);

      if (field_name == "attribute") {
        field.set_index(1);
        continue;
      }

      int comp_count = field.get_component_count(Ioss::Field::InOut::OUTPUT);

      assert(field.get_index() == 0);
      field.set_index(offset);
      offset += comp_count;
    }
    assert((int)offset == attribute_count + 1);
  }

  void check_variable_consistency(IOSS_MAYBE_UNUSED const ex_var_params &exo_params,
                                  IOSS_MAYBE_UNUSED int                  my_processor,
                                  IOSS_MAYBE_UNUSED const std::string &filename,
                                  IOSS_MAYBE_UNUSED const Ioss::ParallelUtils &util)
  {
    IOSS_PAR_UNUSED(exo_params);
    IOSS_PAR_UNUSED(my_processor);
    IOSS_PAR_UNUSED(filename);
    IOSS_PAR_UNUSED(util);
#ifdef SEACAS_HAVE_MPI
    const int        num_types = 10;
    std::vector<int> var_counts(num_types);
    var_counts[0] = exo_params.num_glob;
    var_counts[1] = exo_params.num_node;
    var_counts[2] = exo_params.num_edge;
    var_counts[3] = exo_params.num_face;
    var_counts[4] = exo_params.num_elem;
    var_counts[5] = exo_params.num_nset;
    var_counts[6] = exo_params.num_eset;
    var_counts[7] = exo_params.num_fset;
    var_counts[8] = exo_params.num_sset;
    var_counts[9] = exo_params.num_elset;

    Ioss::IntVector all_counts;
    util.gather(var_counts, all_counts);

    bool               any_diff = false;
    std::ostringstream errmsg;
    if (my_processor == 0) {
      bool diff[num_types];
      // See if any differ...
      for (int iv = 0; iv < 10; iv++) {
        diff[iv] = false;
        std::string type;
        switch (iv) {
        case 0: type = "global"; break;
        case 1: type = "nodal"; break;
        case 2: type = "edge"; break;
        case 3: type = "face"; break;
        case 4: type = "element"; break;
        case 5: type = "nodeset"; break;
        case 6: type = "edgeset"; break;
        case 7: type = "faceset"; break;
        case 8: type = "sideset"; break;
        case 9: type = "elementset"; break;
        }

        for (int ip = 1; ip < util.parallel_size(); ip++) {
          if (var_counts[iv] != all_counts[ip * num_types + iv]) {
            any_diff = true;
            if (!diff[iv]) {
              Ioss::FileInfo db(filename);
              diff[iv] = true;
              fmt::print(errmsg,
                         "\nERROR: Number of {} variables is not consistent on all processors.\n"
                         "       Database: '{}'\n"
                         "\tProcessor 0 count = {}\n",
                         type, db.tailname(), var_counts[iv]);
            }
            fmt::print(errmsg, "\tProcessor {} count = {}\n", ip, all_counts[ip * num_types + iv]);
          }
        }
      }
    }
    else {
      // Give the other processors something to say...
      fmt::print(errmsg, "ERROR: Variable type counts are inconsistent. See processor 0 output for "
                         "more details.\n");
    }
    int idiff = any_diff ? 1 : 0;
    util.broadcast(idiff);
    any_diff = idiff == 1;

    if (any_diff) {
      std::runtime_error x(errmsg.str());
      throw x;
    }
#endif
  }

  void insert_sort_and_unique(const std::vector<std::string> &src, std::vector<std::string> &dest)
  {
    dest.insert(dest.end(), src.begin(), src.end());
    std::sort(dest.begin(), dest.end(), std::less<std::string>());
    auto endIter = std::unique(dest.begin(), dest.end());
    dest.resize(endIter - dest.begin());
  }
} // namespace
