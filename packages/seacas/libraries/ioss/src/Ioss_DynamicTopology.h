// Copyright(C) 2024 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include "Ioss_DatabaseIO.h"      // for DatabaseIO
#include "Ioss_DBUsage.h"
#include "Ioss_ParallelUtils.h"   // for ParallelUtils
#include "Ioss_PropertyManager.h" // for PropertyManager
#include <assert.h>
#include <cstddef> // for size_t, nullptr
#include <cstdint> // for int64_t

#include "Ioss_CodeTypes.h"
#include "Ioss_Utils.h"
#include "ioss_export.h"

#include <iosfwd>     // for ostream
#include <sstream>
#include <string>  // for string, operator<

namespace Ioss {
  class Region;

  /*! The TopologyModified enumeration is used as an argument to the
   *  topology_modified() functions in io to
   *  specify the type of topology modification that has ocurred.  The
   *  calls to topology_modified() are cumulative between
   *  output steps, so a call with TOPOLOGY_REORDER followed by a call
   *  with TOPOLOGY_SHUFFLE will do the right thing.  Typical examples
   *  of when these would be used are:
   *  - TOPOLOGY_SAME: No change, but easier to call function than not.
   *  - TOPOLOGY_REORDER: Element Death which reorders the Registrars
   *  - TOPOLOGY_SHUFFLE: Load Balancing
   *  - TOPOLOGY_HADAPT: H-Adaptivity
   *  - TOPOLOGY_GHOST: Ghost nodes/edges/faces/elements created/destroyed
   *  - TOPOLOGY_GEOMETRY: Model data is modified, overlap removal.
   *  - TOPOLOGY_CREATEDELETE: Surface erosion, particle creation
   *  - TOPOLOGY_UNKNOWN: Something else, catchall option.
   */
  enum TopologyModified {
    TOPOLOGY_SAME       = (       0), //!< No change, also used for initialization
    TOPOLOGY_REORDER    = (1U <<  0), //!< Data structures reordered on processor, no change between procs.
    TOPOLOGY_SHUFFLE    = (1U <<  1), //!< Globally the same, data moved among processors.
    TOPOLOGY_HADAPT     = (1U <<  2), //!< Elements split/combined; not moved cross-proc
    TOPOLOGY_GHOST      = (1U <<  3), //!< Ghost entities created/destroyed
    TOPOLOGY_GEOMETRY   = (1U <<  4), //!< Geometry (mesh coordinates) modified. Restart needs to know this.
    TOPOLOGY_CREATEFACE = (1U <<  5), //!< Face/Edge are created/deleted.
    TOPOLOGY_CREATEELEM = (1U <<  6), //!< Elements are created/deleted.
    TOPOLOGY_CREATENODE = (1U <<  7), //!< Nodes are created/deleted.
    TOPOLOGY_UNKNOWN    = (1U <<  8), //!< Unknown change, recreate from scratch.
    TOPOLOGY_AUXILIARY  = (1U <<  9), //!< An AUXILIARY relation was created/modified.
    TOPOLOGY_CONSTRAINT = (1U << 10)  //!< Contact constraints
  };

  enum class FileControlOption {
    CONTROL_NONE,
    CONTROL_AUTO_MULTI_FILE,
    CONTROL_AUTO_SINGLE_FILE
  };

  class IOSS_EXPORT DynamicTopologyObserver
  {
  public:
    DynamicTopologyObserver(Region *region)
    : m_region(region) {}
    DynamicTopologyObserver() {}

    virtual ~DynamicTopologyObserver() {}

    virtual void reset_topology_modification();
    virtual void set_topology_modification(unsigned int type);
    virtual unsigned int get_topology_modification() const;

    virtual unsigned int get_cumulative_topology_modification() const;
    virtual void set_cumulative_topology_modification(unsigned int type);

    int get_cumulative_topology_modification_field();

    virtual bool is_topology_modified() const;
    virtual bool is_automatic_restart() const { return false; }
    virtual bool is_restart_requested() const { return false; }

    static const std::string topology_modification_change_name() {return std::string("CUMULATIVE_TOPOLOGY_MODIFICATION");}

    void register_region(Region *region);
    Region* get_region() const { return m_region; }

    virtual void define_model(Region& region);
    virtual void write_model(Region& region);
    virtual void define_transient(Region& region);

    virtual FileControlOption get_control_option() const { return FileControlOption::CONTROL_AUTO_MULTI_FILE; }

  protected:
    Region *m_region{nullptr};
    unsigned int m_topologyModification{TOPOLOGY_SAME};
    unsigned int m_cumulativeTopologyModification{TOPOLOGY_SAME};

    bool m_automaticRestart{false};
    bool m_restartRequested{false};

    void check_region() const;
    IOSS_NODISCARD const ParallelUtils &util() const;
    void synchronize_topology_modified_flags();
  };

  class IOSS_EXPORT DynamicTopologyFileControl
  {
  public:
    DynamicTopologyFileControl(Region *region, unsigned int fileCyclicCount,
                               IfDatabaseExistsBehavior &ifDatabaseExists,
                               unsigned int &dbChangeCount);

    void clone_and_replace_output_database(int steps=0);
    void add_output_database_group(int steps=0);

  private:
    Region *m_region{nullptr};
    std::string m_ioDB;
    std::string m_dbType;

    PropertyManager m_properties;

    unsigned int  m_fileCyclicCount;
    IfDatabaseExistsBehavior &m_ifDatabaseExists;
    unsigned int &m_dbChangeCount;

    IOSS_NODISCARD const ParallelUtils &util() const;

    std::string get_unique_filename(DatabaseUsage db_usage);
    std::string construct_database_filename(int& step, DatabaseUsage db_usage);
    bool file_exists(const std::string &filename, const std::string &db_type, DatabaseUsage db_usage);
    bool abort_if_exists(const std::string &filename, const std::string &db_type,
                         DatabaseUsage db_usage);

    DatabaseIO * clone_output_database(int steps);
    bool replace_output_database(DatabaseIO *db);
  };

} // namespace Ioss
