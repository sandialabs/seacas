// Copyright(C) 2024 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#pragma once

#include "Ioss_CodeTypes.h"
#include "Ioss_DBUsage.h"
#include "Ioss_DatabaseIO.h"      // for DatabaseIO
#include "Ioss_ParallelUtils.h"   // for ParallelUtils
#include "Ioss_PropertyManager.h" // for PropertyManager
#include "Ioss_Utils.h"
#include "ioss_export.h"

#include <cstddef> // for size_t, nullptr
#include <cstdint> // for int64_t
#include <iomanip>
#include <sstream>
#include <string> // for string, operator<

namespace Ioss {
  class Region;
  class DynamicTopologyNotifier;

  /*! The TopologyModified enumeration is used as an argument to the
   *  topology_modified() functions in io to
   *  specify the type of topology modification that has occurred.  The
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
    TOPOLOGY_SAME = (0), //!< No change, also used for initialization
    TOPOLOGY_REORDER =
        (1U << 0), //!< Data structures reordered on processor, no change between procs.
    TOPOLOGY_SHUFFLE = (1U << 1), //!< Globally the same, data moved among processors.
    TOPOLOGY_HADAPT  = (1U << 2), //!< Elements split/combined; not moved cross-proc
    TOPOLOGY_GHOST   = (1U << 3), //!< Ghost entities created/destroyed
    TOPOLOGY_GEOMETRY =
        (1U << 4), //!< Geometry (mesh coordinates) modified. Restart needs to know this.
    TOPOLOGY_CREATEFACE     = (1U << 5),  //!< Face/Edge are created/deleted.
    TOPOLOGY_CREATEELEM     = (1U << 6),  //!< Elements are created/deleted.
    TOPOLOGY_CREATENODE     = (1U << 7),  //!< Nodes are created/deleted.
    TOPOLOGY_CREATEASSEMBLY = (1U << 8),  //!< Assemblies are created/deleted.
    TOPOLOGY_UNKNOWN        = (1U << 9),  //!< Unknown change, recreate from scratch.
    TOPOLOGY_AUXILIARY      = (1U << 10), //!< An AUXILIARY relation was created/modified.
    TOPOLOGY_CONSTRAINT     = (1U << 11)  //!< Contact constraints

  };

  enum class FileControlOption { CONTROL_NONE, CONTROL_AUTO_MULTI_FILE, CONTROL_AUTO_GROUP_FILE };

  class IOSS_EXPORT DynamicTopologyObserver
  {
  public:
    DynamicTopologyObserver(Region *region) : m_region(region) {}

    virtual ~DynamicTopologyObserver() {}

    virtual void reset_topology_modification_all();
    virtual void reset_topology_modification();
    virtual void set_topology_modification(unsigned int type);
    virtual void sync_topology_modification(unsigned int modFlag, unsigned int cumulativeModFlag);
    virtual unsigned int get_topology_modification() const;

    virtual unsigned int get_cumulative_topology_modification() const;
    virtual void         set_cumulative_topology_modification(unsigned int type);

    int get_cumulative_topology_modification_field();

    virtual bool is_topology_modified() const;
    virtual bool is_automatic_restart() const { return m_automaticRestart; }
    virtual bool is_restart_requested() const { return m_restartRequested; }

    void set_automatic_restart(bool flag) { m_automaticRestart = flag; }
    void set_restart_requested(bool flag) { m_restartRequested = flag; }

    static const std::string topology_modification_change_name()
    {
      return std::string("CUMULATIVE_TOPOLOGY_MODIFICATION");
    }

    void    register_region(Region *region);
    Region *get_region() const { return m_region; }

    void                     register_notifier(DynamicTopologyNotifier *notifier);
    DynamicTopologyNotifier *get_notifier() const { return m_notifier; }

    virtual void define_model();
    virtual void write_model();
    virtual void define_transient();

    virtual FileControlOption get_control_option() const { return FileControlOption::CONTROL_NONE; }

    virtual bool needs_new_output_file() const;

  protected:
    Region      *m_region{nullptr};
    unsigned int m_topologyModification{TOPOLOGY_SAME};
    unsigned int m_cumulativeTopologyModification{TOPOLOGY_SAME};

    bool m_automaticRestart{false};
    bool m_restartRequested{false};

    DynamicTopologyNotifier *m_notifier{nullptr};

    void                                verify_region_is_registered() const;
    IOSS_NODISCARD const ParallelUtils &util() const;
    void                                synchronize_topology_modified_flags();

    void set_topology_modification_nl(unsigned int type);

  private:
    DynamicTopologyObserver();
  };

  class IOSS_EXPORT DynamicTopologyNotifier
  {
  public:
    DynamicTopologyNotifier(const std::string &model_name) : m_modelName(model_name) {}

    virtual ~DynamicTopologyNotifier() = default;

    std::string name() const { return m_modelName; }

    std::vector<std::shared_ptr<DynamicTopologyObserver>> get_observers() const
    {
      return m_observers;
    }

    void register_observer(std::shared_ptr<DynamicTopologyObserver> observer);

    void unregister_observer(std::shared_ptr<DynamicTopologyObserver> observer);

    void reset_topology_modification();

    void set_topology_modification(unsigned int type);

    template <typename ObserverType> bool has_observer_type() const
    {
      bool found = false;

      for (const std::shared_ptr<DynamicTopologyObserver> &observer : m_observers) {
        if (dynamic_cast<const ObserverType *>(observer.get()) != nullptr) {
          found = true;
          break;
        }
      }
      return found;
    }

    template <typename ObserverType>
    std::vector<std::shared_ptr<ObserverType>> get_observer_type() const
    {
      std::vector<std::shared_ptr<ObserverType>> typed_observers;

      for (const std::shared_ptr<DynamicTopologyObserver> &observer : m_observers) {
        ObserverType *typed_observer = dynamic_cast<ObserverType *>(observer.get());
        if (typed_observer != nullptr) {
          typed_observers.push_back(std::dynamic_pointer_cast<ObserverType>(observer));
        }
      }

      return typed_observers;
    }

  private:
    const std::string                                     m_modelName;
    std::vector<std::shared_ptr<DynamicTopologyObserver>> m_observers;
  };

  class IOSS_EXPORT DynamicTopologyBroker
  {
  public:
    static DynamicTopologyBroker *broker();

    void register_model(const std::string &model_name);
    void remove_model(const std::string &model_name);
    void clear_models();

    std::shared_ptr<DynamicTopologyNotifier> get_notifier(const std::string &model_name) const;
    std::vector<std::shared_ptr<DynamicTopologyObserver>>
    get_observers(const std::string &model_name) const;

    void register_observer(const std::string                       &model_name,
                           std::shared_ptr<DynamicTopologyObserver> observer);
    void register_observer(const std::string                       &model_name,
                           std::shared_ptr<DynamicTopologyObserver> observer, Region &region);

    void reset_topology_modification(const std::string &model_name);
    void set_topology_modification(const std::string &model_name, unsigned int type);

  private:
    DynamicTopologyBroker() {};
    DynamicTopologyBroker(DynamicTopologyBroker &);

    std::map<std::string, std::shared_ptr<DynamicTopologyNotifier>> m_notifiers;
  };

  class IOSS_EXPORT DynamicTopologyFileControl
  {
  public:
    DynamicTopologyFileControl(Region *region);

    void clone_and_replace_output_database(int steps = 0);
    void add_output_database_change_set(int steps = 0);

    static std::string change_set_prefix() { return "IOSS_FILE_GROUP-"; }

    DatabaseIO *get_database() const;

    static std::string get_cyclic_database_filename(const std::string &baseFileName,
                                                    unsigned int       fileCyclicCount,
                                                    unsigned int       step);

    static std::string get_linear_database_filename(const std::string &baseFileName,
                                                    unsigned int       step);

    static std::string get_internal_file_change_set_name(unsigned int step);

    unsigned int             get_topology_change_count() const { return m_dbChangeCount; }
    unsigned int             get_file_cyclic_count() const { return m_fileCyclicCount; }
    IfDatabaseExistsBehavior get_if_database_exists_behavior() const { return m_ifDatabaseExists; }

  private:
    Region     *m_region{nullptr};
    std::string m_ioDB;
    std::string m_dbType;

    PropertyManager m_properties;

    unsigned int             m_fileCyclicCount;
    IfDatabaseExistsBehavior m_ifDatabaseExists;
    unsigned int             m_dbChangeCount;

    IOSS_NODISCARD const ParallelUtils &util() const;

    std::string get_unique_linear_filename(DatabaseUsage db_usage);
    std::string construct_database_filename(int &step, DatabaseUsage db_usage);
    bool        file_exists(const std::string &filename, const std::string &db_type,
                            DatabaseUsage db_usage);
    bool        abort_if_exists(const std::string &filename, const std::string &db_type,
                                DatabaseUsage db_usage);

    DatabaseIO *clone_output_database(int steps);
    bool        replace_output_database(DatabaseIO *db);
  };

  class IOSS_EXPORT DynamicTopologyStateLocator
  {
  public:
    DynamicTopologyStateLocator(Region *region, bool loadAllFiles = true);
    DynamicTopologyStateLocator(Ioss::DatabaseIO *db, const std::string &dbName,
                                const std::string &dbType, unsigned fileCyclicCount = 0,
                                bool loadAllFiles = true);
    DynamicTopologyStateLocator(Ioss::DatabaseIO *db, unsigned fileCyclicCount = 0,
                                bool loadAllFiles = true);

    virtual ~DynamicTopologyStateLocator();
    DynamicTopologyStateLocator()                                    = delete;
    DynamicTopologyStateLocator(const DynamicTopologyStateLocator &) = delete;

    DatabaseIO *get_database() const;

    std::tuple<std::string, int, double> locate_db_state(double targetTime) const;
    std::tuple<std::string, int, double> get_db_min_time() const;
    std::tuple<std::string, int, double> get_db_max_time() const;

  private:
    struct DatabaseState
    {
      DatabaseState(Ioss::DatabaseIO *db)
      {
        if (!db->supports_internal_change_set()) {
          changeSet = db->get_filename();
        }
      }

      std::string changeSet{"/"};
      int         state{-1};
      double      time{-std::numeric_limits<double>::max()};
    };

    using StateLocatorCompare = std::function<bool(double, double)>;

    void locate_state_impl(Ioss::DatabaseIO *db, double targetTime, StateLocatorCompare comparator,
                           DatabaseState &loc) const;

    void locate_state(Ioss::DatabaseIO *db, double targetTime, DatabaseState &loc) const;

    void locate_db_state_impl(double targetTime, DatabaseState &loc) const;

    void get_db_time_impl(double init_time, StateLocatorCompare comparator,
                          DatabaseState &loc) const;

    IOSS_NODISCARD const ParallelUtils &util() const;

    Ioss::DatabaseIO *m_database{nullptr};
    std::string       m_ioDB;
    std::string       m_dbType;
    unsigned          m_fileCyclicCount{0};
    bool              m_loadAllFiles{true};
  };

} // namespace Ioss
