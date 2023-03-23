// Copyright(C) 2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

// -*- Mode: c++ -*-
#pragma once

#include "ionull_export.h"

#include <Ioss_DBUsage.h>
#include <Ioss_Field.h>
#include <Ioss_Map.h>
#include <Ioss_Utils.h>
#include <null/Ionull_BaseDatabaseIO.h>

#include <algorithm>
#include <cstdint>
#include <ctime>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace Ioss {
  class GroupingEntity;
  class Region;
  class EntityBlock;
  class NodeBlock;
  class EdgeBlock;
  class FaceBlock;
  class ElementBlock;
  class EntitySet;
  class NodeSet;
  class EdgeSet;
  class FaceSet;
  class ElementSet;
  class SideBlock;
  class SideSet;
  class StructuredBlock;
  class CommSet;
  class ElementTopology;
} // namespace Ioss

namespace Ionull {
  struct CommunicationMetaData;
} // namespace Ionull

/** \brief A namespace for the file-per-process version of the
 *  parallel exodus database format.
 */
namespace Ionull {
  class IONULL_EXPORT DatabaseIO : public Ionull::BaseDatabaseIO
  {
  public:
    DatabaseIO(Ioss::Region *region, const std::string &filename, Ioss::DatabaseUsage db_usage,
               Ioss_MPI_Comm communicator, const Ioss::PropertyManager &props);
    DatabaseIO(const DatabaseIO &from)            = delete;
    DatabaseIO &operator=(const DatabaseIO &from) = delete;
    ~DatabaseIO() override                        = default;

    // Kluge -- a few applications need access so can directly access exodus API
    int get_file_pointer() const override; // Open file and set nullFilePtr.

  private:
    // Input only database -- these will never be called...
    IOSS_NOOP_GFI(Ioss::Region)
    IOSS_NOOP_GFI(Ioss::NodeBlock)
    IOSS_NOOP_GFI(Ioss::EdgeBlock)
    IOSS_NOOP_GFI(Ioss::FaceBlock)
    IOSS_NOOP_GFI(Ioss::ElementBlock)
    IOSS_NOOP_GFI(Ioss::StructuredBlock)
    IOSS_NOOP_GFI(Ioss::SideBlock)
    IOSS_NOOP_GFI(Ioss::NodeSet)
    IOSS_NOOP_GFI(Ioss::EdgeSet)
    IOSS_NOOP_GFI(Ioss::FaceSet)
    IOSS_NOOP_GFI(Ioss::ElementSet)
    IOSS_NOOP_GFI(Ioss::SideSet)
    IOSS_NOOP_GFI(Ioss::CommSet)
    IOSS_NOOP_GFI(Ioss::Assembly)
    IOSS_NOOP_GFI(Ioss::Blob)

    void read_meta_data__() override {}
    void get_step_times__() override {}

    int64_t put_field_internal(const Ioss::Region *reg, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::Blob *blob, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::Assembly *assem, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::NodeBlock *nb, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::EdgeBlock *eb, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::FaceBlock *eb, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::ElementBlock *eb, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::SideBlock *fb, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::NodeSet *ns, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::EdgeSet *ns, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::FaceSet *ns, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::ElementSet *ns, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::SideSet *ss, const Ioss::Field &field, void *data,
                               size_t data_size) const override;
    int64_t put_field_internal(const Ioss::CommSet *cs, const Ioss::Field &field, void *data,
                               size_t data_size) const override;

    int64_t put_field_internal(const Ioss::StructuredBlock * /* sb */,
                               const Ioss::Field & /* field */, void * /* data */,
                               size_t /* data_size */) const override
    {
      return -1;
    }
    int64_t put_Xset_field_internal(const Ioss::EntitySet *ns, const Ioss::Field &field, void *data,
                                    size_t data_size) const;
    int64_t get_Xset_field_internal(const Ioss::EntitySet *ns, const Ioss::Field &field, void *data,
                                    size_t data_size) const;

  private:
    void compute_node_status() const;

    int64_t write_attribute_field(const Ioss::Field &field, const Ioss::GroupingEntity *ge,
                                  void *data) const;

    // Should be made more generic again so can rejoin with write_element_transient field
    void write_nodal_transient_field(const Ioss::Field &field, const Ioss::NodeBlock *ge,
                                     int64_t count, void *variables) const;
    // Should be made more generic again so can rejoin with write_nodal_transient field
    void write_entity_transient_field(const Ioss::Field &field, const Ioss::GroupingEntity *ge,
                                      int64_t count, void *variables) const;
    void write_meta_data(Ioss::IfDatabaseExistsBehavior behavior) override;
    void gather_communication_metadata(Ionull::CommunicationMetaData *meta);

    // Internal data handling
    int64_t handle_node_ids(void *ids, int64_t num_to_get) const;
    int64_t handle_element_ids(const Ioss::ElementBlock *eb, void *ids, size_t num_to_get) const;
    int64_t handle_face_ids(const Ioss::FaceBlock *eb, void *ids, size_t num_to_get) const;
    int64_t handle_edge_ids(const Ioss::EdgeBlock *eb, void *ids, size_t num_to_get) const;

    int64_t put_side_field(const Ioss::SideBlock *sd_blk, const Ioss::Field &field, void *data,
                           size_t data_size) const;

    //!< true if application code is controlling the processor id.
    mutable bool isSerialParallel{false};
  };
} // namespace Ionull
