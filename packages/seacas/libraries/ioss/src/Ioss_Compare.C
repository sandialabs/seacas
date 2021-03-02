// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_SubSystem.h>
#include <Ioss_DataPool.h>
#include <Ioss_MeshCopyOptions.h>

#include <fmt/chrono.h>
#include <fmt/format.h>
#include <fmt/ostream.h>


// For compare_database...
namespace {
  bool compare_properties(const Ioss::GroupingEntity *ige, const Ioss::GroupingEntity *oge);
  bool compare_qa_info(const Ioss::Region &in, const Ioss::Region &out);
  bool compare_nodeblock(const Ioss::Region &input_region, const Ioss::Region &output_region,
                         DataPool &pool, const Ioss::MeshCopyOptions &options);
  bool compare_elementblocks(const Ioss::Region &input_region, const Ioss::Region &output_region,
                             const Ioss::MeshCopyOptions &options);
  bool compare_edgeblocks(const Ioss::Region &input_region, const Ioss::Region &output_region,
                          const Ioss::MeshCopyOptions &options);
  bool compare_faceblocks(const Ioss::Region &input_region, const Ioss::Region &output_region,
                          const Ioss::MeshCopyOptions &options);
  bool compare_structuredblocks(const Ioss::Region &input_region, const Ioss::Region &output_region,
                                const Ioss::MeshCopyOptions &options);
  bool compare_nodesets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options);
  bool compare_edgesets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options);
  bool compare_facesets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options);
  bool compare_elemsets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options);
  bool compare_sidesets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options);
  bool compare_commsets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options);
  bool compare_coordinate_frames(const Ioss::Region &input_region, const Ioss::Region &output_region,
                                 const Ioss::MeshCopyOptions &options);
  template <typename T>
  bool compare_fields(const std::vector<T *> &in_entities, const std::vector<T *> &out_entities,
                      const Ioss::Field::RoleType role);

  bool compare_fields(const Ioss::GroupingEntity *ige, const Ioss::GroupingEntity *oge,
                      const Ioss::Field::RoleType role);
  template <typename T>
  bool compare_field_data(const std::vector<T *> &in_entities, const std::vector<T *> &out_entities,
                          DataPool &pool, const Ioss::Field::RoleType role,
                          const Ioss::MeshCopyOptions &options);
  bool compare_field_data(const Ioss::GroupingEntity *ige, const Ioss::GroupingEntity *oge, DataPool &pool,
                          const Ioss::Field::RoleType role, const Ioss::MeshCopyOptions &options,
                          const std::string &prefix = "");
  bool compare_field_data_internal(const Ioss::GroupingEntity *ige, const Ioss::GroupingEntity *oge,
                                   DataPool &in_pool, const std::string &field_name,
                                   const Ioss::MeshCopyOptions &options);
} // namespace

bool Ioss::Compare::compare_database(Ioss::Region &input_region, Ioss::Region &output_region,
                                   const Ioss::MeshCopyOptions &options)
{
  DataPool data_pool;
  bool rc;

  Ioss::DatabaseIO *dbi = input_region.get_database();
  int input_rank = dbi->util().parallel_rank();

  // COMPARE all properties of input database...
  if( compare_properties(&input_region, &output_region) == false ) {
    fmt::print(Ioss::DEBUG(), "PROPERTIES mismatch ({})\n", input_region.name().c_str());
    return false;
  }

  if( compare_qa_info(input_region, output_region) == false ) {
    fmt::print(Ioss::DEBUG(), "QA INFO mismatch\n");
    return false;
  }

  if( compare_nodeblock(input_region, output_region, data_pool, options) == false ) {
    fmt::print(Ioss::DEBUG(), "NODEBLOCK mismatch\n");
    return false;
  }

  if( compare_edgeblocks(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "EDGEBLOCK mismatch\n");
    return false;
  }

  if( compare_faceblocks(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "FACEBLOCK mismatch\n");
    return false;
  }

  if( compare_elementblocks(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "ELEMENTBLOCK mismatch\n");
    return false;
  }

  if( compare_structuredblocks(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "STRUCTUREDBLOCK mismatch\n");
    return false;
  }

  if( compare_nodesets(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "NODESET mismatch\n");
    return false;
  }

  if( compare_edgesets(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "EDGESET mismatch\n");
    return false;
  }

  if( compare_facesets(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "FACESET mismatch\n");
    return false;
  }

  if( compare_elemsets(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "ELEMSET mismatch\n");
    return false;
  }

  if( compare_sidesets(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "SIDESET mismatch\n");
    return false;
  }

  if( compare_commsets(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "COMMSET mismatch\n");
    return false;
  }

  if( compare_coordinate_frames(input_region, output_region, options) == false ) {
    fmt::print(Ioss::DEBUG(), "COORDINATE FRAME mismatch\n");
    return false;
  }

  bool node_major = output_region.node_major();

  if (!node_major) {
    rc = compare_field_data(input_region.get_element_blocks(), output_region.get_element_blocks(),
                            data_pool, Ioss::Field::MESH, options);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "FIELD data (element blocks): MESH FIELD data mismatch (node_major = {})\n",
             node_major ? "TRUE" : "FALSE");
      return false;
    }

    rc = compare_field_data(input_region.get_element_blocks(), output_region.get_element_blocks(),
                            data_pool, Ioss::Field::ATTRIBUTE, options);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "FIELD data (element blocks): ATTRIBUTE FIELD data mismatch (node_major = {})\n",
                 node_major ? "TRUE" : "FALSE");
      return false;
    }
  }

  if (input_region.mesh_type() != Ioss::MeshType::STRUCTURED) {
    assert (output_region.mesh_type() != Ioss::MeshType::STRUCTURED);

    rc = compare_field_data(input_region.get_node_blocks(), output_region.get_node_blocks(),
                       data_pool, Ioss::Field::MESH, options);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "FIELD data (node blocks): MESH FIELD data mismatch\n");
      return false;
    }

    rc = compare_field_data(input_region.get_node_blocks(), output_region.get_node_blocks(),
                       data_pool, Ioss::Field::ATTRIBUTE, options);

    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "FIELD data (node blocks): ATTRIBUTE FIELD data mismatch\n");
      return false;
    }
  }
  if (node_major) {
    rc = compare_field_data(input_region.get_element_blocks(), output_region.get_element_blocks(),
                            data_pool, Ioss::Field::MESH, options);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "FIELD data (element blocks): MESH FIELD data mismatch (node_major = {})\n",
                 node_major ? "TRUE" : "FALSE");
      return false;
    }

    rc = compare_field_data(input_region.get_element_blocks(), output_region.get_element_blocks(),
                            data_pool, Ioss::Field::ATTRIBUTE, options);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "FIELD data (element blocks): ATTRIBUTE FIELD data mismatch (node_major = {})\n",
                 node_major ? "TRUE" : "FALSE");
      return false;
    }
  }

  rc = compare_field_data(input_region.get_structured_blocks(),
                          output_region.get_structured_blocks(),
                          data_pool, Ioss::Field::MESH, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (structured blocks): MESH FIELD data mismatch\n");
    return false;
  }


  rc = compare_field_data(input_region.get_structured_blocks(),
                          output_region.get_structured_blocks(), data_pool,
                          Ioss::Field::ATTRIBUTE, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (structured blocks): ATTRIBUTE FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_edge_blocks(), output_region.get_edge_blocks(),
                          data_pool, Ioss::Field::MESH, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (edge blocks): MESH FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_edge_blocks(), output_region.get_edge_blocks(),
                          data_pool, Ioss::Field::ATTRIBUTE, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (edge blocks): ATTRIBUTE FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_face_blocks(), output_region.get_face_blocks(),
                          data_pool, Ioss::Field::MESH, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (face blocks): MESH FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_face_blocks(), output_region.get_face_blocks(),
                          data_pool, Ioss::Field::ATTRIBUTE, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (face blocks): ATTRIBUTE FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_elementsets(), output_region.get_elementsets(),
                          data_pool, Ioss::Field::MESH, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (element sets): MESH FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_elementsets(), output_region.get_elementsets(),
                          data_pool, Ioss::Field::ATTRIBUTE, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (element sets): ATTRIBUTE FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_commsets(), output_region.get_commsets(), data_pool,
                          Ioss::Field::MESH, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (comm sets): MESH FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_commsets(), output_region.get_commsets(), data_pool,
                          Ioss::Field::ATTRIBUTE, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (comm sets): ATTRIBUTE FIELD data mismatch\n");
    return false;
  }

  rc = compare_field_data(input_region.get_commsets(), output_region.get_commsets(), data_pool,
                          Ioss::Field::COMMUNICATION, options);
  if( rc == false ) {
    fmt::print(Ioss::DEBUG(), "FIELD data (comm sets): COMMUNICATION FIELD data mismatch\n");
    return false;
  }


  // Side Sets
  if (input_region.mesh_type() == Ioss::MeshType::UNSTRUCTURED) {
    // This should have already been checked.
    assert (input_region.mesh_type() == Ioss::MeshType::UNSTRUCTURED);

    const auto &in_fss = input_region.get_sidesets();
    const auto &out_fss = output_region.get_sidesets();

    // This should have already been checked.
    assert( in_fss.size() == out_fss.size() );

    for (const auto &ifs : in_fss) {
      const std::string &name = ifs->name();

      // Find matching output sideset
      typename std::vector<Ioss::SideSet *>::const_iterator it;
      for(it = out_fss.begin(); it != out_fss.end(); it++ ) {
        if( name.compare((*it)->name()) == 0 ) break;
      }

      if( it == out_fss.end() ) {
        fmt::print(Ioss::DEBUG(), "COMPARE field data: SIDESET ({}) not found\n", name.c_str());
        return false;
      }

      rc = compare_field_data(ifs, (*it), data_pool, Ioss::Field::MESH, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "FIELD data (side sets): MESH FIELD data mismatch\n");
        return false;
      }

      rc = compare_field_data(ifs, (*it), data_pool, Ioss::Field::ATTRIBUTE, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "FIELD data (side sets): ATTRIBUTE FIELD data mismatch\n");
        return false;
      }

      const auto &in_sbs = ifs->get_side_blocks();
      const auto &out_sbs = (*it)->get_side_blocks();

      // This should have already been checked.
      assert( in_sbs.size() == out_sbs.size() );

      for (const auto &isb : in_sbs) {
        const std::string &sbname = isb->name();

        // Find matching output sideblock
        typename std::vector<Ioss::SideBlock *>::const_iterator iter;
        for(iter = out_sbs.begin(); iter != out_sbs.end(); iter++ ) {
          if( sbname.compare((*iter)->name()) == 0 ) break;
        }

        if( iter == out_sbs.end() ) {
          fmt::print(Ioss::DEBUG(), "COMPARE field data: SIDEBLOCK ({}) not found\n", name.c_str());
          return false;
        }

        rc = compare_field_data(isb, (*iter), data_pool, Ioss::Field::MESH, options);
        if( rc == false ) {
          fmt::print(Ioss::DEBUG(), "FIELD data (side blocks): MESH FIELD data mismatch\n");
          return false;
        }

        rc = compare_field_data(isb, (*iter), data_pool, Ioss::Field::ATTRIBUTE, options);
        if( rc == false ) {
          fmt::print(Ioss::DEBUG(), "FIELD data (side blocks): ATTRIBUTE FIELD data mismatch\n");
          return false;
        }
      }
    }
  }

  // This should have already been checked
  assert(input_region.property_exists("state_count") ==
         output_region.property_exists("state_count"));

  // This should have already been checked
  assert(input_region.get_property("state_count").get_int() ==
         output_region.get_property("state_count").get_int());

  if (input_region.property_exists("state_count") &&
      input_region.get_property("state_count").get_int() > 0) {

    // For each 'TRANSIENT' field in the node blocks and element
    // blocks, transfer to the output node and element blocks.
    rc = compare_fields(&input_region, &output_region, Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (region): mismatch\n");
      return false;
    }

    rc = compare_fields(input_region.get_node_blocks(), output_region.get_node_blocks(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (node blocks): mismatch\n");
      return false;
    }

    rc = compare_fields(input_region.get_edge_blocks(), output_region.get_edge_blocks(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (edge blocks): mismatch\n");
      return false;
    }

    rc = compare_fields(input_region.get_face_blocks(), output_region.get_face_blocks(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (face blocks): mismatch\n");
      return false;
    }

    rc = compare_fields(input_region.get_element_blocks(), output_region.get_element_blocks(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (element blocks): mismatch\n");
      return false;
    }

    rc = compare_fields(input_region.get_structured_blocks(), output_region.get_structured_blocks(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (structured blocks): mismatch\n");
      return false;
    }


    rc = compare_fields(input_region.get_nodesets(), output_region.get_nodesets(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (node sets): mismatch\n");
      return false;
    }

    rc = compare_fields(input_region.get_edgesets(), output_region.get_edgesets(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (edge sets): mismatch\n");
      return false;
    }

    rc = compare_fields(input_region.get_facesets(), output_region.get_facesets(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (face sets): mismatch\n");
      return false;
    }

    rc = compare_fields(input_region.get_elementsets(), output_region.get_elementsets(),
                        Ioss::Field::TRANSIENT);
    if( rc == false ) {
      fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (element sets): mismatch\n");
      return false;
    }

    // Side Sets
    {
      const auto &in_sss = input_region.get_sidesets();
      const auto &out_sss = output_region.get_sidesets();
      for (const auto &iss : in_sss) {
        const std::string &name = iss->name();

        // Find matching output sideset
        typename std::vector<Ioss::SideSet *>::const_iterator it;
        for(it = out_sss.begin(); it != out_sss.end(); it++ ) {
          if( name.compare((*it)->name()) == 0 ) break;
        }

        if( it == out_sss.end() ) {
          fmt::print(Ioss::DEBUG(), "COMPARE field data: SIDESET ({}) not found\n", name.c_str());
          return false;
        }

        {
          rc = compare_fields(iss, (*it), Ioss::Field::TRANSIENT);
          if( rc == false ) {
            fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (side sets): mismatch\n");
            return false;
          }

          const auto &in_sbs = iss->get_side_blocks();
          const auto &out_sbs = (*it)->get_side_blocks();

          if( in_sbs.size() != out_sbs.size() ) {
            fmt::print(Ioss::DEBUG(), "NUMBER of SIDE BLOCKs don't match ({} vs. {})\n",
                   in_sbs.size(), out_sbs.size());
            return false;
          }

          for (const auto &isb : in_sbs) {

            // Find matching output sideblock
            const std::string &sbname = isb->name();

            typename std::vector<Ioss::SideBlock *>::const_iterator iter;
            for(iter = out_sbs.begin(); iter != out_sbs.end(); iter++ ) {
              if( sbname.compare((*iter)->name()) == 0 ) break;
            }

            if( iter == out_sbs.end() ) {
              fmt::print(Ioss::DEBUG(), "COMPARE: SIDEBLOCK ({}) not found in OUTPUT\n", sbname.c_str());
              return false;
            }

            rc = compare_fields(isb, (*iter), Ioss::Field::TRANSIENT);
            if( rc == false ) {
              fmt::print(Ioss::DEBUG(), "TRANSIENT FIELDs (side blocks): mismatch\n");
              return false;
            }
          }
        }
      }
    }

    int in_step_count = input_region.get_property("state_count").get_int();
    int out_step_count = output_region.get_property("state_count").get_int();

    // This should have already been checked
    assert(in_step_count == out_step_count);

    for (int istep = 1; istep <= in_step_count; istep++) {
      double in_time = input_region.get_state_time(istep);
      double out_time = input_region.get_state_time(istep);

      // This should have already been checked
      assert(in_time == out_time);

      if (in_time < options.minimum_time) {
        continue;
      }
      if (in_time > options.maximum_time) {
        break;
      }

      input_region.begin_state(istep);
      output_region.begin_state(istep);

      rc = compare_field_data(&input_region, &output_region, data_pool,
                              Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (region / step {}): mismatch\n", istep);
        return false;
      }

      // This should have already been checked
      assert(input_region.mesh_type() == output_region.mesh_type());

      if (input_region.mesh_type() != Ioss::MeshType::STRUCTURED) {
        rc = compare_field_data(input_region.get_node_blocks(), output_region.get_node_blocks(),
                                data_pool, Ioss::Field::TRANSIENT, options);
        if( rc == false ) {
          fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (node blocks / step {}): mismatch\n", istep);
          return false;
        }
      }

      rc = compare_field_data(input_region.get_edge_blocks(), output_region.get_edge_blocks(),
                              data_pool, Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (edge blocks / step {}): mismatch\n", istep);
        return false;
      }

      rc = compare_field_data(input_region.get_face_blocks(), output_region.get_face_blocks(),
                              data_pool, Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (face blocks / step {}): mismatch\n", istep);
        return false;
      }

      rc = compare_field_data(input_region.get_element_blocks(), output_region.get_element_blocks(),
                              data_pool, Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (element blocks / step {}): mismatch\n", istep);
        return false;
      }

      rc = compare_field_data(input_region.get_structured_blocks(),
                              output_region.get_structured_blocks(),
                              data_pool, Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (structured blocks / step {}): mismatch\n", istep);
        return false;
      }

      rc = compare_field_data(input_region.get_nodesets(), output_region.get_nodesets(),
                              data_pool, Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (node sets / step {}): mismatch\n", istep);
        return false;
      }

      rc = compare_field_data(input_region.get_edgesets(), output_region.get_edgesets(),
                              data_pool, Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (edge sets / step {}): mismatch\n", istep);
        return false;
      }

      rc = compare_field_data(input_region.get_facesets(), output_region.get_facesets(), data_pool,
                              Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (face sets / step {}): mismatch\n", istep);
        return false;
      }

      rc = compare_field_data(input_region.get_elementsets(), output_region.get_elementsets(),
                              data_pool, Ioss::Field::TRANSIENT, options);
      if( rc == false ) {
        fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (element sets / step {}): mismatch\n", istep);
        return false;
      }

      // Side Sets
      const auto &in_sss = input_region.get_sidesets();
      const auto &out_sss = output_region.get_sidesets();

      // This should have already been checked
      assert(in_sss.size() == out_sss.size());

      for (const auto &iss : in_sss) {
        const std::string &name = iss->name();

        // Find matching output sideset
        typename std::vector<Ioss::SideSet *>::const_iterator it;
        for(it = out_sss.begin(); it != out_sss.end(); it++ ) {
          if( name.compare((*it)->name()) == 0 ) break;
        }

        if( it == out_sss.end() ) {
          fmt::print(Ioss::DEBUG(), "COMPARE field data: SIDESET ({}) not found\n", name.c_str());
          return false;
        }

        {
          rc = compare_field_data(iss, (*it), data_pool, Ioss::Field::TRANSIENT, options);
          if( rc == false ) {
            fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (side sets): mismatch\n");
            return false;
          }

          const auto &in_sbs = iss->get_side_blocks();
          const auto &out_sbs = (*it)->get_side_blocks();

          if( in_sbs.size() != out_sbs.size() ) {
            fmt::print(Ioss::DEBUG(), "NUMBER of SIDE BLOCKs don't match ({} vs. {})\n",
                       in_sbs.size(), out_sbs.size());
            return false;
          }

          for (const auto &isb : in_sbs) {

            // Find matching output sideblock
           const std::string &sbname = isb->name();

            typename std::vector<Ioss::SideBlock *>::const_iterator iter;
            for(iter = out_sbs.begin(); iter != out_sbs.end(); iter++ ) {
              if( sbname.compare((*iter)->name()) == 0 ) break;
            }

            if( iter == out_sbs.end() ) {
              fmt::print(Ioss::DEBUG(), "COMPARE: SIDESET ({}) not found\n", name.c_str());
              return false;
            }

            rc = compare_field_data(isb, (*iter), data_pool, Ioss::Field::TRANSIENT, options);
            if( rc == false ) {
              fmt::print(Ioss::DEBUG(), "TRANSIENT FIELD data (side sets): mismatch\n");
              return false;
            }
          }
        }
      }
    }
  }

  Ioss::Utils::clear(data_pool.data);
  return true;
}

namespace {
  bool compare_properties(const Ioss::GroupingEntity *ige, const Ioss::GroupingEntity *oge)
  {
    Ioss::NameList ige_properties;
    ige->property_describe(&ige_properties);

    Ioss::NameList oge_properties;
    oge->property_describe(&oge_properties);

    for (const auto &property : ige_properties) {
      if (!oge->property_exists(property)) {
        // BASED on existing code in transfer_properties(), different databases can result
        // in a different set of properties without affecting their equivalence.  As a result,
        // we'll skip properties that they don't have in common.
        continue;
      }

      if( property.compare("database_name") == 0 ) {
        // IGNORE the database name.  This is generally the filename; we don't care whether
        // the filenames match.
        continue;
      }

      // ALLOW the regions to have different names (when copying between databases, io_shell
      // will create "region_1" (input) and "region_2" (output))
      if( ige->type() == Ioss::REGION && property.compare("name") == 0 ) {
        continue;
      }

      Ioss::Property ige_property = ige->get_property(property);
      Ioss::Property oge_property = oge->get_property(property);
      if( ige_property != oge_property ) {
        if( ige_property.get_type() == Ioss::Property::STRING ) {
          fmt::print(Ioss::DEBUG(), "PROPERTY ({}): input ({}) not equal to output ({})\n",
                     property.c_str(), ige_property.get_string().c_str(), oge_property.get_string().c_str());
        } else if( ige_property.get_type() == Ioss::Property::INTEGER ) {
          fmt::print(Ioss::DEBUG(), "PROPERTY ({}): input ({}) not equal to output ({})\n",
                     property.c_str(), ige_property.get_int(), oge_property.get_int());
        } else {
          fmt::print(Ioss::DEBUG(), "PROPERTY ({}): input not equal to output\n", property.c_str());
        }

        return false;
      }
    }

    return true;
  }

  bool compare_qa_info(const Ioss::Region &in, const Ioss::Region &out)
  {
    std::vector<std::string> in_information_records = in.get_information_records();
    std::vector<std::string> out_information_records = out.get_information_records();

    if( in_information_records.size() != out_information_records.size() ) {
      fmt::print(Ioss::DEBUG(), "WARNING: NUMBER of INFORMATION RECORDs mismatch ({} vs. {})\n",
             in_information_records.size(), out_information_records.size());
    }

    for (const auto &information_record : in_information_records) {
      auto it = std::find(out_information_records.begin(),
                          out_information_records.end(),
                          information_record);

      if( it == out_information_records.end() ) {
        // INFORMATION RECORD was not found
        fmt::print(Ioss::DEBUG(), "WARNING: Input INFORMATION RECORD ({}) not found in output\n",
                   information_record.c_str());
     }
    }

    for (const auto &information_record : out_information_records) {
      auto it = std::find(in_information_records.begin(),
                          in_information_records.end(),
                          information_record);

      if( it == in_information_records.end() ) {
        // INFORMATION RECORD was not found
        fmt::print(Ioss::DEBUG(), "WARNING: Output INFORMATION RECORD ({}) not found in input\n",
                   information_record.c_str());
      }
    }

    // Each QA record consists of four strings.  For now, require identical ordering
    // (i.e., records in the same order) for equality.
    const std::vector<std::string> &in_qa = in.get_qa_records();
    const std::vector<std::string> &out_qa = out.get_qa_records();

    if( in_qa.size() != out_qa.size() ) {
      fmt::print(Ioss::DEBUG(), "WARNING: NUMBER of QA RECORDs mismatch ({} vs. {})\n", in_qa.size(), out_qa.size());
    }

    // CHECK for missing QA records and COMPARE existing records
    for (const auto &in_qa_record : in_qa ) {
      auto it = std::find(out_qa.begin(), out_qa.end(), in_qa_record);

      if( it == out_qa.end() ) {
        // QA RECORD was not found
        fmt::print(Ioss::DEBUG(), "WARNING: Input QA RECORD ({}) not found in output\n", in_qa_record.c_str());
        continue;
      }

      if( in_qa_record.compare(*it) != 0 ) {
        fmt::print(Ioss::DEBUG(), "QA RECORD content mismatch ({} vs. {})\n", in_qa_record.c_str(), (*it).c_str());
        return false;
      }
    }

    for (const auto &out_qa_record : out_qa ) {
      auto it = std::find(in_qa.begin(), in_qa.end(), out_qa_record);

      if( it == in_qa.end() ) {
        // QA RECORD was not found
        fmt::print(Ioss::DEBUG(), "WARNING: Output QA RECORD ({}) not found in input\n", out_qa_record.c_str());
      }
    }

    return true;
  }

  bool compare_nodeblock(const Ioss::Region &input_region, const Ioss::Region &output_region, DataPool &pool,
                         const Ioss::MeshCopyOptions &options)
  {
    Ioss::NodeBlockContainer in_nbs = input_region.get_node_blocks();
    Ioss::NodeBlockContainer out_nbs = output_region.get_node_blocks();

    if( in_nbs.size() != out_nbs.size() ) {
      fmt::print(Ioss::DEBUG(), "NUMBER of nodeblocks ({} vs. {}) don't match\n", in_nbs.size(), out_nbs.size());
      return false;
    }

    for (const auto &inb : in_nbs) {
      Ioss::NodeBlockContainer::iterator it;
      for ( it = out_nbs.begin(); it != out_nbs.end(); it++ ) {
        if( *inb == *(*it) ) break;
      }

      if( it == out_nbs.end() ) {
        fmt::print(Ioss::DEBUG(), "INPUT nodeblock ({}) has no match in OUTPUT nodeblock\n", inb->name().c_str());
        return false;
      }

      // Just to be sure, remove the OUTPUT nodeblock from the container so that we don't
      // inadvertently match against it again
      out_nbs.erase(it);

    }

    return true;
  }

  template <typename T>
  bool compare_blocks(const std::vector<T *> &in_blocks, const std::vector<T *> &out_blocks_const,
                      const Ioss::MeshCopyOptions &options)
  {
    if( in_blocks.size() != out_blocks_const.size() ) {
      fmt::print(Ioss::DEBUG(), "NUMBER of blocks ({} vs. {}) don't match\n",
             in_blocks.size(), out_blocks_const.size());
      return false;
    }

    // COPY the const input vector so that we remove elements as they're matched without
    // affecting the original data structure.
    std::vector<T *> out_blocks = out_blocks_const;

    if (!in_blocks.empty()) {
      for (const auto &in_block : in_blocks) {
        typename std::vector<T *>::const_iterator it;
        for( it = out_blocks.begin(); it != out_blocks.end(); it++ ) {
          if( *(*it) == *in_block ) break;
        }

        if( it == out_blocks.end() ) {
          fmt::print(Ioss::DEBUG(), "INPUT block ({}) has no match in OUTPUT block\n", in_block->name().c_str());
          return false;
        }

        // Just to be sure, remove the OUTPUT nodeblock from the container so that we don't
        // inadvertently match against it again
       out_blocks.erase(it);
      }
    }

    return  true;
  }

  bool compare_elementblocks(const Ioss::Region &input_region, const Ioss::Region &output_region,
                             const Ioss::MeshCopyOptions &options)
  {
    const auto &in_ebs = input_region.get_element_blocks();
    const auto &out_ebs = output_region.get_element_blocks();
    if( compare_blocks(in_ebs, out_ebs, options) == false ) {
      fmt::print(Ioss::DEBUG(), "ELEMENTBLOCKS mismatch\n");
      return false;
    }
    return true;
  }

  bool compare_edgeblocks(const Ioss::Region &input_region, const Ioss::Region &output_region,
                          const Ioss::MeshCopyOptions &options)
  {
    const auto &in_ebs = input_region.get_edge_blocks();
    const auto &out_ebs = output_region.get_edge_blocks();
    if( compare_blocks(in_ebs, out_ebs, options) == false ) {
      fmt::print(Ioss::DEBUG(), "EDGEBLOCKS mismatch\n");
      return false;
    }
    return true;
  }

  bool compare_faceblocks(const Ioss::Region &input_region, const Ioss::Region &output_region,
                          const Ioss::MeshCopyOptions &options)
  {
    const auto &in_fbs = input_region.get_face_blocks();
    const auto &out_fbs = output_region.get_face_blocks();
    if( compare_blocks(in_fbs, out_fbs, options) == false ) {
      fmt::print(Ioss::DEBUG(), "FACEBLOCKS mismatch\n");
      return false;
    }
    return true;
  }

  bool compare_structuredblocks(const Ioss::Region &input_region, const Ioss::Region &output_region,
                                const Ioss::MeshCopyOptions &options)
  {
    auto in_blocks = input_region.get_structured_blocks();
    auto out_blocks_orig = output_region.get_structured_blocks();

    // COPY the const input vector so that we can remove elements as they're matched without
    // affecting the original data structure.
    std::vector<Ioss::StructuredBlock *> out_blocks = out_blocks_orig;

    if( in_blocks.size() != out_blocks.size() ) {
      fmt::print(Ioss::DEBUG(), "STRUCTUREDBLOCKS: size mismatch ({} vs. {})\n", in_blocks.size(), out_blocks.size());
      return false;
    }

    if (!in_blocks.empty()) {
      for (const auto &in_block : in_blocks) {
        std::vector<Ioss::StructuredBlock *>::iterator it;
        for( it = out_blocks.begin(); it != out_blocks.end(); it++ ) {
          if( *(*it) == *in_block ) break;
        }

        if( it == out_blocks.end() ) {
          fmt::print(Ioss::DEBUG(), "INPUT structuredblock ({}) has no match in OUTPUT structuredblock\n",
                 in_block->name().c_str());
          return false;
        }

        // Just to be sure, remove the OUTPUT nodeblock from the container so that we don't
        // inadvertently match against it again
        out_blocks.erase(it);
      }
    }

    return true;
  }

  template <typename T>
  bool compare_sets(const std::vector<T *> &in_sets, const std::vector<T *> &out_sets_const,
                    const Ioss::MeshCopyOptions &options)
  {
    if( in_sets.size() != out_sets_const.size() ) {
      fmt::print(Ioss::DEBUG(), "NUMBER of sets ({} vs. {}) don't match\n",
                 in_sets.size(), out_sets_const.size());
      return false;
    }

    // COPY the const input vector so that we remove elements as they're matched without
    // affecting the original data structure.
    std::vector<T *> out_sets = out_sets_const;

    if (!in_sets.empty()) {
      for (const auto &in_set : in_sets) {
        typename std::vector<T *>::const_iterator it;
        for( it = out_sets.begin(); it != out_sets.end(); it++ ) {
          if( *(*it) == *in_set ) break;
        }

        if( it == out_sets.end() ) {
          fmt::print(Ioss::DEBUG(), "INPUT set ({}) has no match in OUTPUT set\n", in_set->name().c_str());
          return false;
        }

        // Just to be sure, remove the OUTPUT set from the container so that we don't
        // inadvertently match against it again
        out_sets.erase(it);
      }
    }

    return true;
  }

  bool compare_nodesets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options)
  {
    const auto &in_nss = input_region.get_nodesets();
    const auto &out_nss = output_region.get_nodesets();
    bool rc = compare_sets(in_nss, out_nss, options);
    if( !rc ) {
      fmt::print(Ioss::DEBUG(), "NODESET mismatch\n");
    }

    return rc;
  }

  bool compare_edgesets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options)
  {
    const auto &in_ess = input_region.get_edgesets();
    const auto &out_ess = output_region.get_edgesets();
    bool rc = compare_sets(in_ess, out_ess, options);
    if( !rc ) {
      fmt::print(Ioss::DEBUG(), "EDGESET mismatch\n");
    }

    return rc;
  }

  bool compare_facesets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options)
  {
    const auto &in_fss = input_region.get_facesets();
    const auto &out_fss = output_region.get_facesets();
    bool rc = compare_sets(in_fss, out_fss, options);
    if( !rc ) {
      fmt::print(Ioss::DEBUG(), "FACESET mismatch\n");
    }

    return rc;
  }

  bool compare_elemsets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options)
  {
    const auto &in_ess = input_region.get_elementsets();
    const auto &out_ess = output_region.get_elementsets();
    bool rc = compare_sets(in_ess, out_ess, options);
    if( !rc ) {
      fmt::print(Ioss::DEBUG(), "ELEMSET mismatch\n");
    }

    return rc;
  }

  bool compare_sidesets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options)
  {
    const auto &in_sss = input_region.get_sidesets();
    const auto &out_sss = output_region.get_sidesets();
    bool rc = compare_sets(in_sss, out_sss, options);
    if( !rc ) {
      fmt::print(Ioss::DEBUG(), "SIDESET mismatch\n");
    }

    return rc;
  }

  bool compare_commsets(const Ioss::Region &input_region, const Ioss::Region &output_region,
                        const Ioss::MeshCopyOptions &options)
  {
    const auto &in_css = input_region.get_commsets();
    const auto &out_css_orig = output_region.get_commsets();

    if( in_css.size() != out_css_orig.size() ) {
      fmt::print(Ioss::DEBUG(), "NUMBER of COMMSETs ({} vs. {}) don't match\n",
             in_css.size(), out_css_orig.size());
      return false;
    }

    // COPY the const input vector so that we remove elements as they're matched without
    // affecting the original data structure.
    std::vector<Ioss::CommSet *> out_css = out_css_orig;

    if (!in_css.empty()) {
      for( const auto &in_cs : in_css ) {
        typename std::vector<Ioss::CommSet *>::const_iterator it;
        for( it = out_css.begin(); it != out_css.end(); it++ ) {
          if( *(*it) == *in_cs ) break;
        }

        if( it == out_css.end() ) {
          fmt::print(Ioss::DEBUG(), "INPUT COMMset ({}) has no match in OUTPUT COMMset\n", in_cs->name().c_str());
          return false;
        }

        // Just to be sure, remove the OUTPUT set from the container so that we don't
        // inadvertently match against it again
        out_css.erase(it);
      }
    }

    return true;
  }

  bool compare_coordinate_frames(const Ioss::Region &input_region, const Ioss::Region &output_region,
                                 const Ioss::MeshCopyOptions &options)
  {
    const auto &in_cfs = input_region.get_coordinate_frames();
    const auto &out_cfs_orig = output_region.get_coordinate_frames();

    if( in_cfs.size() != out_cfs_orig.size() ) {
      fmt::print(Ioss::DEBUG(), "NUMBER of COORDINATE FRAMEs ({} vs. {}) don't match\n",
                 in_cfs.size(), out_cfs_orig.size());
      return false;
    }

    // COPY the const input vector so that we remove elements as they're matched without
    // affecting the original data structure.
    std::vector<Ioss::CoordinateFrame> out_cfs = out_cfs_orig;

    if (!in_cfs.empty()) {
      for( const auto &in_cf : in_cfs ) {
        typename std::vector<Ioss::CoordinateFrame>::const_iterator it;
        for( it = out_cfs.begin(); it != out_cfs.end(); it++ ) {
          if( (*it) == in_cf ) break;
        }

        if( it == out_cfs.end() ) {
          fmt::print(Ioss::DEBUG(), "INPUT coordinate frame (ID = {}) has no match in OUTPUT coordinate frames\n",
                     in_cf.id());
          return false;
        }

        // Just to be sure, remove the OUTPUT set from the container so that we don't
        // inadvertently match against it again
        out_cfs.erase(it);
      }
    }

    return true;
  }

  template <typename T>
  bool compare_fields(const std::vector<T *> &in_entities, const std::vector<T *> &out_entities,
                      const Ioss::Field::RoleType role)
  {
    if( in_entities.size() != out_entities.size() ) {
      fmt::print(Ioss::DEBUG(), "COMPARE fields : NUMBER of entities don't match ({} vs. {})\n",
                 in_entities.size(), out_entities.size());
      return false;
    }

    for( const auto &in_entity : in_entities ) {
      const std::string &name = in_entity->name();

      typename std::vector<T *>::const_iterator it;
      for(it = out_entities.begin(); it != out_entities.end(); it++ ) {
        if( name.compare((*it)->name()) == 0 ) break;
      }

      if( it == out_entities.end() ) {
        fmt::print(Ioss::DEBUG(), "COMPARE fields: ENTITY ({}) not found\n", name.c_str());
        return false;
      }

      return compare_fields(in_entity, (*it), role);
    }

    return true;
  }

  bool compare_fields(const Ioss::GroupingEntity *ige, const Ioss::GroupingEntity *oge,
                      const Ioss::Field::RoleType role)
  {
    // Check for transient fields...
    Ioss::NameList in_fields;
    ige->field_describe(role, &in_fields);

    Ioss::NameList out_fields;
    oge->field_describe(role, &out_fields);

    if( in_fields.size() != out_fields.size() ) {
      fmt::print(Ioss::DEBUG(), "Number of FIELDs mismatch ({} v. {})\n", in_fields.size(), out_fields.size());
      return false;
    }

    // Iterate through results fields and transfer to output
    // database...  If a prefix is specified, only transfer fields
    // whose names begin with the prefix
    for (const auto &field_name : in_fields) {
      Ioss::Field ige_field = ige->get_field(field_name);
      Ioss::Field oge_field = oge->get_field(field_name);
      if( ige_field != oge_field ) {
        fmt::print(Ioss::DEBUG(), "FIELD ({}) mismatch\n", field_name.c_str());
      }
    }

    return true;
  }

  template <typename T>
  bool compare_field_data(const std::vector<T *> &in_entities, const std::vector<T *> &out_entities,
                          DataPool &pool, const Ioss::Field::RoleType role,
                          const Ioss::MeshCopyOptions &options)
  {
    if( in_entities.size() != out_entities.size() ) {
      fmt::print(Ioss::DEBUG(), "COMPARE field data: NUMBER of entities don't match ({} vs. {})\n",
                 in_entities.size(), out_entities.size());
      return false;
    }

    for( const auto &in_entity : in_entities ) {
      const std::string &name = in_entity->name();

      typename std::vector<T *>::const_iterator it;
      for(it = out_entities.begin(); it != out_entities.end(); it++ ) {
        if( name.compare((*it)->name()) == 0 ) break;
      }

      if( it == out_entities.end() ) {
        fmt::print(Ioss::DEBUG(), "COMPARE field data: ENTITY ({}) not found\n", name.c_str());
        return false;
      }

      return compare_field_data(in_entity, (*it), pool, role, options);
    }

    return true;
  }

  bool compare_field_data(const Ioss::GroupingEntity *ige, const Ioss::GroupingEntity *oge, DataPool &pool,
                          const Ioss::Field::RoleType role, const Ioss::MeshCopyOptions &options,
                          const std::string &prefix)
  {
    bool rc;

    // Iterate through the TRANSIENT-role fields of the input
    // database and transfer to output database.
    Ioss::NameList in_state_fields;
    Ioss::NameList out_state_fields;

    ige->field_describe(role, &in_state_fields);
    oge->field_describe(role, &out_state_fields);

    // Complication here is that if the 'role' is 'Ioss::Field::MESH',
    // then the 'ids' field must be transferred first...
    if( ige->field_exists("ids") != oge->field_exists("ids") ) {
      fmt::print(Ioss::DEBUG(), "FIELD data: field MISMATCH --> "
                 "ige->field_exists(\"ids\") = {} / oge->field_exists(\"ids\") = {}\n",
                 ige->field_exists("ids") ? "TRUE" : "FALSE", oge->field_exists("ids") ? "TRUE" : "FALSE");
      return false;
    }

    if (role == Ioss::Field::MESH && ige->field_exists("ids")) {
      assert(oge->field_exists("ids"));
      rc = compare_field_data_internal(ige, oge, pool, "ids", options);
      if( rc == false ) return false;
    }

    for (const auto &field_name : in_state_fields) {
      // All of the 'Ioss::EntityBlock' derived classes have a
      // 'connectivity' field, but it is only interesting on the
      // Ioss::ElementBlock class. On the other classes, it just
      // generates overhead...
      if (field_name == "connectivity" && ige->type() != Ioss::ELEMENTBLOCK) {
        assert(oge->type() != Ioss::ELEMENTBLOCK);
        continue;
      }
      if (field_name == "ids") {
        continue;
      }
      if (Ioss::Utils::substr_equal(prefix, field_name)) {
        assert(oge->field_exists(field_name));
        rc = compare_field_data_internal(ige, oge, pool, field_name, options);
        if( rc == false ) return false;
      }
    }

    return true;
  }

  bool compare_field_data_internal(const Ioss::GroupingEntity *ige, const Ioss::GroupingEntity *oge,
                                   DataPool &in_pool, const std::string &field_name,
                                   const Ioss::MeshCopyOptions &options)
  {
    size_t isize = ige->get_field(field_name).get_size();
    size_t osize = oge->get_field(field_name).get_size();

    DataPool out_pool;

    if( isize != osize ) {
      fmt::print(Ioss::DEBUG(), "FIELD size mismatch ({} vs. {})\n", isize, osize);
    }

    int basic_type = ige->get_field(field_name).get_type();

    if (field_name == "mesh_model_coordinates_x") {
      return true;
    }
    if (field_name == "mesh_model_coordinates_y") {
      return true;
    }
    if (field_name == "mesh_model_coordinates_z") {
      return true;
    }
    if (field_name == "connectivity_raw") {
      return true;
    }
    if (field_name == "element_side_raw") {
      return true;
    }
    if (field_name == "ids_raw") {
      return true;
    }
    if (field_name == "implicit_ids") {
      return true;
    }
    if (field_name == "node_connectivity_status") {
      return true;
    }
    if (field_name == "owning_processor") {
      return true;
    }
    if (field_name == "entity_processor_raw") {
      return true;
    }
    if (field_name == "ids" && ige->type() == Ioss::SIDEBLOCK) {
      return true;
    }
    if (field_name == "ids" && ige->type() == Ioss::STRUCTUREDBLOCK) {
      return true;
    }
    if (field_name == "cell_ids" && ige->type() == Ioss::STRUCTUREDBLOCK) {
      return true;
    }
    if (field_name == "cell_node_ids" && ige->type() == Ioss::STRUCTUREDBLOCK) {
      return true;
    }

    if (options.data_storage_type == 1 || options.data_storage_type == 2) {
      if (in_pool.data.size() < isize) {
        in_pool.data.resize(isize);
      }
      if (out_pool.data.size() < isize) {
        out_pool.data.resize(isize);
      }
    }
    else {
    }

    assert(in_pool.data.size() >= isize);
    assert(out_pool.data.size() >= isize);

    switch (options.data_storage_type) {
    case 1:
      ige->get_field_data(field_name, in_pool.data.data(), isize);
      oge->get_field_data(field_name, out_pool.data.data(), isize);
      for( unsigned int i = 0; i < isize; i++ ) {
        if( in_pool.data[i] != out_pool.data[i] ) {
          fmt::print(Ioss::DEBUG(), "FIELD data ({}) mismatch\n", field_name.c_str());
          fmt::print(Ioss::DEBUG(), "MISMATCH [{}] (0x{} vs. 0x{})\n", i, in_pool.data[i], out_pool.data[i]);
          return false;
        }
      }
      break;
    default:
      if (field_name == "mesh_model_coordinates") {
        fmt::print(Ioss::DEBUG(), "data_storage option not recognized.");
      }
      return false;
    }

    return true;
  }
}
