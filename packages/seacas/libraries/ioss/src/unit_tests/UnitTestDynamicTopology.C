// Copyright(C) 2024 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <string>
#include <vector>

#ifdef SEACAS_HAVE_MPI
#include "mpi.h"
#endif
#include "gtest/gtest.h"

#include <algorithm>
#include <fstream>
#include <functional>
#include <iostream>
#include <random>

#include <unistd.h> // for unlink

#include "Ionit_Initializer.h"
#include "Ioss_DBUsage.h"
#include "Ioss_DatabaseIO.h" // for DatabaseIO
#include "Ioss_ElementBlock.h"
#include "Ioss_Field.h" // for Field, etc
#include "Ioss_FileInfo.h"
#include "Ioss_IOFactory.h"
#include "Ioss_NodeBlock.h"
#include "Ioss_ParallelUtils.h"
#include "Ioss_Property.h"
#include "Ioss_Region.h"

#include "Ioss_Utils.h"

#include "exodus/Ioex_DatabaseIO.h"

namespace {
  std::string get_many_block_mesh_desc(unsigned numBlocks)
  {
    std::ostringstream    oss;
    std::vector<unsigned> elementIds(numBlocks);
    std::iota(elementIds.begin(), elementIds.end(), 1);

    unsigned proc = 0;
    for (unsigned i = 0; i < numBlocks; ++i) {
      unsigned elemId      = elementIds[i];
      unsigned firstNodeId = i * 4 + 1;
      oss << proc << "," << elemId << ",HEX_8,";
      for (unsigned node = firstNodeId; node < firstNodeId + 8; ++node) {
        oss << node << ",";
      }
      unsigned blockId = i + 1;
      oss << "block_" << blockId;

      if (i < numBlocks - 1) {
        oss << "\n";
      }

      proc++;
    }

    oss << "|coordinates:";

    std::vector<double> planeCoords = {0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};

    for (double coord : planeCoords) {
      oss << coord << ",";
    }

    for (unsigned i = 1; i <= numBlocks; ++i) {
      for (unsigned point = 0; point < 4; ++point) {
        planeCoords[3 * point + 2] += 1;
      }

      for (double coord : planeCoords) {
        oss << coord << ",";
      }
    }

    return oss.str();
  }

  void define_model(const Ioss::Region &i_region, Ioss::Region &o_region)
  {
    Ioss::DatabaseIO *o_database = o_region.get_database();

    o_region.begin_mode(Ioss::STATE_DEFINE_MODEL);

    Ioss::NodeBlock *i_nb        = i_region.get_node_blocks()[0];
    int64_t          spatial_dim = 3;
    int64_t          num_nodes   = i_nb->entity_count();
    Ioss::NodeBlock *o_nb = new Ioss::NodeBlock(o_database, "nodeblock_1", num_nodes, spatial_dim);
    o_region.add(o_nb);

    for (Ioss::ElementBlock *i_eb : i_region.get_element_blocks()) {
      Ioss::ElementBlock *o_eb = new Ioss::ElementBlock(
          o_database, i_eb->name(), i_eb->topology()->name(), i_eb->entity_count());
      o_eb->property_add(i_eb->get_property("id"));
      o_region.add(o_eb);
    }

    o_region.end_mode(Ioss::STATE_DEFINE_MODEL);
  }

  void write_model(const Ioss::Region &i_region, Ioss::Region &o_region)
  {
    Ioss::NodeBlock *i_nb = i_region.get_node_blocks()[0];
    Ioss::NodeBlock *o_nb = o_region.get_node_blocks()[0];

    o_region.begin_mode(Ioss::STATE_MODEL);
    std::vector<double> coordinates;
    std::vector<int>    node_ids;
    i_nb->get_field_data("ids", node_ids);
    i_nb->get_field_data("mesh_model_coordinates", coordinates);

    o_nb->put_field_data("ids", node_ids);
    o_nb->put_field_data("mesh_model_coordinates", coordinates);

    for (Ioss::ElementBlock *i_eb : i_region.get_element_blocks()) {
      Ioss::ElementBlock *o_eb = o_region.get_element_block(i_eb->name());
      std::vector<int>    elem_ids;
      std::vector<int>    connectivity;

      i_eb->get_field_data("ids", elem_ids);
      i_eb->get_field_data("connectivity", connectivity);

      o_eb->put_field_data("ids", elem_ids);
      o_eb->put_field_data("connectivity", connectivity);
    }

    o_region.end_mode(Ioss::STATE_MODEL);
  }

  void define_transient(const Ioss::Region &, Ioss::Region &o_region,
                        const std::string &elemFieldName)
  {
    o_region.begin_mode(Ioss::STATE_DEFINE_TRANSIENT);

    for (Ioss::ElementBlock *o_eb : o_region.get_element_blocks()) {
      size_t      num_elem = o_eb->get_property("entity_count").get_int();
      std::string storage  = "scalar";

      Ioss::Field field(elemFieldName, Ioss::Field::REAL, storage, 1, Ioss::Field::Field::TRANSIENT,
                        num_elem);
      o_eb->field_add(field);
    }
    o_region.end_mode(Ioss::STATE_DEFINE_TRANSIENT);
  }

  int write_transient(Ioss::Region &o_region, const std::string &elemFieldName, const double time)
  {
    o_region.begin_mode(Ioss::STATE_TRANSIENT);
    int step = o_region.add_state(time);
    o_region.begin_state(step);

    for (Ioss::ElementBlock *o_eb : o_region.get_element_blocks()) {
      size_t num_elem = o_eb->get_property("entity_count").get_int();

      std::vector<double> field_data(num_elem);
      std::vector<int>    elem_ids;

      o_eb->get_field_data("ids", elem_ids);
      for (size_t i = 0; i < elem_ids.size(); i++) {
        field_data[i] = (double)elem_ids[i] + 100 * time;
      }

      o_eb->put_field_data(elemFieldName, field_data);
    }

    o_region.end_state(step);
    o_region.end_mode(Ioss::STATE_TRANSIENT);

    return step;
  }

  class Observer : public Ioss::DynamicTopologyObserver
  {
  public:
    Observer(Ioss::Region &inputRegion_, const std::string &elemFieldName_,
             const Ioss::FileControlOption fileControlOption_)
        : inputRegion(inputRegion_), elemFieldName(elemFieldName_),
          fileControlOption(fileControlOption_)
    {
    }

    virtual ~Observer() {}

    void define_model(Ioss::Region &region) override { ::define_model(inputRegion, region); }

    void write_model(Ioss::Region &region) override { ::write_model(inputRegion, region); }

    void define_transient(Ioss::Region &region) override
    {
      ::define_transient(inputRegion, region, elemFieldName);
    }

    Ioss::FileControlOption get_control_option() const override { return fileControlOption; }

  private:
    Observer();

    Ioss::Region           &inputRegion;
    const std::string       elemFieldName;
    Ioss::FileControlOption fileControlOption;
  };

  void run_simple_topology_change(const Ioss::Region &i_region, Ioss::Region &o_region,
                                  std::shared_ptr<Observer> observer,
                                  const std::string        &elemFieldName)
  {
    define_model(i_region, o_region);
    write_model(i_region, o_region);

    define_transient(i_region, o_region, elemFieldName);

    double time = 0.0;
    write_transient(o_region, elemFieldName, time);

    auto min_result1 = o_region.get_min_time();
    EXPECT_EQ(1, min_result1.first);
    EXPECT_NEAR(0.0, min_result1.second, 1.0e-6);

    auto max_result1 = o_region.get_max_time();
    EXPECT_EQ(1, max_result1.first);
    EXPECT_NEAR(0.0, max_result1.second, 1.0e-6);

    observer->set_topology_modification(Ioss::TOPOLOGY_UNKNOWN);

    time = 1.0;
    write_transient(o_region, elemFieldName, time);

    auto min_result2 = o_region.get_min_time();
    EXPECT_EQ(1, min_result2.first);
    EXPECT_NEAR(1.0, min_result2.second, 1.0e-6);

    auto max_result2 = o_region.get_max_time();
    EXPECT_EQ(1, max_result2.first);
    EXPECT_NEAR(1.0, max_result2.second, 1.0e-6);

    observer->set_topology_modification(Ioss::TOPOLOGY_SAME);

    time = 2.0;
    write_transient(o_region, elemFieldName, time);

    auto min_result3 = o_region.get_min_time();
    EXPECT_EQ(1, min_result3.first);
    EXPECT_NEAR(1.0, min_result3.second, 1.0e-6);

    auto max_result3 = o_region.get_max_time();
    EXPECT_EQ(2, max_result3.first);
    EXPECT_NEAR(2.0, max_result3.second, 1.0e-6);
  }

  void cleanup_multi_files(const std::string &outFile)
  {
    Ioss::ParallelUtils util(Ioss::ParallelUtils::comm_world());

    std::string file1 =
        Ioss::Utils::decode_filename(outFile, util.parallel_rank(), util.parallel_size());
    unlink(file1.c_str());

    std::string file2 = Ioss::Utils::decode_filename(outFile + "-s0002", util.parallel_rank(),
                                                     util.parallel_size());
    unlink(file2.c_str());
  }

  void run_multi_file_simple_topology_change(const std::string &elemFieldName,
                                             const std::string &outFile)
  {
    Ioss::Init::Initializer io;
    Ioss::ParallelUtils     util(Ioss::ParallelUtils::comm_world());

    int numBlocks = util.parallel_size();

    std::string meshDesc = get_many_block_mesh_desc(numBlocks);

    Ioss::PropertyManager propertyManager;

    Ioss::DatabaseIO *i_database = Ioss::IOFactory::create(
        "textmesh", meshDesc, Ioss::READ_MODEL, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region i_region(i_database, "input_model");
    EXPECT_TRUE(i_database != nullptr);
    EXPECT_TRUE(i_database->ok(true));

    Ioss::DatabaseIO *o_database = Ioss::IOFactory::create(
        "exodus", outFile, Ioss::WRITE_RESULTS, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region o_region(o_database, "output_model");
    EXPECT_TRUE(o_database != nullptr);
    EXPECT_TRUE(o_database->ok(true));

    auto fileControlOption = Ioss::FileControlOption::CONTROL_AUTO_MULTI_FILE;
    auto observer          = std::make_shared<Observer>(i_region, elemFieldName, fileControlOption);
    o_region.register_mesh_modification_observer(observer);

    run_simple_topology_change(i_region, o_region, observer, elemFieldName);
  }

  TEST(TestDynamicWrite, multi_file_simple_topology_modification)
  {
    std::string outFile("multiFileManyBlocks.g");
    std::string elemFieldName = "elem_field";

    cleanup_multi_files(outFile);
    run_multi_file_simple_topology_change(elemFieldName, outFile);
    cleanup_multi_files(outFile);
  }

  void cleanup_single_file(const std::string &outFile)
  {
    Ioss::ParallelUtils util(Ioss::ParallelUtils::comm_world());

    std::string file1 =
        Ioss::Utils::decode_filename(outFile, util.parallel_rank(), util.parallel_size());
    unlink(file1.c_str());
  }

  void run_single_file_simple_topology_change(const std::string &elemFieldName,
                                              const std::string &outFile)
  {
    Ioss::Init::Initializer io;
    Ioss::ParallelUtils     util(Ioss::ParallelUtils::comm_world());

    int numBlocks = util.parallel_size();

    std::string meshDesc = get_many_block_mesh_desc(numBlocks);

    Ioss::PropertyManager propertyManager;

    Ioss::DatabaseIO *i_database = Ioss::IOFactory::create(
        "textmesh", meshDesc, Ioss::READ_MODEL, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region i_region(i_database, "input_model");
    EXPECT_TRUE(i_database != nullptr);
    EXPECT_TRUE(i_database->ok(true));

    propertyManager.add(Ioss::Property("ENABLE_FILE_GROUPS", 1));
    propertyManager.add(Ioss::Property("APPEND_OUTPUT", Ioss::DB_APPEND_GROUP));
    Ioss::DatabaseIO *o_database = Ioss::IOFactory::create(
        "exodus", outFile, Ioss::WRITE_RESULTS, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region o_region(o_database, "output_model");
    EXPECT_TRUE(o_database != nullptr);
    EXPECT_TRUE(o_database->ok(true));

    auto fileControlOption = Ioss::FileControlOption::CONTROL_AUTO_SINGLE_FILE;
    auto observer          = std::make_shared<Observer>(i_region, elemFieldName, fileControlOption);
    o_region.register_mesh_modification_observer(observer);

    run_simple_topology_change(i_region, o_region, observer, elemFieldName);

    Ioss::NameList names      = o_database->groups_describe(false);
    Ioss::NameList full_names = o_database->groups_describe(true);

    std::vector<std::string> gold_names{"/", "STEP-1", "STEP-2"};
    std::vector<std::string> gold_full_names{"/", "/STEP-1", "/STEP-2"};

    EXPECT_EQ(gold_names, names);
    EXPECT_EQ(gold_full_names, full_names);
  }

  TEST(TestDynamicWrite, single_file_simple_topology_modification)
  {
    std::string outFile("singleFileManyBlocks.g");
    std::string elemFieldName = "elem_field";

    cleanup_single_file(outFile);
    run_single_file_simple_topology_change(elemFieldName, outFile);
    cleanup_single_file(outFile);
  }

  TEST(TestDynamicWrite, single_file_groups_not_enabled)
  {
    Ioss::Init::Initializer io;
    Ioss::ParallelUtils     util(Ioss::ParallelUtils::comm_world());

    int numBlocks = util.parallel_size();
    if (numBlocks > 1)
      GTEST_SKIP();

    std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,block_1"
                           "|coordinates:0,0,0,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,1,1,0,1,1";

    Ioss::PropertyManager propertyManager;

    Ioss::DatabaseIO *i_database = Ioss::IOFactory::create(
        "textmesh", meshDesc, Ioss::READ_MODEL, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region i_region(i_database, "input_model");
    EXPECT_TRUE(i_database != nullptr);
    EXPECT_TRUE(i_database->ok(true));

    std::string outFile("singleFileGroupsNotEnabled.g");
    std::string elemFieldName = "elem_field";
    cleanup_single_file(outFile);

    // Need the line below to allow this to pass
    // propertyManager.add(Ioss::Property("ENABLE_FILE_GROUPS", 1));
    propertyManager.add(Ioss::Property("APPEND_OUTPUT", Ioss::DB_APPEND_GROUP));
    Ioss::DatabaseIO *o_database = Ioss::IOFactory::create(
        "exodus", outFile, Ioss::WRITE_RESULTS, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region o_region(o_database, "output_model");
    EXPECT_TRUE(o_database != nullptr);
    EXPECT_TRUE(o_database->ok(true));

    auto fileControlOption = Ioss::FileControlOption::CONTROL_AUTO_SINGLE_FILE;
    auto observer          = std::make_shared<Observer>(i_region, elemFieldName, fileControlOption);
    EXPECT_THROW(o_region.register_mesh_modification_observer(observer), std::runtime_error);
    cleanup_single_file(outFile);
  }

  TEST(TestDynamicWrite, create_subgroup_with_file_reopen)
  {
    std::string outFile("subgroupManyBlocks.g");
    std::string elemFieldName = "elem_field";

    Ioss::Init::Initializer io;
    Ioss::ParallelUtils     util(Ioss::ParallelUtils::comm_world());

    std::string file1 =
        Ioss::Utils::decode_filename(outFile, util.parallel_rank(), util.parallel_size());
    unlink(file1.c_str());

    int numBlocks = util.parallel_size();
    if (numBlocks > 1)
      GTEST_SKIP();

    std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,block_1"
                           "|coordinates:0,0,0,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,1,1,0,1,1";

    Ioss::PropertyManager propertyManager;

    Ioss::DatabaseIO *i_database = Ioss::IOFactory::create(
        "textmesh", meshDesc, Ioss::READ_MODEL, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region i_region(i_database, "input_model");
    EXPECT_TRUE(i_database != nullptr);
    EXPECT_TRUE(i_database->ok(true));

    {
      propertyManager.add(Ioss::Property("ENABLE_FILE_GROUPS", 1));
      Ioss::DatabaseIO *o_database =
          Ioss::IOFactory::create("exodus", outFile, Ioss::WRITE_RESULTS,
                                  Ioss::ParallelUtils::comm_world(), propertyManager);
      Ioss::Region o_region(o_database, "output_model");
      EXPECT_TRUE(o_database != nullptr);
      EXPECT_TRUE(o_database->ok(true));
      o_database->create_subgroup("GROUP_1");
    }

    {
      propertyManager.add(Ioss::Property("APPEND_OUTPUT", Ioss::DB_APPEND_GROUP));
      Ioss::DatabaseIO *o_database =
          Ioss::IOFactory::create("exodus", outFile, Ioss::WRITE_RESULTS,
                                  Ioss::ParallelUtils::comm_world(), propertyManager);
      Ioss::Region o_region(o_database, "output_model");
      EXPECT_TRUE(o_database != nullptr);
      EXPECT_TRUE(o_database->ok(true));

      // Group pointer is still at root level
      o_database->create_subgroup("GROUP_2");

      Ioss::NameList names      = o_database->groups_describe(false);
      Ioss::NameList full_names = o_database->groups_describe(true);

      std::vector<std::string> gold_names{"/", "GROUP_1", "GROUP_2"};
      std::vector<std::string> gold_full_names{"/", "/GROUP_1", "/GROUP_2"};

      EXPECT_EQ(gold_names, names);
      EXPECT_EQ(gold_full_names, full_names);
    }

    unlink(file1.c_str());
  }

  TEST(TestDynamicWrite, create_subgroup_with_file_persistence_and_child_group)
  {
    std::string outFile("subgroupManyBlocks.g");
    std::string elemFieldName = "elem_field";

    Ioss::Init::Initializer io;
    Ioss::ParallelUtils     util(Ioss::ParallelUtils::comm_world());

    std::string file1 =
        Ioss::Utils::decode_filename(outFile, util.parallel_rank(), util.parallel_size());
    unlink(file1.c_str());

    int numBlocks = util.parallel_size();
    if (numBlocks > 1)
      GTEST_SKIP();

    std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,block_1"
                           "|coordinates:0,0,0,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,1,1,0,1,1";

    Ioss::PropertyManager propertyManager;

    Ioss::DatabaseIO *i_database = Ioss::IOFactory::create(
        "textmesh", meshDesc, Ioss::READ_MODEL, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region i_region(i_database, "input_model");
    EXPECT_TRUE(i_database != nullptr);
    EXPECT_TRUE(i_database->ok(true));

    {
      propertyManager.add(Ioss::Property("ENABLE_FILE_GROUPS", 1));
      propertyManager.add(Ioss::Property("APPEND_OUTPUT", Ioss::DB_APPEND_GROUP));
      Ioss::DatabaseIO *o_database =
          Ioss::IOFactory::create("exodus", outFile, Ioss::WRITE_RESULTS,
                                  Ioss::ParallelUtils::comm_world(), propertyManager);
      Ioss::Region o_region(o_database, "output_model");
      EXPECT_TRUE(o_database != nullptr);
      EXPECT_TRUE(o_database->ok(true));

      o_database->create_subgroup("GROUP_1");

      // Group pointer is at "GROUP_1" ... "GROUP_2" is a child
      o_database->create_subgroup("GROUP_2");

      Ioss::NameList names      = o_database->groups_describe(false);
      Ioss::NameList full_names = o_database->groups_describe(true);

      std::vector<std::string> gold_names{"/", "GROUP_1", "GROUP_2"};
      std::vector<std::string> gold_full_names{"/", "/GROUP_1", "/GROUP_1/GROUP_2"};

      EXPECT_EQ(gold_names, names);
      EXPECT_EQ(gold_full_names, full_names);
    }

    unlink(file1.c_str());
  }

  TEST(TestDynamicWrite, create_subgroup_with_file_persistence_and_no_child_group)
  {
    std::string outFile("subgroupManyBlocks.g");
    std::string elemFieldName = "elem_field";

    Ioss::Init::Initializer io;
    Ioss::ParallelUtils     util(Ioss::ParallelUtils::comm_world());

    std::string file1 =
        Ioss::Utils::decode_filename(outFile, util.parallel_rank(), util.parallel_size());
    unlink(file1.c_str());

    int numBlocks = util.parallel_size();
    if (numBlocks > 1)
      GTEST_SKIP();

    std::string meshDesc = "0,1,HEX_8,1,2,3,4,5,6,7,8,block_1"
                           "|coordinates:0,0,0,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,1,1,0,1,1";

    Ioss::PropertyManager propertyManager;

    Ioss::DatabaseIO *i_database = Ioss::IOFactory::create(
        "textmesh", meshDesc, Ioss::READ_MODEL, Ioss::ParallelUtils::comm_world(), propertyManager);
    Ioss::Region i_region(i_database, "input_model");
    EXPECT_TRUE(i_database != nullptr);
    EXPECT_TRUE(i_database->ok(true));

    {
      propertyManager.add(Ioss::Property("ENABLE_FILE_GROUPS", 1));
      propertyManager.add(Ioss::Property("APPEND_OUTPUT", Ioss::DB_APPEND_GROUP));
      Ioss::DatabaseIO *o_database =
          Ioss::IOFactory::create("exodus", outFile, Ioss::WRITE_RESULTS,
                                  Ioss::ParallelUtils::comm_world(), propertyManager);
      Ioss::Region o_region(o_database, "output_model");
      EXPECT_TRUE(o_database != nullptr);
      EXPECT_TRUE(o_database->ok(true));

      o_database->create_subgroup("GROUP_1");

      // Group pointer is reset to root group
      EXPECT_TRUE(o_database->open_root_group());
      o_database->create_subgroup("GROUP_2");

      Ioss::NameList names      = o_database->groups_describe(false);
      Ioss::NameList full_names = o_database->groups_describe(true);

      std::vector<std::string> gold_names{"/", "GROUP_1", "GROUP_2"};
      std::vector<std::string> gold_full_names{"/", "/GROUP_1", "/GROUP_2"};

      EXPECT_EQ(gold_names, names);
      EXPECT_EQ(gold_full_names, full_names);
    }

    unlink(file1.c_str());
  }

  void test_single_file_simple_topology_change_data(Ioss::Region      &i_region,
                                                    const std::string &elemFieldName, int gold_step,
                                                    double gold_time)
  {
    i_region.begin_state(gold_step);
    for (Ioss::ElementBlock *i_eb : i_region.get_element_blocks()) {
      size_t num_elem = i_eb->get_property("entity_count").get_int();

      std::vector<double> field_data(num_elem);
      std::vector<int>    elem_ids;

      i_eb->get_field_data(elemFieldName, field_data);
      i_eb->get_field_data("ids", elem_ids);

      for (size_t i = 0; i < elem_ids.size(); i++) {
        double gold_value = (double)elem_ids[i] + 100 * gold_time;
        EXPECT_NEAR(gold_value, field_data[i], 1.0e-6);
      }
    }
  }

  void read_and_test_single_file_simple_topology_change(const std::string &elemFieldName,
                                                        const std::string &outFile)
  {
    Ioss::PropertyManager propertyManager;

    Ioss::DatabaseIO *i_database = Ioss::IOFactory::create(
        "exodus", outFile, Ioss::READ_RESTART, Ioss::ParallelUtils::comm_world(), propertyManager);

    Ioss::NameList names      = i_database->groups_describe(false);
    Ioss::NameList full_names = i_database->groups_describe(true);

    std::vector<std::string> gold_names{"/", "STEP-1", "STEP-2"};
    std::vector<std::string> gold_full_names{"/", "/STEP-1", "/STEP-2"};

    EXPECT_EQ(gold_names, names);
    EXPECT_EQ(gold_full_names, full_names);

    EXPECT_TRUE(i_database->open_group("STEP-1"));

    Ioss::Region i_region(i_database, "input_model");
    EXPECT_TRUE(i_database != nullptr);
    EXPECT_TRUE(i_database->ok(true));

    double gold_time   = 0.0;
    int    gold_step   = 1;
    auto   min_result1 = i_region.get_min_time();
    EXPECT_EQ(gold_step, min_result1.first);
    EXPECT_NEAR(gold_time, min_result1.second, 1.0e-6);

    auto max_result1 = i_region.get_max_time();
    EXPECT_EQ(gold_step, max_result1.first);
    EXPECT_NEAR(gold_time, max_result1.second, 1.0e-6);
    test_single_file_simple_topology_change_data(i_region, elemFieldName, gold_step, gold_time);

    EXPECT_TRUE(i_region.load_group_mesh("STEP-2"));

    double gold_min_time = 1.0;
    int    gold_min_step = 1;
    auto   min_result2   = i_region.get_min_time();
    EXPECT_EQ(gold_min_step, min_result2.first);
    EXPECT_NEAR(gold_min_time, min_result2.second, 1.0e-6);
    test_single_file_simple_topology_change_data(i_region, elemFieldName, gold_min_step,
                                                 gold_min_time);

    auto   max_result2   = i_region.get_max_time();
    double gold_max_time = 2.0;
    int    gold_max_step = 2;
    EXPECT_EQ(gold_max_step, max_result2.first);
    EXPECT_NEAR(gold_max_time, max_result2.second, 1.0e-6);
    test_single_file_simple_topology_change_data(i_region, elemFieldName, gold_max_step,
                                                 gold_max_time);
  }

  TEST(TestDynamicRead, single_file_simple_topology_modification)
  {
    std::string outFile("singleFileManyBlocks.g");
    std::string elemFieldName = "elem_field";

    cleanup_single_file(outFile);
    run_single_file_simple_topology_change(elemFieldName, outFile);
    read_and_test_single_file_simple_topology_change(elemFieldName, outFile);
    cleanup_single_file(outFile);
  }

} // namespace
