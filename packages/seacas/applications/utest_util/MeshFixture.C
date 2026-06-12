// Copyright(C) 1999-2020, 2022 National Technology & Engineering Solutions
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

#include "MeshFixture.h"

#include "Ioss_Assembly.h"
#include "Ioss_CommSet.h"
#include "Ioss_CopyDatabase.h"
#include "Ioss_ElementBlock.h"
#include "Ioss_ElementTopology.h"
#include "Ioss_EntityType.h"     // for EntityType, etc
#include "Ioss_Field.h"          // for Field, etc
#include "Ioss_GroupingEntity.h" // for GroupingEntity
#include "Ioss_IOFactory.h"      // for IOFactory
#include "Ioss_MeshType.h"       // for MeshType, etc
#include "Ioss_NodeBlock.h"
#include "Ioss_NodeSet.h"
#include "Ioss_ParallelUtils.h"

namespace {
Ioss::MeshCopyOptions get_default_mesh_copy_options()
{
  Ioss::MeshCopyOptions options{};
  options.verbose              = true;
  options.output_summary       = true;
  options.debug                = false;
  options.ints_64_bit          = false;
  options.data_storage_type    = 1;
  options.add_proc_id          = true;

  return options;
}

Ioss::PropertyManager get_default_properties_from_region(Ioss::Region *inputRegion)
{
  Ioss::PropertyManager properties;

  auto inputDB = inputRegion->get_database();

  // Get length of longest name on input file...
  int max_name_length = inputDB->maximum_symbol_length();
  if (max_name_length > 0) {
    properties.add(Ioss::Property("MAXIMUM_NAME_LENGTH", max_name_length));
  }

  // Get integer size being used on the input file and propagate
  // to output file...
  int int_byte_size_api = inputDB->int_byte_size_api();
  if (!properties.exists("INTEGER_SIZE_API")) {
    properties.add(Ioss::Property("INTEGER_SIZE_DB", int_byte_size_api));
    properties.add(Ioss::Property("INTEGER_SIZE_API", int_byte_size_api));
  }

  return properties;
}
}

namespace utest_util {

void MeshFixture::test_property_from_file(const std::string &inputFile, const std::string &propertyName,
                                          const std::string &propertyValue)
{
  test_property_from_file(get_comm(), inputFile, propertyName, propertyValue);
}

void MeshFixture::test_property_from_file(Ioss_MPI_Comm comm, const std::string &inputFile,
                                          const std::string &propertyName, const std::string &propertyValue)
{
  Ioss::DatabaseIO *testDB = Ioss::IOFactory::create("exodusII", inputFile, Ioss::READ_RESTART, comm);
  ASSERT_FALSE(testDB == nullptr || !testDB->ok(true));

  Ioss::Region testRegion(testDB, "test_region");

  Ioss::ElementBlock *testBlock = testRegion.get_element_block("block_1");
  EXPECT_TRUE(testBlock != nullptr);

  bool exists = testBlock->property_exists(propertyName);
  EXPECT_TRUE(exists) << "The exodus property: " << propertyName
                      << " with value: " << propertyValue
                      << " does not exist in the output file";

  if (exists) {
    auto prop = testBlock->get_property(propertyName);
    EXPECT_EQ(propertyValue, prop.get_string())
        << "The exodus property: " << propertyName << " with value: " << prop.get_string()
        << " does not match expected value: " << propertyValue;
  }
}

void MeshFixture::add_material_property_to_element_block(Ioss::Region *region, const std::string &blockName,
                                                         const std::string &propertyName,
                                                         const std::string &propertyValue)
{
  Ioss::ElementBlock *elemBlock = region->get_element_block(blockName);
  EXPECT_TRUE(elemBlock != nullptr) << "Could not find element block named: " << blockName
                                    << " in input region named: " << region->name();

  EXPECT_FALSE(elemBlock->property_exists(propertyName));
  elemBlock->property_add(
      Ioss::Property(propertyName, propertyValue, Ioss::Property::Origin::ATTRIBUTE));
}

void MeshFixture::write_region_to_file(Ioss::Region *inputRegion, Ioss::PropertyManager &properties,
                                       Ioss::MeshCopyOptions &options, const std::string &outputFile)
{
  // Write the mesh
  Ioss::DatabaseIO *dbo = Ioss::IOFactory::create("exodusII", outputFile, Ioss::WRITE_RESTART,
                                                  get_comm(), properties);
  ASSERT_FALSE(dbo == nullptr || !dbo->ok(true));

  // NOTE: 'outputRegion' owns 'dbo' pointer at this time
  Ioss::Region outputRegion(dbo, "region_2");

  Ioss::copy_database(*inputRegion, outputRegion, options);
}

void MeshFixture::write_region_to_file(Ioss::Region *inputRegion, Ioss::PropertyManager &properties,
                                       const std::string &outputFile)
{
  Ioss::MeshCopyOptions options = get_default_mesh_copy_options();
  write_region_to_file(inputRegion, properties, options, outputFile);
}

void MeshFixture::write_region_to_file(Ioss::Region *inputRegion, const std::string &outputFile)
{
  Ioss::MeshCopyOptions options = get_default_mesh_copy_options();

  Ioss::PropertyManager properties = get_default_properties_from_region(inputRegion);

  write_region_to_file(inputRegion, properties, options, outputFile);
}

}
