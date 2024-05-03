// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catalyst_tests/Iocatalyst_DatabaseIOTest.h>


#include <Ioss_ElementBlock.h>


TEST_F(Iocatalyst_DatabaseIOTest, WriteThreeElementBlocksWith24Cells)
{
  Iocatalyst::BlockMesh bmOne;
  setBlockMeshSize(2, 2, 2);
  addBlockMesh(bmOne);

  Iocatalyst::BlockMesh bmTwo;
  setOrigin(2, 0, 0);
  setBlockMeshSize(2, 2, 2);
  addBlockMesh(bmTwo);

  Iocatalyst::BlockMesh bmThree;
  setOrigin(4, 0, 0);
  setBlockMeshSize(2, 2, 2);
  addBlockMesh(bmThree);

  runUnstructuredTest("test_eb_3_cells_24");
}

TEST_F(Iocatalyst_DatabaseIOTest, WriteOneElementBlockWith8Cells)
{
  Iocatalyst::BlockMesh bm;
  setBlockMeshSize(2, 2, 2);
  addBlockMesh(bm);
  runUnstructuredTest("test_eb_1_cells_8");
}

TEST_F(Iocatalyst_DatabaseIOTest, WriteOneElementBlockWith300Cells)
{
  Iocatalyst::BlockMesh bm;
  setBlockMeshSize(10, 10, 3);
  addBlockMesh(bm);
  runUnstructuredTest("test_eb_1_cells_300");
}

TEST_F(Iocatalyst_DatabaseIOTest, WriteThreeElementBlocksWith835Cells)
{
  Iocatalyst::BlockMesh bmOne;
  setBlockMeshSize(5, 15, 3);
  addBlockMesh(bmOne);

  Iocatalyst::BlockMesh bmTwo;
  setOrigin(5, 0, 0);
  setBlockMeshSize(8, 14, 4);
  addBlockMesh(bmTwo);

  Iocatalyst::BlockMesh bmThree;
  setOrigin(13, 0, 0);
  setBlockMeshSize(3, 6, 9);
  addBlockMesh(bmThree);

  runUnstructuredTest("test_eb_3_cells_835");
}

TEST_F(Iocatalyst_DatabaseIOTest, Exodus_Prop_ENABLE_FIELD_RECOGNITION_ON)
{
  Iocatalyst::BlockMesh bm;
  setBlockMeshSize(2, 2, 2);

  bm.addTransientCellField("foo_x", 2);
  bm.addTransientCellField("foo_y", 3);
  bm.addTransientCellField("foo_z", 4);


  addBlockMesh(bm);

  Ioss::PropertyManager iossProp;
  //iossProp.add(Ioss::Property("ENABLE_FIELD_RECOGNITION", "OFF"));
  Ioss::DatabaseIO *exo_d = writeAndGetExodusDatabaseOnRead("test_eb_1_enable_field_recog", iossProp);
  //Ioss::DatabaseIO *cat_d = writeAndGetCatalystDatabaseOnRead();

  Iocatalyst::BlockMeshSet::IOSSparams iop("cat", EXODUS_DATABASE_TYPE, iossProp);

  Ioss::DatabaseIO *cat_d = bmSet.getCatalystDatabase(iop);

  if(cat_d == nullptr){ EXPECT_TRUE(false) << "Catalyst db unable to initialize on read"; }
  Ioss::Region cat_reg(cat_d);
  
  Ioss::Region exo_reg(exo_d);
  
  auto cat_elemBlock = cat_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));
  auto exo_elemBlock = exo_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));

  bool exo_foo_exists = exo_elemBlock->field_exists("foo");
  bool cat_foo_exists = cat_elemBlock->field_exists("foo");
  EXPECT_TRUE(exo_foo_exists);
  EXPECT_TRUE(cat_foo_exists);
  if(exo_foo_exists && cat_foo_exists) 
    EXPECT_TRUE(exo_elemBlock->get_field("foo") == cat_elemBlock->get_field("foo"));
  
}

//Sanity Test
TEST_F(Iocatalyst_DatabaseIOTest, Exodus_Prop_ENABLE_FIELD_RECOGNITION_OFF)
{
  Iocatalyst::BlockMesh bm;
  setBlockMeshSize(2, 2, 2);

  bm.addTransientCellField("foo_x", 2);
  bm.addTransientCellField("foo_y", 3);
  bm.addTransientCellField("foo_z", 4);


  addBlockMesh(bm);

  Ioss::PropertyManager iossProp;
  iossProp.add(Ioss::Property("ENABLE_FIELD_RECOGNITION", "OFF"));
  Ioss::DatabaseIO *exo_d = writeAndGetExodusDatabaseOnRead("test_eb_1_enable_field_recog", iossProp);
  //Ioss::DatabaseIO *cat_d = writeAndGetCatalystDatabaseOnRead();

  Iocatalyst::BlockMeshSet::IOSSparams iop("cat", EXODUS_DATABASE_TYPE, iossProp);

  Ioss::DatabaseIO *cat_d = bmSet.getCatalystDatabase(iop);

  if(cat_d == nullptr){ EXPECT_TRUE(false) << "Catalyst db unable to initialize on read"; }
  Ioss::Region cat_reg(cat_d);
  
  Ioss::Region exo_reg(exo_d);
  
  auto cat_elemBlock = cat_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));
  auto exo_elemBlock = exo_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));

  bool exo_foo_x_exists = exo_elemBlock->field_exists("foo_x");
  bool cat_foo_x_exists = cat_elemBlock->field_exists("foo_x");
  EXPECT_TRUE(exo_foo_x_exists);
  EXPECT_TRUE(cat_foo_x_exists);
  if(exo_foo_x_exists && cat_foo_x_exists) 
    EXPECT_TRUE(exo_elemBlock->get_field("foo_x") == cat_elemBlock->get_field("foo_x"));
  
}


TEST_F(Iocatalyst_DatabaseIOTest, Exodus_Prop_IGNORE_REALN_FIELDS)
{
  Iocatalyst::BlockMesh bm;
  setBlockMeshSize(2, 2, 2);

  bm.addTransientCellField("foo_1", 2);
  bm.addTransientCellField("foo_2", 3);
  bm.addTransientCellField("foo_3", 4);


  addBlockMesh(bm);

  Ioss::PropertyManager iossProp;
  iossProp.add(Ioss::Property("IGNORE_REALN_FIELDS", "ON"));
  Ioss::DatabaseIO *exo_d = writeAndGetExodusDatabaseOnRead("test_eb_1_ignore_realn_fields", iossProp);
  //Ioss::DatabaseIO *cat_d = writeAndGetCatalystDatabaseOnRead();

  Iocatalyst::BlockMeshSet::IOSSparams iop("cat", EXODUS_DATABASE_TYPE, iossProp);

  Ioss::DatabaseIO *cat_d = bmSet.getCatalystDatabase(iop);

  if(cat_d == nullptr){ EXPECT_TRUE(false) << "Catalyst db unable to initialize on read"; }
  Ioss::Region cat_reg(cat_d);
  
  Ioss::Region exo_reg(exo_d);
  
  auto cat_elemBlock = cat_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));
  auto exo_elemBlock = exo_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));

  bool exo_foo_1_exists = exo_elemBlock->field_exists("foo_1");
  bool cat_foo_1_exists = cat_elemBlock->field_exists("foo_1");
  EXPECT_TRUE(exo_foo_1_exists);
  EXPECT_TRUE(cat_foo_1_exists);
  if(exo_foo_1_exists && cat_foo_1_exists) 
    EXPECT_TRUE(exo_elemBlock->get_field("foo_1") == cat_elemBlock->get_field("foo_1"));
}

TEST_F(Iocatalyst_DatabaseIOTest, Exodus_Prop_FIELD_SUFFIX_SEPARATOR)
{
  Iocatalyst::BlockMesh bm;
  setBlockMeshSize(2, 2, 2);

  bm.addTransientCellField("foo_x", 2);
  bm.addTransientCellField("foo_y", 3);
  bm.addTransientCellField("foo_z", 4);
  bm.addTransientCellField("bar:x", 5);
  bm.addTransientCellField("bar:y", 6);
  bm.addTransientCellField("bar:z", 7);


  addBlockMesh(bm);

  Ioss::PropertyManager iossProp;
  iossProp.add(Ioss::Property("FIELD_SUFFIX_SEPARATOR", ":"));
  
  Ioss::DatabaseIO *exo_d = writeAndGetExodusDatabaseOnRead("test_eb_1_field_suf_sep", iossProp);
  //Ioss::DatabaseIO *cat_d = writeAndGetCatalystDatabaseOnRead();

  Iocatalyst::BlockMeshSet::IOSSparams iop("cat", EXODUS_DATABASE_TYPE, iossProp);
  Ioss::DatabaseIO *cat_d = bmSet.getCatalystDatabase(iop);


  if(cat_d == nullptr){ EXPECT_TRUE(false) << "Catalyst db unable to initialize on read"; }
  Ioss::Region cat_reg(cat_d);
  
  Ioss::Region exo_reg(exo_d);
  
  auto cat_elemBlock = cat_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));
  auto exo_elemBlock = exo_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));

  bool exo_foo_x_exists = exo_elemBlock->field_exists("foo_x");
  bool cat_foo_x_exists = cat_elemBlock->field_exists("foo_x");
  EXPECT_TRUE(exo_foo_x_exists);
  EXPECT_TRUE(cat_foo_x_exists);
  if(exo_foo_x_exists && cat_foo_x_exists) 
    EXPECT_TRUE(exo_elemBlock->get_field("foo_x") == cat_elemBlock->get_field("foo_x"));
  
  bool exo_bar_exists = exo_elemBlock->field_exists("bar");
  bool cat_bar_exists = cat_elemBlock->field_exists("bar");
  EXPECT_TRUE(exo_bar_exists);
  EXPECT_TRUE(cat_bar_exists);
  if(exo_bar_exists && cat_bar_exists) 
    EXPECT_TRUE(exo_elemBlock->get_field("bar") == cat_elemBlock->get_field("bar"));

}

TEST_F(Iocatalyst_DatabaseIOTest, Exodus_Prop_FIELD_STRIP_TRAILING_UNDERSCORE)
{
  Iocatalyst::BlockMesh bm;
  setBlockMeshSize(2, 2, 2);

  bm.addTransientCellField("foo_x", 2);
  bm.addTransientCellField("foo_y", 3);
  bm.addTransientCellField("foo_z", 4);


  addBlockMesh(bm);

  Ioss::PropertyManager iossProp;
  iossProp.add(Ioss::Property("FIELD_STRIP_TRAILING_UNDERSCORE", "ON"));
  iossProp.add(Ioss::Property("FIELD_SUFFIX_SEPARATOR", ""));

  Ioss::DatabaseIO *exo_d = writeAndGetExodusDatabaseOnRead("test_eb_1_field_strip_tr_unders", iossProp);
  //Ioss::DatabaseIO *cat_d = writeAndGetCatalystDatabaseOnRead();

  Iocatalyst::BlockMeshSet::IOSSparams iop("cat", EXODUS_DATABASE_TYPE, iossProp);

  Ioss::DatabaseIO *cat_d = bmSet.getCatalystDatabase(iop);

  if(cat_d == nullptr){ EXPECT_TRUE(false) << "Catalyst db unable to initialize on read"; }
  Ioss::Region cat_reg(cat_d);
  
  Ioss::Region exo_reg(exo_d);
  
  auto cat_elemBlock = cat_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));
  auto exo_elemBlock = exo_reg.get_element_block(bmSet.getUnstructuredBlockName(bm.getID()));

  /*for(auto fname : exo_elemBlock->field_describe(Ioss::Field::TRANSIENT)){
    std::cout<<fname.c_str()<<std::endl;
  }*/

  bool exo_foo_exists = exo_elemBlock->field_exists("foo");
  bool cat_foo_exists = cat_elemBlock->field_exists("foo");
  EXPECT_TRUE(exo_foo_exists);
  EXPECT_TRUE(cat_foo_exists);
  if(exo_foo_exists && cat_foo_exists) 
    EXPECT_TRUE(exo_elemBlock->get_field("foo") == cat_elemBlock->get_field("foo"));

}

TEST_F(Iocatalyst_DatabaseIOTest, Exodus_Prop_SURFACE_SPLIT_TYPE)
{
  Iocatalyst::BlockMesh bm;
  setBlockMeshSize(2, 2, 2);

  addBlockMesh(bm);

  Ioss::PropertyManager iossProp;
  iossProp.add(Ioss::Property("SURFACE_SPLIT_TYPE", "BLOCK"));

  Ioss::DatabaseIO *exo_d = writeAndGetExodusDatabaseOnRead("test_eb_1_surface_spl_type", iossProp);
  //Ioss::DatabaseIO *cat_d = writeAndGetCatalystDatabaseOnRead();

  Iocatalyst::BlockMeshSet::IOSSparams iop("cat", EXODUS_DATABASE_TYPE, iossProp);

  Ioss::DatabaseIO *cat_d = bmSet.getCatalystDatabase(iop);

  if(cat_d == nullptr){ EXPECT_TRUE(false) << "Catalyst db unable to initialize on read"; }
  Ioss::Region cat_reg(cat_d);
  
  Ioss::Region exo_reg(exo_d);

  auto cat_sideSet = cat_reg.get_sideset("SideSet" + std::to_string(bm.getID()));
  auto exo_sideSet = exo_reg.get_sideset("SideSet" + std::to_string(bm.getID()));
  EXPECT_TRUE(exo_sideSet!=nullptr && exo_sideSet==cat_sideSet);

}
