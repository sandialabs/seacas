// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <catalyst_tests/Iocatalyst_DatabaseIOTest.h>
#include <catalyst_tests/Iocatalyst_LoggingTest.h>
#include <catalyst/Iocatalyst_CatalystManager.h>
#include <Ioss_ParallelUtils.h>

TEST_F(LoggingTest, LoggingDefault)
{
    Ioss::ParallelUtils putils;
    Iocatalyst::CatalystManager::getInstance().writeToCatalystLogFile(putils, props);

    //log.setProperties(&props);
    //writeToCatalystLogFile(const DatabaseInfo &dbinfo, const Ioss::PropertyManager &props)
    //log.writeToLogFile();


}
