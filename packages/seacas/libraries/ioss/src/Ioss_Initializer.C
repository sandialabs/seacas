// Copyright(C) 1999-2020, 2023 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_ElementPermutation.h>

#include "Ioss_Beam2.h"
#include "Ioss_Beam3.h"
#include "Ioss_Beam4.h"
#include "Ioss_Edge2.h"
#include "Ioss_Edge3.h"
#include "Ioss_Edge4.h"
#include "Ioss_Hex16.h"
#include "Ioss_Hex20.h"
#include "Ioss_Hex27.h"
#include "Ioss_Hex32.h"
#include "Ioss_Hex64.h"
#include "Ioss_Hex8.h"
#include "Ioss_Hex9.h"
#include "Ioss_Initializer.h"
#include "Ioss_Node.h"
#include "Ioss_Pyramid13.h"
#include "Ioss_Pyramid14.h"
#include "Ioss_Pyramid18.h"
#include "Ioss_Pyramid19.h"
#include "Ioss_Pyramid5.h"
#include "Ioss_Quad12.h"
#include "Ioss_Quad16.h"
#include "Ioss_Quad4.h"
#include "Ioss_Quad6.h"
#include "Ioss_Quad8.h"
#include "Ioss_Quad9.h"
#include "Ioss_Shell4.h"
#include "Ioss_Shell8.h"
#include "Ioss_Shell9.h"
#include "Ioss_ShellLine2D2.h"
#include "Ioss_ShellLine2D3.h"
#include "Ioss_Sphere.h"
#include "Ioss_Spring2.h"
#include "Ioss_Spring3.h"
#include "Ioss_Super.h"
#include "Ioss_Tet10.h"
#include "Ioss_Tet11.h"
#include "Ioss_Tet14.h"
#include "Ioss_Tet15.h"
#include "Ioss_Tet16.h"
#include "Ioss_Tet4.h"
#include "Ioss_Tet40.h"
#include "Ioss_Tet8.h"
#include "Ioss_Tri13.h"
#include "Ioss_Tri3.h"
#include "Ioss_Tri4.h"
#include "Ioss_Tri6.h"
#include "Ioss_Tri7.h"
#include "Ioss_Tri9.h"
#include "Ioss_TriShell3.h"
#include "Ioss_TriShell4.h"
#include "Ioss_TriShell6.h"
#include "Ioss_TriShell7.h"
#include "Ioss_Unknown.h"
#include "Ioss_Wedge12.h"
#include "Ioss_Wedge15.h"
#include "Ioss_Wedge16.h"
#include "Ioss_Wedge18.h"
#include "Ioss_Wedge20.h"
#include "Ioss_Wedge21.h"
#include "Ioss_Wedge24.h"
#include "Ioss_Wedge52.h"
#include "Ioss_Wedge6.h"
#if defined IOSS_THREADSAFE
#include <mutex>
#endif

Ioss::Initializer::Initializer()
{
  // List all storage types here with a call to their factory method.
  // This is Used to get the linker to pull in all needed libraries.
  Ioss::NullPermutation::factory();
  Ioss::SpherePermutation::factory();
  Ioss::LinePermutation::factory();
  Ioss::SpringPermutation::factory();
  Ioss::TriPermutation::factory();
  Ioss::QuadPermutation::factory();
  Ioss::TetPermutation::factory();
  Ioss::PyramidPermutation::factory();
  Ioss::WedgePermutation::factory();
  Ioss::HexPermutation::factory();
  Ioss::SuperPermutation::factory();

  Ioss::Sphere::factory();

  Ioss::Edge2::factory();
  Ioss::Edge3::factory();
  Ioss::Edge4::factory();

  Ioss::Spring2::factory();
  Ioss::Spring3::factory();
  Ioss::Beam2::factory();
  Ioss::Beam3::factory();
  Ioss::Beam4::factory();
  Ioss::ShellLine2D2::factory();
  Ioss::ShellLine2D3::factory();

  Ioss::Hex8::factory();
  Ioss::Hex9::factory();
  Ioss::Hex16::factory();
  Ioss::Hex20::factory();
  Ioss::Hex27::factory();
  Ioss::Hex32::factory();
  Ioss::Hex64::factory();

  Ioss::Node::factory();

  Ioss::Pyramid5::factory();
  Ioss::Pyramid13::factory();
  Ioss::Pyramid14::factory();
  Ioss::Pyramid18::factory();
  Ioss::Pyramid19::factory();

  Ioss::Quad4::factory();
  Ioss::Quad6::factory();
  Ioss::Quad8::factory();
  Ioss::Quad9::factory();
  Ioss::Quad12::factory();
  Ioss::Quad16::factory();

  Ioss::Shell4::factory();
  Ioss::Shell8::factory();
  Ioss::Shell9::factory();

  Ioss::Tet4::factory();
  Ioss::Tet8::factory();
  Ioss::Tet10::factory();
  Ioss::Tet11::factory();
  Ioss::Tet14::factory();
  Ioss::Tet15::factory();
  Ioss::Tet16::factory();
  Ioss::Tet40::factory();

  Ioss::Tri3::factory();
  Ioss::Tri4::factory();
  Ioss::Tri6::factory();
  Ioss::Tri7::factory();
  Ioss::Tri9::factory();
  Ioss::Tri13::factory();

  Ioss::TriShell3::factory();
  Ioss::TriShell4::factory();
  Ioss::TriShell6::factory();
  Ioss::TriShell7::factory();

  Ioss::Unknown::factory();

  Ioss::Wedge6::factory();
  Ioss::Wedge12::factory();
  Ioss::Wedge15::factory();
  Ioss::Wedge16::factory();
  Ioss::Wedge18::factory();
  Ioss::Wedge20::factory();
  Ioss::Wedge21::factory();
  Ioss::Wedge24::factory();
  Ioss::Wedge52::factory();

  Ioss::Super::factory();
}
