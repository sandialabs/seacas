// Copyright(C) 1999-2021, 2025 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "Ioss_ElementVariableType.h" // for ElementVariableType
#include "Ioss_Polyhedral.h"
#include <cassert> // for assert
#include <string>

#include "Ioss_CodeTypes.h"       // for IntVector
#include "Ioss_ElementTopology.h" // for ElementTopology

// ========================================================================
namespace Ioss {
  const char *Polyhedral::name = "nfaced";
  class St_Polyhedral : public ElementVariableType
  {
  public:
    static void factory();

  protected:
    St_Polyhedral() : ElementVariableType(Ioss::Polyhedral::name, 13) {} // 13 is a magic number
  };
} // namespace Ioss
void Ioss::St_Polyhedral::factory() { static Ioss::St_Polyhedral const registerThis; }

// ========================================================================
namespace {
  struct Constants
  {
    static const int nnode     = 0;
    static const int nedge     = 0;
    static const int nedgenode = 0;
    static const int nface     = 0;
    static const int nfacenode = 0;
  };
} // namespace

void Ioss::Polyhedral::factory()
{
  static Ioss::Polyhedral const registerThis;
  Ioss::St_Polyhedral::factory();
}

Ioss::Polyhedral::Polyhedral() : Ioss::ElementTopology(Ioss::Polyhedral::name, Ioss::Polyhedral::name)
{
  Ioss::ElementTopology::alias(Ioss::Polyhedral::name, "Polyhedral");
  Ioss::ElementTopology::alias(Ioss::Polyhedral::name, "nfaced");
}

int Ioss::Polyhedral::parametric_dimension() const { return 3; }
int Ioss::Polyhedral::spatial_dimension() const { return 3; }
int Ioss::Polyhedral::order() const { return 0; }

int Ioss::Polyhedral::number_corner_nodes() const { return number_nodes(); }
int Ioss::Polyhedral::number_nodes() const { return Constants::nnode; }
int Ioss::Polyhedral::number_edges() const { return Constants::nedge; }
int Ioss::Polyhedral::number_faces() const { return Constants::nface; }

int Ioss::Polyhedral::number_nodes_edge(int /* edge */) const { return Constants::nedgenode; }

int Ioss::Polyhedral::number_nodes_face(int face) const
{
  // face is 1-based.  0 passed in for all faces.
  assert(face >= 0 && face <= number_faces());
  IOSS_ASSERT_USED(face);
  return Constants::nfacenode;
}

int Ioss::Polyhedral::number_edges_face(int face) const
{
  // face is 1-based.  0 passed in for all faces.
  assert(face >= 0 && face <= number_faces());
  IOSS_ASSERT_USED(face);
  return Constants::nfacenode;
}

Ioss::IntVector Ioss::Polyhedral::edge_connectivity(int edge_number) const
{
  Ioss::IntVector connectivity;
  assert(edge_number >= 0 && edge_number <= Constants::nedge);
  IOSS_ASSERT_USED(edge_number);
  return connectivity;
}

Ioss::IntVector Ioss::Polyhedral::face_connectivity(int face_number) const
{
  assert(face_number >= 0 && face_number <= number_faces());
  IOSS_ASSERT_USED(face_number);
  Ioss::IntVector connectivity;
  return connectivity;
}

Ioss::IntVector Ioss::Polyhedral::element_connectivity() const
{
  Ioss::IntVector connectivity;
  return connectivity;
}

Ioss::ElementTopology *Ioss::Polyhedral::face_type(int face_number) const
{
  // face_number == 0 returns topology for all faces if
  // all faces are the same topology; otherwise, returns nullptr
  // face_number is 1-based.

  assert(face_number >= 0 && face_number <= number_faces());
  IOSS_ASSERT_USED(face_number);
  return Ioss::ElementTopology::factory("nsided");
}

Ioss::ElementTopology *Ioss::Polyhedral::edge_type(int edge_number) const
{
  // edge_number == 0 returns topology for all edges if
  // all edges are the same topology; otherwise, returns nullptr
  // edge_number is 1-based.

  assert(edge_number >= 0 && edge_number <= number_edges());
  IOSS_ASSERT_USED(edge_number);
  return Ioss::ElementTopology::factory("edge2");
}
