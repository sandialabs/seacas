// Copyright(C) 1999-2021, 2025 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include "Ioss_ElementVariableType.h" // for ElementVariableType
#include "Ioss_Polygonal.h"
#include <cassert> // for assert
#include <string>

#include "Ioss_CodeTypes.h"       // for IntVector
#include "Ioss_ElementTopology.h" // for ElementTopology

// ========================================================================
namespace Ioss {
  const char *Polygonal::name = "nsided";
  class St_Polygonal : public ElementVariableType
  {
  public:
    static void factory();

  protected:
    St_Polygonal() : ElementVariableType(Ioss::Polygonal::name, 13) {} // 13 is a magic number
  };
} // namespace Ioss
void Ioss::St_Polygonal::factory() { static Ioss::St_Polygonal const registerThis; }

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

void Ioss::Polygonal::factory()
{
  static Ioss::Polygonal const registerThis;
  Ioss::St_Polygonal::factory();
}

Ioss::Polygonal::Polygonal() : Ioss::ElementTopology(Ioss::Polygonal::name, Ioss::Polygonal::name)
{
  Ioss::ElementTopology::alias(Ioss::Polygonal::name, "Polygonal");
  Ioss::ElementTopology::alias(Ioss::Polygonal::name, "nsided");
}

int Ioss::Polygonal::parametric_dimension() const { return 2; }
int Ioss::Polygonal::spatial_dimension() const { return 2; }
int Ioss::Polygonal::order() const { return 0; }

int Ioss::Polygonal::number_corner_nodes() const { return number_nodes(); }
int Ioss::Polygonal::number_nodes() const { return Constants::nnode; }
int Ioss::Polygonal::number_edges() const { return Constants::nedge; }
int Ioss::Polygonal::number_faces() const { return Constants::nface; }

int Ioss::Polygonal::number_nodes_edge(int /* edge */) const { return Constants::nedgenode; }

int Ioss::Polygonal::number_nodes_face(int face) const
{
  // face is 1-based.  0 passed in for all faces.
  assert(face >= 0 && face <= number_faces());
  IOSS_ASSERT_USED(face);
  return Constants::nfacenode;
}

int Ioss::Polygonal::number_edges_face(int face) const
{
  // face is 1-based.  0 passed in for all faces.
  assert(face >= 0 && face <= number_faces());
  IOSS_ASSERT_USED(face);
  return Constants::nfacenode;
}

Ioss::IntVector Ioss::Polygonal::edge_connectivity(int edge_number) const
{
  Ioss::IntVector connectivity;
  assert(edge_number >= 0 && edge_number <= Constants::nedge);
  IOSS_ASSERT_USED(edge_number);
  return connectivity;
}

Ioss::IntVector Ioss::Polygonal::face_connectivity(int face_number) const
{
  assert(face_number >= 0 && face_number <= number_faces());
  IOSS_ASSERT_USED(face_number);
  Ioss::IntVector connectivity;
  return connectivity;
}

Ioss::IntVector Ioss::Polygonal::element_connectivity() const
{
  Ioss::IntVector connectivity;
  return connectivity;
}

Ioss::ElementTopology *Ioss::Polygonal::face_type(int face_number) const
{
  // face_number == 0 returns topology for all faces if
  // all faces are the same topology; otherwise, returns nullptr
  // face_number is 1-based.

  return (Ioss::ElementTopology *)nullptr;
}

Ioss::ElementTopology *Ioss::Polygonal::edge_type(int edge_number) const
{
  // edge_number == 0 returns topology for all edges if
  // all edges are the same topology; otherwise, returns nullptr
  // edge_number is 1-based.

  assert(edge_number >= 0 && edge_number <= number_edges());
  IOSS_ASSERT_USED(edge_number);
  return Ioss::ElementTopology::factory("edge2");
}
