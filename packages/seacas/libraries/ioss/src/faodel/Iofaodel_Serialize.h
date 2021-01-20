// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#ifndef Iofaodel_Serialize_h
#define Iofaodel_Serialize_h

#include <Ioss_CommSet.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_DatabaseIO.h> // for DatabaseIO
#include <Ioss_EdgeBlock.h>
#include <Ioss_EdgeSet.h>
#include <Ioss_ElementBlock.h>
#include <Ioss_ElementSet.h>
#include <Ioss_FaceBlock.h>
#include <Ioss_FaceSet.h>
#include <Ioss_Field.h>   // for Region
#include <Ioss_IOFactory.h>
#include <Ioss_NodeBlock.h>
#include <Ioss_NodeSet.h>
#include <Ioss_Property.h>   // for Region
#include <Ioss_Region.h>
#include <Ioss_Region.h>   // for Region
#include <Ioss_SideBlock.h>
#include <Ioss_SideSet.h>
#include <Ioss_State.h>      // for State
#include <Ioss_StructuredBlock.h>

#include <cstddef>           // for size_t
#include <cstdint>           // for int64_t
#include <string>            // for string
#include <vector>            // for vector

#include <kelpie/Kelpie.hh>

namespace Ioss {
class CommSet;
class EdgeBlock;
class EdgeSet;
class ElementBlock;
class ElementSet;
class EntityBlock;
class FaceBlock;
class FaceSet;
class Field;
class GroupingEntity;
class NodeBlock;
class NodeSet;
class PropertyManager;
class Region;
class SideBlock;
class SideSet;
class StructuredBlock;
} // namespace Ioss

#endif // Iofaodel_Serialize
