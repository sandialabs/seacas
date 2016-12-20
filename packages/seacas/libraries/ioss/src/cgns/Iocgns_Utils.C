#include <Ioss_Bar2.h>
#include <Ioss_Bar3.h>
#include <Ioss_Hex20.h>
#include <Ioss_Hex27.h>
#include <Ioss_Hex8.h>
#include <Ioss_Node.h>
#include <Ioss_Pyramid13.h>
#include <Ioss_Pyramid14.h>
#include <Ioss_Pyramid5.h>
#include <Ioss_Quad4.h>
#include <Ioss_Quad8.h>
#include <Ioss_Quad9.h>
#include <Ioss_StructuredBlock.h>
#include <Ioss_Tet10.h>
#include <Ioss_Tet4.h>
#include <Ioss_Tri3.h>
#include <Ioss_Tri4.h>
#include <Ioss_Tri6.h>
#include <Ioss_Unknown.h>
#include <Ioss_Wedge15.h>
#include <Ioss_Wedge18.h>
#include <Ioss_Wedge6.h>

#include <cgns/Iocgns_Utils.h>

#include <cgnsconfig.h>
#if CG_BUILD_PARALLEL
#include <pcgnslib.h>
#else
#include <cgnslib.h>
#endif

namespace {
  struct Range
  {
    Range(int a, int b) : m_beg(a < b ? a : b), m_end(a < b ? b : a), m_reversed(b < a) {}

    int  m_beg;
    int  m_end;
    bool m_reversed;
  };

  bool overlaps(const Range &a, const Range &b) { return a.m_beg <= b.m_end && b.m_beg <= a.m_end; }

  bool bc_overlaps(const Ioss::StructuredBlock *block, const Ioss::BoundaryCondition &bc)
  {
    int ordinal[3];
    ordinal[0] = block->get_property("ni").get_int();
    ordinal[1] = block->get_property("nj").get_int();
    ordinal[2] = block->get_property("nk").get_int();

    if (ordinal[0] == 0 && ordinal[1] == 0 && ordinal[2] == 0) {
      return false;
    }

    int offset[3];
    offset[0] = block->get_property("offset_i").get_int();
    offset[1] = block->get_property("offset_j").get_int();
    offset[2] = block->get_property("offset_k").get_int();

    // Note that block range is nodes and m_ordinal[] is cells, so need to add 1 to range.
    Range z_i(1 + offset[0], ordinal[0] + offset[0] + 1);
    Range z_j(1 + offset[1], ordinal[1] + offset[1] + 1);
    Range z_k(1 + offset[2], ordinal[2] + offset[2] + 1);

    Range gc_i(bc.m_rangeBeg[0], bc.m_rangeEnd[0]);
    Range gc_j(bc.m_rangeBeg[1], bc.m_rangeEnd[1]);
    Range gc_k(bc.m_rangeBeg[2], bc.m_rangeEnd[2]);

    return overlaps(z_i, gc_i) && overlaps(z_j, gc_j) && overlaps(z_k, gc_k);
  }

  Range subset_range(const Range &a, const Range &b)
  {
    Range ret(std::max(a.m_beg, b.m_beg), std::min(a.m_end, b.m_end));
    ret.m_reversed = a.m_reversed || b.m_reversed;
    return ret;
  }

  void bc_subset_range(const Ioss::StructuredBlock *block, Ioss::BoundaryCondition &bc)
  {
    if (bc_overlaps(block, bc)) {
      int ordinal[3];
      ordinal[0] = block->get_property("ni").get_int();
      ordinal[1] = block->get_property("nj").get_int();
      ordinal[2] = block->get_property("nk").get_int();

      int offset[3];
      offset[0] = block->get_property("offset_i").get_int();
      offset[1] = block->get_property("offset_j").get_int();
      offset[2] = block->get_property("offset_k").get_int();

      // NOTE: Updates the range in bc

      // Note that block range is nodes and m_ordinal[] is cells, so need to add 1 to range.
      Range z_i(1 + offset[0], ordinal[0] + offset[0] + 1);
      Range z_j(1 + offset[1], ordinal[1] + offset[1] + 1);
      Range z_k(1 + offset[2], ordinal[2] + offset[2] + 1);

      Range gc_i(bc.m_rangeBeg[0], bc.m_rangeEnd[0]);
      Range gc_j(bc.m_rangeBeg[1], bc.m_rangeEnd[1]);
      Range gc_k(bc.m_rangeBeg[2], bc.m_rangeEnd[2]);

      Range gc_ii = subset_range(z_i, gc_i);
      Range gc_jj = subset_range(z_j, gc_j);
      Range gc_kk = subset_range(z_k, gc_k);

      bc.m_rangeBeg[0] = gc_ii.m_reversed ? gc_ii.m_end : gc_ii.m_beg;
      bc.m_rangeEnd[0] = gc_ii.m_reversed ? gc_ii.m_beg : gc_ii.m_end;
      bc.m_rangeBeg[1] = gc_jj.m_reversed ? gc_jj.m_end : gc_jj.m_beg;
      bc.m_rangeEnd[1] = gc_jj.m_reversed ? gc_jj.m_beg : gc_jj.m_end;
      bc.m_rangeBeg[2] = gc_kk.m_reversed ? gc_kk.m_end : gc_kk.m_beg;
      bc.m_rangeEnd[2] = gc_kk.m_reversed ? gc_kk.m_beg : gc_kk.m_end;
    }
    else {
      bc.m_rangeBeg = {{0, 0, 0}};
      bc.m_rangeEnd = {{0, 0, 0}};
    }
  }
}

void Iocgns::Utils::cgns_error(int cgnsid, const char *file, const char *function, int lineno,
                               int processor)
{
  std::ostringstream errmsg;
  errmsg << "CGNS error '" << cg_get_error() << "' at line " << lineno << " in file '" << file
         << "' in function '" << function << "'";
  if (processor >= 0) {
    errmsg << " on processor " << processor;
  }
  errmsg << ". Please report to gdsjaar@sandia.gov if you need help.";
  if (cgnsid > 0) {
#if CG_BUILD_PARALLEL
    cgp_close(cgnsid);
#else
    cg_close(cgnsid);
#endif
  }
  IOSS_ERROR(errmsg);
}

void Iocgns::Utils::update_property(const Ioss::GroupingEntity *ge, const std::string &property, int64_t value)
{
  if (ge->property_exists(property)) {
    if (ge->get_property(property).get_int() != value) {
      auto *nge = const_cast<Ioss::GroupingEntity*>(ge);
      nge->property_erase(property);
      nge->property_add(Ioss::Property(property, value));
    }
  }
  else {
    auto *nge = const_cast<Ioss::GroupingEntity*>(ge);
    nge->property_add(Ioss::Property(property, value));
  }
}

CG_ZoneType_t Iocgns::Utils::check_zone_type(int cgnsFilePtr)
{
  // ========================================================================
  // Get the number of zones (element blocks) in the mesh...
  int base      = 1;
  int num_zones = 0;
  int ierr      = cg_nzones(cgnsFilePtr, base, &num_zones);
  if (ierr != CG_OK) {
    cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
  }

  CG_ZoneType_t common_zone_type = CG_ZoneTypeNull;

  for (cgsize_t zone = 1; zone <= num_zones; zone++) {
    CG_ZoneType_t zone_type;
    ierr = cg_zone_type(cgnsFilePtr, base, zone, &zone_type);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }

    if (common_zone_type == CG_ZoneTypeNull) {
      common_zone_type = zone_type;
    }

    if (common_zone_type != zone_type) {
      std::ostringstream errmsg;
      errmsg << "ERROR: CGNS: Zone " << zone << " is not the same zone type as previous zones."
             << " This is currently not allowed or supported (hybrid mesh).";
      IOSS_ERROR(errmsg);
    }
  }
  return common_zone_type;
}

void Iocgns::Utils::common_write_meta_data(int cgnsFilePtr, const Ioss::Region &region, std::vector<size_t> &zone_offset)
{
  // Make sure mesh is not hybrid...
  if (region.mesh_type() == Ioss::MeshType::HYBRID) {
    std::ostringstream errmsg;
    errmsg << "ERROR: CGNS: The mesh on region " << region.name() << " is of type 'hybrid'."
           << " This is currently not allowed or supported.";
    IOSS_ERROR(errmsg);
  }

  int base           = 0;
  int phys_dimension = region.get_property("spatial_dimension").get_int();
  int ierr           = cg_base_write(cgnsFilePtr, "Base", phys_dimension, phys_dimension, &base);
  if (ierr != CG_OK) {
    cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
  }

  ierr = cg_goto(cgnsFilePtr, base, "end");
  if (ierr != CG_OK) {
    cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
  }

  ierr = cg_descriptor_write("Information", "IOSS: CGNS Writer version -1");
  if (ierr != CG_OK) {
    cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
  }

  // Output the sidesets as Family_t nodes

  const auto &sidesets = region.get_sidesets();
  for (const auto &ss : sidesets) {
    int fam = 0;
    ierr    = cg_family_write(cgnsFilePtr, base, ss->name().c_str(), &fam);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
    int         bc_index = 0;
    CG_BCType_t bocotype = CG_BCTypeNull;
    if (ss->property_exists("bc_type")) {
      bocotype = (CG_BCType_t)ss->get_property("bc_type").get_int();
    }

    int64_t id = fam;
    if (ss->property_exists("id")) {
      id = ss->get_property("id").get_int();
    }

    ierr = cg_fambc_write(cgnsFilePtr, base, fam, "FamBC", bocotype, &bc_index);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }

    ierr = cg_goto(cgnsFilePtr, base, "Family_t", fam, NULL);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
    ierr = cg_descriptor_write("FamBC_TypeId", Ioss::Utils::to_string(bocotype).c_str());
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
    ierr = cg_descriptor_write("FamBC_TypeName", BCTypeName[bocotype]);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
    ierr = cg_descriptor_write("FamBC_UserId", Ioss::Utils::to_string(id).c_str());
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
    ierr = cg_descriptor_write("FamBC_UserName", ss->name().c_str());
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
  }


#if 0
  // Defer this to put_field_internal for ElementBlock so can generate
  // node_count if not kn
  const auto &element_blocks = region.get_element_blocks();
  for (const auto &eb : element_blocks) {
    int      zone    = 0;
    cgsize_t size[3] = {1, 0, 0};
    size[1]          = eb->get_property("entity_count").get_int();
    if (eb->property_exists("node_count")) {
      size[0] = eb->get_property("node_count").get_int();
    }
    ierr = cg_zone_write(cgnsFilePtr, base, eb->name().c_str(), size, CG_Unstructured, &zone);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
    update_property(eb, "zone", zone);
    update_property(eb, "section", zone);
    update_property(eb, "base", base);

    zone_offset[zone] = size[1];
  }
#endif
  
  const auto &structured_blocks = region.get_structured_blocks();
  for (const auto &sb : structured_blocks) {
    int      zone    = 0;
    cgsize_t size[9] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
    size[3]          = sb->get_property("ni_global").get_int();
    size[4]          = sb->get_property("nj_global").get_int();
    size[5]          = sb->get_property("nk_global").get_int();

    size[0] = size[3] + 1;
    size[1] = size[4] + 1;
    size[2] = size[5] + 1;

    ierr = cg_zone_write(cgnsFilePtr, base, sb->name().c_str(), size, CG_Structured, &zone);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
    update_property(sb, "zone", zone);
    update_property(sb, "base", base);

    assert(zone > 0);
    zone_offset[zone] = zone_offset[zone-1] + sb->get_property("cell_count").get_int();

    // Add GridCoordinates Node...
    int grid_idx = 0;
    ierr         = cg_grid_write(cgnsFilePtr, base, zone, "GridCoordinates", &grid_idx);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }

    // Transfer boundary condition nodes...
    for (const auto &bc : sb->m_boundaryConditions) {
      cgsize_t bc_idx = 0;
      ierr = cg_boco_write(cgnsFilePtr, base, zone, bc.m_bcName.c_str(), CG_FamilySpecified,
                           CG_PointRange, 2, &bc.m_ownerRange[0], &bc_idx);
      if (ierr != CG_OK) {
        cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
      }

      ierr = cg_goto(cgnsFilePtr, base, sb->name().c_str(), 0, "ZoneBC_t", 1, bc.m_bcName.c_str(),
                     0, "end");
      if (ierr != CG_OK) {
        cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
      }
      ierr = cg_famname_write(bc.m_bcName.c_str());
      if (ierr != CG_OK) {
        cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
      }

      ierr = cg_boco_gridlocation_write(cgnsFilePtr, base, zone, bc_idx, CG_Vertex);
      if (ierr != CG_OK) {
        cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
      }
    }

    // Transfer Zone Grid Connectivity...
    for (const auto &zgc : sb->m_zoneConnectivity) {
      if (zgc.m_intraBlock) {
        continue;
      }
      cgsize_t zgc_idx = 0;
      ierr             = cg_1to1_write(cgnsFilePtr, base, zone, zgc.m_connectionName.c_str(),
                           zgc.m_donorName.c_str(), &zgc.m_ownerRange[0], &zgc.m_donorRange[0],
                           &zgc.m_transform[0], &zgc_idx);
      if (ierr != CG_OK) {
        cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
      }
    }
  }
}

std::string Iocgns::Utils::map_cgns_to_topology_type(CG_ElementType_t type)
{
  std::string topology = "unknown";
  switch (type) {
  case CG_NODE: topology     = Ioss::Node::name; break;
  case CG_BAR_2: topology    = Ioss::Bar2::name; break;
  case CG_BAR_3: topology    = Ioss::Bar3::name; break;
  case CG_TRI_3: topology    = Ioss::Tri3::name; break;
  case CG_TRI_6: topology    = Ioss::Tri6::name; break;
  case CG_QUAD_4: topology   = Ioss::Quad4::name; break;
  case CG_QUAD_8: topology   = Ioss::Quad8::name; break;
  case CG_QUAD_9: topology   = Ioss::Quad9::name; break;
  case CG_TETRA_4: topology  = Ioss::Tet4::name; break;
  case CG_TETRA_10: topology = Ioss::Tet10::name; break;
  case CG_PYRA_5: topology   = Ioss::Pyramid5::name; break;
  case CG_PYRA_13: topology  = Ioss::Pyramid13::name; break;
  case CG_PYRA_14: topology  = Ioss::Pyramid14::name; break;
  case CG_PENTA_6: topology  = Ioss::Wedge6::name; break;
  case CG_PENTA_15: topology = Ioss::Wedge15::name; break;
  case CG_PENTA_18: topology = Ioss::Wedge18::name; break;
  case CG_HEXA_8: topology   = Ioss::Hex8::name; break;
  case CG_HEXA_20: topology  = Ioss::Hex20::name; break;
  case CG_HEXA_27: topology  = Ioss::Hex27::name; break;
  default:
    std::cerr << "WARNING: Found topology of type " << cg_ElementTypeName(type)
              << " which is not currently supported.\n";
    topology = Ioss::Unknown::name;
  }
  return topology;
}

CG_ElementType_t Iocgns::Utils::map_topology_to_cgns(const std::string &name)
{
  CG_ElementType_t topo = CG_ElementTypeNull;
  if (name == Ioss::Node::name) {
    topo = CG_NODE;
  }
  else if (name == Ioss::Bar2::name) {
    topo = CG_BAR_2;
  }
  else if (name == Ioss::Bar3::name) {
    topo = CG_BAR_3;
  }
  else if (name == Ioss::Tri3::name) {
    topo = CG_TRI_3;
  }
  else if (name == Ioss::Tri6::name) {
    topo = CG_TRI_6;
  }
  else if (name == Ioss::Quad4::name) {
    topo = CG_QUAD_4;
  }
  else if (name == Ioss::Quad8::name) {
    topo = CG_QUAD_8;
  }
  else if (name == Ioss::Quad9::name) {
    topo = CG_QUAD_9;
  }
  else if (name == Ioss::Tet4::name) {
    topo = CG_TETRA_4;
  }
  else if (name == Ioss::Tet10::name) {
    topo = CG_TETRA_10;
  }
  else if (name == Ioss::Pyramid5::name) {
    topo = CG_PYRA_5;
  }
  else if (name == Ioss::Pyramid13::name) {
    topo = CG_PYRA_13;
  }
  else if (name == Ioss::Pyramid14::name) {
    topo = CG_PYRA_14;
  }
  else if (name == Ioss::Wedge6::name) {
    topo = CG_PENTA_6;
  }
  else if (name == Ioss::Wedge15::name) {
    topo = CG_PENTA_15;
  }
  else if (name == Ioss::Wedge18::name) {
    topo = CG_PENTA_18;
  }
  else if (name == Ioss::Hex8::name) {
    topo = CG_HEXA_8;
  }
  else if (name == Ioss::Hex20::name) {
    topo = CG_HEXA_20;
  }
  else if (name == Ioss::Hex27::name) {
    topo = CG_HEXA_27;
  }
  else {
    std::cerr << "WARNING: Found topology of type " << name
              << " which is not currently supported.\n";
  }
  return topo;
}

void Iocgns::Utils::add_sidesets(int cgnsFilePtr, Ioss::DatabaseIO *db)
{
  cgsize_t base         = 1;
  cgsize_t num_families = 0;
  int      ierr         = cg_nfamilies(cgnsFilePtr, base, &num_families);
  if (ierr != CG_OK) {
    cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
  }
  for (cgsize_t family = 1; family <= num_families; family++) {
    char        name[33];
    CG_BCType_t bocotype;
    cgsize_t    num_bc  = 0;
    cgsize_t    num_geo = 0;
    ierr                = cg_family_read(cgnsFilePtr, base, family, name, &num_bc, &num_geo);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }
#if defined(IOSS_DEBUG_OUTPUT)
    std::cout << "Family " << family << " named " << name << " has " << num_bc << " BC, and "
              << num_geo << " geometry references\n";
#endif
    if (num_bc > 0) {
      // Create a sideset...
      std::string ss_name(name); // Use name here before cg_fambc_read call overwrites it...

      ierr = cg_fambc_read(cgnsFilePtr, base, family, 1, name, &bocotype);
      if (ierr != CG_OK) {
        cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
      }
      auto *ss = new Ioss::SideSet(db, ss_name);
      ss->property_add(Ioss::Property("id", family));
      ss->property_add(Ioss::Property("bc_type", bocotype));
      db->get_region()->add(ss);
    }
  }
}

size_t Iocgns::Utils::resolve_nodes(Ioss::Region &region, int my_processor)
{
  // Each structured block has its own set of "cell_nodes"
  // At block boundaries, there are duplicate nodes which need to be resolved for the
  // unstructured mesh output.

  // We need to iterate all of the blocks and then each blocks zgc to determine
  // which nodes are shared between blocks. For all shared nodes, the node in the lowest
  // numbered zone is considered the "owner" and all other nodes are shared.

  // Create a vector of size which is the sum of the on-processor cell_nodes size for each block
  auto &blocks = region.get_structured_blocks();

  ssize_t ss_max               = std::numeric_limits<ssize_t>::max();
  size_t  num_total_cell_nodes = 0;
  for (auto &block : blocks) {
    size_t node_count = block->get_property("node_count").get_int();
    num_total_cell_nodes += node_count;
  }
  std::vector<ssize_t> cell_node_map(num_total_cell_nodes, ss_max);

  // Each cell_node location in the cell_node_map is currently initialized to ss_max.
  // Iterate each block and then each blocks intra-block (i.e., not
  // due to proc decomps) zgc instances and update cell_node_map
  // such that for each shared node, it points to the owner nodes
  // location.
  for (auto &block : blocks) {
    for (const auto &zgc : block->m_zoneConnectivity) {
      if (!zgc.m_intraBlock && zgc.m_isActive) { // Not due to processor decomposition.
        // NOTE: In parallel, the owner block should exist, but may not have
        // any cells on this processor.  We can access its global i,j,k, but
        // don't store or access any "bulk" data on it.
        auto owner_block = region.get_structured_block(zgc.m_donorName);
        assert(owner_block != nullptr);

        std::vector<int> i_range = zgc.get_range(1);
        std::vector<int> j_range = zgc.get_range(2);
        std::vector<int> k_range = zgc.get_range(3);
        for (auto &k : k_range) {
          for (auto &j : j_range) {
            for (auto &i : i_range) {
              Ioss::IJK_t index{{i, j, k}};
              Ioss::IJK_t owner = zgc.transform(index);

              // The nodes as 'index' and 'owner' are contiguous and
              // should refer to the same node. 'owner' should be
              // the owner (unless it is already owned by another
              // block)

              ssize_t global_offset = block->get_global_node_offset(index[0], index[1], index[2]);
              ssize_t owner_global_offset =
                  owner_block->get_global_node_offset(owner[0], owner[1], owner[2]);

              if (global_offset > owner_global_offset) {
                assert(zgc.m_donorProcessor != -1);
                if (zgc.m_donorProcessor != my_processor) {
                  size_t block_local_offset =
                      block->get_block_local_node_offset(index[0], index[1], index[2]);
                  block->m_globalIdMap.emplace_back(block_local_offset, owner_global_offset + 1);
                }
                else {
                  size_t  local_offset = block->get_local_node_offset(index[0], index[1], index[2]);
                  ssize_t owner_local_offset =
                      owner_block->get_local_node_offset(owner[0], owner[1], owner[2]);

                  if (cell_node_map[local_offset] == ss_max) {
                    cell_node_map[local_offset] = owner_local_offset;
                  }
#if defined(IOSS_DEBUG_OUTPUT)
                  else {
                    if (cell_node_map[local_offset] != owner_local_offset) {
                      std::cerr << "DUPLICATE?: " << local_offset << " " << owner_local_offset
                                << " " << cell_node_map[local_offset] << " " << global_offset << " "
                                << owner_global_offset << "\n";
                    }
                  }
#endif
                }
              }
            }
          }
        }
      }
    }
  }

  // Now iterate cell_node_map.  If an entry == ss_max, then it is
  // an owned node and needs to have its index into the unstructed
  // mesh node map set; otherwise, the value points to the owner
  // node, so the index at this location should be set to the owner
  // nodes index.
  size_t index = 0;
  for (auto &node : cell_node_map) {
    if (node == ss_max || node < 0) {
      node = index++;
    }
    else {
      node = -node;
    }
  }

  for (auto &node : cell_node_map) {
    if (node < 0) {
      node = cell_node_map[-node];
    }
  }

  for (auto &block : blocks) {
    size_t node_count = block->get_property("node_count").get_int();
    block->m_blockLocalNodeIndex.resize(node_count);

    size_t beg = block->get_node_offset();
    size_t end = beg + node_count;
    for (size_t idx = beg, i = 0; idx < end; idx++) {
      block->m_blockLocalNodeIndex[i++] = cell_node_map[idx];
    }
  }
  return index;
}

void Iocgns::Utils::add_structured_boundary_conditions(int                    cgnsFilePtr,
                                                       Ioss::StructuredBlock *block)
{
  cgsize_t base = block->get_property("base").get_int();
  cgsize_t zone = block->get_property("zone").get_int();
  int      num_bcs;
  int      ierr = cg_nbocos(cgnsFilePtr, base, zone, &num_bcs);
  if (ierr != CG_OK) {
    cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
  }

  cgsize_t range[6];
  for (int ibc = 0; ibc < num_bcs; ibc++) {
    char              boconame[33];
    CG_BCType_t       bocotype;
    CG_PointSetType_t ptset_type;
    cgsize_t          npnts;
    cgsize_t          NormalListSize;
    CG_DataType_t     NormalDataType;
    int               ndataset;

    // All we really want from this is 'boconame'
    ierr = cg_boco_info(cgnsFilePtr, base, zone, ibc + 1, boconame, &bocotype, &ptset_type, &npnts,
                        nullptr, &NormalListSize, &NormalDataType, &ndataset);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }

    ierr = cg_boco_read(cgnsFilePtr, base, zone, ibc + 1, range, nullptr);
    if (ierr != CG_OK) {
      cgns_error(cgnsFilePtr, __FILE__, __func__, __LINE__, -1);
    }

    // There are some BC that are applied on an edge or a vertex;
    // Don't want those (yet?), so filter them out at this time...
    int same_count = (range[0] == range[3] ? 1 : 0) + (range[1] == range[4] ? 1 : 0) +
                     (range[2] == range[5] ? 1 : 0);
    if (same_count != 1) {
      std::cerr << "WARNING: CGNS: Skipping Boundary Condition '" << boconame << "' on block '"
                << block->name() << "'. It is applied to "
                << (same_count == 2 ? "an edge" : "a vertex")
                << ". This code only supports surfaces.\n";
      continue;
    }
    Ioss::SideSet *sset = block->get_database()->get_region()->get_sideset(boconame);
    if (sset == nullptr) {
      // Need to create a new sideset since didn't see this earlier.
      auto *db = block->get_database();
      sset     = new Ioss::SideSet(db, boconame);
      sset->property_add(Ioss::Property("id", ibc + 1)); // Not sure this is unique id...
      db->get_region()->add(sset);
    }

    if (sset != nullptr) {
      Ioss::IJK_t range_beg{{std::min(range[0], range[3]), std::min(range[1], range[4]),
                             std::min(range[2], range[5])}};

      Ioss::IJK_t range_end{{std::max(range[0], range[3]), std::max(range[1], range[4]),
                             std::max(range[2], range[5])}};

      // Determine overlap of surface with block (in parallel, a block may
      // be split among multiple processors and the block face this is applied
      // to may not exist on this decomposed block)
      auto        bc   = Ioss::BoundaryCondition(boconame, range_beg, range_end);
      std::string name = std::string(boconame) + "/" + block->name();

      bc_subset_range(block, bc);
      block->m_boundaryConditions.push_back(bc);
      auto sb = new Ioss::SideBlock(block->get_database(), name, "Quad4", "Hex8",
                                    block->m_boundaryConditions.back().get_face_count());
      sb->set_parent_block(block);
      sset->add(sb);
      sb->property_add(Ioss::Property("base", base));
      sb->property_add(Ioss::Property("zone", zone));
      sb->property_add(Ioss::Property("section", ibc + 1));
      sb->property_add(Ioss::Property("id", (int)block->m_boundaryConditions.size() - 1));

      // Set a property on the sideset specifying the boundary condition type (bocotype)
      // In CGNS, the bocotype is an enum; we store it as the integer value of the enum.
      if (sset->property_exists("bc_type")) {
        // Check that the 'bocotype' value matches the value of the property.
        auto old_bocotype = sset->get_property("bc_type").get_int();
        if (old_bocotype != bocotype && bocotype != CG_FamilySpecified) {
          IOSS_WARNING << "On sideset " << sset->name()
                       << ", the boundary condition type was previously set to " << old_bocotype
                       << " which does not match the current value of " << bocotype
                       << ". It will keep the old value.\n";
        }
      }
      else {
        sset->property_add(Ioss::Property("bc_type", (int)bocotype));
      }
    }
    else {
      std::ostringstream errmsg;
      errmsg << "ERROR: CGNS: StructuredBlock '" << block->name()
             << "' Did not find matching sideset with name '" << boconame << "'";
      IOSS_ERROR(errmsg);
    }
  }
}
