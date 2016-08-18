#include <cgns/Iocgns_Utils.h>
#include <Ioss_StructuredBlock.h>

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
}

void Iocgns::Utils::cgns_error(int cgnsid, const char *file, const char *function, int lineno,
			       int processor)
{
  std::ostringstream errmsg;
  errmsg << "CGNS error '" << cg_get_error() << "' at line " << lineno << " in file '" << file
	 << "' in function '" << function << "' on processor " << processor
	 << ". Please report to gdsjaar@sandia.gov if you need help.";
  if (cgnsid > 0) {
    cg_close(cgnsid);
  }
  IOSS_ERROR(errmsg);
}

std::string Iocgns::Utils::map_cgns_to_topology_type(CG_ElementType_t type)
{
  std::string topology = "unknown";
  switch (type) {
  case CG_NODE: topology     = "tetra4"; break;
  case CG_BAR_2: topology    = "bar2"; break;
  case CG_BAR_3: topology    = "bar3"; break;
  case CG_TRI_3: topology    = "tri3"; break;
  case CG_TRI_6: topology    = "tri6"; break;
  case CG_QUAD_4: topology   = "quad4"; break;
  case CG_QUAD_8: topology   = "quad8"; break;
  case CG_QUAD_9: topology   = "quad9"; break;
  case CG_TETRA_4: topology  = "tetra4"; break;
  case CG_TETRA_10: topology = "tetra10"; break;
  case CG_PYRA_5: topology   = "pyramid5"; break;
  case CG_PYRA_13: topology  = "pyramid13"; break;
  case CG_PYRA_14: topology  = "pyramid14"; break;
  case CG_PENTA_6: topology  = "wedge6"; break;
  case CG_PENTA_15: topology = "wedge15"; break;
  case CG_PENTA_18: topology = "wedge18"; break;
  case CG_HEXA_8: topology   = "hex8"; break;
  case CG_HEXA_20: topology  = "hex20"; break;
  case CG_HEXA_27: topology  = "hex27"; break;
  default:
    std::cerr << "WARNING: Found topology of type " << cg_ElementTypeName(type)
	      << " which is not currently supported.\n";
    topology = "unknown";
  }
  return topology;
}

  void Iocgns::Utils::add_sidesets(int cgnsFilePtr, Ioss::DatabaseIO *db)
  {
    cgsize_t base         = 1;
    cgsize_t num_families = 0;
    cg_nfamilies(cgnsFilePtr, base, &num_families);
    for (cgsize_t family = 1; family <= num_families; family++) {
      char     name[33];
      cgsize_t num_bc  = 0;
      cgsize_t num_geo = 0;
      cg_family_read(cgnsFilePtr, base, family, name, &num_bc, &num_geo);
#if defined(DEBUG_OUTPUT)
      std::cout << "Family " << family << " named " << name << " has " << num_bc << " BC, and "
		<< num_geo << " geometry references\n";
#endif
      if (num_bc > 0) {
	// Create a sideset...
	std::string    ss_name(name);
	Ioss::SideSet *ss = new Ioss::SideSet(db, ss_name);
	ss->property_add(Ioss::Property("id", family));
	db->get_region()->add(ss);
      }
    }
  }

void Iocgns::Utils::add_structured_boundary_conditions(int cgnsFilePtr,
						       Ioss::StructuredBlock *block)
{
  cgsize_t base = block->get_property("base").get_int();
  cgsize_t zone = block->get_property("zone").get_int();
  int num_bcs;
  cg_nbocos(cgnsFilePtr, base, zone, &num_bcs);

  cgsize_t range[6];
  for (int ibc = 0; ibc < num_bcs; ibc++) {
    char boconame[32];
    CG_BCType_t bocotype;
    CG_PointSetType_t ptset_type;
    cgsize_t npnts;
    cgsize_t NormalListSize;
    CG_DataType_t NormalDataType;
    int ndataset;

    // All we really want from this is 'boconame'
    cg_boco_info(cgnsFilePtr, base, zone, ibc+1,
		 boconame, &bocotype, &ptset_type,
		 &npnts, NULL, &NormalListSize,
		 &NormalDataType, &ndataset);
      
    cg_boco_read(cgnsFilePtr, base, zone, ibc+1, range, NULL);
    Ioss::SideSet *sset = block->get_database()->get_region()->get_sideset(boconame);
    if (sset) {
      std::array<cgsize_t, 3> range_beg{{range[0], range[1], range[2]}};
      std::array<cgsize_t, 3> range_end{{range[3], range[4], range[5]}};

      // Determine overlap of surface with block (in parallel, a block may
      // be split among multiple processors and the block face this is applied
      // to may not exist on this decomposed block)
      auto bc = Ioss::BoundaryCondition(boconame, range_beg, range_end);
      if (bc_overlaps(block, bc)) {
	bc_subset_range(block, bc);
	block->m_boundaryConditions.push_back(bc);
	auto sb = new Ioss::SideBlock(block->get_database(), boconame, "Quad4", "Hex8",
				      block->m_boundaryConditions.back().get_face_count());
	sb->set_parent_block(block);
	sset->add(sb);
      }
    }
    else {
      std::ostringstream errmsg;
      errmsg << "ERROR: CGNS: StructuredBlock '" << block->name() 
	     << "' Did not find matching sideset with name '"
	     << boconame << "'";
      IOSS_ERROR(errmsg);
    }
  }
}
