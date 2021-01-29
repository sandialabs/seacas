// Copyright(C) 1999-2020 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <Ioss_CoordinateFrame.h>
#include <fmt/ostream.h>

namespace Ioss {
  CoordinateFrame::CoordinateFrame(int64_t my_id, char my_tag, const double *point_list)
      : id_(my_id), tag_(my_tag)
  {
    pointList_.reserve(9);
    for (int i = 0; i < 9; i++) {
      pointList_.push_back(point_list[i]);
    }
  }

  int64_t CoordinateFrame::id() const { return id_; }
  char    CoordinateFrame::tag() const { return tag_; }

  const double *CoordinateFrame::coordinates() const { return &pointList_[0]; }
  const double *CoordinateFrame::origin() const { return &pointList_[0]; }
  const double *CoordinateFrame::axis_3_point() const { return &pointList_[3]; }
  const double *CoordinateFrame::plane_1_3_point() const { return &pointList_[6]; }

  bool Ioss::CoordinateFrame::operator==(const Ioss::CoordinateFrame &rhs) const
  {
    if( this->id_ != rhs.id_ ) { 
      return false;
    }   

    if( this->pointList_ != rhs.pointList_ ) { 
      return false;
    }   

    if( this->id_ != rhs.id_ ) { 
      return false;
    }   

    return true;
  }

  bool Ioss::CoordinateFrame::operator!=(const Ioss::CoordinateFrame &rhs) const
  {
    return !(*this == rhs);
  }

  bool Ioss::CoordinateFrame::equal(const Ioss::CoordinateFrame &rhs) const
  {
    if( this->id_ != rhs.id_ ) { 
      fmt::print(stderr, "CoordinateFrame : ID mismatch (%ld vs. %ld)\n", this->id_, rhs.id_);
      return false;
    }   

    if( this->pointList_ != rhs.pointList_ ) { 
      fmt::print(stderr, "CoordinateFrame : Point list mismatch ([ ");
      for( auto &point : this->pointList_ ) { 
        fmt::print(stderr, "%f ", point);
      }   
      fmt::print(stderr, "] vs [");
      for( auto &point : rhs.pointList_ ) { 
        fmt::print(stderr, "%f ", point);
      }   
      fmt::print(stderr, "])\n");
      return false;
    }   

    if( this->id_ != rhs.id_ ) { 
      fmt::print(stderr, "CoordinateFrame : TAG mismatch (%d vs. %d)\n", this->tag_, rhs.tag_);
      return false;
    }   

    return true;
  }

} // namespace Ioss
