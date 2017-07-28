#include <Ioss_Tracer.h>
#include <cassert>
#include <iostream>

namespace Ioss {
  int Tracer::level;

  Tracer::Tracer(const char *function)
  {
    std::cerr << "Entering Function: " << function << " at level " << ++level << "\n";
    assert(level == 1);
  }

  Tracer::~Tracer() { --level; }
} // namespace Ioss
