/*
 * Copyright(C) 1999-2020, 2022 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */
#pragma once

#include "Ioss_GetLongOpt.h" // for GetLongOption
#include <iosfwd>            // for ostream
#include <string>            // for string

/** \brief A special namespace for the line_decomp demonstration program interFace.
 */
namespace Line_Decomp {
  class Interface
  {
  public:
    Interface();
    ~Interface();

    bool parse_options(int argc, char **argv);

    bool ints_64_bit() const { return ints64Bit_; }

    int         debug() const { return debugLevel_; }
    std::string input_filename() const { return inputFile_; }
    std::string output_filename() const { return outputFile_; }
    std::string input_type() const { return inFiletype_; }
    std::string output_type() const { return outFiletype_; }

  private:
    void enroll_options();

    Ioss::GetLongOption options_;

    std::string inputFile_;
    std::string outputFile_;
    std::string inFiletype_{"unknown"};
    std::string outFiletype_{"unknown"};

  public:
    std::string surfaceList;
    std::string decomp_method;
    std::string compose_output{"default"};
    int         compression_level{0};
    int         debugLevel_{};
    bool        shuffle{false};
    bool        statistics{false};
    bool        ints64Bit_{false};
    bool        netcdf4_{false};
  };
} // namespace Line_Decomp
