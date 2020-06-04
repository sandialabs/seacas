/*
 * Copyright(C) 1999-2017 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of NTESS nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef __PARAVIEW_CATALYST_CGNS_ADAPTER_H
#define __PARAVIEW_CATALYST_CGNS_ADAPTER_H

#include "CatalystParserInterface.h"
#include <stdint.h>
#include <string>
#include <vector>

class ParaViewCatalystCGNSAdapterBase
{
public:
  ParaViewCatalystCGNSAdapterBase(){};
  virtual ~ParaViewCatalystCGNSAdapterBase(){};
  virtual void CreateNewPipeline(const char *catalyst_python_filename,
                                 const char *catalyst_sierra_block_json) = 0;
  virtual void CleanupCatalyst() = 0;
  virtual void PerformCoProcessing() = 0;
  virtual void SetTimeData(double currentTime, int timeStep) = 0;
  virtual void CreateBase(int base_id,
                          const std::string& base_name) = 0;
  virtual void AddStructuredZoneData(int base_id,
                                     int zone_id,
                                     const std::string& zone_name,
                                     const std::string& data_name,
                                     int ni,
                                     int nj,
                                     int nk,
                                     int comp_count,
                                     bool is_cell_field,
                                     char field_suffix_separator,
                                     double* data,
                                     int size) = 0;
  virtual int parseFile(const std::string &filepath,
                        CatalystParserInterface::parse_info &pinfo) = 0;
  virtual int parseString(const std::string &s, CatalystParserInterface::parse_info &pinfo) = 0;
};

typedef ParaViewCatalystCGNSAdapterBase *(*ParaViewCatalystCGNSAdapterBaseSignature)();

extern "C" {
ParaViewCatalystCGNSAdapterBase *ParaViewCatalystCGNSAdapterCreateInstance();
}

class ParaViewCatalystCGNSAdapterImplementation;

class ParaViewCatalystCGNSAdapter : public ParaViewCatalystCGNSAdapterBase
{
public:
  ParaViewCatalystCGNSAdapter() {}
  virtual ~ParaViewCatalystCGNSAdapter() {}
  virtual void CreateNewPipeline(const char *catalyst_python_filename,
                                 const char *catalyst_sierra_block_json);
  virtual void CleanupCatalyst();
  virtual void PerformCoProcessing();
  virtual void SetTimeData(double currentTime, int timeStep);
  virtual void CreateBase(int base_id,
                          const std::string& base_name);
  virtual void AddStructuredZoneData(int base_id,
                                     int zone_id,
                                     const std::string& zone_name,
                                     const std::string& data_name,
                                     int ni,
                                     int nj,
                                     int nk,
                                     int comp_count,
                                     bool is_cell_field,
                                     char field_suffix_separator,
                                     double* data,
                                     int size);
  virtual int parseFile(const std::string &filepath,
                        CatalystParserInterface::parse_info &pinfo);
  virtual int parseString(const std::string &s, CatalystParserInterface::parse_info &pinfo);
};

#endif /* __PARAVIEW_CATALYST_CGNS_ADAPTER_H */
