/*
 * Copyright(C) 2010-2017 National Technology & Engineering Solutions
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
#ifndef __VTK_CGNS_MULTI_BLOCK_DATA_SET_H
#define __VTK_CGNS_MULTI_BLOCK_DATA_SET_H

#include "vtkMultiBlockDataSet.h"
#include <map>

class vtkCGNSMultiBlockDataSet : public vtkMultiBlockDataSet
{
public:
  vtkTypeMacro(vtkCGNSMultiBlockDataSet, vtkMultiBlockDataSet);
  void PrintSelf(ostream &os, vtkIndent indent);

  static vtkCGNSMultiBlockDataSet *New();

  void CreateBase(int base_id,
                  const std::string& base_name);

  void AddStructuredZoneData(int base_id,
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

protected:
  vtkCGNSMultiBlockDataSet();
  ~vtkCGNSMultiBlockDataSet();

private:
  vtkCGNSMultiBlockDataSet(const vtkCGNSMultiBlockDataSet &); // Not implemented.
  void operator=(const vtkCGNSMultiBlockDataSet &);           // Not implemented.

  struct base {
    int base_location;
    std::map<int, int> zone_id_to_zone_location_map;
  };
 
  std::map<int, base> base_id_to_base_map;
};

#endif /* __VTK_CGNS_MULTI_BLOCK_DATA_SET_H */
