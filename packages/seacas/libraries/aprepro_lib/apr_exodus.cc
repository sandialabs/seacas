/*
 * Copyright (c) 2014-2017 National Technology & Engineering Solutions
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
 *
 */

#if defined(EXODUS_SUPPORT)
#include "aprepro.h"
#include "exodusII.h"

#include "apr_util.h"
#include "aprepro_parser.h"

#include <cctype>
#include <cstdlib>
#include <cstring>

#define __STDC_FORMAT_MACROS
#include <cinttypes>
#ifndef PRId64
#error "PRId64 not defined"
#endif

namespace {
  void LowerCaseTrim(char *name);

  bool matches_prefix(const char *pre, const char *str)
  {
    return strncmp(pre, str, strlen(pre)) == 0;
  }
} // namespace

namespace SEAMS {
  extern SEAMS::Aprepro *aprepro;

  int open_exodus_file(char *filename)
  {
    int   cpu = sizeof(double);
    int   io  = 0;
    int   exo;
    float version;

    exo = ex_open(filename, EX_READ | EX_ALL_INT64_API, &cpu, &io, &version);
    if (exo < 0) {
      /* If there is an include path specified, try opening file there */
      std::string file_path(aprepro->ap_options.include_path);
      if (!file_path.empty()) {
        file_path += "/";
        file_path += filename;
        exo = ex_open(file_path.c_str(), EX_READ | EX_ALL_INT64_API, &cpu, &io, &version);
      }
      if (exo < 0) {
        yyerror(*aprepro, "Error opening exodusII file.");
      }
    }

    SEAMS::symrec *ptr = aprepro->getsym("ex_version");
    if (!ptr) {
      ptr = aprepro->putsym("ex_version", Aprepro::VARIABLE, 0);
    }
    ptr->value.var = version;
    return exo;
  }

  const char *do_exodus_info(char *filename, char *prefix)
  {
    char *ret_string = NULL;

    /*
     * Open the specified exodusII file, read the info records
     * then parse them as input to aprepro.
     */
    int exoid = open_exodus_file(filename);
    if (exoid < 0)
      return "";

    int count = ex_inquire_int(exoid, EX_INQ_INFO);

    if (count > 0) {
      auto info = new char *[count];
      for (int i = 0; i < count; i++) {
        info[i] = new char[MAX_LINE_LENGTH + 1];
        memset(info[i], '\0', MAX_LINE_LENGTH + 1);
      }

      ex_get_info(exoid, info);

      std::string lines;
      size_t      prefix_len = strlen(prefix);
      for (int i = 0; i < count; i++) {
        if (matches_prefix(prefix, info[i])) {
          lines += std::string(info[i]).substr(prefix_len);
          lines += "\n";
        }
      }

      new_string(lines.c_str(), &ret_string);

      if (count > 0) {
        for (int i = 0; i < count; i++) {
          delete[] info[i];
        }
        delete[] info;
      }
      ex_close(exoid);
      return ret_string;
    }
    else {
      ex_close(exoid);
      return "";
    }
  }

  const char *do_exodus_info_range(char *filename, char *beg, char *end)
  {
    char *ret_string = NULL;

    /*
     * Open the specified exodusII file, read the info records
     * then parse them as input to aprepro.
     */
    int exoid = open_exodus_file(filename);
    if (exoid < 0)
      return "";

    int count = ex_inquire_int(exoid, EX_INQ_INFO);

    if (count > 0) {
      auto info = new char *[count];
      for (int i = 0; i < count; i++) {
        info[i] = new char[MAX_LINE_LENGTH + 1];
        memset(info[i], '\0', MAX_LINE_LENGTH + 1);
      }

      ex_get_info(exoid, info);

      bool        in_range = false;
      std::string lines;
      for (int i = 0; i < count; i++) {
        if (in_range && strcmp(info[i], end) == 0) {
          in_range = false;
        }
        if (in_range) {
          lines += std::string(info[i]);
          lines += "\n";
        }
        if (!in_range && strcmp(info[i], beg) == 0) {
          in_range = true;
        }
      }

      new_string(lines.c_str(), &ret_string);

      if (count > 0) {
        for (int i = 0; i < count; i++) {
          delete[] info[i];
        }
        delete[] info;
      }
      ex_close(exoid);
      return ret_string;
    }
    else {
      ex_close(exoid);
      return "";
    }
  }

  const char *do_exodus_meta(char *filename)
  {

    /*
     * Open the specified exodusII file, read the metadata and set
     * variables for each item.
     * Examples include "node_count", "element_count", ...
     */
    int exoid = open_exodus_file(filename);
    if (exoid < 0)
      return "";

    /* read database paramters */
    char    title[MAX_LINE_LENGTH + 1];
    int64_t ndim, nnodes, nelems, nblks, nnsets, nssets;
    ex_get_init(exoid, title, &ndim, &nnodes, &nelems, &nblks, &nnsets, &nssets);

    SEAMS::symrec *ptr = aprepro->putsym("ex_title", Aprepro::STRING_VARIABLE, 0);
    ptr->value.svar    = title;

    ptr            = aprepro->putsym("ex_dimension", Aprepro::VARIABLE, 0);
    ptr->value.var = ndim;

    ptr            = aprepro->putsym("ex_node_count", Aprepro::VARIABLE, 0);
    ptr->value.var = nnodes;

    ptr            = aprepro->putsym("ex_element_count", Aprepro::VARIABLE, 0);
    ptr->value.var = nelems;

    ptr            = aprepro->putsym("ex_block_count", Aprepro::VARIABLE, 0);
    ptr->value.var = nblks;

    ptr            = aprepro->putsym("ex_nodeset_count", Aprepro::VARIABLE, 0);
    ptr->value.var = nnsets;

    ptr            = aprepro->putsym("ex_sideset_count", Aprepro::VARIABLE, 0);
    ptr->value.var = nssets;

    { /* Nemesis Information */
      int  proc_count;
      int  proc_in_file;
      char file_type[MAX_STR_LENGTH + 1];

      ex_get_init_info(exoid, &proc_count, &proc_in_file, file_type);

      if (proc_count > 1) {
        int64_t global_nodes;
        int64_t global_elements;
        int64_t global_blocks;
        int64_t global_nsets;
        int64_t global_ssets;

        ptr            = aprepro->putsym("ex_processor_count", Aprepro::VARIABLE, 0);
        ptr->value.var = proc_count;

        ex_get_init_global(exoid, &global_nodes, &global_elements, &global_blocks, &global_nsets,
                           &global_ssets);

        ptr            = aprepro->putsym("ex_node_count_global", Aprepro::VARIABLE, 0);
        ptr->value.var = global_nodes;

        ptr            = aprepro->putsym("ex_element_count_global", Aprepro::VARIABLE, 0);
        ptr->value.var = global_elements;
      }
    }

    /*
     * Read The Element Blocks, Node Sets, and Side Sets and set variables for each of these.
     * The Scheme Is:
     * -- 'ex_block_ids' Is an array of the element block ids. (ex_block_count, 1)
     * -- 'ex_block_info' is an array of the element block info (id, num_elem, num_node_per_element,
     * num_attrib) for each block (ex_block_count,4)
     * -- 'ex_nodeset_ids'
     * -- 'ex_nodeset_info'
     * -- 'ex_sideset_ids'
     * -- 'ex_sideset_info'
     */

    int max_name_length = ex_inquire_int(exoid, EX_INQ_DB_MAX_USED_NAME_LENGTH);
    ex_set_max_name_length(exoid, max_name_length);
    char *name = new char[max_name_length + 1];
    char *tmp  = nullptr;

    if (nblks > 0) {
      auto array_data       = new array(nblks, 1);
      auto array_block_info = new array(nblks, 4);

      std::vector<int64_t> ids(nblks);
      ex_get_ids(exoid, EX_ELEM_BLOCK, ids.data());

      char    type[MAX_STR_LENGTH + 1];
      int64_t nel;
      int64_t nnel;
      int64_t natr;

      std::string names;
      std::string topology;

      int64_t idx = 0;
      for (int64_t i = 0; i < nblks; i++) {
        ex_get_block(exoid, EX_ELEM_BLOCK, ids[i], type, &nel, &nnel, 0, 0, &natr);
        array_data->data[i]           = ids[i];
        array_block_info->data[idx++] = ids[i];
        array_block_info->data[idx++] = nel;
        array_block_info->data[idx++] = nnel;
        array_block_info->data[idx++] = natr;

        ex_get_name(exoid, EX_ELEM_BLOCK, ids[i], name);
        if (name[0] == '\0') {
          sprintf(name, "block_%" PRId64, ids[i]);
        }

        if (i > 0) {
          topology += ",";
          names += ",";
        }
        topology += type;
        names += name;
      }

      ptr = aprepro->putsym("ex_block_topology", Aprepro::STRING_VARIABLE, 0);

      new_string(topology.c_str(), &tmp);
      /* lowercase the string */
      LowerCaseTrim(tmp);
      ptr->value.svar = tmp;

      ptr = aprepro->putsym("ex_block_names", Aprepro::STRING_VARIABLE, 0);
      new_string(names.c_str(), &tmp);
      ptr->value.svar = tmp;

      ptr             = aprepro->putsym("ex_block_ids", Aprepro::ARRAY_VARIABLE, 0);
      ptr->value.avar = array_data;

      ptr             = aprepro->putsym("ex_block_info", Aprepro::ARRAY_VARIABLE, 0);
      ptr->value.avar = array_block_info;
    }

    // Nodesets...
    if (nnsets > 0) {
      auto array_data     = new array(nnsets, 1);
      auto array_set_info = new array(nnsets, 3);

      std::vector<int64_t> ids(nnsets);
      ex_get_ids(exoid, EX_NODE_SET, ids.data());

      std::string names;
      int64_t     idx = 0;
      for (int64_t i = 0; i < nnsets; i++) {
        int64_t num_entry;
        int64_t num_dist;
        ex_get_set_param(exoid, EX_NODE_SET, ids[i], &num_entry, &num_dist);
        array_data->data[i]         = ids[i];
        array_set_info->data[idx++] = ids[i];
        array_set_info->data[idx++] = num_entry;
        array_set_info->data[idx++] = num_dist;

        ex_get_name(exoid, EX_NODE_SET, ids[i], name);
        if (name[0] == '\0') {
          sprintf(name, "nodeset_%" PRId64, ids[i]);
        }

        if (i > 0) {
          names += ",";
        }
        names += name;
      }

      ptr = aprepro->putsym("ex_nodeset_names", Aprepro::STRING_VARIABLE, 0);
      new_string(names.c_str(), &tmp);
      ptr->value.svar = tmp;

      ptr             = aprepro->putsym("ex_nodeset_ids", Aprepro::ARRAY_VARIABLE, 0);
      ptr->value.avar = array_data;

      ptr             = aprepro->putsym("ex_nodeset_info", Aprepro::ARRAY_VARIABLE, 0);
      ptr->value.avar = array_set_info;
    }

    // Sidesets...
    if (nssets > 0) {
      auto array_data     = new array(nssets, 1);
      auto array_set_info = new array(nssets, 3);

      std::vector<int64_t> ids(nssets);
      ex_get_ids(exoid, EX_SIDE_SET, ids.data());

      std::string names;
      int64_t     idx = 0;
      for (int64_t i = 0; i < nssets; i++) {
        int64_t num_entry;
        int64_t num_dist;
        ex_get_set_param(exoid, EX_SIDE_SET, ids[i], &num_entry, &num_dist);
        array_data->data[i]         = ids[i];
        array_set_info->data[idx++] = ids[i];
        array_set_info->data[idx++] = num_entry;
        array_set_info->data[idx++] = num_dist;

        ex_get_name(exoid, EX_SIDE_SET, ids[i], name);
        if (name[0] == '\0') {
          sprintf(name, "sideset_%" PRId64, ids[i]);
        }

        if (i > 0) {
          names += ",";
        }
        names += name;
      }

      ptr = aprepro->putsym("ex_sideset_names", Aprepro::STRING_VARIABLE, 0);
      new_string(names.c_str(), &tmp);
      ptr->value.svar = tmp;

      ptr             = aprepro->putsym("ex_sideset_ids", Aprepro::ARRAY_VARIABLE, 0);
      ptr->value.avar = array_data;

      ptr             = aprepro->putsym("ex_sideset_info", Aprepro::ARRAY_VARIABLE, 0);
      ptr->value.avar = array_set_info;
    }

    {
      /* Get timestep count */
      int64_t ts_count = ex_inquire_int(exoid, EX_INQ_TIME);
      ptr              = aprepro->putsym("ex_timestep_count", Aprepro::VARIABLE, 0);
      ptr->value.var   = ts_count;

      if (ts_count > 0) {
        std::vector<double> timesteps(ts_count);
        ex_get_all_times(exoid, timesteps.data());

        auto ts_array_data = new array(ts_count, 1);
        for (int64_t i = 0; i < ts_count; i++) {
          ts_array_data->data[i] = timesteps[i];
        }
        ptr             = aprepro->putsym("ex_timestep_times", Aprepro::ARRAY_VARIABLE, 0);
        ptr->value.avar = ts_array_data;
      }
    }

    ex_close(exoid);
    return "";
  }
} // namespace SEAMS

namespace {
  void LowerCaseTrim(char *name)
  {
    /*
     * Convert all characters to lowercase. Strip leading whitespace and
     * trim string at first 'whitespace' character (after the leading)
     */

    char *p = name;
    char *o = name;
    while (*p != '\0' && isspace(*p))
      ++p;

    while (*p != '\0') {
      if (isspace(*p)) {
        *o = '\0';
        return;
      }
      else {
        *o = tolower(*p);
      }
      o++;
      p++;
    }
    *o = '\0';
  }
} // namespace
#endif
