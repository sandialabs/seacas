/*
 * Copyright (c) 2005-2017 National Technology & Engineering Solutions
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

#include "exodusII.h"     // for ex_assembly, ex_err, etc
#include "exodusII_int.h" // for EX_FATAL, etc

/*!
 * writes the assembly parameters and optionally assembly data for one assembly
 * \param   exoid                   exodus file id
 * \param  *assembly                ex_assembly structure
 */

int ex_put_assembly(int exoid, const struct ex_assembly assembly)
{
  int    needs_define = 0;
  int    assembly_stat;
  int    dimid, varid = 0, varid_id, varid_st, status, dims[1];
  int    assembly_id_ndx = 0;
  size_t start[1];
  int    cur_num_assemblys;
  char   errmsg[MAX_ERR_LENGTH];
  int    assembly_to_define = 0;

  int int_type;

  EX_FUNC_ENTER();

  ex__check_valid_file_id(exoid, __func__);

  /* Note that this routine can be called:
     1) just define the assembly
     2) just output the assembly data (after a previous call to define)
     3) define and output the assembly data in one call.
  */

  /* first check if any assembly are specified */
  if ((status = nc_inq_dimid(exoid, ex__dim_num_objects(EX_ASSEMBLY), &dimid)) != NC_NOERR) {
    if (status == NC_EBADDIM) {
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: no assemblies defined for file id %d", exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
    }
    else {
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to locate assemblies defined in file id %d",
               exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
    }
    EX_FUNC_LEAVE(EX_FATAL);
  }

  if (assembly.id < 0) {
    /* We are adding an assembly with id = -assembly.id. We want to
     * define everything, but we don't want to increment the number
     * of assembly...  Major kluge / proof of concept
     */
    needs_define       = 1;
    assembly_to_define = -1;
  }
  else {
    status = ex__id_lkup(exoid, EX_ASSEMBLY, assembly.id);
    if (status != -EX_LOOKUPFAIL) { /* found the assembly id, so assembly is already defined... */
      assembly_to_define = 0;
    }
    else {
      needs_define       = 1;
      assembly_to_define = 1;
    }
  }

  if (needs_define == 1) {
    /* put netcdf file into define mode  */
    if ((status = nc_redef(exoid)) != NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to put file id %d into define mode", exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }

    if (assembly_to_define > 0) {
      /*   NOTE: ex__inc_file_item finds the current number of assembly defined
           for a specific file and returns that value incremented. */
      cur_num_assemblys  = ex__inc_file_item(exoid, ex__get_counter_list(EX_ASSEMBLY));
      assembly_id_ndx    = cur_num_assemblys + 1;
      assembly_to_define = assembly_id_ndx;
    }
    else {
      cur_num_assemblys  = ex__get_file_item(exoid, ex__get_counter_list(EX_ASSEMBLY));
      assembly_id_ndx    = cur_num_assemblys;
      assembly_to_define = assembly_id_ndx;
    }

    if (assembly.entity_count > 0) {
      char *numentryptr = DIM_NUM_ENTRY_ASSEMBLY(assembly_id_ndx);

      /* define dimensions and variables */
      if ((status = nc_def_dim(exoid, numentryptr, assembly.entity_count, &dimid)) != NC_NOERR) {
        if (status == NC_ENAMEINUSE) {
          snprintf(errmsg, MAX_ERR_LENGTH,
                   "ERROR: assembly %" PRId64 " -- size already defined in file id %d",
                   assembly.id, exoid);
          ex_err_fn(exoid, __func__, errmsg, status);
        }
        else {
          snprintf(errmsg, MAX_ERR_LENGTH,
                   "ERROR: failed to define number of entries in assembly %" PRId64
                   " in file id %d",
                   assembly.id, exoid);
          ex_err_fn(exoid, __func__, errmsg, status);
        }
        goto error_ret;
      }

      int_type = NC_INT;
      if (ex_int64_status(exoid) & EX_BULK_INT64_DB) {
        int_type = NC_INT64;
      }

      /* create variable array in which to store the entry lists */
      dims[0] = dimid;
      if ((status = nc_def_var(exoid, VAR_ENTRY_ASSEMBLY(assembly_id_ndx), int_type, 1, dims,
                               &varid)) != NC_NOERR) {
        if (status == NC_ENAMEINUSE) {
          snprintf(errmsg, MAX_ERR_LENGTH,
                   "ERROR: entry list already exists for assembly %" PRId64 " in file id %d",
                   assembly.id, exoid);
          ex_err_fn(exoid, __func__, errmsg, status);
        }
        else {
          snprintf(errmsg, MAX_ERR_LENGTH,
                   "ERROR: failed to create entry list for assembly %" PRId64 " in file id %d",
                   assembly.id, exoid);
          ex_err_fn(exoid, __func__, errmsg, status);
        }
        goto error_ret; /* exit define mode and return */
      }
      ex__compress_variable(exoid, varid, 1);
    }

    if ((status = nc_put_att_int(exoid, varid, ASSEMBLY_TYPE, NC_INT, 1, &assembly.type)) !=
        NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to store assembly type %d in file id %d",
               assembly.type, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      goto error_ret; /* exit define mode and return */
    }

    {
      char *contains = ex_name_of_object(assembly.type);
      if ((status = nc_put_att_text(exoid, varid, ASSEMBLY_NAME, strlen(contains) + 1, contains)) !=
          NC_NOERR) {
        snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to store assembly name %s in file id %d",
                 assembly.name, exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
        goto error_ret; /* exit define mode and return */
      }
    }

    /* leave define mode  */
    if ((status = ex__leavedef(exoid, __func__)) != NC_NOERR) {
      EX_FUNC_LEAVE(EX_FATAL);
    }

    /* Output the set ids and status... */
    /* first: get id of set id variable */
    if ((status = nc_inq_varid(exoid, VAR_ID_ASSEMBLY, &varid_id)) != NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to locate assembly %" PRId64 " in file id %d",
               assembly.id, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }

    /* write out set id */
    start[0]     = assembly_to_define - 1;
    long long id = assembly.id;
    if (id < 0) {
      id = -id;
    }
    status = nc_put_var1_longlong(exoid, varid_id, start, &id);

    if (status != NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to store assembly id %" PRId64 " in file id %d", assembly.id, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }

    assembly_stat = (assembly.entity_count == 0) ? 0 : 1;

    if ((status = nc_inq_varid(exoid, VAR_STAT_ASSEMBLY, &varid_st)) != NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to locate assembly status in file id %d",
               exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }

    if ((status = nc_put_var1_int(exoid, varid_st, start, &assembly_stat)) != NC_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to store assembly %" PRId64 " status to file id %d", assembly.id,
               exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }
  }

  /* Assembly are now all defined; see if any set data needs to be output... */
  status = EX_NOERR;
  if (assembly.entity_list != NULL) {
    if (assembly_id_ndx == 0) {
      assembly_id_ndx = ex__id_lkup(exoid, EX_ASSEMBLY, assembly.id);
    }
    if (varid == 0) {
      if ((status = nc_inq_varid(exoid, VAR_ENTRY_ASSEMBLY(assembly_id_ndx), &varid)) != NC_NOERR) {
        snprintf(errmsg, MAX_ERR_LENGTH,
                 "ERROR: failed to locate entity list for assembly %" PRId64 " in file id %d",
                 assembly.id, exoid);
        ex_err_fn(exoid, __func__, errmsg, status);
        EX_FUNC_LEAVE(EX_FATAL);
      }
    }

    if (ex_int64_status(exoid) & EX_BULK_INT64_API) {
      status = nc_put_var_longlong(exoid, varid, assembly.entity_list);
    }
    else {
      status = nc_put_var_int(exoid, varid, assembly.entity_list);
    }
    if (status != EX_NOERR) {
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to output entity list for assembly %" PRId64 " in file id %d",
               assembly.id, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }
  }
  EX_FUNC_LEAVE(status);

/* Fatal error: exit definition mode and return */
error_ret:
  ex__leavedef(exoid, __func__);
  EX_FUNC_LEAVE(EX_FATAL);
}
