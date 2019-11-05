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

int ex_get_assembly(int exoid, ex_assembly *assembly)
{
  struct ex__file_item *file            = NULL;
  int                   status          = 0;
  int                   assembly_id_ndx = 0;
  int                   entlst_id       = 0;
  int                   dimid           = 0;
  size_t                len             = 0;
  char                  errmsg[MAX_ERR_LENGTH];

  EX_FUNC_ENTER();

  file = ex__find_file_item(exoid);
  if (!file) {
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: unknown file id %d.", exoid);
    ex_err_fn(exoid, __func__, errmsg, EX_BADFILEID);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  /* First, locate index of assembly id in VAR_ID_EL_BLK array */
  assembly_id_ndx = ex__id_lkup(exoid, EX_ASSEMBLY, assembly->id);
  if (assembly_id_ndx <= 0) {
    ex_get_err(NULL, NULL, &status);
    if (status != 0) {
      if (assembly->name != NULL) {
        ex_copy_string(assembly->name, "NULL", MAX_STR_LENGTH + 1); /* NULL element type name */
      }
      assembly->entity_count = 0;
      assembly->type         = EX_INVALID;
      if (status == EX_NULLENTITY) { /* NULL element assembly?    */
        EX_FUNC_LEAVE(EX_NOERR);
      }
      snprintf(errmsg, MAX_ERR_LENGTH,
               "ERROR: failed to locate assembly id  %" PRId64 " in id array in file id %d",
               assembly->id, exoid);
      ex_err_fn(exoid, __func__, errmsg, status);
      EX_FUNC_LEAVE(EX_FATAL);
    }
  }

  /* read the name */
  if (assembly->name != NULL) {
    int varid = 0;
    nc_inq_varid(exoid, VAR_NAME_ASSEMBLY, &varid);
    int db_name_size  = ex_inquire_int(exoid, EX_INQ_DB_MAX_ALLOWED_NAME_LENGTH);
    int api_name_size = ex_inquire_int(exoid, EX_INQ_MAX_READ_NAME_LENGTH);
    int name_size     = db_name_size < api_name_size ? db_name_size : api_name_size;

    status = ex__get_name(exoid, varid, assembly_id_ndx - 1, assembly->name, name_size, EX_ASSEMBLY,
                          __func__);
    if (status != NC_NOERR) {
      EX_FUNC_LEAVE(EX_FATAL);
    }
  }

  char *numentryptr = DIM_NUM_ENTRY_ASSEMBLY(assembly_id_ndx);
  if ((status = nc_inq_dimid(exoid, numentryptr, &dimid)) != NC_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to locate number of entities in assembly %" PRId64 " in file id %d",
             assembly->id, exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  if ((status = nc_inq_dimlen(exoid, dimid, &len)) != NC_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to get number of entities in assembly %" PRId64 " in file id %d",
             assembly->id, exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  assembly->entity_count = len;

  /* look up entity list array for this assembly id */
  if ((status = nc_inq_varid(exoid, VAR_ENTRY_ASSEMBLY(assembly_id_ndx), &entlst_id)) != NC_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: failed to locate entity list array for assembly %" PRId64 " in file id %d",
             assembly->id, exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  /* Get the type of entities stored in the entity list... */
  if ((status = nc_get_att_int(exoid, entlst_id, ASSEMBLY_TYPE, &assembly->type)) != NC_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get assembly %" PRId64 " type in file id %d",
             assembly->id, exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    EX_FUNC_LEAVE(EX_FATAL);
  }
  EX_FUNC_LEAVE(EX_NOERR);
}
