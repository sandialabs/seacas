/*
 * Copyright(C) 1999-2021 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */
/*****************************************************************************
 *
 * exgnm - ex_get_id_map
 *
 * entry conditions -
 *   input parameters:
 *       int     exoid                   exodus file id
 *       int     map_type                type of map (node, edge, face, element)
 *
 * exit conditions -
 *       int*    map                     map
 *
 *****************************************************************************/

#include "exodusII.h"     // for ex_err, etc
#include "exodusII_int.h" // for EX_FATAL, EX_NOERR, etc

/*
 * reads the id map
 */

int ex_get_id_map(int exoid, ex_entity_type map_type, void_int *map)
{
  int         status;
  char        errmsg[MAX_ERR_LENGTH];
  const char *dnumentries;
  const char *vmap;
  const char *tname;

  EX_FUNC_ENTER();
  if (exi_check_valid_file_id(exoid, __func__) == EX_FATAL) {
    EX_FUNC_LEAVE(EX_FATAL);
  }

  switch (map_type) {
  case EX_NODE_MAP:
    tname       = "node";
    dnumentries = DIM_NUM_NODES;
    vmap        = VAR_NODE_NUM_MAP;
    break;
  case EX_EDGE_MAP:
    tname       = "edge";
    dnumentries = DIM_NUM_EDGE;
    vmap        = VAR_EDGE_NUM_MAP;
    break;
  case EX_FACE_MAP:
    tname       = "face";
    dnumentries = DIM_NUM_FACE;
    vmap        = VAR_FACE_NUM_MAP;
    break;
  case EX_ELEM_MAP:
    tname       = "element";
    dnumentries = DIM_NUM_ELEM;
    vmap        = VAR_ELEM_NUM_MAP;
    break;
  default:
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: Bad map type (%d) specified for file id %d", map_type,
             exoid);
    ex_err_fn(exoid, __func__, errmsg, EX_BADPARAM);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  /* See if any entries are stored in this file */
  int dimid;
  if (nc_inq_dimid(exoid, dnumentries, &dimid) != EX_NOERR) {
    EX_FUNC_LEAVE(EX_NOERR);
  }

  /* The caller's buffer holds num_entries values (the length of the
   * entity-count dimension); never read more than that. */
  size_t num_entries;
  if ((status = nc_inq_dimlen(exoid, dimid, &num_entries)) != EX_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get number of %ss in file id %d", tname,
             exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  int mapid;
  if (nc_inq_varid(exoid, vmap, &mapid) != EX_NOERR) {
    /* generate default map of 1..n, where n is num_entries */
    if (ex_int64_status(exoid) & EX_MAPS_INT64_API) {
      int64_t *lmap = (int64_t *)map;
      for (size_t i = 0; i < num_entries; i++) {
        lmap[i] = i + 1;
      }
    }
    else {
      int *lmap = (int *)map;
      for (size_t i = 0; i < num_entries; i++) {
        lmap[i] = i + 1;
      }
    }

    EX_FUNC_LEAVE(EX_NOERR);
  }

  /* The map variable's own shape comes from the file and may not match
   * the entity count; read exactly num_entries values from a 1-D variable
   * (netCDF reports an error if the variable is shorter). */
  int ndims;
  if ((status = nc_inq_varndims(exoid, mapid, &ndims)) != EX_NOERR || ndims != 1) {
    snprintf(errmsg, MAX_ERR_LENGTH,
             "ERROR: %s id map in file id %d is not a one-dimensional variable", tname, exoid);
    ex_err_fn(exoid, __func__, errmsg, status != EX_NOERR ? status : EX_BADPARAM);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  /* read in the id map  */
  size_t start[] = {0};
  size_t count[] = {num_entries};
  if (ex_int64_status(exoid) & EX_MAPS_INT64_API) {
    status = nc_get_vara_longlong(exoid, mapid, start, count, map);
  }
  else {
    status = nc_get_vara_int(exoid, mapid, start, count, map);
  }

  if (status != EX_NOERR) {
    snprintf(errmsg, MAX_ERR_LENGTH, "ERROR: failed to get %s id map in file id %d", tname, exoid);
    ex_err_fn(exoid, __func__, errmsg, status);
    EX_FUNC_LEAVE(EX_FATAL);
  }

  EX_FUNC_LEAVE(EX_NOERR);
}
