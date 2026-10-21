/*
 * Copyright(C) 2026 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

/*
 * Basis and quadrature cardinality attributes must be single NC_INT values
 * before ex_get_basis/ex_get_quadrature read them into stack integers.
 */

#include "exodusII.h"
#include <netcdf.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define EXCHECK(funcall)                                                                           \
  do {                                                                                             \
    if ((funcall) != EX_NOERR) {                                                                   \
      fprintf(stderr, "%s failed\n", #funcall);                                                    \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

#define NCCHECK(funcall)                                                                           \
  do {                                                                                             \
    int nc_status_ = (funcall);                                                                    \
    if (nc_status_ != NC_NOERR) {                                                                  \
      fprintf(stderr, "%s failed: %s\n", #funcall, nc_strerror(nc_status_));                       \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

static const char *filename = "field-cardinality-att.exo";

static int create_file(void)
{
  int cpu_word_size = 8;
  int io_word_size  = 8;
  int exoid         = ex_create(filename, EX_CLOBBER, &cpu_word_size, &io_word_size);
  if (exoid < 0) {
    return 1;
  }

  EXCHECK(ex_put_init(exoid, "field cardinality", 2, 0, 0, 0, 0, 0));

  int    basis_dim[2]      = {1, 2};
  int    basis_ord[2]      = {1, 2};
  int    basis_dof_ord[2]  = {1, 2};
  int    basis_num_dof[2]  = {1, 1};
  double basis_xi[2]       = {0.0, 1.0};
  double basis_eta[2]      = {0.0, 0.0};
  double basis_zeta[2]     = {0.0, 0.0};
  ex_basis basis           = {0};
  strcpy(basis.name, "good_basis");
  basis.cardinality      = 2;
  basis.subc_dim         = basis_dim;
  basis.subc_ordinal     = basis_ord;
  basis.subc_dof_ordinal = basis_dof_ord;
  basis.subc_num_dof     = basis_num_dof;
  basis.xi               = basis_xi;
  basis.eta              = basis_eta;
  basis.zeta             = basis_zeta;
  EXCHECK(ex_put_basis(exoid, basis));

  double quad_xi[2]     = {0.25, 0.75};
  double quad_eta[2]    = {0.0, 0.0};
  double quad_zeta[2]   = {0.0, 0.0};
  double quad_weight[2] = {1.0, 1.0};
  ex_quadrature quad    = {0};
  strcpy(quad.name, "good_quad");
  quad.cardinality = 2;
  quad.xi          = quad_xi;
  quad.eta         = quad_eta;
  quad.zeta        = quad_zeta;
  quad.weight      = quad_weight;
  EXCHECK(ex_put_quadrature(exoid, quad));

  EXCHECK(ex_close(exoid));
  return 0;
}

static void free_basis(ex_basis *basis)
{
  if (basis != NULL) {
    free(basis->subc_dim);
    free(basis->subc_ordinal);
    free(basis->subc_dof_ordinal);
    free(basis->subc_num_dof);
    free(basis->xi);
    free(basis->eta);
    free(basis->zeta);
  }
  free(basis);
}

static void free_quadrature(ex_quadrature *quad)
{
  if (quad != NULL) {
    free(quad->xi);
    free(quad->eta);
    free(quad->zeta);
    free(quad->weight);
  }
  free(quad);
}

static int expect_valid_metadata(void)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }

  ex_basis *basis     = NULL;
  int       num_basis = 0;
  int       status    = ex_get_basis(exoid, &basis, &num_basis);
  if (status != EX_NOERR || num_basis != 1 || basis[0].cardinality != 2) {
    fprintf(stderr, "valid basis metadata failed: status %d count %d\n", status, num_basis);
    ex_close(exoid);
    free_basis(basis);
    return 1;
  }
  free_basis(basis);

  ex_quadrature *quad = NULL;
  int            num_quad;
  status = ex_get_quadrature(exoid, &quad, &num_quad);
  if (status != EX_NOERR || num_quad != 1 || quad[0].cardinality != 2) {
    fprintf(stderr, "valid quadrature metadata failed: status %d count %d\n", status, num_quad);
    ex_close(exoid);
    free_quadrature(quad);
    return 1;
  }
  free_quadrature(quad);

  ex_close(exoid);
  return 0;
}

static int overwrite_cardinality(const char *attname, nc_type type, size_t len)
{
  int ncid;
  NCCHECK(nc_open(filename, NC_WRITE, &ncid));
  NCCHECK(nc_redef(ncid));
  if (type == NC_INT) {
    int values[3] = {2, 3, 4};
    NCCHECK(nc_put_att_int(ncid, NC_GLOBAL, attname, NC_INT, len, values));
  }
  else {
    double values[1] = {2.0};
    NCCHECK(nc_put_att_double(ncid, NC_GLOBAL, attname, NC_DOUBLE, len, values));
  }
  NCCHECK(nc_enddef(ncid));
  NCCHECK(nc_close(ncid));
  return 0;
}

static int expect_basis_status(int expected)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }
  ex_basis *basis     = NULL;
  int       num_basis = 0;
  int       status    = ex_get_basis(exoid, &basis, &num_basis);
  if (status != expected) {
    fprintf(stderr, "basis status %d, expected %d\n", status, expected);
    ex_close(exoid);
    free_basis(basis);
    return 1;
  }
  ex_close(exoid);
  free_basis(basis);
  return 0;
}

static int expect_quadrature_status(int expected)
{
  int   cpu_word_size = 8;
  int   io_word_size  = 0;
  float version;
  int   exoid = ex_open(filename, EX_READ, &cpu_word_size, &io_word_size, &version);
  if (exoid < 0) {
    return 1;
  }
  ex_quadrature *quad = NULL;
  int            num_quad;
  int            status = ex_get_quadrature(exoid, &quad, &num_quad);
  if (status != expected) {
    fprintf(stderr, "quadrature status %d, expected %d\n", status, expected);
    ex_close(exoid);
    free_quadrature(quad);
    return 1;
  }
  ex_close(exoid);
  free_quadrature(quad);
  return 0;
}

int main(void)
{
  ex_opts(EX_VERBOSE);

  if (create_file() != 0 || expect_valid_metadata() != 0) {
    return 1;
  }

  if (create_file() != 0 ||
      overwrite_cardinality("Basis@good_basis@cardinality", NC_INT, 3) != 0 ||
      expect_basis_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 ||
      overwrite_cardinality("Basis@good_basis@cardinality", NC_DOUBLE, 1) != 0 ||
      expect_basis_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 || overwrite_cardinality("Quad@good_quad@cardinality", NC_INT, 3) != 0 ||
      expect_quadrature_status(EX_FATAL) != 0) {
    return 1;
  }

  if (create_file() != 0 ||
      overwrite_cardinality("Quad@good_quad@cardinality", NC_DOUBLE, 1) != 0 ||
      expect_quadrature_status(EX_FATAL) != 0) {
    return 1;
  }

  printf("test-field-cardinality-att: all checks passed\n");
  return 0;
}
