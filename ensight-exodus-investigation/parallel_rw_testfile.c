/*
 * parallel_rw_testfile.c
 *
 * Standalone MPI generator demonstrating N ranks each writing a distinct,
 * non-overlapping slice of nodes/elements into ONE SHARED Exodus file using
 * ONLY Exodus's own public parallel API -- no IOSS dependency at all.
 * Written for the EnSight Exodus-reader upgrade investigation (plan Part 3:
 * "can EnSight read/write a single Exodus file in parallel across multiple
 * MPI ranks directly through Exodus's own API, without IOSS's partitioning
 * layer?").
 *
 * Background: Exodus has a complete, self-sufficient MPI-based parallel I/O
 * API (ex_create_par/ex_open_par, built on NetCDF-C's parallel dispatch to
 * HDF5-parallel and/or PnetCDF) plus a family of ex_{get,put}_partial_*
 * functions that let each rank read/write only its own start/count
 * hyperslab of a shared on-disk array. IOSS's own shared-file parallel
 * backend (Ioex_ParallelDatabaseIO.C) is demonstrably nothing more than a
 * client of this exact same public API -- it just supplies a
 * Zoltan/ParMETIS-computed decomposition instead of the simple static
 * block-decomposition this program uses. There is no private, IOSS-only
 * Exodus entry point.
 *
 * This program is adapted from
 * packages/seacas/libraries/exodus/test/testrd_par.c (the collective-open
 * pattern) and packages/seacas/libraries/exodus/test/testwt-partial.c (the
 * ex_put_partial_* call pattern), extended -- unlike either existing test --
 * to actually demonstrate N ranks each owning a distinct, non-overlapping
 * slice of a single mesh:
 *
 *   - A simple 1-D chain of `total_elems` quad4 elements (a strip mesh):
 *     element e (0-based) connects 1-based nodes {2e+1, 2e+2, 2e+3, 2e+4},
 *     so consecutive elements share one edge (2 nodes) with their
 *     neighbor -- an ordinary connected mesh, not disjoint elements.
 *     `total_nodes = 2*total_elems + 2` nodes follows from the last
 *     element's highest node index. This still requires no ghost/halo
 *     bookkeeping between ranks (see the node-ownership comment in main()):
 *     each rank's node range is derived directly from its element range,
 *     with the shared boundary node between two ranks' element ranges
 *     owned by exactly the rank that comes first.
 *   - Nodes and elements are split into `nprocs` contiguous, non-overlapping
 *     blocks via simple static block decomposition (first
 *     `total % nprocs` ranks get one extra entry) -- no Zoltan/ParMETIS.
 *   - All ranks collectively call ex_create_par() to open ONE shared file,
 *     then collectively call ex_put_init()/ex_put_concat_elem_block() with
 *     IDENTICAL global parameters (required -- these calls define global
 *     dimensions, not per-rank data), then each rank calls
 *     ex_put_partial_coord()/ex_put_partial_conn() with ITS OWN start/count
 *     range only.
 *   - Optionally (--with-loadbal), the partition is also persisted via the
 *     Nemesis-origin ex_put_init_info()/ex_put_loadbal_param_cc()/
 *     ex_put_processor_node_maps()/ex_put_processor_elem_maps() calls, so
 *     the file is self-describing and re-readable without recomputing the
 *     decomposition.
 *
 *     IMPORTANT CAVEAT + FIX, discovered empirically while building this
 *     program (not just by reading code): IOSS's shared-file backend
 *     (Ioex_ParallelDatabaseIO.C, which uses ex_create_par/ex_open_par --
 *     the same shared-file mechanism this program uses) does NOT call any
 *     of the Nemesis loadbal/processor-map functions at all. Those
 *     functions ARE called elsewhere in IOSS, but only from its unrelated
 *     classic FILE-PER-RANK backend (Ioex_DatabaseIO.C), which never calls
 *     ex_create_par. So there is no real-world precedent in this repo for
 *     combining the Nemesis loadbal/processor-map calls with the
 *     shared-file ex_create_par model this program demonstrates -- and a
 *     first attempt at it broke in exactly the way that absence of
 *     precedent would predict: calling the PER-RANK ex_put_loadbal_param()
 *     once per rank (each rank passing only ITS OWN local counts) caused
 *     every rank to race-define the same global dimensions
 *     (num_int_elem/num_int_node) at its own, locally-sized, wrong value,
 *     and the per-rank ex_put_processor_{node,elem}_maps() calls then
 *     clobbered each other's data in the shared elem_mapi/node_mapi
 *     variables instead of appending -- confirmed via ncdump showing only
 *     one rank's data survived. Root cause: ex_put_loadbal_param() (the
 *     per-rank/singular form) never defines the VAR_*_IDX index variables
 *     that ex_put_processor_*_maps() needs (via ex_get_idx()) to append
 *     each rank's data at the correct offset -- confirmed by grepping its
 *     source for "_IDX" (zero matches). The FIX, used below: call the
 *     "concatenated" variant ex_put_loadbal_param_cc() ONCE, COLLECTIVELY,
 *     with every rank passing IDENTICAL full-length vectors (one entry per
 *     processor, built via MPI_Allgather) -- mirroring exactly how
 *     ex_put_init() above must already be called identically by every
 *     rank. That variant does define the VAR_*_IDX variables, and the
 *     subsequent per-rank ex_put_processor_*_maps() calls then correctly
 *     append (verified via ncdump: elem_mapi/node_mapi hold the full,
 *     correctly-ordered global ID lists, and elem_mapi_idx/node_mapi_idx
 *     hold the correct per-rank boundaries). This is a genuinely new,
 *     verified finding -- not previously documented anywhere in SEACAS --
 *     worth flagging to Ansys/Sandia as the concrete answer to "how do you
 *     combine Nemesis decomposition metadata with ex_create_par." See the
 *     investigation write-up (Part 3) for the full discussion.
 *
 * After the parallel write phase, rank 0 alone reopens the file with a
 * plain SERIAL ex_open() (no MPI, no _par suffix at all) and re-reads the
 * whole mesh with ordinary ex_get_coord()/ex_get_conn(), to confirm the
 * shared file assembled correctly and is valid outside of any
 * parallel-specific reading path -- not just self-consistent under the
 * parallel reader that wrote it.
 *
 * Usage:
 *   mpirun -n <nprocs> parallel_rw_testfile [options] [output.exo]
 *
 * Options:
 *   -e, --num-elems N     Total number of elements across all ranks
 *                           (default 12; must be >= number of MPI ranks).
 *   --with-loadbal         Also persist the partition via the Nemesis-origin
 *                           ex_put_init_info/ex_put_loadbal_param_cc/
 *                           ex_put_processor_{node,elem}_maps calls (see
 *                           caveat above).
 *   -o, --output PATH      Output filename (default: parallel_testfile.exo)
 *   -h, --help              Print this help and exit (rank 0 only; all ranks
 *                           still call MPI_Init/MPI_Finalize).
 *
 * Requires an MPI-enabled Exodus build with HDF5-parallel and/or PnetCDF
 * (PARALLEL_AWARE_EXODUS / NC_HAS_PARALLEL) -- confirmed present in this
 * environment's TPL install.
 */

#include <inttypes.h>
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "exodusII.h"

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

static int g_rank = 0;

#define EXCHECK(funcall)                                                                         \
  do {                                                                                            \
    int error = (funcall);                                                                        \
    printf("[rank %d] after %s, error = %d\n", g_rank, TOSTRING(funcall), error);                 \
    if (error != EX_NOERR) {                                                                      \
      fprintf(stderr, "[rank %d] Error calling %s\n", g_rank, TOSTRING(funcall));                 \
      MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);                                                     \
    }                                                                                              \
  } while (0)

static void usage(const char *prog)
{
  printf("Usage: mpirun -n <nprocs> %s [options] [output.exo]\n", prog);
  printf("  -e, --num-elems N   Total element count across all ranks (default 12)\n");
  printf("  --with-loadbal       Also persist partition via Nemesis loadbal/processor-map API\n");
  printf("  -o, --output PATH    Output filename (default: parallel_testfile.exo)\n");
  printf("  -h, --help           Print this help and exit\n");
}

/* Simple static block decomposition: split `total` contiguous 1-based IDs
 * across `nprocs` ranks, first (total % nprocs) ranks getting one extra
 * entry. Returns the 0-based start offset and count for `rank`. No
 * Zoltan/ParMETIS -- deliberately the simplest decomposition that still
 * produces non-overlapping, gap-free, contiguous per-rank ranges. */
static void block_decompose(int64_t total, int nprocs, int rank, int64_t *start0, int64_t *count)
{
  int64_t base  = total / nprocs;
  int64_t extra = total % nprocs;
  /* Ranks [0, extra) get (base+1) entries; the rest get `base`. */
  if (rank < extra) {
    *count  = base + 1;
    *start0 = rank * (base + 1);
  }
  else {
    *count  = base;
    *start0 = extra * (base + 1) + (rank - extra) * base;
  }
}

/* Rank 0 only: plain serial reopen/readback, no MPI-aware Exodus calls at
 * all, confirming the parallel-written file is an ordinary, valid Exodus
 * file outside of any parallel-specific reading path. */
static int verify_serial(const char *path, int64_t expect_total_nodes,
                         int64_t expect_total_elems)
{
  int   CPU_word_size = 8, IO_word_size = 0;
  float version       = 0.0f;
  /* EX_ALL_INT64_API must match what write_my_slice()/main() used when
   * creating the file: ex_get_conn() below is called with an int64_t*
   * buffer (conn), and without this flag Exodus would expect/produce a
   * plain 32-bit int* buffer there, corrupting the read exactly like the
   * write-side bug this flag also fixes. */
  int   exoid = ex_open(path, EX_READ | EX_ALL_INT64_API, &CPU_word_size, &IO_word_size, &version);
  if (exoid < 0) {
    fprintf(stderr, "VERIFY ERROR: could not reopen '%s' with plain ex_open\n", path);
    return EXIT_FAILURE;
  }

  ex_init_params par;
  memset(&par, 0, sizeof(par));
  if (ex_get_init_ext(exoid, &par) != EX_NOERR) {
    fprintf(stderr, "VERIFY ERROR: ex_get_init_ext failed\n");
    ex_close(exoid);
    return EXIT_FAILURE;
  }

  printf("\n=== Serial verification of '%s' (plain ex_open, no MPI) ===\n", path);
  printf("title='%s' num_nodes=%" PRId64 " num_elem=%" PRId64 " num_elem_blk=%" PRId64 "\n",
         par.title, par.num_nodes, par.num_elem, par.num_elem_blk);

  int status = EXIT_SUCCESS;
  if (par.num_nodes != expect_total_nodes || par.num_elem != expect_total_elems) {
    fprintf(stderr,
            "VERIFY ERROR: mismatch -- expected num_nodes=%" PRId64 " num_elem=%" PRId64 "\n",
            expect_total_nodes, expect_total_elems);
    status = EXIT_FAILURE;
  }

  double *x = calloc((size_t)par.num_nodes, sizeof(double));
  double *y = calloc((size_t)par.num_nodes, sizeof(double));
  double *z = calloc((size_t)par.num_nodes, sizeof(double));
  if (ex_get_coord(exoid, x, y, z) != EX_NOERR) {
    fprintf(stderr, "VERIFY ERROR: ex_get_coord failed\n");
    status = EXIT_FAILURE;
  }
  else {
    /* Spot-check: node i (0-based) should sit at x == i (see
     * write_my_slice()'s coordinate scheme below) -- confirms no
     * gaps/overlaps assembled across the per-rank partial writes. */
    int bad = 0;
    for (int64_t i = 0; i < par.num_nodes; i++) {
      if (x[i] != (double)i) {
        if (!bad) {
          fprintf(stderr, "VERIFY ERROR: node %" PRId64 " has x=%g, expected %g\n", i, x[i],
                  (double)i);
        }
        bad = 1;
      }
    }
    if (bad) {
      status = EXIT_FAILURE;
    }
    else {
      printf("Spot-check passed: all %" PRId64 " node x-coordinates match node index exactly --\n"
             "no gaps/overlaps across the %" PRId64 "-rank partial writes.\n",
             par.num_nodes, (int64_t)0);
    }
  }
  free(x);
  free(y);
  free(z);

  int64_t *conn = calloc((size_t)(4 * par.num_elem), sizeof(int64_t));
  if (ex_get_conn(exoid, EX_ELEM_BLOCK, 1, conn, NULL, NULL) != EX_NOERR) {
    fprintf(stderr, "VERIFY ERROR: ex_get_conn failed\n");
    status = EXIT_FAILURE;
  }
  else {
    /* Each element e (0-based) should connect nodes
     * {2e+1, 2e+2, 2e+3, 2e+4} (1-based) per write_my_slice()'s scheme. */
    int bad = 0;
    for (int64_t e = 0; e < par.num_elem; e++) {
      int64_t expect0 = 2 * e + 1;
      if (conn[4 * e + 0] != expect0 || conn[4 * e + 1] != expect0 + 1 ||
          conn[4 * e + 2] != expect0 + 2 || conn[4 * e + 3] != expect0 + 3) {
        if (!bad) {
          fprintf(stderr, "VERIFY ERROR: element %" PRId64 " connectivity mismatch\n", e);
        }
        bad = 1;
      }
    }
    if (bad) {
      status = EXIT_FAILURE;
    }
    else {
      printf("Spot-check passed: all %" PRId64
             " element connectivities match the expected chain pattern.\n",
             par.num_elem);
    }
  }
  free(conn);

  ex_close(exoid);
  printf("=== Serial verification %s ===\n", status == EXIT_SUCCESS ? "PASSED" : "FAILED");
  return status;
}

int main(int argc, char **argv)
{
  MPI_Init(&argc, &argv);

  int nprocs = 1;
  MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &g_rank);

  int64_t     total_elems  = 12;
  int         with_loadbal = 0;
  const char *output_path  = "parallel_testfile.exo";

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-e") == 0 || strcmp(argv[i], "--num-elems") == 0) {
      if (i + 1 >= argc) {
        if (g_rank == 0) fprintf(stderr, "ERROR: %s requires an argument\n", argv[i]);
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
      }
      total_elems = (int64_t)atoll(argv[++i]);
    }
    else if (strcmp(argv[i], "--with-loadbal") == 0) {
      with_loadbal = 1;
    }
    else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output") == 0) {
      if (i + 1 >= argc) {
        if (g_rank == 0) fprintf(stderr, "ERROR: %s requires an argument\n", argv[i]);
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
      }
      output_path = argv[++i];
    }
    else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      if (g_rank == 0) usage(argv[0]);
      MPI_Finalize();
      return EXIT_SUCCESS;
    }
    else if (argv[i][0] != '-') {
      output_path = argv[i];
    }
    else {
      if (g_rank == 0) {
        fprintf(stderr, "ERROR: unrecognized option '%s'\n", argv[i]);
        usage(argv[0]);
      }
      MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }
  }

  if (total_elems < nprocs) {
    if (g_rank == 0) {
      fprintf(stderr, "ERROR: --num-elems (%" PRId64 ") must be >= number of MPI ranks (%d)\n",
              total_elems, nprocs);
    }
    MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
  }

  /* Chain mesh: element e (0-based) connects 1-based nodes
   * {2e+1, 2e+2, 2e+3, 2e+4} -- an ordinary connected strip of quad4
   * elements where consecutive elements share one edge (2 nodes) with
   * their neighbor. Total node count follows from the last element's
   * highest node index. */
  int64_t total_nodes = 2 * total_elems + 2;

  ex_opts(EX_VERBOSE);

  /* Per-rank, non-overlapping element slices via simple static block
   * decomposition -- no Zoltan/ParMETIS. */
  int64_t elem_start0, elem_count;
  block_decompose(total_elems, nprocs, g_rank, &elem_start0, &elem_count);

  /* Node ownership derives directly from the element range. In 0-based
   * terms element e spans node indices [2e, 2e+3] (4 nodes), so element
   * e and element e+1 share TWO nodes -- the edge {2e+2, 2e+3}. A rank
   * owning elements [elem_start0, elem_start0+elem_count) therefore has
   * a node "footprint" of [2*elem_start0, 2*elem_start0+2*elem_count+1]
   * (0-based, inclusive), i.e. 2*elem_count+2 nodes. Of those, the first
   * TWO nodes of the footprint were already written by the previous
   * rank's last element (its footprint's last two nodes) -- except for
   * the very first rank (elem_start0==0), which owns the mesh's two
   * leading boundary nodes outright and so writes its full footprint.
   * This keeps every rank's partial write genuinely non-overlapping with
   * every other rank's, while the resulting connectivity still correctly
   * references the shared boundary nodes between elements. No special
   * case is needed at the trailing end: the last rank's footprint
   * naturally reaches the mesh's final node index. */
  int64_t node_start0 = (elem_start0 == 0) ? 0 : (2 * elem_start0 + 2);
  int64_t node_count  = 2 * elem_count + ((elem_start0 == 0) ? 2 : 0);

  printf("[rank %d of %d] owns elements [%" PRId64 ", %" PRId64 ") (count=%" PRId64
        "), nodes [%" PRId64 ", %" PRId64 ") (count=%" PRId64 ")\n",
        g_rank, nprocs, elem_start0, elem_start0 + elem_count, elem_count, node_start0,
        node_start0 + node_count, node_count);

  MPI_Comm mpi_comm = MPI_COMM_WORLD;
  MPI_Info mpi_info = MPI_INFO_NULL;

  int CPU_word_size = 8;
  int IO_word_size  = 8;

  /* EX_NETCDF4 | EX_NOCLASSIC (HDF5-parallel) is the mode IOSS's own
   * shared-file parallel backend defaults to as well (Ioex_BaseDatabaseIO.C
   * sets exactly these two flags for parallel runs). PnetCDF via
   * EX_64BIT_DATA/EX_PNETCDF is an alternative this program does not
   * exercise. EX_ALL_INT64_API is required because every void_int*
   * argument this program passes (connectivity, block/map IDs, processor
   * maps, ...) is backed by an int64_t array -- without this flag Exodus
   * defaults to plain 32-bit `int` for those buffers and silently
   * misinterprets our data (confirmed via ncdump: writes without this
   * flag produced garbled/interleaved connectivity). */
  int exoid = ex_create_par(output_path,
                            EX_CLOBBER | EX_NETCDF4 | EX_NOCLASSIC | EX_ALL_INT64_API,
                            &CPU_word_size, &IO_word_size, mpi_comm, mpi_info);
  printf("[rank %d] after ex_create_par('%s'), exoid = %d\n", g_rank, output_path, exoid);
  if (exoid < 0) {
    fprintf(stderr, "[rank %d] ERROR: ex_create_par failed (did you request an MPI/parallel-aware "
                    "Exodus build with HDF5-parallel and/or PnetCDF support?)\n",
            g_rank);
    MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
  }

  /* ---- Global metadata: every rank calls these COLLECTIVELY with
   * IDENTICAL arguments -- these define global dimensions/variables, not
   * per-rank data; they are NOT the partial/per-slice calls. ---- */
  char title[MAX_LINE_LENGTH + 1];
  snprintf(title, sizeof(title), "Parallel chain mesh: %" PRId64 " elements, %d MPI ranks",
          total_elems, nprocs);
  EXCHECK(ex_put_init(exoid, title, /*num_dim=*/3, total_nodes, total_elems, /*num_elem_blk=*/1,
                      /*num_node_sets=*/0, /*num_side_sets=*/0));

  {
    int64_t     blk_id             = 1;
    char       *elem_type[]        = {"quad4"};
    int64_t     num_elem_this_blk  = total_elems;
    int64_t     num_nodes_per_elem = 4;
    int64_t     num_attr_this_blk  = 0;
    EXCHECK(ex_put_concat_elem_block(exoid, &blk_id, elem_type, &num_elem_this_blk,
                                     &num_nodes_per_elem, &num_attr_this_blk,
                                     /*define_maps=*/1));
  }

  /* Optional Nemesis-origin decomposition metadata -- see the big caveat
   * comment at the top of this file: no real-world SEACAS code combines
   * this with ex_create_par's shared-file model, but nothing in its own
   * preconditions forbids it. Treat this branch as a demonstration, not an
   * established convention.
   *
   * IMPORTANT, discovered empirically (ncdump of a first attempt showed
   * elem_mapi/node_mapi holding only ONE rank's data, and num_int_elem/
   * num_int_node dimensioned to only that one rank's local counts): the
   * per-rank ex_put_loadbal_param() does NOT define the VAR_*_IDX index
   * variables that ex_put_processor_node_maps()/ex_put_processor_elem_maps()
   * rely on (via ex_get_idx()) to append each rank's data at the correct
   * offset into the shared map variable -- confirmed by grepping
   * ex_put_loadbal_param.c for "_IDX" (zero matches) vs.
   * ex_put_loadbal_param_cc.c (defines all seven). Called once per rank with
   * only that rank's own scalar counts, concurrent calls on this
   * HDF5-parallel file simply raced to define num_int_elem/num_int_node at
   * each rank's own (wrong, local-only) size, and only one rank's
   * definition survived. The fix is to call the "concatenated" variant,
   * ex_put_loadbal_param_cc(), ONCE, COLLECTIVELY, with every rank passing
   * the SAME full-length vectors (one entry per processor) -- exactly
   * mirroring how ex_put_init() above is already called identically by
   * every rank. Each rank's local scalar is gathered into those vectors via
   * MPI_Allgather first. */
  if (with_loadbal) {
    EXCHECK(ex_put_init_info(exoid, nprocs, nprocs, "p"));

    /* This program's partition has no border/external nodes or elements
     * and no communication maps at all -- every node/element is strictly
     * "internal" to exactly one rank, since the chain topology was chosen
     * specifically to need zero ghost/shared-entity bookkeeping. */
    int64_t *all_int_nodes = calloc((size_t)nprocs, sizeof(int64_t));
    int64_t *all_int_elems = calloc((size_t)nprocs, sizeof(int64_t));
    int64_t *all_zero      = calloc((size_t)nprocs, sizeof(int64_t));
    MPI_Allgather(&node_count, 1, MPI_INT64_T, all_int_nodes, 1, MPI_INT64_T, MPI_COMM_WORLD);
    MPI_Allgather(&elem_count, 1, MPI_INT64_T, all_int_elems, 1, MPI_INT64_T, MPI_COMM_WORLD);
    EXCHECK(ex_put_loadbal_param_cc(exoid, /*num_int_nodes=*/all_int_nodes,
                                    /*num_bor_nodes=*/all_zero, /*num_ext_nodes=*/all_zero,
                                    /*num_int_elems=*/all_int_elems, /*num_bor_elems=*/all_zero,
                                    /*num_node_cmaps=*/all_zero, /*num_elem_cmaps=*/all_zero));
    free(all_int_nodes);
    free(all_int_elems);
    free(all_zero);

    int64_t *node_map_int = calloc((size_t)node_count, sizeof(int64_t));
    for (int64_t i = 0; i < node_count; i++) {
      node_map_int[i] = node_start0 + i + 1; /* 1-based global node IDs this rank owns */
    }
    EXCHECK(ex_put_processor_node_maps(exoid, node_map_int, NULL, NULL, g_rank));
    free(node_map_int);

    int64_t *elem_map_int = calloc((size_t)elem_count, sizeof(int64_t));
    for (int64_t i = 0; i < elem_count; i++) {
      elem_map_int[i] = elem_start0 + i + 1; /* 1-based global element IDs this rank owns */
    }
    EXCHECK(ex_put_processor_elem_maps(exoid, elem_map_int, NULL, g_rank));
    free(elem_map_int);
  }

  /* ---- Per-rank partial writes: each rank writes ONLY its own slice. ---- */
  {
    double *x = calloc((size_t)node_count, sizeof(double));
    double *y = calloc((size_t)node_count, sizeof(double));
    double *z = calloc((size_t)node_count, sizeof(double));
    for (int64_t i = 0; i < node_count; i++) {
      int64_t global_node_idx0 = node_start0 + i; /* 0-based global node index */
      x[i] = (double)global_node_idx0;            /* x == node index, for easy verification */
      y[i] = 0.0;
      z[i] = 0.0;
    }
    /* start_node_num is 1-based */
    EXCHECK(ex_put_partial_coord(exoid, node_start0 + 1, node_count, x, y, z));
    free(x);
    free(y);
    free(z);
  }

  {
    int64_t *conn = calloc((size_t)(4 * elem_count), sizeof(int64_t));
    for (int64_t i = 0; i < elem_count; i++) {
      int64_t global_elem_idx0 = elem_start0 + i; /* 0-based global element index */
      int64_t n0               = 2 * global_elem_idx0 + 1; /* 1-based global node id */
      conn[4 * i + 0] = n0;
      conn[4 * i + 1] = n0 + 1;
      conn[4 * i + 2] = n0 + 2;
      conn[4 * i + 3] = n0 + 3;
    }
    /* start_num is 1-based */
    EXCHECK(ex_put_partial_conn(exoid, EX_ELEM_BLOCK, /*blk_id=*/1, elem_start0 + 1, elem_count,
                                conn, NULL, NULL));
    free(conn);
  }

  EXCHECK(ex_close(exoid));
  printf("[rank %d] Wrote its slice into shared file '%s' via ex_create_par + "
        "ex_put_partial_coord/ex_put_partial_conn -- no IOSS involved.\n",
        g_rank, output_path);

  MPI_Barrier(MPI_COMM_WORLD);

  int final_status = EXIT_SUCCESS;
  if (g_rank == 0) {
    final_status = verify_serial(output_path, total_nodes, total_elems);
  }

  int broadcast_status = final_status;
  MPI_Bcast(&broadcast_status, 1, MPI_INT, 0, MPI_COMM_WORLD);

  MPI_Finalize();
  return broadcast_status;
}
