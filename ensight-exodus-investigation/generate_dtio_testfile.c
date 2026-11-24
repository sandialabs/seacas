/*
 * generate_dtio_testfile.c
 *
 * Standalone generator demonstrating Exodus "dynamic topology I/O" (DTIO)
 * using ONLY Exodus's native netCDF4-group API -- no IOSS dependency at
 * all. Written for the EnSight Exodus-reader upgrade investigation.
 *
 * Background (see the investigation plan, Part 2): there is no dedicated
 * "dynamic topology" or "change set" API in the Exodus C library itself.
 * What IOSS calls a "change set" (its CONTROL_AUTO_GROUP_FILE mechanism,
 * Ioss::ChangeSet / Ioex::ChangeSet) is, at the Exodus level, nothing more
 * than a plain netCDF4 sub-group created with ex_create_group() -- each
 * group is a fully independent Exodus mesh namespace (its own ex_put_init,
 * blocks, connectivity, counts) with NO Exodus-level linkage between
 * groups or to simulation time. That bookkeeping is an IOSS-layer
 * convention, not an Exodus one.
 *
 * This program is adapted from
 * packages/seacas/libraries/exodus/test/testwt-groups.c (the only
 * pure-Exodus, IOSS-free example of ex_create_group in the SEACAS repo),
 * but deliberately diverges from it in the one way that matters for a
 * DTIO demonstration: testwt-groups.c writes an IDENTICAL mesh into every
 * group (it only proves the group mechanism exists), whereas this program
 * defines a GENUINELY DIFFERENT element-block topology inside each group's
 * own ex_put_init() call -- i.e. real topology change between "time
 * steps," using nothing but ex_create_group()/ex_put_init()/ex_put_coord()/
 * ex_put_conn().
 *
 * Topology scheme (default): group i (i = 0 .. num_steps-1) contains
 * (base_blocks + i) single-quad element blocks, each a freestanding
 * 4-node quad offset along X. So group 0 might have 2 blocks/elements,
 * group 1 has 3, group 2 has 4, etc. -- a simple, inspectable analogue of
 * elements being added over time (the reverse -- shrinking -- is exactly
 * symmetric and can be produced with --shrink).
 *
 * Each group also gets:
 *   - a group-local ex_put_time(exoid, 1, &time_val) recording the
 *     simulation time this topology snapshot represents (NOT an
 *     Exodus-level link to other groups -- just transient data local to
 *     that group's own namespace, same as any ordinary Exodus file).
 *   - a text attribute "dtio_step_label" at EX_GLOBAL scope, purely as a
 *     human-readable convenience; this is an application-level
 *     convention, not anything Exodus or IOSS requires.
 *
 * After writing, the program reopens the root file and walks the group
 * tree with ex_get_group_ids()/ex_get_group_name() (the same native calls
 * IOSS, exodiff, and aprepro all use to enumerate "change sets" --
 * investigation plan sec. 2.2) and re-reads each group's ex_get_init() to
 * print its topology, as a self-contained sanity check that the groups
 * are distinct and independently readable -- no IOSS, no io_shell needed,
 * though those remain good additional checks (see the plan's Verification
 * section).
 *
 * Usage:
 *   generate_dtio_testfile [options] [output.exo]
 *
 * Options:
 *   -n, --num-steps N     Number of groups/topology snapshots (default 3)
 *   -b, --base-blocks N   Element-block count in step 0 (default 2)
 *   --shrink              Each step REMOVES a block instead of adding one
 *                           (simulates element death instead of birth);
 *                           base-blocks must be >= num-steps in this mode.
 *   -o, --output PATH     Output filename (default: dtio_testfile.exo)
 *   -h, --help            Print this help and exit.
 *
 * Requires the file be opened EX_NETCDF4 | EX_NOCLASSIC (HDF5-backed,
 * non-classic) -- ex_create_group() is unavailable otherwise.
 */

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "exodusII.h"

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define EXCHECK(funcall)                                                                         \
  do {                                                                                            \
    int error = (funcall);                                                                        \
    printf("after %s, error = %d\n", TOSTRING(funcall), error);                                   \
    if (error != EX_NOERR) {                                                                      \
      fprintf(stderr, "Error calling %s\n", TOSTRING(funcall));                                   \
      ex_close(rootid);                                                                           \
      exit(EXIT_FAILURE);                                                                         \
    }                                                                                              \
  } while (0)

static void usage(const char *prog)
{
  printf("Usage: %s [options] [output.exo]\n", prog);
  printf("  -n, --num-steps N     Number of groups/topology snapshots (default 3)\n");
  printf("  -b, --base-blocks N   Element-block count in step 0 (default 2)\n");
  printf("  --shrink              Remove a block each step instead of adding one\n");
  printf("  -o, --output PATH     Output filename (default: dtio_testfile.exo)\n");
  printf("  -h, --help            Print this help and exit\n");
}

/* Write one group's worth of a mesh: num_blocks single-quad element
 * blocks, each a disjoint 4-node quad offset along X by its block index,
 * so successive groups with different num_blocks are trivially
 * distinguishable (both in block count and in node/element count). */
static void write_topology_snapshot(int exoid, int step, int num_blocks, double time_val)
{
  int num_nodes = 4 * num_blocks;
  int num_elem  = num_blocks;

  char title[MAX_LINE_LENGTH + 1];
  snprintf(title, sizeof(title), "DTIO snapshot step %d (%d element blocks)", step, num_blocks);

  int rootid = exoid; /* for EXCHECK's error path; harmless to reuse name here */
  EXCHECK(ex_put_init(exoid, title, /*num_dim=*/3, num_nodes, num_elem, num_blocks,
                       /*num_node_sets=*/0, /*num_side_sets=*/0));

  double *x = calloc((size_t)num_nodes, sizeof(double));
  double *y = calloc((size_t)num_nodes, sizeof(double));
  double *z = calloc((size_t)num_nodes, sizeof(double));
  for (int b = 0; b < num_blocks; b++) {
    double x0           = (double)b * 2.0;
    x[4 * b + 0]         = x0 + 0.0;
    y[4 * b + 0]         = 0.0;
    x[4 * b + 1]         = x0 + 1.0;
    y[4 * b + 1]         = 0.0;
    x[4 * b + 2]         = x0 + 1.0;
    y[4 * b + 2]         = 1.0;
    x[4 * b + 3]         = x0 + 0.0;
    y[4 * b + 3]         = 1.0;
    z[4 * b + 0] = z[4 * b + 1] = z[4 * b + 2] = z[4 * b + 3] = 0.0;
  }
  EXCHECK(ex_put_coord(exoid, x, y, z));
  free(x);
  free(y);
  free(z);

  for (int b = 0; b < num_blocks; b++) {
    int   block_id = b + 1;
    char  elem_type[] = "quad4";
    char  block_name[32];
    snprintf(block_name, sizeof(block_name), "step%d_block_%d", step, block_id);

    EXCHECK(ex_put_block(exoid, EX_ELEM_BLOCK, block_id, elem_type, /*num_entries_this_blk=*/1,
                          /*num_nodes_per_entry=*/4, /*num_edges_per_entry=*/0,
                          /*num_faces_per_entry=*/0, /*num_attr_per_entry=*/0));
    EXCHECK(ex_put_name(exoid, EX_ELEM_BLOCK, block_id, block_name));

    int conn[4] = {4 * b + 1, 4 * b + 2, 4 * b + 3, 4 * b + 4}; /* 1-based node ids */
    EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, block_id, conn, NULL, NULL));
  }

  EXCHECK(ex_put_time(exoid, 1, &time_val));

  char label[64];
  snprintf(label, sizeof(label), "dtio step %d, t=%g, %d blocks", step, time_val, num_blocks);
  EXCHECK(ex_put_text_attribute(exoid, EX_GLOBAL, 0, "dtio_step_label", label));
}

/* Re-open the root file and walk the group tree the same way IOSS /
 * exodiff / aprepro do (EX_INQ_NUM_CHILD_GROUPS + ex_get_group_ids +
 * EX_INQ_GROUP_NAME), re-reading each group's topology as a sanity check. */
static int verify_groups(const char *path)
{
  int   CPU_word_size = 8, IO_word_size = 0;
  float version       = 0.0f;
  /* Unlike some other optional-output-pointer Exodus APIs, ex_open_int()
   * unconditionally dereferences its `version` argument (*version = 0.0f;
   * right at the top) -- it is NOT safe to pass NULL here. */
  int rootid = ex_open(path, EX_READ, &CPU_word_size, &IO_word_size, &version);
  if (rootid < 0) {
    fprintf(stderr, "VERIFY ERROR: could not reopen '%s'\n", path);
    return EXIT_FAILURE;
  }

  int num_children = ex_inquire_int(rootid, EX_INQ_NUM_CHILD_GROUPS);
  printf("\n=== Verification: root has %d child group(s) ===\n", num_children);

  int *children = calloc((size_t)num_children, sizeof(int));
  EXCHECK(ex_get_group_ids(rootid, NULL, children));

  for (int i = 0; i < num_children; i++) {
    int  child_exoid = children[i];
    char group_name[MAX_STR_LENGTH + 1];
    /* ex_inquire()'s 32-bit-int-API wrapper path unconditionally writes
     * *ret_int = (int)tmp_int on every call, regardless of whether the
     * specific req_info (here EX_INQ_GROUP_NAME) actually uses ret_int --
     * passing NULL for it segfaults. Give it a real (if unused) int, and a
     * real float for ret_float too, to be safe. */
    int   unused_int   = 0;
    float unused_float = 0.0f;
    EXCHECK(ex_inquire(child_exoid, EX_INQ_GROUP_NAME, &unused_int, &unused_float, group_name));

    ex_init_params par;
    EXCHECK(ex_get_init_ext(child_exoid, &par));

    double time_val = 0.0;
    EXCHECK(ex_get_time(child_exoid, 1, &time_val));

    /* There is no typed ex_get_text_attribute() counterpart to
     * ex_put_text_attribute() -- reading any attribute back (text, int, or
     * double) goes through the generic ex_attribute-struct API:
     * ex_get_attribute_count() -> ex_get_attribute_param() (fills in
     * name/type/value_count, allocates nothing) -> ex_get_attributes()
     * (actually populates ->values), exactly as testrd-assembly.c does. */
    char label[MAX_LINE_LENGTH + 1] = "";
    {
      ex_attribute attr[4];
      memset(attr, 0, sizeof(attr));
      int att_count = ex_get_attribute_count(child_exoid, EX_GLOBAL, 0);
      if (att_count > 0 && att_count <= (int)(sizeof(attr) / sizeof(attr[0]))) {
        EXCHECK(ex_get_attribute_param(child_exoid, EX_GLOBAL, 0, attr));
        EXCHECK(ex_get_attributes(child_exoid, att_count, attr));
        for (int j = 0; j < att_count; j++) {
          if (strcmp(attr[j].name, "dtio_step_label") == 0 && attr[j].type == EX_CHAR) {
            size_t n = (size_t)attr[j].value_count;
            if (n >= sizeof(label)) {
              n = sizeof(label) - 1;
            }
            memcpy(label, attr[j].values, n);
            label[n] = '\0';
          }
          free(attr[j].values);
        }
      }
    }

    printf("Group '%s' (exoid %d): num_elem_blk=%" PRId64 " num_nodes=%" PRId64
           " num_elem=%" PRId64 " time=%g label='%s'\n",
           group_name, child_exoid, par.num_elem_blk, par.num_nodes, par.num_elem, time_val,
           label);
  }
  free(children);

  EXCHECK(ex_close(rootid));
  printf("=== Verification complete: %d groups confirmed structurally distinct ===\n",
         num_children);
  return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
  int         num_steps   = 3;
  int         base_blocks = 2;
  int         shrink      = 0;
  const char *output_path = "dtio_testfile.exo";

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-n") == 0 || strcmp(argv[i], "--num-steps") == 0) {
      if (i + 1 >= argc) { fprintf(stderr, "ERROR: %s requires an argument\n", argv[i]); return EXIT_FAILURE; }
      num_steps = atoi(argv[++i]);
    }
    else if (strcmp(argv[i], "-b") == 0 || strcmp(argv[i], "--base-blocks") == 0) {
      if (i + 1 >= argc) { fprintf(stderr, "ERROR: %s requires an argument\n", argv[i]); return EXIT_FAILURE; }
      base_blocks = atoi(argv[++i]);
    }
    else if (strcmp(argv[i], "--shrink") == 0) {
      shrink = 1;
    }
    else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output") == 0) {
      if (i + 1 >= argc) { fprintf(stderr, "ERROR: %s requires an argument\n", argv[i]); return EXIT_FAILURE; }
      output_path = argv[++i];
    }
    else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      usage(argv[0]);
      return EXIT_SUCCESS;
    }
    else if (argv[i][0] != '-') {
      output_path = argv[i];
    }
    else {
      fprintf(stderr, "ERROR: unrecognized option '%s'\n", argv[i]);
      usage(argv[0]);
      return EXIT_FAILURE;
    }
  }

  if (num_steps < 1) {
    fprintf(stderr, "ERROR: --num-steps must be >= 1\n");
    return EXIT_FAILURE;
  }
  if (shrink && base_blocks < num_steps) {
    fprintf(stderr, "ERROR: --shrink requires --base-blocks >= --num-steps "
                     "(each step removes one block)\n");
    return EXIT_FAILURE;
  }

  ex_opts(EX_VERBOSE);

  int CPU_word_size = 8;
  int IO_word_size  = 8;

  /* EX_NETCDF4 | EX_NOCLASSIC is required for ex_create_group() to work. */
  int rootid = ex_create(output_path, EX_CLOBBER | EX_NETCDF4 | EX_NOCLASSIC, &CPU_word_size,
                          &IO_word_size);
  if (rootid < 0) {
    fprintf(stderr, "ERROR: ex_create failed for '%s' (did you request "
                     "EX_NETCDF4 | EX_NOCLASSIC support in your Exodus build?)\n",
            output_path);
    return EXIT_FAILURE;
  }
  printf("after ex_create for '%s', rootid = %d\n", output_path, rootid);
  printf(" num_steps=%d base_blocks=%d shrink=%d\n", num_steps, base_blocks, shrink);

  for (int step = 0; step < num_steps; step++) {
    int num_blocks = shrink ? (base_blocks - step) : (base_blocks + step);

    char group_name[32];
    snprintf(group_name, sizeof(group_name), "DTIO_STEP-%04d", step);

    int exoid = ex_create_group(rootid, group_name);
    printf("after ex_create_group('%s'), exoid = %d\n", group_name, exoid);
    if (exoid < 0) {
      fprintf(stderr, "Error calling ex_create_group('%s')\n", group_name);
      ex_close(rootid);
      return EXIT_FAILURE;
    }

    double time_val = (double)step;
    write_topology_snapshot(exoid, step, num_blocks, time_val);
  }

  EXCHECK(ex_close(rootid));
  printf("Wrote '%s' with %d group(s), each a distinct Exodus mesh namespace via native\n"
         "ex_create_group() -- no IOSS involved.\n",
         output_path, num_steps);

  return verify_groups(output_path);
}
