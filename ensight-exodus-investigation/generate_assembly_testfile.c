/*
 * generate_assembly_testfile.c
 *
 * Standalone generator for an Exodus file exercising native Exodus
 * "assemblies" (ex_assembly / ex_put_assembly(ies) / ex_get_assembly(ies)),
 * written for the EnSight Exodus-reader upgrade investigation.
 *
 * Adapted from packages/seacas/libraries/exodus/test/testwt-assembly.c
 * (SEACAS repo, branch ensight-exodus-seacas-investigation, off master).
 * That test is the canonical in-repo reference for the assembly write API;
 * this program strips it down to the assembly-defining core by default, and
 * exposes the attribute / reduction-variable sections (which testwt-assembly.c
 * always includes) as opt-in flags so a reader implementor can start from
 * the minimal case and grow into the fuller one that testrd-assembly.c
 * expects on readback.
 *
 * Produces a tiny mesh (7 single-node "sphere" element blocks, ids 10-16)
 * and a 2-level nested assembly tree:
 *
 *   100 "Root"   (type=EX_ASSEMBLY)   -> members {200, 300, 400}
 *   200 "Child2" (type=EX_ELEM_BLOCK) -> members {10, 11, 12, 13}
 *   300 "Child3" (type=EX_ELEM_BLOCK) -> members {14, 15, 16}
 *   400 "Child4" (type=EX_ELEM_BLOCK) -> members {10, 16}   <- block 10 and 16
 *                                                               each belong to
 *                                                               TWO assemblies
 *
 * This demonstrates: nesting (Root's members are themselves assemblies),
 * the "define now, populate entity_list later" two-call pattern (Root and
 * Child4 are defined with a NULL entity_list and populated in a second
 * ex_put_assembly call), and a block shared by multiple assemblies.
 *
 * Usage:
 *   generate_assembly_testfile [options] [output.exo]
 *
 * Options:
 *   --with-attributes       Attach per-assembly attributes (Scale, Offset,
 *                            Dimension, Units) via both the typed
 *                            (ex_put_double/text/integer_attribute) and
 *                            generic (ex_put_attribute) APIs, matching
 *                            testwt-assembly.c's attribute section.
 *   --with-reduction-vars   Define 4 reduction variables (Momentum_X/Y/Z,
 *                            Kinetic_Energy) on EX_ASSEMBLY and write them
 *                            across 10 time steps, matching
 *                            testwt-assembly.c's transient section.
 *   --with-all              Shorthand for both of the above (full parity
 *                            with testwt-assembly.c).
 *   -o, --output PATH       Output filename (default: assembly_testfile.exo)
 *   -h, --help              Print this help and exit.
 *
 * Verify the result with the existing testrd-assembly test program (or
 * exodiff / ncdump), per the investigation plan's Verification section.
 */

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
      ex_close(exoid);                                                                            \
      exit(EXIT_FAILURE);                                                                         \
    }                                                                                              \
  } while (0)

static void usage(const char *prog)
{
  printf("Usage: %s [options] [output.exo]\n", prog);
  printf("  --with-attributes       Attach per-assembly attributes\n");
  printf("  --with-reduction-vars   Write assembly reduction variables over 10 steps\n");
  printf("  --with-all              Both of the above (full testwt-assembly.c parity)\n");
  printf("  -o, --output PATH       Output filename (default: assembly_testfile.exo)\n");
  printf("  -h, --help              Print this help and exit\n");
}

int main(int argc, char **argv)
{
  int         with_attributes     = 0;
  int         with_reduction_vars = 0;
  const char *output_path         = "assembly_testfile.exo";

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--with-attributes") == 0) {
      with_attributes = 1;
    }
    else if (strcmp(argv[i], "--with-reduction-vars") == 0) {
      with_reduction_vars = 1;
    }
    else if (strcmp(argv[i], "--with-all") == 0) {
      with_attributes     = 1;
      with_reduction_vars = 1;
    }
    else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "ERROR: %s requires an argument\n", argv[i]);
        return EXIT_FAILURE;
      }
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

  ex_opts(EX_VERBOSE);

  /* Specify compute and i/o word size */
  int CPU_word_size = 8;
  int IO_word_size  = 8;

  /* create EXODUS II file */
  int exoid = ex_create(output_path,     /* filename path */
                         EX_CLOBBER,     /* create mode */
                         &CPU_word_size, /* CPU double word size in bytes */
                         &IO_word_size); /* I/O double word size in bytes */
  if (exoid < 0) {
    fprintf(stderr, "ERROR: ex_create failed for '%s'\n", output_path);
    return EXIT_FAILURE;
  }
  printf("after ex_create for '%s', exoid = %d\n", output_path, exoid);
  printf(" cpu word size: %d io word size: %d\n", CPU_word_size, IO_word_size);
  printf(" with_attributes = %d, with_reduction_vars = %d\n", with_attributes,
         with_reduction_vars);

  int num_elem_blk = 7;
  int num_assembly = 4;
  {
    /* initialize file with parameters */
    ex_init_params par = {.num_dim       = 3,
                           .num_nodes    = 1,
                           .num_elem     = 7,
                           .num_elem_blk = num_elem_blk,
                           .num_node_sets = 0,
                           .num_side_sets = 0,
                           .num_assembly  = num_assembly};

    char *title = "EnSight investigation: minimal assembly test file";
    ex_copy_string(par.title, title, MAX_LINE_LENGTH + 1);
    EXCHECK(ex_put_init_ext(exoid, &par));
  }

  double coord[] = {0.0};
  EXCHECK(ex_put_coord(exoid, coord, coord, coord));

  /* ======================================================================== */
  /* write element block parameters -- 7 single-node "sphere" blocks */
  struct ex_block blocks[7];
  for (int i = 0; i < num_elem_blk; i++) {
    blocks[i] = (ex_block){.type = EX_ELEM_BLOCK, .num_entry = 1, .id = i + 10};
    ex_copy_string(blocks[i].topology, "sphere", MAX_STR_LENGTH + 1);
    blocks[i].num_nodes_per_entry = 1;
  }

  EXCHECK(ex_put_block_params(exoid, num_elem_blk, blocks));

  int connect[] = {1};
  for (int i = 0; i < num_elem_blk; i++) {
    EXCHECK(ex_put_conn(exoid, EX_ELEM_BLOCK, blocks[i].id, connect, NULL, NULL));
  }

  /* Write element block names */
  char *block_names[] = {"block_A", "block_B", "block_C", "block_D",
                          "block_E", "block_F", "block_G"};
  for (int i = 0; i < num_elem_blk; i++) {
    EXCHECK(ex_put_name(exoid, EX_ELEM_BLOCK, blocks[i].id, block_names[i]));
  }

  /* ======================================================================== */
  /* Define and Output Assemblies -- 2-level nested tree; block 10 and 16
   * each belong to two assemblies (Child2/Child4 and Child3/Child4
   * respectively). */
  int64_t list_100[] = {200, 300, 400};
  int64_t list_200[] = {10, 11, 12, 13};
  int64_t list_300[] = {14, 15, 16};
  int64_t list_400[] = {10, 16};

  /* Root and Child4 are defined WITHOUT an entity list, then populated in a
   * second ex_put_assembly call below -- demonstrating the define-then-
   * populate two-call pattern that an EnSight writer path might also need
   * to support. */
  ex_assembly assembly[] = {{100, "Root", EX_ASSEMBLY, 3, NULL},
                             {200, "Child2", EX_ELEM_BLOCK, 4, list_200},
                             {300, "Child3", EX_ELEM_BLOCK, 3, list_300},
                             {400, "Child4", EX_ELEM_BLOCK, 2, NULL}};

  for (int i = 0; i < num_assembly; i++) {
    EXCHECK(ex_put_assembly(exoid, assembly[i]));
  }

  /* Now populate the entity lists that were deferred above. */
  assembly[0].entity_list = list_100;
  EXCHECK(ex_put_assembly(exoid, assembly[0]));

  assembly[3].entity_list = list_400;
  EXCHECK(ex_put_assembly(exoid, assembly[3]));

  /* ======================================================================== */
  /* Optional: arbitrary attributes on assemblies (and, for comparison, on a
   * non-assembly entity and EX_GLOBAL) -- mirrors testwt-assembly.c. */
  if (with_attributes) {
    double scale     = 1.5;
    double offset[]  = {1.1, 2.2, 3.3};
    char  *dimension = "length";
    int    units[]   = {1, 0, 0, -1};

    EXCHECK(ex_put_double_attribute(exoid, EX_ASSEMBLY, 100, "Scale", 1, &scale));
    EXCHECK(ex_put_double_attribute(exoid, EX_ASSEMBLY, 200, "Offset", 3, offset));
    EXCHECK(ex_put_text_attribute(exoid, EX_ASSEMBLY, 300, "Dimension", dimension));
    EXCHECK(ex_put_integer_attribute(exoid, EX_ASSEMBLY, 400, "Units", 4, units));

    ex_attribute attribute = {.entity_type = EX_ASSEMBLY,
                               .entity_id   = 100,
                               .name        = {"Units"},
                               .type        = EX_INTEGER,
                               .value_count = 4,
                               .values      = units};
    EXCHECK(ex_put_attribute(exoid, attribute));

    ex_attribute attr_offset = {EX_ASSEMBLY, 300, "Offset", EX_DOUBLE, 3, offset};
    EXCHECK(ex_put_attribute(exoid, attr_offset));

    /* Attributes work the same way for non-assembly entities and EX_GLOBAL. */
    EXCHECK(ex_put_integer_attribute(exoid, EX_ELEM_BLOCK, 11, "Units", 4, units));
    EXCHECK(ex_put_text_attribute(exoid, EX_GLOBAL, 0, "SOLID_MODEL", "STEP-X-43-1547836-Rev 0"));
  }

  /* ======================================================================== */
  /* Optional: reduction (per-assembly, not per-member) transient variables
   * over 10 time steps -- mirrors testwt-assembly.c. */
  if (with_reduction_vars) {
    int num_assem_vars = 4;
    EXCHECK(ex_put_reduction_variable_param(exoid, EX_ASSEMBLY, num_assem_vars));

    {
      char *var_names[] = {"Momentum_X", "Momentum_Y", "Momentum_Z", "Kinetic_Energy"};
      EXCHECK(ex_put_reduction_variable_names(exoid, EX_ASSEMBLY, num_assem_vars, var_names));
    }

    double *var_vals = (double *)calloc(num_assem_vars, (size_t)CPU_word_size);
    for (int ts = 0; ts < 10; ts++) {
      double time_val = (double)(ts + 1) / 100.0;

      EXCHECK(ex_put_time(exoid, ts + 1, &time_val));

      for (int k = 0; k < num_assembly; k++) {
        for (int var_idx = 0; var_idx < num_assem_vars; var_idx++) {
          var_vals[var_idx] = (double)(var_idx + 2) * time_val + k;
        }
        EXCHECK(ex_put_reduction_vars(exoid, ts + 1, EX_ASSEMBLY, assembly[k].id, num_assem_vars,
                                       var_vals));
      }
    }
    free(var_vals);
  }

  EXCHECK(ex_close(exoid));
  printf("Wrote '%s'\n", output_path);
  return EXIT_SUCCESS;
}
