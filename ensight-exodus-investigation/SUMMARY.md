# Exodus Assemblies, DTIO, and Parallel I/O — Findings and Test Generators

**For:** Ansys/Sandia EnSight Exodus-reader upgrade discussion
**Branch:** `ensight-exodus-seacas-investigation` (off SEACAS `master`, Exodus 9.06)
**Scope:** this directory, `ensight-exodus-investigation/`

This note answers the three questions raised in the prior email thread —
assemblies, dynamic-topology I/O (DTIO), and parallel I/O — and ships three
small, standalone, heavily-commented C/MPI programs that generate real
Exodus files exercising each feature, so an EnSight reader prototype has
something concrete to test against. All three were adapted from existing
SEACAS test/example code, built, run, and verified in this environment
(details and exact commands below). None of them depend on IOSS — only on
the public Exodus C API — which was itself one of the questions asked.

Everything below assumes you're starting from scratch with these files;
treat this as the onboarding doc for whoever picks this up next.

---

## 0. Before you start: building and running anything here

All three programs are plain C, compiled against the Exodus library already
built in this checkout at `/projects/seacas` (install prefix: headers in
`/projects/seacas/include`, libraries in `/projects/seacas/lib`). You do not
need to rebuild SEACAS to use them — just compile each `.c` file directly
against the existing install.

### One-time environment setup

```sh
export LD_LIBRARY_PATH=/projects/seacas/lib:$LD_LIBRARY_PATH
```

Without this, every binary fails immediately with:
```
error while loading shared libraries: libexodus.so.2: cannot open shared object file
```
(The compile commands below also bake in an `-Wl,-rpath,...` so the binaries
work without this export too, but set it anyway — you'll want it for `ncdump`
and other inspection tools.)

### Tools available in this environment for inspecting output

- **`ncdump`** (and `ncgen`, `nccopy`, etc.) — at `/usr/local/seacas/bin/ncdump`.
  Since Exodus files *are* netCDF files, `ncdump -h file.exo` dumps the full
  schema (dimensions/variables/attributes) and `ncdump -v varname file.exo`
  dumps actual data — this was the primary tool used to verify all three
  generators below, and is the fastest way to sanity-check a new run without
  writing more C.
- **`exodiff` / `io_shell`** — searched for in this checkout; **not currently
  built** (`find /projects/seacas -iname exodiff` and `-iname io_shell` both
  return nothing). The plan originally suggested using these for
  verification; they'd need to be built from the `applications/` tree first.
  Everything below was verified without them, using `ncdump` plus each
  generator's own built-in readback/verification pass instead.
- **No `gdb` or `valgrind`** in this environment. For the parallel program,
  AddressSanitizer (`-fsanitize=address`) was tried first but turned out to
  be **incompatible with this environment's OpenMPI** — it segfaults on a
  completely trivial `MPI_Init`/`MPI_Finalize` program with no Exodus
  involved at all. If you need memory-debugging on the parallel program in a
  different environment, try ASan there first on a `hello_mpi.c` before
  trusting it on the real program.

---

## 1. Exodus Assemblies

### 1.1 What they are

`ex_assembly` is Exodus's native grouping concept — a named, typed
collection of entity IDs (element blocks, side sets, node sets, or other
assemblies), optionally nested. It maps naturally onto EnSight's "Groups" UI
concept. The recommendation from the earlier email thread stands: treat
assemblies as organizational metadata mapped to EnSight Groups, without
changing part construction initially.

Key facts for an implementor:
- An assembly's `type` field is homogeneous — all its members must be the
  same `ex_entity_type` (e.g. all `EX_ELEM_BLOCK`, or all `EX_ASSEMBLY` for
  nesting). IOSS enforces this too (`Ioss_Assembly.C`).
- A block (or any entity) can belong to more than one assembly.
- Nesting works by making an assembly's member list contain other
  assemblies' IDs, with `type = EX_ASSEMBLY`.
- Assemblies support the generic entity-oriented APIs: names
  (`ex_get_name`/`ex_put_name`), arbitrary attributes (`ex_*_attribute`),
  and **reduction variables** — per-assembly transient scalars (not
  per-node/per-element), e.g. aggregate momentum or energy over an
  assembly's members.
- **No Fortran bindings exist** for assembly read/write — C/C++ only.
- **No standalone design doc exists** in SEACAS for assemblies — the only
  documentation is source comments on the four core functions
  (`ex_put_assembly(ies)`/`ex_get_assembly(ies)`) and the test programs
  below. Worth flagging to Sandia as a documentation gap if this becomes a
  priority.

### 1.2 The generator: `generate_assembly_testfile.c`

Adapted from `packages/seacas/libraries/exodus/test/testwt-assembly.c`, the
canonical in-repo reference for the assembly write API. Produces a tiny mesh
(7 single-node "sphere" element blocks, IDs 10–16) and a 2-level nested
assembly tree:

```
100 "Root"   (type=EX_ASSEMBLY)   -> members {200, 300, 400}
200 "Child2" (type=EX_ELEM_BLOCK) -> members {10, 11, 12, 13}
300 "Child3" (type=EX_ELEM_BLOCK) -> members {14, 15, 16}
400 "Child4" (type=EX_ELEM_BLOCK) -> members {10, 16}   <- block 10 and 16
                                                             each belong to
                                                             TWO assemblies
```

This single tiny file exercises: nesting (Root's members are themselves
assemblies), the "define now with a NULL entity list, populate it in a
second call later" two-call pattern (Root and Child4 are defined empty, then
filled in), and a block shared across multiple assemblies — the three
structural cases most likely to trip up a first-pass reader implementation.

#### How to build it

```sh
cd /projects/seacas/ensight-exodus-investigation
export LD_LIBRARY_PATH=/projects/seacas/lib:$LD_LIBRARY_PATH
gcc -std=c11 -Wall -Wextra -O2 \
    -I/projects/seacas/include -o generate_assembly_testfile \
    generate_assembly_testfile.c \
    -L/projects/seacas/lib -lexodus -lm -Wl,-rpath,/projects/seacas/lib
```

#### How to run it

```sh
./generate_assembly_testfile --help
```
```
Usage: ./generate_assembly_testfile [options] [output.exo]
  --with-attributes       Attach per-assembly attributes
  --with-reduction-vars   Write assembly reduction variables over 10 steps
  --with-all              Both of the above (full testwt-assembly.c parity)
  -o, --output PATH       Output filename (default: assembly_testfile.exo)
  -h, --help              Print this help and exit
```

Minimal file (just the 4-assembly tree, no attributes/reduction vars):
```sh
./generate_assembly_testfile -o minimal_assembly.exo
```

Full-parity file (matches everything `testwt-assembly.c` writes, including
per-assembly attributes via both the typed `ex_put_double/text/integer_attribute`
calls and the generic `ex_put_attribute` struct API, plus 4 reduction
variables — `Momentum_X/Y/Z`, `Kinetic_Energy` — written across 10 time
steps):
```sh
./generate_assembly_testfile --with-all -o full_assembly.exo
```

Use `--with-attributes` or `--with-reduction-vars` alone to isolate just one
of those two additional code paths if you're debugging a reader's handling
of one in particular.

#### How to inspect/verify the output

Quickest check — dump the schema and look for assembly-related dimensions
and variables (each assembly becomes a netCDF dimension
`num_entity_assembly<id>` and a variable `assembly_entity<id>` carrying
`_id`/`_type`/`_name`/`_typename` attributes plus the member-ID data):
```sh
ncdump -h minimal_assembly.exo
```

Dump one assembly's actual member list:
```sh
ncdump -v assembly_entity100 minimal_assembly.exo   # Root's members: 200 300 400
ncdump -v assembly_entity400 minimal_assembly.exo   # Child4's members: 10 16
```

For a full read-side exercise (not just a schema dump), build and run the
existing SEACAS test program that was specifically written to validate this
exact file shape — it calls the complete read API (`ex_get_init_ext`,
`ex_inquire_int`, `ex_get_ids`, `ex_get_names`, per-assembly and bulk
`ex_get_assembly(ies)`, attribute and reduction-variable readback):
```sh
cd /projects/seacas/packages/seacas/libraries/exodus/test
gcc -std=c11 -I/projects/seacas/include -o testrd-assembly testrd-assembly.c \
    -L/projects/seacas/lib -lexodus -lm -Wl,-rpath,/projects/seacas/lib
# testrd-assembly expects a file named test-assembly.exo in the cwd:
cp /projects/seacas/ensight-exodus-investigation/full_assembly.exo ./test-assembly.exo
./testrd-assembly
```
(Use the `--with-all` output for this, since `testrd-assembly.c` expects the
attributes and reduction variables to be present.)

#### What to adapt if you need a different test case

- Change `num_elem_blk` and the `blocks[]` loop (lines ~142–176) to vary the
  underlying mesh.
- Change the `assembly[]` array and the four `int64_t list_*[]` arrays
  (lines ~189–201) to build a different tree shape — e.g. a 3-level nest, or
  an assembly whose members are side sets/node sets instead of element
  blocks (just change `type` and point `entity_list` at valid IDs of that
  type).
- To test "assembly added after initial file creation" (append-mode), see
  `packages/seacas/libraries/exodus/test/test-add-assembly.c` instead — it
  demonstrates opening an existing file in `EX_WRITE` mode and adding a new
  assembly (`222 "NewAssembly"`, wrapping the original root + its 3
  children) to it. Not wrapped into this generator, but a direct adaptation
  target if that scenario matters for the EnSight writer path too.

---

## 2. DTIO / Dynamic Topology

### 2.1 Headline finding

**"DTIO" is not an Exodus concept — it's IOSS vocabulary layered on top of
Exodus's plain, pre-existing netCDF4-group API.** There is no "dynamic
topology" or "change set" function in the Exodus C library itself. What
IOSS calls a "change set" (`Ioss::ChangeSet`, `CONTROL_AUTO_GROUP_FILE`) is,
at the Exodus level, nothing more than calling `ex_create_group()` to make a
plain netCDF4 sub-group — each group is a fully independent Exodus mesh
namespace (its own `ex_put_init`, blocks, connectivity, node/element counts)
with **no Exodus-level linkage** between groups or to simulation time. All
of that bookkeeping — naming convention, deciding when a new group is
needed, finding which group holds a given time — is an IOSS-layer
convention (`Ioss_DynamicTopology*`, `Ioss_ChangeSet*`,
`Ioss_DynamicTopologyStateLocator`), not an Exodus one.

**Direct answer to "can EnSight access DTIO through Exodus without IOSS":
yes, mechanically** — `ex_create_group()`/`ex_get_group_id(s)` are ordinary
public Exodus calls, nothing IOSS-exclusive. But doing so means EnSight's
reader must reimplement, itself, everything IOSS's orchestration layer
currently provides for free: deciding group-vs-new-file, a naming
convention, and locating which group/file holds a given time. Also worth
flagging: `ENABLE_FILE_GROUPS` (the IOSS property enabling this mode) is
explicitly marked **experimental** in `Ioss_Doxygen.h`, and real IOSS writers
may use *either* the group mechanism *or* a separate-multi-file mechanism —
a writer-side choice EnSight's reader can't control — so a robust reader
likely needs to handle both regardless of which library it's built on.

### 2.2 The generator: `generate_dtio_testfile.c`

Adapted from `packages/seacas/libraries/exodus/test/testwt-groups.c` — the
only pure-Exodus, IOSS-free example of `ex_create_group` in the SEACAS repo
— but deliberately diverges from it in the one way that matters for a DTIO
demonstration: `testwt-groups.c` writes an *identical* mesh into every
group (it only proves the mechanism exists), whereas this program defines a
**genuinely different element-block topology inside each group's own
`ex_put_init()` call** — real topology change between "time steps," built
from nothing but `ex_create_group()`/`ex_put_init()`/`ex_put_coord()`/
`ex_put_conn()`.

Default topology scheme: group *i* (i = 0 .. num_steps−1) contains
`(base_blocks + i)` single-quad element blocks — so by default (3 steps, 2
base blocks) group 0 has 2 blocks/elements, group 1 has 3, group 2 has 4.
`--shrink` reverses this (each step *removes* a block — simulating element
death instead of birth). Each group also records a group-local
`ex_put_time()` value and a human-readable text attribute
(`dtio_step_label` at `EX_GLOBAL` scope) — purely an application-level
convenience, not anything Exodus or IOSS requires.

#### How to build it

```sh
cd /projects/seacas/ensight-exodus-investigation
export LD_LIBRARY_PATH=/projects/seacas/lib:$LD_LIBRARY_PATH
gcc -std=c11 -Wall -Wextra -O2 \
    -I/projects/seacas/include -o generate_dtio_testfile \
    generate_dtio_testfile.c \
    -L/projects/seacas/lib -lexodus -lm -Wl,-rpath,/projects/seacas/lib
```

#### How to run it

```sh
./generate_dtio_testfile --help
```
```
Usage: ./generate_dtio_testfile [options] [output.exo]
  -n, --num-steps N     Number of groups/topology snapshots (default 3)
  -b, --base-blocks N   Element-block count in step 0 (default 2)
  --shrink              Remove a block each step instead of adding one
  -o, --output PATH     Output filename (default: dtio_testfile.exo)
  -h, --help            Print this help and exit
```

Default (3 steps, growing 2→3→4 blocks):
```sh
./generate_dtio_testfile -o growing.exo
```

Longer sequence, more blocks:
```sh
./generate_dtio_testfile -n 6 -b 3 -o growing_long.exo    # 3,4,5,6,7,8 blocks
```

Shrinking sequence (element death — note `base_blocks` must be `>= num_steps`):
```sh
./generate_dtio_testfile -n 4 -b 6 --shrink -o shrinking.exo   # 6,5,4,3 blocks
```

The program **self-verifies on every run** — after writing, it reopens the
root file and walks the group tree exactly the way IOSS/exodiff/aprepro do
(`EX_INQ_NUM_CHILD_GROUPS` + `ex_get_group_ids` + `EX_INQ_GROUP_NAME`),
re-reading each group's topology and printing a summary line per group, e.g.:
```
Group 'DTIO_STEP-0000' (exoid 65536): num_elem_blk=2 num_nodes=8 num_elem=2 time=0 label='dtio step 0, t=0, 2 blocks'
Group 'DTIO_STEP-0001' (exoid 65537): num_elem_blk=3 num_nodes=12 num_elem=3 time=1 label='dtio step 1, t=1, 3 blocks'
Group 'DTIO_STEP-0002' (exoid 65538): num_elem_blk=4 num_nodes=16 num_elem=4 time=2 label='dtio step 2, t=2, 4 blocks'
=== Verification complete: 3 groups confirmed structurally distinct ===
```
A non-growing block count across groups, or a verification failure, means
something regressed — check this output first before reaching for `ncdump`.

#### How to inspect/verify the output further

```sh
ncdump -h growing.exo | less
```
Look for top-level child groups (printed as `group: DTIO_STEP-0000 {`,
`group: DTIO_STEP-0001 {`, etc. — each with its *own* complete
`dimensions:`/`variables:` block, confirming they're independent
namespaces, not shared arrays).

To confirm there's genuinely no Exodus-level linkage between groups (the
headline finding above), check that no group references another by
name/path anywhere in its attributes — `ncdump -h` on any individual
group's section will show it's a completely self-contained Exodus file
structure with no pointer back to its siblings. The *only* place the
sequence is recorded is the group *names themselves*
(`DTIO_STEP-0000`, `-0001`, ...) and each group's own `ex_put_time` value —
both pure application convention.

#### What to adapt if you need a different test case

- The per-group mesh is built in `write_topology_snapshot()` (lines
  ~105–157) — currently N disjoint single-quad blocks offset along X. Swap
  in a more realistic mesh (shared nodes between blocks, non-quad element
  types, multiple blocks changing simultaneously, etc.) by editing this one
  function; the group-creation driver loop in `main()` doesn't need to
  change.
- To exercise IOSS's actual `Ioss::ChangeSet`/`DynamicTopologyObserver`
  machinery on top of this same raw-group mechanism (useful for diffing
  "what IOSS adds" against this pure-Exodus baseline), see
  `packages/seacas/libraries/ioss/src/unit_tests/UnitTestDynamicTopology.C`
  — specifically the `TestDynamicWrite.single_file_simple_topology_modification`
  test and the lower-level `create_subgroup_with_file_reopen` tests. This
  requires building IOSS (not just Exodus) and was flagged as a stretch
  goal, not yet attempted.

---

## 3. Parallel I/O: reading/writing a single Exodus file across MPI ranks without IOSS

### 3.1 Headline finding

**Yes — EnSight could plug its own partitioning infrastructure directly
into Exodus's parallel I/O primitives, without IOSS at all.** Exodus has a
complete, self-sufficient MPI-based parallel I/O API
(`ex_create_par`/`ex_open_par`, built on NetCDF-C's parallel dispatch to
HDF5-parallel and/or PnetCDF) plus a family of `ex_{get,put}_partial_*`
functions letting each rank read/write only its own `start`/`count`
hyperslab of a shared on-disk array. IOSS's own parallel backend
(`Ioex_ParallelDatabaseIO.C`) is demonstrably nothing more than a client of
this exact same public API — it just supplies a Zoltan/ParMETIS-computed
decomposition instead of whatever decomposition the calling application
already has. There is no private, IOSS-only Exodus entry point.

**Caveat confirmed by this environment:** file-per-rank ("Nemesis-style")
remains SEACAS's dominant real-world pattern — the shared-single-file path
this generator demonstrates is real and working but comparatively
less-traveled in the broader tool ecosystem. A robust EnSight parallel
reader likely needs to support both regardless of which mechanism it
chooses for its own output.

### 3.2 The generator: `parallel_rw_testfile.c`

An MPI program where N ranks collectively open **one shared Exodus file**
via `ex_create_par`, then each rank writes **only its own non-overlapping
slice** of a single connected mesh via `ex_put_partial_coord`/
`ex_put_partial_conn` — no file-per-rank, no IOSS, no Zoltan/ParMETIS. This
is the one piece of this investigation that doesn't exist anywhere else in
SEACAS even as a test: the closest existing examples
(`exodus/test/testrd_par.c`, `testwt-partial.c`) demonstrate the API
primitives but not an actual N-ranks-each-own-a-slice decomposition.

**Mesh**: a 1-D chain of `total_elems` quad4 elements. Element *e* (0-based)
connects 1-based nodes `{2e+1, 2e+2, 2e+3, 2e+4}`, so consecutive elements
share one edge (2 nodes) with their neighbor — an ordinary connected strip
mesh. `total_nodes = 2*total_elems + 2`.

**Decomposition**: simple static block decomposition (no Zoltan/ParMETIS) —
`total_elems` split into `nprocs` contiguous ranges, with the first
`(total_elems % nprocs)` ranks getting one extra element each. Node
ownership is derived directly from each rank's element range (see the
detailed comment in `main()` for the exact boundary-node arithmetic — this
took two rounds of debugging to get right; see §3.4 below).

**Two significant, genuinely new findings came out of actually building and
running this** (not just reading code) — both are documented in detail in
the program's own comments, and summarized in §3.4 below, because they're
directly relevant to anyone else attempting the same thing:
1. `EX_ALL_INT64_API` is **required** when passing `int64_t*` buffers to any
   `void_int*` Exodus parameter (connectivity, maps, IDs) — without it,
   Exodus silently treats those buffers as 32-bit `int*` and corrupts the
   data. This is easy to get wrong and the failure mode (garbled,
   interleaved-looking data) doesn't look like an obvious type mismatch at
   first glance.
2. The **singular, per-rank** `ex_put_loadbal_param()` cannot safely be
   called once per rank on a shared `ex_create_par`'d file — it races to
   define global dimensions at each rank's own local (wrong) size. The
   **concatenated** `ex_put_loadbal_param_cc()`, called once collectively
   by all ranks with `MPI_Allgather`'d full-length vectors, is the fix.

#### How to build it

Needs `mpicc`, confirmed available in this environment (wraps `gcc 15.2.0`
via OpenMPI):
```sh
cd /projects/seacas/ensight-exodus-investigation
export LD_LIBRARY_PATH=/projects/seacas/lib:$LD_LIBRARY_PATH
mpicc -std=c11 -Wall -Wextra -O2 \
    -I/projects/seacas/include -o parallel_rw_testfile \
    parallel_rw_testfile.c \
    -L/projects/seacas/lib -lexodus -lm -Wl,-rpath,/projects/seacas/lib
```
Requires an MPI-enabled Exodus build with HDF5-parallel and/or PnetCDF
support (`PARALLEL_AWARE_EXODUS` / NetCDF's `NC_HAS_PARALLEL`) — confirmed
present in this checkout's TPL install
(`NC_HAS_HDF5=1`, `NC_HAS_PNETCDF=1`, `NC_HAS_PARALLEL4=1`,
`NC_HAS_PARALLEL=1` in `/projects/seacas/include/netcdf_meta.h`). If you
move this to a different Exodus install, check that header first — if
those are all `0`, `ex_create_par` will fail immediately with an error
pointing at exactly this.

**Do NOT compile with `-fsanitize=address`** in this environment — ASan is
incompatible with the installed OpenMPI here and will crash on
`MPI_Init`/`MPI_Finalize` alone, with or without Exodus involved. This was
confirmed directly (see §0 above) before concluding it wasn't a bug in this
program.

#### How to run it

```sh
./parallel_rw_testfile --help
```
```
Usage: mpirun -n <nprocs> ./parallel_rw_testfile [options] [output.exo]
  -e, --num-elems N   Total element count across all ranks (default 12)
  --with-loadbal       Also persist partition via Nemesis loadbal/processor-map API
  -o, --output PATH    Output filename (default: parallel_testfile.exo)
  -h, --help           Print this help and exit
```

Must be run under `mpirun`/`mpiexec` — `total_elems` must be `>=` the
number of ranks (every rank needs at least one element).

Basic 2-rank run, 12 elements (the default), written to the default
filename:
```sh
mpirun -n 2 ./parallel_rw_testfile
```
(If running as root in a container, OpenMPI needs `--allow-run-as-root`:
`mpirun --allow-run-as-root -n 2 ./parallel_rw_testfile`.)

Specific element count and output path:
```sh
mpirun --allow-run-as-root -n 4 ./parallel_rw_testfile -e 100 -o par_100elem_4rank.exo
```

With the Nemesis decomposition metadata also persisted (processor maps,
load-balance params — see the fix described in §3.4):
```sh
mpirun --allow-run-as-root -n 4 ./parallel_rw_testfile -e 100 --with-loadbal -o par_100elem_4rank_lb.exo
```

Uneven split (7 elements across 4 ranks — exercises the "first N ranks get
one extra element" code path in `block_decompose()`):
```sh
mpirun --allow-run-as-root -n 4 ./parallel_rw_testfile -e 7 -o par_uneven.exo
```

Boundary case (elements exactly equal to rank count — each rank gets
exactly 1 element):
```sh
mpirun --allow-run-as-root -n 4 ./parallel_rw_testfile -e 4 -o par_boundary.exo
```

**Every run self-verifies**: after all ranks finish their partial writes and
close the file, rank 0 alone reopens it with a **plain serial** `ex_open()`
(no `_par` suffix, no MPI awareness at all) and spot-checks every node
coordinate and every element's connectivity against the expected pattern,
printing:
```
=== Serial verification of 'par_100elem_4rank.exo' (plain ex_open, no MPI) ===
title='Parallel chain mesh: 100 elements, 4 MPI ranks' num_nodes=202 num_elem=100 num_elem_blk=1
Spot-check passed: all 202 node x-coordinates match node index exactly --
no gaps/overlaps across the 0-rank partial writes.
Spot-check passed: all 100 element connectivities match the expected chain pattern.
=== Serial verification PASSED ===
```
A `VERIFY ERROR` or `=== Serial verification FAILED ===` here means
something about the decomposition or partial writes is wrong — this is
the single most useful thing to check first. The whole MPI job's exit code
reflects this result (propagated to all ranks via `MPI_Bcast`), so
`mpirun ...; echo $?` is a reliable pass/fail signal for scripting.

#### How to inspect/verify the output further

```sh
ncdump -h par_100elem_4rank.exo
```
Confirm `num_nodes`/`num_elem` match the global totals you expect, and that
`:int64_status` reflects `EX_ALL_INT64_API` having been set (nonzero).

Dump actual coordinate/connectivity data to manually confirm no gaps/overlaps:
```sh
ncdump -v coordx,connect1 par_100elem_4rank.exo | less
```

With `--with-loadbal`, inspect the decomposition metadata directly:
```sh
ncdump -v elem_mapi,node_mapi,elem_mapi_idx,node_mapi_idx,int_e_stat,int_n_stat par_100elem_4rank_lb.exo
```
`elem_mapi`/`node_mapi` should hold the complete, correctly-ordered global
1-based ID lists (length = total element/node count, **not** any single
rank's count); `elem_mapi_idx`/`node_mapi_idx` should hold `nprocs` strictly
increasing values marking each rank's cumulative boundary (e.g. for 3 ranks
owning 4/3/3 elements: `elem_mapi_idx = 4, 7, 10`).

To confirm the shared file is valid completely outside this program (not
just self-consistent with its own verification pass), write a trivial
standalone serial reader — or just reuse the pattern in `verify_serial()`
(lines ~149–245) as a template; it's deliberately written as plain,
ordinary, non-parallel-aware Exodus C (`ex_open`, `ex_get_coord`,
`ex_get_conn`) that would work identically on a file produced by any other
tool.

#### What to adapt if you need a different test case

- **Different mesh topology**: `block_decompose()` (lines ~131–144) and the
  node-ownership derivation in `main()` (search for "Node ownership derives
  directly from the element range") are the two places that encode the
  chain-mesh assumption. If you want a 2-D or 3-D decomposition instead of a
  1-D chain, both would need rework — the current scheme deliberately
  trades mesh realism for an easy-to-verify, easy-to-read reference
  implementation of the *mechanism*, not a realistic decomposition
  algorithm.
- **Ghost/shared nodes between ranks** (border/external nodes, in Nemesis
  terminology): this program's partition has zero border/external
  nodes/elements and zero communication maps by construction (every
  node/element is "internal" to exactly one rank) — chosen specifically to
  avoid needing that bookkeeping for a first example. If EnSight's own
  real decompositions have ghost zones, extending the `--with-loadbal`
  block (search for "num_bor_nodes") to pass nonzero border/external counts
  and populate `ex_put_processor_node_maps`'s `node_mapb`/`node_mape`
  arguments (currently always `NULL`) is the next logical step — and would
  be new, unexplored territory in this investigation (not verified here).
- **PnetCDF instead of HDF5-parallel**: the file is currently created with
  `EX_NETCDF4 | EX_NOCLASSIC` (HDF5-parallel). `EX_64BIT_DATA | EX_PNETCDF`
  is the alternative mode flag combination for PnetCDF-backed parallel I/O
  — not exercised by this program; see `ex_create_par`'s doc comment and
  `exodus/include/Exodus-Formats.md` for the flag semantics if you need to
  compare both backends' behavior (the collective-access rules differ
  slightly — HDF5-parallel sets `NC_COLLECTIVE` per-variable;
  PnetCDF-backed files use a file-wide collective/independent setting).

### 3.4 Deep-dive: the two bugs found while building this (worth reading if extending this program)

These are documented in detail in the source comments (search for "FIX" and
"CAVEAT" in `parallel_rw_testfile.c`), but are summarized here because
they're the kind of thing that will bite anyone else writing similar code
against this API for the first time:

1. **`EX_ALL_INT64_API` is required for `int64_t*` buffers.** All the
   `void_int*`-typed Exodus parameters (connectivity, block/map IDs,
   processor maps) default to expecting plain 32-bit `int*` unless the file
   is created/opened with the `EX_ALL_INT64_API` mode flag. This program
   uses `int64_t` arrays throughout (for consistency with `ex_entity_id`
   and to avoid overflow on large meshes), so this flag must be OR'd into
   both the `ex_create_par()` mode argument *and* the plain `ex_open()` mode
   argument used by the serial verification pass — missing it on either
   side corrupts/misreads the data. The failure signature if you forget
   this is subtle: no error is returned, but `ncdump` shows garbled,
   seemingly-doubled, zero-interleaved data (each 8-byte `int64_t` written
   gets reinterpreted as two 4-byte `int`s). If you ever see data that
   looks "duplicated with zeros inserted," check this first.

2. **`ex_put_loadbal_param()` (singular/per-rank) is incompatible with the
   `ex_create_par` shared-file model; use `ex_put_loadbal_param_cc()`
   (concatenated/collective) instead.** Confirmed empirically: calling the
   per-rank form once per rank — each rank passing only its own local
   counts — causes every rank to race-define the same global dimension
   (`num_int_elem`, `num_int_node`) at its own, locally-sized (and
   therefore wrong) value on this collectively-accessed HDF5-parallel file;
   only one rank's definition survives, and the subsequent per-rank
   `ex_put_processor_{node,elem}_maps()` calls clobber each other's data in
   the shared `elem_mapi`/`node_mapi` variables instead of appending to
   them. Root cause, found by reading the source: the per-rank
   `ex_put_loadbal_param()` never defines the `VAR_*_IDX` index variables
   (e.g. `node_mapi_idx`) that `ex_put_processor_*_maps()` relies on (via
   `ex_get_idx()`) to find the correct offset to append its data at —
   confirmed by grepping `ex_put_loadbal_param.c` for `"_IDX"` (zero
   matches) versus `ex_put_loadbal_param_cc.c` (defines all seven). The fix
   is to call `ex_put_loadbal_param_cc()` exactly once, collectively, with
   every rank passing *identical* full-length vectors — one entry per
   processor, gathered via `MPI_Allgather` — mirroring exactly how
   `ex_put_init()` must already be called identically by every rank
   earlier in the same program. This is a genuinely new finding, not
   previously documented anywhere in SEACAS as far as could be determined
   in this investigation (grepping the whole repo for prior combination of
   these two APIs turned up nothing) — likely useful to flag to Sandia
   directly, since it's exactly the kind of gotcha that would otherwise
   cost someone else a debugging session.

Both bugs were caught by actually running the program under `mpirun` with
multiple ranks and inspecting the output with `ncdump`, not by code review
alone — reinforcing that `ncdump`-based inspection (§0) is the right first
diagnostic step for anyone extending this generator, well before reaching
for heavier tools.

---

## 4. Recommendations for the next steps with Ansys/Sandia

1. **Assemblies** (§1): the `ex_assembly` API surface is small, stable, and
   well-suited to EnSight's Group concept. Point Ansys at
   `testwt-assembly.c`/`testrd-assembly.c` (and this directory's
   `generate_assembly_testfile.c`) as the reference examples. Flag the
   documentation gap (§1.1) — a short design note from Sandia may be worth
   requesting before an EnSight integration starts, since right now the
   only "spec" is source comments.

2. **DTIO** (§2): there is no dedicated Exodus API — it's IOSS vocabulary
   over Exodus's pre-existing, **experimental**-flagged netCDF4-group
   feature. The real tradeoff for EnSight is not "Exodus vs. IOSS" at the
   byte level (both ultimately call `ex_create_group`) but whether EnSight
   reimplements group-naming/timestep-location logic itself, or depends on
   IOSS's (experimental) `ChangeSet`/`DynamicTopologyStateLocator` classes.
   Either way, a robust reader needs to handle *both* the group mechanism
   and the separate-multi-file mechanism, since that's a writer-side choice
   outside the reader's control. Confirm current production stability of
   `ENABLE_FILE_GROUPS` with the SEACAS team before committing to it as
   EnSight's DTIO mechanism.

3. **Parallel I/O** (§3): **yes**, EnSight can plug its own partitioning
   directly into Exodus's `ex_create_par`/`ex_{get,put}_partial_*` API,
   without IOSS, exactly where IOSS's own Zoltan/ParMETIS decomposition
   currently plugs in — demonstrated concretely by `parallel_rw_testfile.c`
   in this directory, including two previously-undocumented gotchas (§3.4)
   now written up for anyone else attempting this. Caveat: file-per-rank
   remains the dominant real-world SEACAS pattern, so a production EnSight
   parallel reader likely still needs to support reading that format too.

4. **Suggested handoff artifact for Ansys**: this directory (three `.c`
   files + this `SUMMARY.md`) as-is. Each program is self-contained,
   dependency-light (one needs only `libexodus` + libc; the parallel one
   additionally needs MPI), under 530 lines, and includes both inline
   design-rationale comments and a self-verification pass — intended to be
   readable and runnable by someone on Ansys's side with no additional
   context beyond this document.

---

## Appendix: file inventory

| File | Lines | Depends on | Purpose |
|---|---|---|---|
| `generate_assembly_testfile.c` | 276 | libexodus | §1 — nested assembly tree generator |
| `generate_dtio_testfile.c` | 328 | libexodus | §2 — netCDF4-group topology-change generator |
| `parallel_rw_testfile.c` | 527 | libexodus (MPI/parallel-aware build), MPI | §3 — shared-file multi-rank partial I/O generator |
| `SUMMARY.md` | this file | — | this write-up |

All three were built and run to completion (including the parallel one
under `mpirun -n 1,2,3,4,5` with various element counts and both with/without
`--with-loadbal`) in this environment as part of producing this document;
see each program's own `--help` output and source comments for anything not
covered above.
