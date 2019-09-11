#!/usr/bin/env python
"""
Test routine for SEACAS exodus.py module
"""
import sys
import exodus

DATABASE_PATH = "baseline.g"


# Test outputing c-type arrays and numpy arrays
ARRAY_TYPES = ['ctype', 'numpy']
for array_type in ARRAY_TYPES:
    EXO = exodus.exodus(DATABASE_PATH, array_type=array_type)
    print("Exodus file has title:", EXO.title())
    print("Exodus file has", EXO.num_dimensions(), "dimensions")
    print("Exodus file has", EXO.num_nodes(), "nodes")
    print("Exodus file has", EXO.num_elems(), "elements")
    print("Exodus file has", EXO.num_blks(), "blocks")
    print("Exodus file has", EXO.num_node_sets(), "node sets")
    print("Exodus file has", EXO.num_side_sets(), "side sets")
    print("Exodus file has", EXO.num_times(), "time steps")
    if EXO.num_times() > 0:
        TIMES = EXO.get_times()
        for time in TIMES:
            print("time = ", time)

    BLOCKS = EXO.get_elem_blk_ids()
    for block in BLOCKS:
        name = EXO.get_elem_blk_name(block)
        print("block id = {}, name = {}".format(block, name))

    SIDESETS = EXO.get_side_set_ids()
    for sideset in SIDESETS:
        print("side set id = ", sideset)

    NODESETS = EXO.get_node_set_ids()
    for nodeset in NODESETS:
        print("node set id = ", nodeset)

    COORDINATES = EXO.get_coords()
    print("Local Node Id 1 has COORDINATES: {} {} {}"
          .format(COORDINATES[0][0], COORDINATES[1][0], COORDINATES[2][0]))
    NN = (EXO.num_nodes() - 1)
    print("Local Node Id {} has COORDINATES: {} {} {}"
          .format(EXO.num_nodes(), COORDINATES[0][NN], COORDINATES[1][NN], COORDINATES[2][NN]))
    print("Side Set Variable Names")

    SSVARNAMES = EXO.get_side_set_variable_names()
    for name in SSVARNAMES:
        print("ssvar = ", name)

    print("Side Set Cosa Variable Values")
    step = 1

    if EXO.num_times() > 0:
        for time in TIMES:
            print("time = ", time)
        ssvals = EXO.get_side_set_variable_values(1, "cosa", step)
        for ssval in ssvals:
            print("value =", ssval)
        step += 1

    EXO.close()

# Test reading in data from exodus database, and then copying it into another database
for array_type in ARRAY_TYPES:
    new_DATABASE_PATH = DATABASE_PATH[:-2] + '_' + array_type + '_copy.e'
    exodus.copyTransfer(DATABASE_PATH, new_DATABASE_PATH, array_type=array_type)
    print("Database copied using " + array_type + " arrays.")

# Test the exodus.py `copy` function which calls the C API `ex_copy`
DB_PATH = "base_ioshell.g"
EXO = exodus.exodus(DB_PATH)
NEW_DATABASE_PATH = DB_PATH[:-2] + '_copy.e'
EXO_COPY = EXO.copy(NEW_DATABASE_PATH, True)
if sys.version_info[0] >= 3:
    EXO_COPY.summarize()


print("Exodus file has", EXO_COPY.num_blks(), "blocks")
BLOCKS = EXO_COPY.get_elem_blk_ids()
for block in BLOCKS:
    name = EXO_COPY.get_elem_blk_name(block)
    print("\tblock id = {}, name = {}".format(block, name))

print("Exodus file has", EXO_COPY.num_side_sets(), "side sets")
SIDESETS = EXO_COPY.get_side_set_ids()
for sideset in SIDESETS:
    name = EXO_COPY.get_side_set_name(sideset)
    print("\tside set id = {}, name = {}".format(sideset, name))

print("Exodus file has", EXO_COPY.num_node_sets(), "node sets")
NODESETS = EXO_COPY.get_node_set_ids()
for nodeset in NODESETS:
    name = EXO_COPY.get_node_set_name(nodeset)
    print("\tnode set id = {}, name = {}".format(nodeset, name))

COORDINATES = EXO_COPY.get_coords()
print("Local Node Id 1 has COORDINATES: {} {} {}"
      .format(COORDINATES[0][0], COORDINATES[1][0], COORDINATES[2][0]))
NN = (EXO_COPY.num_nodes() - 1)
print("Local Node Id {} has COORDINATES: {} {} {}"
      .format(EXO_COPY.num_nodes(), COORDINATES[0][NN], COORDINATES[1][NN], COORDINATES[2][NN]))

print("Exodus file has", EXO_COPY.num_times(), "time steps")
if EXO_COPY.num_times() > 0:
    TIMES = EXO_COPY.get_times()
    for time in TIMES:
        print("\ttime = ", time)

    SSVARNAMES = EXO_COPY.get_side_set_variable_names()
    print("Side Set Variable Names:")
    for name in SSVARNAMES:
        print("\tSideSet Variable = ", name)
        step = 2
        ssvals = EXO_COPY.get_side_set_variable_values(2, "SideBlock_2", step)

EXO_COPY.close()
