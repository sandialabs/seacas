#!/usr/bin/env python
import sys

import exodus

DATABASE_PATH = "8-block.e"

# Test outputing c-type arrays and numpy arrays
EXO = exodus.exodus(DATABASE_PATH, mode="a", array_type="ctype")
print ("Exodus file has title:", EXO.title())
print ("Exodus file has", EXO.num_dimensions(), "dimensions")
print ("Exodus file has", EXO.num_nodes(), "nodes")
print ("Exodus file has", EXO.num_elems(), "elements")
print ("Exodus file has", EXO.num_blks(), "blocks")
print ("Exodus file has", EXO.num_node_sets(), "node sets")
print ("Exodus file has", EXO.num_side_sets(), "side sets")
print ("Exodus file has", EXO.num_times(), "time steps")

scale = 2.0
offset = 10.0

if EXO.num_times() > 0:
    TIMES = EXO.get_times()
    step = 1
    for time in TIMES:
        new_time = time * scale + offset
        print "Time = {}, New_Time = {}, Step = {}".format(time, new_time, step)
        EXO.put_time(step, time)
        step = step + 1
