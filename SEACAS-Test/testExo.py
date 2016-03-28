#!/usr/bin/env python
import sys
import os
module_dir = os.path.dirname(os.path.realpath(__file__))
module_dir += '/modules'
sys.path.append(module_dir)
import exodus

database_path = "baseline.g"

#Test outputing c-type arrays and numpy arrays
array_types = ['ctype', 'numpy']
for array_type in array_types:
    e = exodus.exodus(database_path, array_type = array_type)
    print "Exodus file has title:", e.title()
    print "Exodus file has", e.num_dimensions(), "dimensions"
    print "Exodus file has", e.num_nodes(), "nodes"
    print "Exodus file has", e.num_elems(), "elements"
    print "Exodus file has", e.num_blks(), "blocks"
    print "Exodus file has", e.num_node_sets(), "node sets"
    print "Exodus file has", e.num_side_sets(), "side sets"
    print "Exodus file has", e.num_times(), "time steps"
    if e.num_times() > 0:
      times = e.get_times()
      for time in times:
        print "time = ", time
        
    blocks = e.get_elem_blk_ids()
    for block in blocks:
      print "block id = ", block
    sidesets = e.get_side_set_ids()
    for sideset in sidesets:
      print "side set id = ", sideset
    nodesets = e.get_node_set_ids()
    for nodeset in nodesets:
      print "node set id = ", nodeset
    coordinates = e.get_coords()
    print "Local Node Id 1 has coordinates:", coordinates[0][0], coordinates[1][0], coordinates[2][0]
    nn = (e.num_nodes() - 1)
    print "Local Node Id", e.num_nodes(), "has coordinates:", coordinates[0][nn], coordinates[1][nn], coordinates[2][nn] 
    print "Side Set Variable Names"
    ssVarNames = e.get_side_set_variable_names()
    for name in ssVarNames:
      print "ssvar = " + name
    print "Side Set Cosa Variable Values"
    step = 1

    if e.num_times() > 0:
      for time in times:
        print "time = ", time
        ssvals = e.get_side_set_variable_values(1,"cosa",step)
        for ssval in ssvals:
          print "value =", ssval
        step += 1

    e.close()

#Test reading in data from exodus database, and then copying it into another database
for array_type in array_types:
    new_database_path = database_path[:-2] + '_' + array_type + '_copy.e'
    exodus.copyTransfer(database_path, new_database_path, array_type = array_type)
    print "Database copied using " + array_type + " arrays."
