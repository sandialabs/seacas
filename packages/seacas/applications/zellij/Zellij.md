# Zellij

Zellij is a "mesh concatenation" application for generating a mesh
consisting of a "lattice" containing one or more "unit cell" template
meshes.  The lattice is a two-dimensional arrangement of the unit cell
template meshes.

The unit cell template meshes are placed by zellij into the specified
locations in the lattice and the nodes on the boundaries of the unit
cell meshes are united or coincident.  Each unit cell mesh must have
the same exterior boundary meshes and coordinate extents on the X and
Y coordinate faces, but the Z faces are only required to have the same
coordinate extent; the Z face meshes are not required to be the same
among the different unit cells.

The lattice can be represented as a IxJ regular grid with each "cell"
in the grid or lattice containing one of the unit cell template
meshes.

* [Execution](#execution)
* [Lattice Description File Format](#Lattice Description File Format)
  * [Unit Cell Dictionary](#Unit Cell Dictionary)
  * [Unit Cell Template Mesh Requirements](#Unit Cell Template Mesh Requirements)
* [Execution Complexity](#Execution Complexity)
  * [Memory Complexity](#Memory Complexity)
  * [Execution Time Complexity](#Execution Time Complexity)
  * [Efficiency at the NetCDF level](#Efficiency at the NetCDF level)
  * [Format](#Format)
  * [Integer Size](#Integer Size)
  * [Compression](#Compression)
  * [Recommendations](#Recommendations)
* [TODO](#TODO)


## Execution

Executing zellij with the `-help` option will result in output similar to the following:

```
zellij -help

Zellij
	(A code for tiling 1 or more template databases into a single output database.)
	(Version: 0.0.6) Modified: 2021/01/14

zellij [options] -lattice <lattice_definition_file>
	-help (Print this summary and exit)
	-version (Print version and exit)
	-lattice <$val> (Name of file to read lattice definition from.)
	-output <$val> (Name of output file to create)
	-netcdf4 (Output database will be a netcdf4 hdf5-based file instead of the classical netcdf file format)
	-netcdf5 (Output database will be a netcdf5 (CDF5) file instead of the classical netcdf file format)
	-32-bit (True if forcing the use of 32-bit integers for the output file)
	-64-bit (True if forcing the use of 64-bit integers for the output file (default))
	-zlib (Use the Zlib / libz compression method if compression is enabled (default) [exodus only].)
	-szip (Use SZip compression. [exodus only, enables netcdf-4])
	-compress <$val> (Specify the hdf5 zlib compression level [0..9] or szip [even, 4..32] to be used on the output file.)
	-debug <$val> (debug level (values are or'd)
		  1 = time stamp information.
		  2 = memory information.
		  4 = Verbose Unit Cell information.
		  8 = put exodus library into verbose mode.
)
	-copyright (Show copyright and license data.)

	Can also set options via ZELLIJ_OPTIONS environment variable.

	->->-> Send email to gdsjaar@sandia.gov for zellij support.<-<-<-
```
The only required option is `-lattice` followed by the name of the file containing the lattice description.  The other options are used to specify compression of the output file; the format of the output file; or to request additional debug ouput.

## Lattice Description File Format
The format of the lattice description file is fairly simple, but is also very rigid.  There are two sections of the file -- the _unit cell_ dictionary and the _lattice_ definition.

### Unit Cell Dictionary
The unit cell dictionary defines the unit cell template meshes that will be placed in the lattice.  The dictionary begins with a line containing `BEGIN_DICTIONARY` followed by one or more lines defining the unit cells and is then ended with a line containing `END_DICTIONARY`

The syntax of the lines defining the unit cells consists of two fields -- an arbitrary _key_ and the filename containing the Exodus file defining the mesh for this unit cell.  The only restriction on the _key_ is that it must be unique in the dictionary.  The filenames must specify the path (either absolute or relative to the current execution directory) to the Exodus file; it can optionally be delimited by double quotes (`"`).  The filenames do not need to be unique, but it is more efficient in both memory and time if each unit cell template mesh is unique.

As an example, here is a valid dictionary definition:

```
BEGIN_DICTIONARY
  0001 "../zellij-example/xatom-1b.e"
  0002 "../zellij-example/xatom-Y.e"
  0003 "../zellij-example/xatom-X.e"
  0004 "../zellij-example/xatom-2b.e"
END_DICTIONARY
```
The unit cell dictionary must appear before the lattice definition in the lattice description file.

If an error is detected during the parsing of the unit cell dictionary, the code will ouptut an error message and terminate.  Errors can be incorrect syntax, missing unit cell template meshes, duplicate keys, or problems reading the mesh description from a unit cell template mesh.  The unit cell template mesh file is accessed and partially read at the time that zellij parses the corresponding unit cell dictionary line.

### Lattice Definition

The lattice definition specifies the size of the lattice and the distribution of the unit cell(s) within that lattice.  The lattice definition must follow the unit cell dictionary in the lattice description file.

The first line of the lattice definition begins with the line `BEGIN_LATTICE {i} {j} 1` where `{i}` and `{j}` specify the size of the `IxJ` arrangement of unit cells.  For example, the line `BEGIN_LATTICE 5 5 1` would define a lattice containing 25 unit cell instances arranged in a 5 by 5 regular grid.

The last line of the lattice definition is the line `END_LATTICE`.  When that line is encountered, zellij will begin outputting the mesh.

Between the `BEGIN_LATTICE` and `END_LATTICE` are `{j}` lines with `{i}` entries per line.  The entries are any of the _key_s that were specified in the unit cell dictionary.  

As an example, here is a valid lattice defintion using the keys of the example dictionary from the previous section:

```
BEGIN_LATTICE 5  5  1
  0001 0002 0003 0002 0001
  0002 0003 0003 0003 0002
  0003 0003 0004 0003 0003
  0002 0003 0003 0003 0002
  0001 0002 0003 0002 0001
END_LATTICE
```

Although the lattice is typically symmetric and square, this is not a requirement and is not checked.

If an error is detected during the parsing of the _lattice_, the code will output an error message and terminate.  Errors can include invalid keys, incorrect number of lattice definition lines, or incorrect number of keys on a definition line.

Note that zellij does not require that the unit cell keys be numeric; the following example shows a different method for specifying the same lattice definiton file as the previous example:

```
BEGIN_DICTIONARY
  - "../zellij-example/xatom-1b.e"
  | "../zellij-example/xatom-Y.e"
  + "../zellij-example/xatom-X.e"
  * "../zellij-example/xatom-2b.e"
END_DICTIONARY

BEGIN_LATTICE 5  5  1
  - | + | -
  | + + + |
  + + * + +
  | + + + |
  - | + | -
END_LATTICE
```

## Unit Cell Template Mesh Requirements
Zellij requires that the boundary mesh (`X` and `Y` faces) of each of the unit cell templates be a _regular_ "structured" mesh.  Basically this means that the faces of the mesh elements on the boundary are in a regular rectangular grid such that each mesh face is rectangular (90 degree corners) and that the boundary mesh on the minimum `X` face is the same as that on the maximum `X` face and similarly for the minimum `Y` face and the maximum `Y` face.

Additionally, the X faces on *all* unit cells must match and the Y faces on *all* 
unit cells must match both in structure and in coordinate extent. This requirement is verified during execution. The `Z` faces are less constrained with the only requirement being that the coordinate extents of all `Z` faces must be the same (which follows from the `X` and `Y` face requirement); the structure of the mesh on the `Z` faces is arbitrary.

The unit cell meshes can contain any number of element blocks; however, each element block *must* contain hexahedral elements with 8-nodes per element.  The element blocks do not need to be the same in each unit cell mesh, but if they do share the same element block `id`, then those elements will be combined into the same element block in the output mesh with the same `id`.  

The output mesh will contain the union of all element blocks existing on the input mesh unit cells.  For example, if:

* unit cell `0001` has element blocks `1 10 100`
* unit cell `0002` has element blocks `2 20 200`
* unit cell `0003` has element blocks `1 2 10 20`
* unit cell `0004` has element blocks `10 20 100 200`

The output mesh will have element blocks `1 2 10 20 100 200`

## Execution Complexity

Zellij is intended to produce extremely large meshes and is therefore very concerned with both memory efficiency and execution time efficiency.

### Memory Complexity

Zellij stores the following data:

* For each unit cell template mesh:
  * metadata
  * 64-bit Ids of nodes on each min_I, max_I, min_J, max_J face
* For each entry in the lattice definition:
  * metadata (approximately 1KiByte)
  * temporarily it will hold 64-bit Ids of nodes on the max_I and max_J faces. This will be deleted once the upper `I` and upper `J` "neighbor" entry has been processed (see below)
* For the lattice:
  * vector containing the lattice definition.

The main memory use once the output file is being processed is the temporary storage containing the nodes on the `max_I` and `max_J` faces.  The lattice is processed cell by cell.  For an `II by JJ` sized grid, the cells are processed in the order `(1,1), (2,1), ... , (II, 1), (1,2), (2,2), ..., (II, JJ)`.  The temporary storage on the `max_I` face is only needed until the next cell is processed.  That is, for cell `(i,j)`, its `max_I` nodes will be used during the processing of cell `(i+1, j)` and then deleted.

The temporary storage on the `max_J` face is retained for a longer time.  For cell `(i,j)`, the `max_J` storage is needed for cell `(i, j+1)` and then deleted.

For a grid of size `(II, JJ)`, there will at most be:

* 1 temporary vector of size `max_I` nodes
* `II` temporary vectors of size `max_J` nodes.

If you have a lattice that is rectangular (`II != JJ`), then it is more efficient for memory usage to make the `I` direction the smallest value if possible.

In addition to the above memory usage, zellij must also transfer the mesh coordinate data and element block connectivity data for each lattice entry to the output file.  Zellij outputs the model using the following pseudo-code:

```
for each j : J
  for each i : I
     read cell(i,j)  x, y, and z local coordinates
     map coordinates to offset in output mesh
     eliminate nodes that join to an already output neighbor cell
     write cell(i,j) x, y, and z global coordinates

for each j : J
   for each i : I
      for each element block in cell(i,j) mesh
         read block connectivity
         map local node ids to global node ids
         write block connectivity 
```

The maximum memory use will be the size of storage needed for the `x` `y` and `z` coordinates of a unit cell mesh or the storage needed to hold the connectivity for a single unit cell element block.

Note that the memory requirements are proportional to the size of an individual unit cell mesh and not a function of the size of the output mesh.  It is possible to create meshes which are much larger than the amount of memory present on the compute system running zellij.

The memory being used by zellij during execution will be output if the `--debug 2` argument is specified at execution time.

### Execution Time Complexity

For a large model, the majority of the execution time is related to:

* Read/process/write element block connectivity
* Read/process/write nodal coordinates
* Categorize boundary nodes on each unit cell mesh

### Efficiency at the NetCDF level
The Exodus format which is used for the unit cell template meshes and the output mesh uses the NetCDF library for on-disk storage.  There are several variants of the NetCDF on-disk storage including the format: `netcdf3`, `netcdf4`, and `netcdf5` and the integer size (32-bit integers or 64-bit integers).  Although these details are usually transparent to the user, they can affect the execution time especially when very large meshes are being processed.

#### Format
The `netcdf3` format is the original native NetCDF format.  At the time the library was being developed, the `byte endianess` of data stored on disk was not standard among the computes in use at that time and the NetCDF developers had to pick an `endianess` for the data.  They picked the XDR standard which stood for _eXternal Data Representation_ which was used for communicating between different computer systems.  Regretfully, the representation used by XDR turned out to be opposite of the representation used by (almost?) all systems in use today, so each read and write of data in the `netcdf3` format results in a translation of the endianess.  This translation is very fast, but is overhead that would not be needed if the on-disk format was the opposite representation.  This representation is also used by the `netcdf5` format.  

However, the NetCDF `netcdf4` format is based on using the HDF5 library to manage the underlying data format on disk and it can read and write data using the native endianess of the system on which the data is being read and written and therefore does not incur the cost of transforming the data's endianess.

#### Integer Size
By default, most current mesh generators will output a mesh using 32-bit integer data.  This is sufficient to represent a mesh with up to approximately 2.1 billion nodes and elements.  

If the input mesh and the output mesh have the same integer size, then there is no data conversion needed.  The data will be read as `N`-bit integers, processed as `N`-bit integers, and written as `N`-bit integers.  However, if the input mesh is `N`-bit integers and the output mesh is `M`-bit integers, then the NetCDF library will convert all integer data (element block connectivity typically) from `N` bits to `M` bits which for large meshes can incur an execution time overhead.

#### Compression
The NetCDF library supports compression of the output file.  Typically, the `zlib` compression algorithm is used, but recently NetCDF begain supporting the `szip` compression and a few more algorithms are starting to be supported.

The benefit of the compression is that it can result in much smaller output (and input) mesh files; the disadvantage is that the default `zlib` compression algorithm is not very fast and can increase the execution time of zellij.  The `szip` compression algorithm is faster with typically (but not always) slightly less compression, but it still will incur an overhead in execution time.

#### Recommendations
For minimal overhead, it is recommended that:

* Use the `netcdf4` format for all input and output meshes
* Use the same integer size for all input and output meshes
  * The integer size of the output mesh can be specified using the `-32` or `-64` options.  
  * The `-64` option is the default.

It is most efficient if the format and integer size of the input mesh matches the output mesh.  The format of the input meshes can be converted using the `io_shell` application with the `-netcdf4` and `-64` or `-32` options.
         
For illustration, here is the execution time for several runs with different format and integer size.  In all cases, the input and output mesh sizes are the same:

| input   | output  | integer input | integer output | execution time |
|:-------:|:-------:|:-------------:|:--------------:|:--------------:|
| netcdf3 | netcdf3 |       32      |       32       |  7.0           |
| netcdf3 | netcdf4 |       32      |       32       |  2.6           |
| netcdf3 | netcdf4 |       32      |       64       |  3.8           |
| netcdf4 | netcdf3 |       32      |       32       |  6.5           |
| netcdf4 | netcdf3 |       64      |       32       |  7.4           |
| netcdf4 | netcdf5 |       64      |       64       |  9.4           |
| netcdf4 | netcdf4 |       32      |       32       |  2.4           |
| netcdf4 | netcdf4 |       32      |       64       |  3.6           |
| netcdf4 | netcdf4 |       64      |       32       |  3.2           |
| netcdf4 | netcdf4 |       64      |       64       |  3.3           |

The fastest option is both input and output using 32-bit integers and the `netcdf4` format.  Almost as fast is the case where the input format is `netcdf3` and the output `netcdf4`.  The `64-bit` integer options with both input and output using `netcdf4` are slightly slower, but this is probably due to the doubling of the size of the integer data being read and written.

The output mesh in this case consisted of 37.3 million elements and 38.5 million nodes in a grid of 46 x 46 unit cells.  There were 56 unit cell template meshes.

## TODO

* Parallel Support
  * Serial Execution with Parallel file output
  * Parallel Execution with Parallel file output