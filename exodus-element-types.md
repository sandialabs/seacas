* [Element Types](#element-types)
* [Exodus Node Ordering Convention](#exodus-node-ordering-convention)
* [Circle and Sphere Element Types](#circle-and-sphere-element-types)
* [Bar, Truss, Beam Element Types](#bar-truss-beam-element-types)
* [Circle and Sphere Element Types](#circle-and-sphere-element-types)
* [Bar, Truss, Beam Element Types](#bar-truss-beam-element-types)
* [Triangular Element Types](#triangular-element-types)
* [Quadrilateral Element Types](#quadrilateral-element-types)
* [Tetrahedral Element Types](#tetrahedral-element-types)
* [Pyramidal Element Types](#pyramidal-element-types)
* [Wedge Element Types](#wedge-element-types)
* [Hexahedral Element Types](#hexahedral-element-types)

# Element Types
The following table shows the element types supported in the Exodus
library, the IOSS library, nem_slice, and nem_spread.

 Element|Exodus| IOSS  | nem_slice | nem_spread |
 -------|------|-------|-----------|------------|
 circle |  X   |    X  |    X      |  X |
 sphere |  X   |    X  |    X      |  X |
        |      |       |           |    |
 beam2  |  X   |    X  |    X      |  X |
 beam3  |  X   |    X  |    X      |  X |
        |      |       |           |    |
 quad4  |  X   |    X  |    X      |  X |
 quad5  |  X   |       |           |    |
 quad9  |  X   |    X  |    X      |  X |
        |      |       |           |    |
 tri3 (2D)|  X   |    X  |    X      |  X |
 tri4 (2D)|  X   |    X  |    X      |  X |
 tri6 (2D)|  X   |    X  |    X      |  X |
 tri7 (2D)|  X   |    X  |    X      |  X |
        |      |       |           |    |
 tri3 (3D)|  X   |    X  |    X      |  X |
 tri4 (3D)|  X   |    X  |    X      |  X |
 tri6 (3D)|  X   |    X  |    X      |  X |
 tri7 (3D)|  X   |    X  |    X      |  X |
        |      |       |           |    |
 shell2 |  X   |    X  |    X      |  X |
 shell3 |      |    X  |    X      |  X |
 shell4 |  X   |    X  |    X      |  X |
 shell8 |  X   |    X  |    X      |  X |
 shell9 |  X   |    X  |    X      |  X |
        |      |       |           |    |
 tet4   |  X   |    X  |    X      |  X |
 tet5   |  X   |       |           |    |
 tet7   |      |    X  |           |    |
 tet8   |  X   |    X  |    X      |  X |
 tet10  |  X   |    X  |    X      |  X |
 tet11  |  X   |    X  |           |    |
 tet14  |  X   |    X  |    X      |  X |
 tet15  |  X   |    X  |    X      |  X |
        |      |       |           |    |
 pyramid5 | X  |    X  |    X      |  X |
 pyramid13| X  |    X  |    X      |  X |
 pyramid14| X  |    X  |    X      |  X |
 pyramid18| X  |    X  |    X      |  X |
 pyramid19| X  |    X  |    X      |  X |
          |    |       |           |    |
 wedge6 |  X   |    X  |    X      |  X |
 wedge15|  X   |    X  |    X      |  X |
 wedge16|  X   |    X  |    X      |  X |
 wedge18|  X   |    X  |           |    |
 wedge20|  X   |    X  |    X      |  X |
 wedge21|  X   |    X  |    X      |  X |
        |      |       |           |    |
 hex8   |  X   |    X  |    X      |  X |
 hex9   |  X   |       |           |    |
 hex20  |  X   |    X  |    X      |  X |
 hex27  |  X   |    X  |    X      |  X |

The IOSS library, nem_slice, and nem_spread cannot correctly handle a
mesh unless it contains elements of a known type.

However, the exodus library can store elements of an unknown type with
a few limitations. For an unsupported element type, the exodus library
will not:

 * return the side set node list.
 * return the side set node list node count.
 * convert side set nodes to sides or vice versa.

If none of the above functions are required, then an element unknown
to the exodus library can can be written to and read from an exodus
file with no loss of information.

    
## Exodus Node Ordering Convention

Node ordering follows the conventions illustrated in the figures
below. The node ordering conventions follow the element topology used
in PATRAN. Thus, for higher-order elements than those illustrated, use
the ordering prescribed in the PATRAN Element Library Manual Manual
https://web.mscsoftware.com/training_videos/patran/reverb3/index.html#page/Finite%2520Element%2520Modeling/elem_lib_topics.16.1.html#ww33606.


### Circle and Sphere Element Types
For elements of type circle or sphere, the topology is one node at the center of the circle or sphere element.
A circle or sphere element has no sides.

![Circle](packages/seacas/doc-source/exodus/topology/circle.png)
![Sphere](packages/seacas/doc-source/exodus/topology/sphere.png)

### Bar, Truss, Beam Element Types
The element name for this element type can be "bar", "beam", "truss"

![Bar2](packages/seacas/doc-source/exodus/topology/bar2.png)
![Bar3](packages/seacas/doc-source/exodus/topology/bar3.png)

### Triangular Element Types
These are usable in 2D or 3D meshes.  In a 3D mesh, they would represent triangular shells.  The element name is "triangle" or "tri" in either case.

![Tri3](packages/seacas/doc-source/exodus/topology/tri3.png)
![Tri4](packages/seacas/doc-source/exodus/topology/tri4.png)
![Tri6](packages/seacas/doc-source/exodus/topology/tri6.png)
![Tri7](packages/seacas/doc-source/exodus/topology/tri7.png)

The side numbering for a tri element is shown below.  For a triangular shell, there are five sides. Side 1 is the "top" of the triangle given by nodes 1-2-3 and side 2 is the "bottom" of the triangle given by nodes 3-2-1.  Sides 3,4,5 correspond to sides 1,2,3 of the 2D triangle.

 Element Type |Side \#|Node Order  |
 -------------|-------|------------|
 TRI (2D)     | 1     | 1, 2       |
              | 2     | 2, 3       |
              | 3     | 3, 1       |

 Element Type |Side \#|Node Order  |
--------------|-------|------------|
 TRI (3D)     | 1     | 1, 2, 3    |
              | 2     | 1, 3, 2    |
 (Edges)      | 3     | 1, 2       |
              | 4     | 2, 3       |
              | 5     | 3, 1       |

![Tri-Number](packages/seacas/doc-source/exodus/topology/triangle-face-numbering.png)

### Quadrilateral Element Types
These are usable in 2D or 3D meshes.  In a 3D mesh, they represent a quadrilateral shell element and the element name is "shell"; in a 2D mesh, the element name is "quad".

![Quad4](packages/seacas/doc-source/exodus/topology/quad4.png)
![Quad5](packages/seacas/doc-source/exodus/topology/quad5.png)
![Quad8](packages/seacas/doc-source/exodus/topology/quad8.png)
![Quad9](packages/seacas/doc-source/exodus/topology/quad9.png)

The side numbering for a 2D quadrilateral element and a 3D quadrilateral shell element are shown below.

 Element Type |Side \#|Node Order  |
 -------------|-------|------------|
 QUAD (2D)    | 1     | 1, 2       |
              | 2     | 2, 3       |
              | 3     | 3, 4       |
              | 4     | 4, 1       |

![Quad-Number](packages/seacas/doc-source/exodus/topology/quad-face-numbering.png)

 Element Type |Side \#|Node Order  |
--------------|-------|------------|
 SHELL (3D)   | 1     | 1, 2, 3, 4 |
              | 2     | 1, 4, 3, 2 |
 (Edges)      | 3     | 1, 2       |
              | 4     | 2, 3       |
              | 5     | 3, 4       |
              | 6     | 4, 1       |

![Shell-Number](packages/seacas/doc-source/exodus/topology/shell-face-numbering.png)



### Tetrahedral Element Types
![Tet4](packages/seacas/doc-source/exodus/topology/tet04.png)
![Tet5](packages/seacas/doc-source/exodus/topology/tet05.png)
![Tet10](packages/seacas/doc-source/exodus/topology/tet10.png)
![Tet11](packages/seacas/doc-source/exodus/topology/tet11.png)
![Tet14](packages/seacas/doc-source/exodus/topology/tet14.png)
![Tet15](packages/seacas/doc-source/exodus/topology/tet15.png)

The side numbering for a tetrahedral element type is shown below.

 Element Type |Side \#|Node Order  |
--------------|-------|------------|
TETRA         | 1     | 1, 2, 4    |
              | 2     | 2, 3, 4    |
              | 3     | 1, 4, 3    |
              | 4     | 1, 3, 2    |

![Tet-Number](packages/seacas/doc-source/exodus/topology/tet-face-numbering.png)

### Pyramidal Element Types
![Pyramid](packages/seacas/doc-source/exodus/topology/pyramid.png)

The side numbering for a pyramidal element type is shown below.

 Element Type |Side \#|Node Order  |
--------------|-------|------------|
PYRAMID|1 | 1, 2, 5 |
       | 2 | 2, 3, 5 |
       | 3 | 3, 4, 5 |
       | 4 | 4, 1, 5 |
       | 5 | 1, 4, 3, 2 |

![Pyramid-Number](packages/seacas/doc-source/exodus/topology/pyramid-face-numbering.png)


### Wedge Element Types
![Wedge6](packages/seacas/doc-source/exodus/topology/wedge06.png)
![Wedge15](packages/seacas/doc-source/exodus/topology/wedge15.png)
![Wedge16](packages/seacas/doc-source/exodus/topology/wedge16.png)
![Wedge20](packages/seacas/doc-source/exodus/topology/wedge20.png)
![Wedge21](packages/seacas/doc-source/exodus/topology/wedge21.png)

The side numbering for a wedge element type is shown below. Note that
the face mapping for this element does not match the MSC/Patran face
mapping. In exodus, sides 1,2,3 are quadrilateral and sides 4,5 are
triangular; in MSC/Patran, sides 1,2 are triangular and sides 4,5,6
are quadrilateral. It is unclear when and why this digression
occurred.


 Element Type |Side \#|Node Order  | Patran Side |
--------------|-------|------------|-------------|
WEDGE         | 1     | 1, 2, 5, 4 | 3           |
              | 2     | 2, 3, 6, 5 | 5           |
              | 3     | 1, 4, 6, 3 | 4           |
              | 4     | 1, 3, 2    | 1           |
              | 5     | 4, 5, 6    | 2           |

![Wedge-Number](packages/seacas/doc-source/exodus/topology/wedge-face-numbering.png)

### Hexahedral Element Types
![Hex8](packages/seacas/doc-source/exodus/topology/hex08.png)
![Hex9](packages/seacas/doc-source/exodus/topology/hex09.png)
![Hex20](packages/seacas/doc-source/exodus/topology/hex20.png)
![Hex27](packages/seacas/doc-source/exodus/topology/hex27.png)

The side numbering for a hexahedral element type is shown below.

 Element Type |Side \#|Node Order  |
--------------|-------|------------|
HEX           | 1     | 1, 2, 6, 5 |
              | 2     | 2, 3, 7, 6 |
              | 3     | 3, 4, 8, 7 |
              | 4     | 1, 5, 8, 4 |
              | 5     | 1, 4, 3, 2 |
              | 6     | 5, 6, 7, 8 |

![Hex-Number](packages/seacas/doc-source/exodus/topology/hex-face-numbering.png)
`
