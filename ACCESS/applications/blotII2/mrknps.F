C Copyright(C) 2009 Sandia Corporation. Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C certain rights in this software.
C         
C Redistribution and use in source and binary forms, with or without
C modification, are permitted provided that the following conditions are
C met:
C 
C     * Redistributions of source code must retain the above copyright
C       notice, this list of conditions and the following disclaimer.
C 
C     * Redistributions in binary form must reproduce the above
C       copyright notice, this list of conditions and the following
C       disclaimer in the documentation and/or other materials provided
C       with the distribution.
C     * Neither the name of Sandia Corporation nor the names of its
C       contributors may be used to endorse or promote products derived
C       from this software without specific prior written permission.
C 
C THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
C "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
C LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
C A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
C OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
C SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
C LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
C DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
C THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
C OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

C $Id: mrknps.F,v 1.1 2009/03/25 04:47:53 gdsjaar Exp $
C $Log: mrknps.F,v $
C Revision 1.1  2009/03/25 04:47:53  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.5  2000/07/11 16:50:23  gdsjaar
C SGI had problems with logical statment function, inlined it
C
C Revision 1.4  1999/03/02 19:12:36  gdsjaar
C Fixed nodeset scaling interpolation equation
C
C Revision 1.3  1997/06/26 17:51:37  caforsy
C Substituted if NeedsDoubleEscape for ifdef sun.  This define is needed on cougar, sun and ibm platforms
C
C Revision 1.2  1995/11/02 16:59:11  caforsy
C Changes compute and io word size defaults.  Changed node set
C markings to "O"'s. Node set markings will scale depending upon the
C number of nodes.
C
C Revision 1.1  1994/04/07 20:05:14  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
CRevision 1.3  1993/07/08  21:57:16  gdsjaar
CReplaced SUN with sun in all ifdef statements.
C
CRevision 1.2  1990/12/14  08:53:37  gdsjaar
CAdded RCS Id and Log to all files
C
C=======================================================================
      SUBROUTINE MRKNPS (HIDENP,
     &   XN, YN, ZN, IX2NP, IN2ELB, DODEAD, IDN2B,
     &   NNPSET, ISSNPS, IDNPS, NNNPS, IXNNPS, LTNNPS, *)
C=======================================================================

C   --*** MRKNPS *** (SETS) Mark node set on mesh
C   --   Written by Amy Gilkey - revised 04/08/88
C   --
C   --MRKNPS marks all the nodes in selected node sets on the mesh.
C   --Each node is marked with an X in the color assigned to its node set.
C   --
C   --Parameters:
C   --   HIDENP(i)  - IN - true iff node i is hidden (3D only)
C   --   XN, YN, ZN - IN - the nodal coordinates
C   --   IX2NP      - IN - the node number for each mesh index
C   --                     (MASTER process only)
C   --   IN2ELB     - IN - the element block for each node;
C   --                     <0 if not in any selected element block
C   --                     =0 if in more than one selected element block
C   --   DODEAD     - IN - mark dead nodes iff true
C   --   IDN2B      - IN - the element block for each dead node; dead if >= 0
C   --   NNPSET     - IN - the number of selected node sets
C   --   ISSNPS     - IN - the indices of the selected node sets
C   --   IDNPS      - IN - the node set ID for each set
C   --   NNNPS      - IN - the number of nodes for each set
C   --   IXNNPS     - IN - the index of the first node for each set
C   --   LTNNPS     - IN - the nodes for all sets
C   --   * - return statement if the cancel function is active
C   --
C   --Common Variables:
C   --   Uses NELBLK of /DBNUMS/
C   --   Uses IS3DIM, NUMNPF of /D3NUMS/

      PARAMETER (KHCHSZ=1, KSCHSZ=2)

      common /debugc/ cdebug
      common /debugn/ idebug
      character*8 cdebug

      include 'dbnums.blk'
      include 'dbnumgq.blk'
      include 'd3nums.blk'

      common /dualpr/ proces
      character*8 proces

      LOGICAL HIDENP(*)
      REAL XN(*), YN(*), ZN(*), NPFACT
      INTEGER IX2NP(NUMNPF)
      INTEGER IN2ELB(NUMNPF)
      LOGICAL DODEAD
      INTEGER IDN2B(NUMNPF)
      INTEGER ISSNPS(*)
      INTEGER IDNPS(*)
      INTEGER NNNPS(*)
      INTEGER IXNNPS(*)
      INTEGER LTNNPS(*)

      LOGICAL GRABRT
      LOGICAL PLTGTT, PLTSTT, LDUM
      logical logt

C   --Set the symbol size, do not exit before resetting
      LDUM = PLTGTT (KSCHSZ, SZSYM)
C     If no sets are selected, return
      IF (NNPSET .EQ. 0) THEN
         RETURN
      END IF

C     sum the number of nodes for all selected node sets
      ISUM = 0
      DO 10 I = 1, NNPSET
         ISUM = ISUM + NNNPS(ISSNPS(I))
 10   CONTINUE

C     compute the npfact
      IF (ISUM .LT. 100) THEN
         NPFACT = 1.0
      ELSE IF (ISUM .GT. 2100) THEN
         NPFACT = 0.40
      ELSE
         NPFACT = -0.0003 * ISUM + 1.03
      END IF
      LDUM = PLTSTT (KSCHSZ, NPFACT*SZSYM)

      DO 110 IX = 1, NNPSET
         INPS = ISSNPS(IX)

         CALL GRCOLR (INPS)

         IX0 = IXNNPS(INPS) - 1
         DO 100 INE = 1, NNNPS(INPS)
            INP = LTNNPS(IX0+INE)
            if (proces .eq. 'MASTER') then
               INP = LOCINT (INP, NUMNPF, IX2NP)
               IF (INP .EQ. 0) GOTO 100
            end if
            IF (IS3DIM) THEN
               IF (HIDENP(INP)) GOTO 100
            END IF

            logt = .false.
            if (in2elb(inp) .ge. 0) then
               logt = .true.
            else if (DODEAD) then
               if (IDN2B(inp) .GE. 0) logt = .true.
            end if
            if (logt) then
#if NeedsDoubleEscape
               CALL MPD2SY (1, XN(INP), YN(INP), '\\CCI')
#else
               CALL MPD2SY (1, XN(INP), YN(INP), '\CCI')
#endif
            END IF
  100    CONTINUE

         CALL PLTFLU
  110 CONTINUE

C   --Reset the symbol size, do not exit before resetting
      LDUM = PLTSTT (KSCHSZ, SZSYM)
      RETURN

  120 CONTINUE
C   --Reset the symbol size, do not exit before resetting
      LDUM = PLTSTT (KSCHSZ, SZSYM)
      RETURN 1
      END

