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

C $Id: idedge.f,v 1.1 2009/03/25 04:47:53 gdsjaar Exp $
C $Log: idedge.f,v $
C Revision 1.1  2009/03/25 04:47:53  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:03:33  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:52:30  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE IDEDGE (IEDSET, NEDGES, HIDENP, LINSET, LENL, IXLIN)
C=======================================================================

C   --*** IDEDGE *** (MESH) Identify 3D lines on edge of visible mesh
C   --   Written by Amy Gilkey - revised 10/02/86
C   --
C   --IDEDGE finds edges from IEDSET in the line set and marks them as
C   --"must draw" with a -n in LINSET(3,x).
C   --
C   --Parameters:
C   --   IEDSET - IN - the edge line set;
C   --      (0) = face defining edge; 0 to delete edge
C   --   NEDGES - IN - the number of lines in the edge set
C   --   HIDENP - IN/OUT - node status (as in HIDDEN)
C   --   LINSET - IN/OUT - the sorted line set
C   --   LENL - IN - the length of the line set
C   --   IXLIN - SCRATCH - index line set to be searched, length = LENL

      PARAMETER (KNVIS=0, KNFOVR=10, KNHID=100)

      common /debugc/ cdebug
      common /debugn/ idebug
      character*8 cdebug

      COMMON /D3NUMS/ IS3DIM, NNPSUR, NUMNPF, LLNSET
      LOGICAL IS3DIM

      INTEGER IEDSET(0:2,*)
      INTEGER HIDENP(*)
      INTEGER LINSET(LLNSET,*)
      INTEGER IXLIN(*)

C   --Get indices of line set to search and order line set nodes

      LENIX = 0
      DO 100 IL = 1, LENL
         IF (LINSET(3,IL) .GE. 1) THEN
            LENIX = LENIX + 1
            IXLIN(LENIX) = IL
            IF (LINSET(3,IL) .EQ. 1) THEN
               IF (LINSET(1,IL) .GT. LINSET(2,IL)) THEN
                  IF (HIDENP(LINSET(1,IL)) .GT. KNVIS) THEN
                     I = LINSET(1,IL)
                     LINSET(1,IL) = LINSET(2,IL)
                     LINSET(2,IL) = I
                  END IF
               END IF
            END IF
         END IF
  100 CONTINUE

      DO 130 IEDG = 1, NEDGES
         IF (IEDSET(0,IEDG) .EQ. 0) GOTO 130

C      --Make sure edge set is still ordered (some nodes may have become
C      --visible)

         N1 = IEDSET(1,IEDG)
         N2 = IEDSET(2,IEDG)
         IF (N1 .GT. N2) THEN
            IF (HIDENP(N1) .GT. KNVIS) THEN
               I = N1
               N1 = N2
               N2 = I
            END IF
         END IF

C      --Find edge in ordered line set

         DO 110 IX = 1, LENIX
            IL = IXLIN(IX)
            IF (LINSET(1,IL) .EQ. N1) THEN
               IF (LINSET(2,IL) .EQ. N2) GOTO 120
            END IF
  110    CONTINUE
         GOTO 130
  120    CONTINUE

C      --Mark line set as an edge

         LINSET(3,IL) = - LINSET(3,IL)
  130 CONTINUE

      RETURN
      END
