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

C $Id: facelb.f,v 1.1 2009/03/25 04:47:53 gdsjaar Exp $
C=======================================================================
      SUBROUTINE FACELB (IELB, LENE, NLNKE, LINKE,
     &   NLNKSC, LINKSC, IF2ESC, MAXNPF, NPFS, NFACES, NAMELB)
C=======================================================================

C   --*** FACELB *** (MESH) Match element block faces
C   --   Written by Amy Gilkey - revised 02/24/88
C   --              Sam Key, 06/01/85
C   --
C   --FACELB makes up a list of faces for all faces in an element block.
C   --All matching faces within the block are combined.
C   --
C   --Parameters:
C   --   IELB - IN - the element block number
C   --   LENE - IN - the cumulative element counts by element block
C   --   NLNKE - IN - the number of nodes per element
C   --   LINKE - IN - the connectivity for all elements
C   --   NLNKSC - IN - the number of nodes per face
C   --   LINKSC - IN/OUT - the connectivity for all faces (this block only)
C   --   IF2ESC - IN/OUT - the element number(s) of each face in LINKSC
C   --   MAXNPF - IN - the maximum length of the NPFS entry
C   --   NPFS - SCRATCH - the list of unmatched faces containing a node;
C   --      (0,i) = the length of the list
C   --   NFACES - OUT - the number of unique faces in this element block
C   --
C   --Common Variables:
C   --   Uses NUMNP of /DBNUMS/

      PARAMETER (MXSTLN=32)
      PARAMETER (MXLNLN=80)

      include 'dbnums.blk'

      INTEGER LENE(0:NELBLK), LINKE(NLNKE,*)
      INTEGER LINKSC(*)
      INTEGER IF2ESC(2,*)
      INTEGER NPFS(0:MAXNPF,*)

      INTEGER LINKF1(4)
      CHARACTER*(MXSTLN) NAMELB

      do 90 i=1, numnp
        npfs(0,i) = 0
 90   continue
c$$$      CALL ININPF (NUMNP, MAXNPF, NPFS)

      NOVER = 0
      NFACES = 0
      IXF = 1
      DO 110 IEL = LENE(IELB-1)+1, LENE(IELB)
         IXEL = IEL - LENE(IELB-1)

         IF (LINKE(1,IXEL) .NE. 0) THEN

            IF (NLNKE .eq. 4 .and. NAMELB(:3) .eq. 'TET') THEN
               DO 200 IFACE = 1, 4
                  CALL TNODES (IFACE, LINKE(1,IXEL), LINKF1)
                  IF (    (NPFS(0,LINKF1(1)) .GT. 0)
     &              .AND. (NPFS(0,LINKF1(2)) .GT. 0)
     &              .AND. (NPFS(0,LINKF1(3)) .GT. 0)) THEN
                    IMATCH = MATFAT (LINKF1, MAXNPF, NPFS, iel, IERR)
                  else
                    imatch = 0
                  endif
                  
                  IF (IMATCH .LE. 0) THEN
                     NFACES = NFACES + 1
                     CALL CPYINT (4, LINKF1, LINKSC(IXF))
                     IXF = IXF + 4
                     IF2ESC(1,NFACES) = IEL
                     IF2ESC(2,NFACES) = 0

                     CALL FILNPF (4, LINKF1, NFACES,
     &                  MAXNPF, NPFS, NOVER)

                  ELSE
                     IF2ESC(2,IMATCH) = IEL
                  END IF
 200           CONTINUE

            ELSE IF (NLNKE .LT. 8) THEN
               NFACES = NFACES+1
               CALL CPYINT (NLNKSC, LINKE(1,IXEL), LINKSC(IXF))
               IXF = IXF + NLNKSC
               IF2ESC(1,NFACES) = IEL
               IF2ESC(2,NFACES) = 0

            ELSE IF (NLNKE .EQ. 8) THEN
               DO 100 IFACE = 1, 6
                  CALL FNODES (IFACE, LINKE(1,IXEL), LINKF1)
                  IF (    (NPFS(0,LINKF1(1)) .GT. 0)
     &              .AND. (NPFS(0,LINKF1(2)) .GT. 0)
     &              .AND. (NPFS(0,LINKF1(3)) .GT. 0)
     &              .AND. (NPFS(0,LINKF1(4)) .GT. 0)) THEN
                    IMATCH = MATFAC (LINKF1, MAXNPF, NPFS, iel, IERR)
                  else
                    imatch = 0
                  endif
                  
                  IF (IMATCH .LE. 0) THEN
                     NFACES = NFACES + 1
                     CALL CPYINT (NLNKSC, LINKF1, LINKSC(IXF))
                     IXF = IXF + NLNKSC
                     IF2ESC(1,NFACES) = IEL
                     IF2ESC(2,NFACES) = 0

                     CALL FILNPF (NLNKSC, LINKF1, NFACES,
     &                  MAXNPF, NPFS, NOVER)

                  ELSE
                     IF2ESC(2,IMATCH) = IEL
                  END IF
  100          CONTINUE
            END IF
         END IF
  110 CONTINUE

      IF (NOVER .GT. 0) THEN
         WRITE (*, 10000) 'in FACELB, MAXNPF =', MAXNPF, ', # =', NOVER
10000     FORMAT (1X, 'PROGRAM ERROR - ', A, I5, A, I5)
      END IF

      RETURN
      END
