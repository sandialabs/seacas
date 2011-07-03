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

C $Id: sndmxy.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: sndmxy.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:13:06  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:57:44  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE SNDMXY (OPTION,
     &   NNPSUR, NPSURF, NDIM, XN, YN, ZN, DODEAD, IDN2B,
     &   INTPCK, REAPCK, *)
C=======================================================================

C   --*** SNDMXY *** (BLOT) Send mesh coordinates
C   --   Written by Amy Gilkey - revised 05/04/88
C   --
C   --SNDMXY sends the mesh coordinate array as a message.  Only the
C   --coordinates for surface nodes (and dead nodes) are sent.
C   --The node numbers are also sent.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to send all, else message options:
C   --      'H' to send number of defined nodes
C   --      'N' to send node numbers
C   --      'C' to send coordinates
C   --      'D' to send node numbers of dead nodes
C   --   NNPSUR - IN - the number of surface nodes
C   --   NPSURF - IN - the indices of the surface nodes
C   --   NDIM - IN - the number of coordinates per node
C   --   XN, YN, ZN - IN - the coordinates (if OPTION)
C   --   DODEAD - IN - true iff dead nodes are defined
C   --   IDN2B - IN - the node number of each dead node (if OPTION)
C   --   INTPCK - SCRATCH - size = NNPSUR
C   --   REAPCK - SCRATCH - size = NDIM * NNPSUR
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      INTEGER NPSURF(*)
      REAL XN(*), YN(*), ZN(*)
      LOGICAL DODEAD
      INTEGER IDN2B(*)
      INTEGER INTPCK(*)
      REAL REAPCK(*)

      INTEGER PUTIR, PUTRR
      CHARACTER*8 TAG
      LOGICAL ISOK

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'H') .GT. 0)) THEN
         TAG = 'MESHNODE'
         L = 1
         IERR = PUTIR (TAG, NNPSUR, L)
         CALL CHKPUT (IERR, TAG, L, ISOK)
         IF (.NOT. ISOK) RETURN 1
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'N') .GT. 0)) THEN
         IF (NNPSUR .GT. 0) THEN
            TAG = 'MESHNODE'
            L = NNPSUR
            IERR = PUTIR (TAG, NPSURF, L)
            CALL CHKPUT (IERR, TAG, L, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'C') .GT. 0)) THEN
         IF (NNPSUR .GT. 0) THEN
            N = 0
            DO 100 I = 1, NNPSUR
               INP = NPSURF(I)
               REAPCK(N+1) = XN(INP)
               N = N + 1
  100       CONTINUE
            DO 110 I = 1, NNPSUR
               INP = NPSURF(I)
               REAPCK(N+1) = YN(INP)
               N = N + 1
  110       CONTINUE
            IF (NDIM .GE. 3) THEN
               DO 120 I = 1, NNPSUR
                  INP = NPSURF(I)
                  REAPCK(N+1) = ZN(INP)
                  N = N + 1
  120          CONTINUE
            END IF

            TAG = 'MESHNODE'
            L = N
            IERR = PUTRR (TAG, REAPCK, L)
            CALL CHKPUT (IERR, TAG, L, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'D') .GT. 0)) THEN
         IF (DODEAD .AND. (NNPSUR .GT. 0)) THEN
            N = 0
            DO 130 I = 1, NNPSUR
               INP = NPSURF(I)
               INTPCK(N+1) = IDN2B(INP)
               N = N + 1
  130       CONTINUE

            TAG = 'MESHNODE'
            L = N
            IERR = PUTIR (TAG, INTPCK, L)
            CALL CHKPUT (IERR, TAG, L, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      RETURN
      END
