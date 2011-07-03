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

C $Id: rcvmxy.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: rcvmxy.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:17  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:56:01  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE RCVMXY (OPTION,
     &   NUMNPF, NDIM, XN, YN, ZN, IX2NP, DODEAD, IDN2B,
     &   REAPCK, *)
C=======================================================================

C   --*** RCVMXY *** (BLOT) Receive mesh coordinates
C   --   Written by Amy Gilkey - revised 05/09/88
C   --
C   --RCVMXY receives the mesh coordinate array as a message.  The
C   --node numbers are also received.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to receive all, else message options:
C   --      'H' to receive number of defined nodes
C   --      'N' to receive node numbers
C   --      'C' to receive coordinates
C   --      'D' to receive node numbers of dead nodes
C   --   NUMNPF - IN - the number of defined nodes
C   --   NDIM - IN - the number of coordinates per node
C   --   XN, YN, ZN - OUT - the mesh coordinates (if OPTION)
C   --   IX2NP - OUT - the node number of each defined node (if OPTION)
C   --   DODEAD - IN - true iff dead nodes are defined
C   --   IDN2B - OUT - the node number of each dead node (if OPTION)
C   --   REAPCK - SCRATCH - size = NDIM * NUMNPF
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      REAL XN(*), YN(*), ZN(*)
      LOGICAL DODEAD
      INTEGER IX2NP(*)
      INTEGER IDN2B(*)
      REAL REAPCK(*)

      INTEGER GETIR, GETRR
      CHARACTER*8 TAGEXP, TAG
      LOGICAL ISOK

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'H') .GT. 0)) THEN
         TAGEXP = 'MESHNODE'
         LEXP = 1
         IERR = GETIR (TAG, NUMNPF, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'N') .GT. 0)) THEN
         IF (NUMNPF .GT. 0) THEN
            TAGEXP = 'MESHNODE'
            LEXP = NUMNPF
            IERR = GETIR (TAG, IX2NP, LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'C') .GT. 0)) THEN
         IF (NUMNPF .GT. 0) THEN
            TAGEXP = 'MESHNODE'
            LEXP = NDIM * NUMNPF
            IERR = GETRR (TAG, REAPCK, LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1

            N = 0
            CALL CPYREA (NUMNPF, REAPCK(N+1), XN)
            N = N + NUMNPF
            CALL CPYREA (NUMNPF, REAPCK(N+1), YN)
            N = N + NUMNPF
            IF (NDIM .GE. 3) THEN
               CALL CPYREA (NUMNPF, REAPCK(N+1), ZN)
               N = N + NUMNPF
            END IF
         END IF
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'D') .GT. 0)) THEN
         IF (DODEAD .AND. (NUMNPF .GT. 0)) THEN
            TAGEXP = 'MESHNODE'
            LEXP = NUMNPF
            IERR = GETIR (TAG, IDN2B, LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      RETURN
      END
