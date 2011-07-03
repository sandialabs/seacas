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

C $Id: rcvelb.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: rcvelb.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:02  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:55:52  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE RCVELB (OPTION, NELBLK,
     &   IDELB, LENE, NUMELB, NUMLNK, NUMATR,
     &   A, KLINK, KATRIB, INTPCK, *)
C=======================================================================

C   --*** RCVELB *** (BLOT) Receive database element blocks
C   --   Written by Amy Gilkey - revised 05/09/88
C   --
C   --RCVELB receives the element block information as a message.
C   --
C   --The dynamic memory arrays LINK and ATRIB must be reserved
C   --if the connectivity and attributes are to be received.  These arrays
C   --will be expanded by this routine to hold the new data.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to receive all, else message options:
C   --      'H' to receive all header information
C   --      'C' to receive connectivity
C   --      'A' to receive attributes
C   --   NELBLK - IN - the number of element blocks
C   --   IDELB - OUT - the element block IDs for each block (if OPTION)
C   --   LENE - OUT - the cumulative element count by block (if OPTION)
C   --   NUMELB - OUT - the number of elements in each block (if OPTION)
C   --   NUMLNK - OUT - the number of nodes per element in each block
C   --      (if OPTION)
C   --   NUMATR - OUT - the number of attributes in each block (if OPTION)
C   --   A - IN/OUT - the dynamic memory base array
C   --   KLINK - OUT - pointer to the connectivity for each block (if OPTION)
C   --   KATRIB - OUT - pointer to the attributes for each block (if OPTION)
C   --   INTPCK - SCRATCH - size = 4 * NELBLK
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      INTEGER IDELB(*)
      INTEGER LENE(0:*)
      INTEGER NUMELB(*)
      INTEGER NUMLNK(*)
      INTEGER NUMATR(*)
      DIMENSION A(*)
      INTEGER INTPCK(*)

      INTEGER GETIR
      CHARACTER*8 TAGEXP, TAG
      LOGICAL ISOK

      IF (NELBLK .LE. 0) RETURN

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'H') .GT. 0)) THEN
         TAGEXP = 'BLOCKS'
         LEXP = 4 * NELBLK
         IERR = GETIR (TAG, INTPCK, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1

         N = 0
         CALL CPYINT (NELBLK, INTPCK(N+1), IDELB)
         N = N + NELBLK
         LENE(0) = 0
         CALL CPYINT (NELBLK, INTPCK(N+1), LENE(1))
         N = N + NELBLK
         DO 100 I = 1, NELBLK
            NUMELB(I) = LENE(I) - LENE(I-1)
  100    CONTINUE
         CALL CPYINT (NELBLK, INTPCK(N+1), NUMLNK)
         N = N + NELBLK
         CALL CPYINT (NELBLK, INTPCK(N+1), NUMATR)
         N = N + NELBLK
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'C') .GT. 0)) THEN
         CALL CNTLNK (NELBLK, LENE, NUMLNK, N, IDUM)
         IF (N .GT. 0) THEN
            CALL MDLONG ('LINK', KLINK, N)
            CALL MDSTAT (NERR, MEM)
            IF (NERR .GT. 0) GOTO 110

            TAGEXP = 'BLOCKS'
            LEXP = N
            IERR = GETIR (TAG, A(KLINK), LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'A') .GT. 0)) THEN
         CALL CNTLNK (NELBLK, LENE, NUMATR, N, IDUM)
         IF (N .GT. 0) THEN
            CALL MDLONG ('ATRIB', KATRIB, N)
            CALL MDSTAT (NERR, MEM)
            IF (NERR .GT. 0) GOTO 110

            TAGEXP = 'BLOCKS'
            LEXP = 4 * NELBLK
            IERR = GETIR (TAG, A(KATRIB), LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

  110 CONTINUE
      RETURN
      END
