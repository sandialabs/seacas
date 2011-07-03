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

C $Id: rcvtim.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: rcvtim.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:36  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:56:12  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE RCVTIM (OPTION, NSTEPS, NSTEPW, A, KTIMES, KWHOLE, *)
C=======================================================================

C   --*** RCVTIM *** (BLOT) Receive database time step times
C   --   Written by Amy Gilkey - revised 05/09/88
C   --
C   --RCVTIM receives all time steps times as a message, storing the time for
C   --each step in dynamic memory.  It also sets the number of time steps.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to receive all, else message options:
C   --      'H' to receive number of times
C   --      'T' to receive times
C   --      'W' to receive whole times indicator
C   --   NSTEPS - OUT - the number of database time steps (if OPTION)
C   --   NSTEPW - OUT - the number of whole (versus history) database time steps
C   --      (if OPTION)
C   --   A - IN/OUT - the dynamic memory base array
C   --   KTIMES - OUT - the dynamic memory index of TIMES (if OPTION)
C   --   KWHOLE - OUT - the dynamic memory index of WHOTIM (if OPTION);
C   --      WHOTIM(i) is true iff TIMES(i) is a whole (versus history) time step
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      DIMENSION A(*)

      INTEGER GETLR, GETIR, GETRR
      CHARACTER*8 TAGEXP, TAG
      LOGICAL ISOK
      INTEGER INTPCK(2)

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'H') .GT. 0)) THEN
         TAGEXP = 'TIMES'
         LEXP = 2
         IERR = GETIR (TAG, INTPCK, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1

         N = 0
         NSTEPS = INTPCK(N+1)
         N = N + 1
         NSTEPW = INTPCK(N+1)
         N = N + 1
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'T') .GT. 0)) THEN
         CALL MDRSRV ('TIMES', KTIMES, NSTEPS)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 100

         IF (NSTEPS .GT. 0) THEN
            TAGEXP = 'TIMES'
            LEXP = NSTEPS
            IERR = GETRR (TAG, A(KTIMES), LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'W') .GT. 0)) THEN
         CALL MDRSRV ('WHOTIM', KWHOLE, NSTEPS)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 100

         IF (NSTEPS .GT. 0) THEN
            TAGEXP = 'WHOTIM'
            LEXP = NSTEPS
            IERR = GETLR (TAG, A(KWHOLE), LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

  100 CONTINUE
      RETURN
      END
