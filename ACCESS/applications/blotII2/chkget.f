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

C $Id: chkget.f,v 1.1 2009/03/25 04:47:53 gdsjaar Exp $
C=======================================================================
      SUBROUTINE CHKGET (IERR, TAG, TAGEXP, L, LEXP, OK)
C=======================================================================

C   --*** CHKGET *** (BLOT) Check received message
C   --   Written by Amy Gilkey - revised 05/13/88
C   --
C   --CHKGET checks the error indicator and the tag and the length of the
C   --received message.  It also checks for the special message that
C   --indicates a database error.
C   --
C   --Parameters:
C   --   IERR        - IN - the error indicator (0 = no error)
C   --   TAG, TAGEXP - IN - the received and expected message tag
C   --   L, LEXP     - IN - the received and expected message length
C   --   OK          - IN - returns true iff no error and the tag
C   --                      and length match

      CHARACTER*8 TAGEXP, TAG
      LOGICAL OK

      OK = .TRUE.
      IF (TAG .EQ. 'DBERR') THEN
         CALL PRTERR ('FATAL',
     &      'Database error, expecting ' // TAGEXP(:LENSTR(TAGEXP)))
         OK = .FALSE.
         GOTO 100
      END IF

      IF (IERR .NE. 0) THEN
         WRITE (*, 10000, IOSTAT=IDUM) IERR, TAGEXP(:LENSTR(TAGEXP))
10000    FORMAT (1X, '*** MESSAGE GET ERROR: ', I5, ', expected ', A)
         OK = .FALSE.
      END IF
      IF (TAG .NE. TAGEXP) THEN
         WRITE (*, 10010) TAGEXP(:LENSTR(TAGEXP)), TAG(:LENSTR(TAG))
10010    FORMAT (1X, '*** MESSAGE TAG PROBLEM:    expected ', 
     &           A, ', received ', A)
         OK = .FALSE.
      END IF
      IF (L .NE. LEXP) THEN
         WRITE (*, 10020, IOSTAT=IDUM) TAG(:LENSTR(TAG)), LEXP, L
10020    FORMAT (1X, '*** MESSAGE LENGTH PROBLEM: ', 'tag ', A,
     &           ', expected ', I5, ', received ', I5)
         OK = .FALSE.
      END IF

  100 CONTINUE
      RETURN
      END
