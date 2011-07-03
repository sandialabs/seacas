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

C $Id: rcvqa.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: rcvqa.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:29  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:56:08  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE RCVQA (OPTION, MAXQA, MAXINF,
     &   NQAREC, QAREC, NINFO, INFREC, EXODUS, *)
C=======================================================================

C   --*** RCVQA *** (BLOT) Receive QA and information records
C   --   Written by Amy Gilkey - revised 05/09/88
C   --
C   --RCVQA receives the QA records and the information records.
C   --
C   --Note that the number of QA records and information records to be read
C   --are read in this routine.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to receive all, else message options:
C   --      'H' to receive number of records
C   --      'Q' to receive QA records
C   --      'I' to receive information records
C   --   MAXQA - IN - the maximum number of QA records to receive
C   --   MAXINF - IN - the number of information records to receive
C   --   NQAREC - OUT - the number of QA records; <0 if end-of-file (if OPTION)
C   --   QAREC - OUT - the QA records containing: (if OPTION)
C   --      (1) - the analysis code name
C   --      (2) - the analysis code QA descriptor
C   --      (3) - the analysis date
C   --      (4) - the analysis time
C   --   NINFO - OUT - the number of information records; <0 if end-of-file
C   --      (if OPTION)
C   --   INFO - OUT - the information records (if OPTION)
C   --   EXODUS - OUT - false if GENESIS file, true if EXODUS file so far
C   --      (if OPTION)
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      CHARACTER*8 QAREC(4,*)
      CHARACTER*80 INFREC(*)
      LOGICAL EXODUS

      INTEGER GETCR, GETIR
      CHARACTER*8 TAGEXP, TAG
      LOGICAL ISOK
      INTEGER INTPCK(3)

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'H') .GT. 0)) THEN
         TAGEXP = 'QAINFO'
         LEXP = 3
         IERR = GETIR (TAG, INTPCK, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1

         N = 0
         EXODUS = (INTPCK(N+1) .NE. 0)
         N = N + 1
         NQAREC = INTPCK(N+1)
         N = N + 1
         NINFO = INTPCK(N+1)
         N = N + 1
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'Q') .GT. 0)) THEN
         IF (NQAREC .GT. 0) THEN
            TAGEXP = 'QA'
            LEXP = NQAREC * 4 * 8
            IERR = GETCR (TAG, QAREC, LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'I') .GT. 0)) THEN
         IF (NINFO .GT. 0) THEN
            TAGEXP = 'INFOREC'
            LEXP = NINFO * 80
            IERR = GETCR (TAG, INFREC, LEXP, L)
            CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      RETURN
      END
