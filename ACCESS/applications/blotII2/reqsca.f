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

C $Id: reqsca.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: reqsca.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:51  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:56:21  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE REQSCA (OPTION, IVAR, NALVAR, USESEL, NELBLK, IELBST,
     &   REAPCK, *)
C=======================================================================

C   --*** REQSCA *** (BLOT) Request scaling parameters
C   --   Written by Amy Gilkey - revised 05/04/88
C   --
C   --REQSCA sends a message requesting the scaling parameters for a
C   --variable.
C   --
C   --Parameters:
C   --   OPTION - IN - the information being requested
C   --   IVAR - IN - the variable index
C   --   NALVAR - IN - the element birth/death variable (element variable only)
C   --   USESEL - IN - use the element blocks selected array iff true,
C   --      else all selected
C   --   NELBLK - IN - the number of element blocks
C   --   IELBST - IN - the element block status (>0 if selected)
C   --      (element variable only)
C   --   REAPCK - SCRATCH - size = 3 + NELBLK
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      LOGICAL USESEL
      INTEGER IELBST(*)
      REAL REAPCK(3+NELBLK)

      INTEGER PUTRR
      CHARACTER*8 TAG
      LOGICAL ISOK
      CHARACTER TYP

      CALL DBVTYP (IVAR, TYP, IDUM)

      N = 0
      REAPCK(N+1) = IVAR
      N = N + 1
      IF (TYP .EQ. 'E') THEN
         REAPCK(N+1) = NALVAR
         N = N + 1
         IF (USESEL) THEN
            REAPCK(N+1) = -1
            N = N + 1
            DO 100 I = 1, NELBLK
               REAPCK(N+1) = IELBST(I)
               N = N + 1
  100       CONTINUE
         ELSE
            REAPCK(N+1) = 0
            N = N + 1
         END IF
      END IF

      TAG = 'SCALE'
      L = N
      IERR = PUTRR (TAG, REAPCK, L)
      CALL CHKPUT (IERR, TAG, L, ISOK)
      IF (.NOT. ISOK) RETURN 1

      RETURN
      END
