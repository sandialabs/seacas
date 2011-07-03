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

C $Id: rcvsca.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: rcvsca.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:33  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:56:10  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE RCVSCA (OPTION, IVAR, NELBLK,
     &   VALMIN, NUMMIN, XYZMIN, ISTMIN,
     &   VALMAX, NUMMAX, XYZMAX, ISTMAX, REAPCK, *)
C=======================================================================

C   --*** RCVSCA *** (BLOT) Receive variable scaling parameters
C   --   Written by Amy Gilkey - revised 05/09/88
C   --
C   --RCVSCA receives the database scaling parameter as a message.  For
C   --element variables, the parameters are calculated for each element
C   --block.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to receive all, else message options:
C   --   IVAR - IN - the variable index
C   --   NELBLK - IN - the number of element blocks (0 if not element)
C   --   VALMIN, VALMAX - OUT - the minimum and maximum value
C   --   NUMMIN, NUMMAX - OUT - the node or element number of the minimum and
C   --      maximum value (if nodal or element)
C   --   XYZMIN, XYZMAX - OUT - the coordinates of NUMMIN, NUMMAX
C   --      (if nodal or element)
C   --   ISTMIN, ISTMAX - OUT - the step number of the minimum and maximum
C   --      value
C   --   REAPCK - SCRATCH - size = 2 * 6 * (1+NELBLK)
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      REAL VALMIN(0:NELBLK), VALMAX(0:NELBLK)
      INTEGER NUMMIN(0:NELBLK), NUMMAX(0:NELBLK)
      REAL XYZMIN(3,0:NELBLK), XYZMAX(3,0:NELBLK)
      INTEGER ISTMIN(0:NELBLK), ISTMAX(0:NELBLK)
      REAL REAPCK(2*6*(1+NELBLK))

      INTEGER GETRR
      CHARACTER*8 TAGEXP, TAG
      LOGICAL ISOK

      CHARACTER TYP

      CALL DBVTYP (IVAR, TYP, IDUM)

      TAGEXP = 'SCALE'
      IF ((TYP .EQ. 'N') .OR. (TYP .EQ. 'E')) THEN
         LEXP = (1+NELBLK) * 2 * (1 + 1 + 3 + 1)
      ELSE
         LEXP = (1+NELBLK) * 2 * (1 + 1)
      END IF
      IERR = GETRR (TAG, REAPCK, LEXP, L)
      CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
      IF (.NOT. ISOK) RETURN 1

      N = 0
      DO 120 IELB = 0, NELBLK
         VALMIN(IELB) = REAPCK(N+1)
         N = N + 1
         IF ((TYP .EQ. 'N') .OR. (TYP .EQ. 'E')) THEN
            NUMMIN(IELB) = INT(REAPCK(N+1))
            N = N + 1
            DO 100 I = 1, 3
               XYZMIN(I,IELB) = REAPCK(N+1)
               N = N + 1
  100       CONTINUE
         END IF
         ISTMIN(IELB) = INT(REAPCK(N+1))
         N = N + 1
         VALMAX(IELB) = REAPCK(N+1)
         N = N + 1
         IF ((TYP .EQ. 'N') .OR. (TYP .EQ. 'E')) THEN
            NUMMAX(IELB) = INT(REAPCK(N+1))
            N = N + 1
            DO 110 I = 1, 3
               XYZMAX(I,IELB) = REAPCK(N+1)
               N = N + 1
  110       CONTINUE
         END  IF
         ISTMAX(IELB) = INT(REAPCK(N+1))
         N = N + 1
  120 CONTINUE

      RETURN
      END
