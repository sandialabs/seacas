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

C $Id: sndmvr.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: sndmvr.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:13:02  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:57:42  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE SNDMVR (OPTION, TYP,
     &   NNPSUR, NPSURF, NELBLK, LENF, IF2EL, VAR, REAPCK, *)
C=======================================================================

C   --*** SNDMVR *** (BLOT) Send mesh variable values
C   --   Written by Amy Gilkey - revised 05/04/88
C   --
C   --SNDMVR sends the mesh variable values as a message.  Only the values
C   --for surface nodes are sent for a nodal variable.  Only the values
C   --for surface faces are sent for an element variable.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to send all, else message options:
C   --   TYP - IN - the variable type: 'N'odal or 'E'lement
C   --   NNPSUR - IN - the number of surface nodes (if nodal)
C   --   NPSURF - IN - the indices of the surface nodes (if nodal)
C   --   NELBLK - IN - the number of element blocks (if element)
C   --   LENF - IN - the cumulative face counts by blocks (if element)
C   --   IF2EL - IN - the element number for each face (if element)
C   --   VAR - IN - the variable values
C   --   REAPCK - SCRATCH - size = NNPSUR (if nodal) or LENF(NELBLK)
C   --      (if element)
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      CHARACTER TYP
      INTEGER NPSURF(*)
      INTEGER LENF(0:*)
      INTEGER IF2EL(*)
      REAL VAR(*)
      REAL REAPCK(*)

      INTEGER PUTRR
      CHARACTER*8 TAG
      LOGICAL ISOK

      IF (TYP .EQ. 'N') THEN
         DO 100 I = 1, NNPSUR
            INP = NPSURF(I)
            REAPCK(I) = VAR(INP)
  100    CONTINUE
         N = NNPSUR
      ELSE
         N = 0
         DO 120 I = 1, NELBLK
            DO 110 J = LENF(I-1)+1, LENF(I)
               N = N + 1
               IEL = IF2EL(N)
               REAPCK(N) = VAR(IEL)
  110       CONTINUE
  120    CONTINUE
      END IF

      TAG = 'MESHVAR'
      L = N
      IERR = PUTRR (TAG, REAPCK, L)
      CALL CHKPUT (IERR, TAG, L, ISOK)
      IF (.NOT. ISOK) RETURN 1

      RETURN
      END
