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

C $Id: reqmlk.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: reqmlk.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:45  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:56:18  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE REQMLK (OPTION, ISTEP, NALVAR, DEADNP,
     &   NELBLK, NEWELB, IELBST,
     &   NEWCUT, ISCUT, CUTPT, CUTNRM, REAPCK, *)
C=======================================================================

C   --*** REQMLK *** (BLOT) Request mesh connectivity
C   --   Written by Amy Gilkey - revised 05/04/88
C   --
C   --REQMLK sends a message requesting the mesh connectivity.  All
C   --information needed to define the mesh is sent in the message.
C   --
C   --Parameters:
C   --   OPTION - IN - the information being requested
C   --   ISTEP - IN - the time step number
C   --   NALVAR - IN - the index of the element birth/death variable
C   --   DEADNP - IN - true iff dead nodes are to be displayed
C   --   NELBLK - IN - the number of element blocks
C   --   NEWELB - IN - the new element blocks flag
C   --   IELBST - IN - the element block status
C   --   NEWCUT - IN - true iff faces are not set for the new cut
C   --   ISCUT - IN - true iff cutting plane is defined (3D only)
C   --   CUTPT - IN - A point defining the cutting plane (3D only);
C   --   CUTNRM - IN - The normal to the cutting plane (3D only);
C   --   REAPCK - SCRATCH - size = 1 + 2 + 1+NELBLK + 12
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      LOGICAL DEADNP
      INTEGER IELBST(*)
      LOGICAL NEWCUT
      LOGICAL ISCUT
      REAL CUTPT(3), CUTNRM(3)

      INTEGER PUTRR
      CHARACTER*8 TAG
      LOGICAL ISOK
      REAL REAPCK(*)

      N = 0
      REAPCK(N+1) = ISTEP

      N = N + 1
      REAPCK(N+1) = NALVAR
      N = N + 1
      IF ((NALVAR .GT. 0) .AND. DEADNP) THEN
         REAPCK(N+1) = -1
      ELSE
         REAPCK(N+1) = 0
      END IF
      N = N + 1

      REAPCK(N+1) = NEWELB
      N = N + 1
      DO 100 I = 1, NELBLK
         REAPCK(N+1) = IELBST(I)
         N = N + 1
  100 CONTINUE

      IF (NEWCUT) THEN
         REAPCK(N+1) = -1
      ELSE
         REAPCK(N+1) = 0
      END IF
      N = N + 1
      IF (ISCUT) THEN
         REAPCK(N+1) = -1
         N = N + 1
         CALL CPYREA (3, CUTPT, REAPCK(N+1))
         N = N + 3
         CALL CPYREA (3, CUTNRM, REAPCK(N+1))
         N = N + 3
      ELSE
         REAPCK(N+1) = 0
         N = N + 1
      END IF

      TAG = 'MESHLINK'
      L = N
      IERR = PUTRR (TAG, REAPCK, L)
      CALL CHKPUT (IERR, TAG, L, ISOK)
      IF (.NOT. ISOK) RETURN 1

      RETURN
      END
