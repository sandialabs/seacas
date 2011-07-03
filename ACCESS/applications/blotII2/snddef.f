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

C $Id: snddef.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: snddef.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:12:37  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:57:30  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE SNDDEF (OPTION, DEFOK, DEFFAC, IXDEF, IYDEF, IZDEF,
     &   UNMESH, ALMESH, *)
C=======================================================================

C   --*** SNDDEF *** (BLOT) Send deformation initialization
C   --   Written by Amy Gilkey - revised 05/04/88
C   --
C   --SNDDEF sends the initial deformation constants and the limits
C   --of the undeformed and deformed mesh as a message.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to send all, else message options:
C   --   DEFOK - IN - true iff displacement variables exist
C   --   DEFFAC - IN - the calculated magnification factor
C   --   IXDEF, IYDEF, IZDEF - IN - the indices of the displacement variables
C   --   UNMESH - IN - the limits of the entire undeformed mesh
C   --   ALMESH - IN - the limits of the entire deformed mesh
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      LOGICAL DEFOK
      REAL UNMESH(6), ALMESH(6)

      INTEGER PUTRR
      CHARACTER*8 TAG
      LOGICAL ISOK
      REAL REAPCK(20)

      N = 0
      IF (DEFOK) THEN
         REAPCK(N+1) = 1.0
      ELSE
         REAPCK(N+1) = 0.0
      END IF
      N = N + 1
      REAPCK(N+1) = DEFFAC
      N = N + 1
      REAPCK(N+1) = IXDEF
      N = N + 1
      REAPCK(N+1) = IYDEF
      N = N + 1
      REAPCK(N+1) = IZDEF
      N = N + 1
      CALL CPYREA (6, UNMESH, REAPCK(N+1))
      N = N + 6
      CALL CPYREA (6, ALMESH, REAPCK(N+1))
      N = N + 6

      TAG = 'DEFDEF'
      L = N
      IERR = PUTRR (TAG, REAPCK, L)
      CALL CHKPUT (IERR, TAG, L, ISOK)
      IF (.NOT. ISOK) RETURN 1

      RETURN
      END
