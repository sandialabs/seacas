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

C $Id: rcvini.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: rcvini.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:08  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:55:56  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE RCVINI (OPTION, TITLE,
     &   NDIM, NUMNP, NUMEL, NELBLK,
     &   NUMNPS, LNPSNL, NUMESS, LESSEL, LESSNL, *)
C=======================================================================

C   --*** RCVINI *** (BLOT) Receive database title and initial variables
C   --   Written by Amy Gilkey - revised 05/09/88
C   --
C   --RCVINI receives the title and the initial variables as a message.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to receive all, else message options:
C   --   TITLE - OUT - the database title
C   --   NDIM - OUT - the number of coordinates per node
C   --   NUMNP - OUT - the number of nodes
C   --   NUMEL - OUT - the number of elements
C   --   NELBLK - OUT - the number of element blocks
C   --   NUMNPS - OUT - the number of node sets
C   --   LNPSNL - OUT - the length of the node sets node list
C   --   NUMESS - OUT - the number of side sets
C   --   LESSEL - OUT - the length of the side sets element list
C   --   LESSNL - OUT - the length of the side sets node list
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      CHARACTER*80 TITLE

      INTEGER GETCR, GETIR
      CHARACTER*8 TAGEXP, TAG
      INTEGER INTPCK(12)
      LOGICAL ISOK

      TAGEXP = 'TITLE'
      LEXP = 80
      IERR = GETCR (TAG, TITLE, LEXP, L)
      CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
      IF (.NOT. ISOK) RETURN 1

      TAGEXP = 'INITIAL'
      LEXP = 9
      IERR = GETIR (TAG, INTPCK, LEXP, L)
      CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
      IF (.NOT. ISOK) RETURN 1

      N = 0
      NDIM = INTPCK(N+1)
      N = N + 1
      NUMNP = INTPCK(N+1)
      N = N + 1
      NUMEL = INTPCK(N+1)
      N = N + 1
      NELBLK = INTPCK(N+1)
      N = N + 1
      NUMNPS = INTPCK(N+1)
      N = N + 1
      LNPSNL = INTPCK(N+1)
      N = N + 1
      NUMESS = INTPCK(N+1)
      N = N + 1
      LESSEL = INTPCK(N+1)
      N = N + 1
      LESSNL = INTPCK(N+1)
      N = N + 1

      RETURN
      END
