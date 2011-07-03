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

C $Id: filnpf.f,v 1.1 2009/03/25 04:47:53 gdsjaar Exp $
C $Log: filnpf.f,v $
C Revision 1.1  2009/03/25 04:47:53  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:00:50  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:50:34  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE FILNPF (NLNKF, LINKF1, NFACES, MAXNPF, NPFS, NOVER)
C=======================================================================

C   --*** FILNPF *** (MESH) Point to face from NPFS
C   --   Written by Amy Gilkey - revised 10/27/87
C   --              Sam Key, 06/01/85
C   --
C   --FILNPF puts a pointer to the given face into each of the face's node
C   --NPFS array.
C   --
C   --Parameters:
C   --   NLNKF - IN - the number of nodes per face
C   --   LINKF1 - IN - the connectivity for the face
C   --   NFACES - IN - the face number
C   --   MAXNPF - IN - the maximum length of the NPFS entry
C   --   NPFS - IN/OUT - the list of unmatched faces containing a node;
C   --      (0,i) = the length of the list
C   --   NOVER - IN/OUT - the number of overrun errors

      INTEGER LINKF1(NLNKF)
      INTEGER NPFS(0:MAXNPF,*)

      DO 100 ILINK = 1, NLNKF
         INF = LINKF1(ILINK)
         IF (NPFS(0,INF) .LT. MAXNPF) THEN
            L = NPFS(0,INF) + 1
            NPFS(L,INF) = NFACES
            NPFS(0,INF) = L
         ELSE
            NOVER = NOVER + 1
         END IF
  100 CONTINUE

      RETURN
      END
