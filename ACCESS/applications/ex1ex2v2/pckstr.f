C Copyright (C) 2009 Sandia Corporation.  Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C certain rights in this software
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
C 
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
C 

C=======================================================================
      SUBROUTINE PCKSTR (NSTR, STR)
C=======================================================================
C$Id: pckstr.f,v 1.1 1994/01/24 23:00:54 vryarbe Exp $
C$Log: pckstr.f,v $
CRevision 1.1  1994/01/24 23:00:54  vryarbe
CInitial revision
C
c Revision 1.1.1.1  1990/08/14  16:16:06  gdsjaar
c Testing
c
c Revision 1.1  90/08/14  16:16:05  gdsjaar
c Initial revision
c 
c Revision 1.1  90/08/09  13:39:43  gdsjaar
c Initial revision
c 

C   --*** PCKSTR *** (STRLIB) Remove all blanks from string
C   --   Written by Amy Gilkey - revised 03/21/88
C   --
C   --PCKSTR removes all imbedded blanks (left-justified) from an array
C   --of strings.
C   --
C   --Parameters:
C   --   NSTR - IN - the number of strings to be packed
C   --   STR - IN/OUT - the array of strings, returned packed, may be up
C   --      to 132 characters long

      INTEGER NSTR
      CHARACTER*(*) STR(*)

      CHARACTER*132 TMPSTR

      DO 20 I = 1, NSTR
         LSTR = LENSTR (STR(I))

         IBLK = INDEX (STR(I), ' ')
   10    CONTINUE
         IF ((IBLK .GT. 0) .AND. (IBLK .LT. LSTR)) THEN
            TMPSTR = STR(I)(IBLK+1:LSTR)
            STR(I)(IBLK:LSTR) = TMPSTR
            LSTR = LSTR - 1
            IBLK = INDEX (STR(I), ' ')
            GOTO 10
         END IF

   20 CONTINUE

      RETURN
      END
