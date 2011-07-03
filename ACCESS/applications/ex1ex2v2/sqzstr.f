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
      SUBROUTINE SQZSTR (STRING, LSTR)
C=======================================================================
C$Id: sqzstr.f,v 1.1 2009/06/09 13:32:43 gdsjaar Exp $
C$Log: sqzstr.f,v $
CRevision 1.1  2009/06/09 13:32:43  gdsjaar
CReceived permission to assert copyright on nem_spread and ex1ex2v2.
C
CBoth are released under the BSD Open Source licence.
C
CRevision 1.1  1994/01/24 23:00:54  vryarbe
CInitial revision
C
c Revision 1.1.1.1  1990/08/14  16:16:22  gdsjaar
c Testing
c
c Revision 1.1  90/08/14  16:16:21  gdsjaar
c Initial revision
c 
c Revision 1.1  90/08/09  13:39:45  gdsjaar
c Initial revision
c 

C   --*** SQZSTR *** (STRLIB) Remove extra blanks from string
C   --   Written by Amy Gilkey - revised 06/02/87
C   --
C   --SQZSTR deletes leading and extra blanks within a string.
C   --To prevent problems, an empty string is returned with a length of 1.
C   --
C   --Parameters:
C   --   STRING - IN/OUT - the string to be compressed, returned, may be
C   --      up to 132 characters long
C   --   LSTR - OUT - the new string length

C   --Routines Called:
C   --   LENSTR - (STRLIB) Find string length

      CHARACTER*(*) STRING
      INTEGER LSTR

      CHARACTER*132 TMPSTR

      IF (STRING .EQ. ' ') RETURN

      LSTR = LENSTR (STRING)

      IBLK = INDEX (STRING, '  ')
  100 CONTINUE
      IF ((IBLK .GT. 0) .AND. (IBLK .LE. LSTR)) THEN

         INOBLK = IBLK + 2
  110    CONTINUE
         IF (STRING(INOBLK:INOBLK) .EQ. ' ') THEN
            INOBLK = INOBLK + 1
            GOTO 110
         END IF

         TMPSTR = STRING(INOBLK:LSTR)
         STRING(IBLK+1:LSTR) = TMPSTR
         LSTR = LSTR - (INOBLK-IBLK-1)
         IBLK = INDEX (STRING, '  ')
         GOTO 100
      END IF

      IF (STRING(1:1) .EQ. ' ') THEN
         TMPSTR = STRING(2:LSTR)
         STRING(1:LSTR) = TMPSTR
         LSTR = LSTR - 1
      END IF

      RETURN
      END
