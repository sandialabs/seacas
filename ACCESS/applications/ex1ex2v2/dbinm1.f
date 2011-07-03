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
      SUBROUTINE DBINM1 (NDB, OPTION, NELBLK, NVAREL, ISEVOK, IEVOK,
     &   IERR, NELBDM, *)
C=======================================================================
C$Id: dbinm1.f,v 1.1 2009/06/09 13:32:43 gdsjaar Exp $
C$Log: dbinm1.f,v $
CRevision 1.1  2009/06/09 13:32:43  gdsjaar
CReceived permission to assert copyright on nem_spread and ex1ex2v2.
C
CBoth are released under the BSD Open Source licence.
C
CRevision 1.1  1994/01/24 23:00:52  vryarbe
CInitial revision
C
c Revision 1.1.1.1  1990/08/14  16:12:51  gdsjaar
c Testing
c
c Revision 1.1  90/08/14  16:12:50  gdsjaar
c Initial revision
c 
c Revision 1.1  90/08/09  13:39:10  gdsjaar
c Initial revision
c 

C   --*** DBINM1 *** (EXOLIB) Internal to DBINAM
C   --   Written by Amy Gilkey - revised 02/18/88
C   --
C   --DBINM1 reads the element block variable truth table.
C   --
C   --Parameters:
C   --   NDB - IN - the database number
C   --   OPTION - IN - ' ' to not store, '*' to store all, else store options:
C   --      'T' to store element block variable truth table
C   --   NELBLK - IN - the number of element blocks
C   --   NVAREL - IN - the number of element variables
C   --   ISEVOK - OUT - the element block variable truth table;
C   --      variable i of block j exists iff ISEVOK(j,i)
C   --   IEVOK - OUT - the element block variable truth table;
C   --      variable i of block j exists iff ISEVOK(j,i) is NOT 0
C   --   IERR - OUT - the returned read error flag
C   --   * - return statement if error encountered, including end-of-file;
C   --      NO message is printed
C   --
C   --Database must be positioned in front of truth table upon entry;
C   --upon exit positioned after table.

      INTEGER NDB
      CHARACTER*(*) OPTION
      INTEGER NELBLK, NVAREL
c      LOGICAL ISEVOK(nvarel,*)
      integer ISEVOK(nvarel,*)
      INTEGER IEVOK(nvarel,*)
      INTEGER IERR

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'T') .GT. 0)) THEN
         READ (NDB, END=120, ERR=120, IOSTAT=IERR)
     &      ((IEVOK(i,IELB), I=1,NVAREL), IELB=1,NELBLK)

         DO 110 IELB = 1, NELBLK
           DO 100 I = 1, NVAREL
               if (ievok(i,ielb) .ne. 0) then
                  isevok(i,ielb) = 1
               endif
c               ISEVOK(i,IELB) = (IEVOK(i, IELB) .NE. 0)
  100       CONTINUE
  110    CONTINUE
      ELSE
         READ (NDB, END=120, ERR=120, IOSTAT=IERR)
      END IF

      RETURN

  120 CONTINUE
      RETURN 1
      END
