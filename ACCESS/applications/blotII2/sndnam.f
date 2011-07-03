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

C $Id: sndnam.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: sndnam.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:13:10  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
CRevision 1.2  1990/12/14  08:57:46  gdsjaar
CAdded RCS Id and Log to all files
C
C=======================================================================
      SUBROUTINE SNDNAM (OPTION, NDIM, NELBLK,
     &   NNDIM, NNELB, NVARHI, NVARGL, NVARNP, NVAREL,
     &   NAMECO, NAMELB, NAMEHV, NAMEGV, NAMENV, NAMEEV,
     &   ISEVOK, EXODUS, *)
C=======================================================================

C   --*** SNDNAM *** (BLOT) Send database names
C   --   Written by Amy Gilkey - revised 05/04/88
C   --
C   --SNDNAM sends the names of the coordinates, the element block types,
C   --and the database variables as a message.  The element block variable
C   --truth table is also sent.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to send all, else message options:
C   --      'H' to send number of names
C   --      'C' to send coordinate names
C   --      'B' to send element block names
C   --      'V' to send variables names
C   --      'T' to send element block variable truth table
C   --   NDIM - IN - the number of coordinates per node
C   --   NELBLK - IN - the number of element blocks
C   --   NNDIM - IN - the number of coordinates per node; <0 if end-of-file
C   --      (if OPTION)
C   --   NNELB - IN - the number of element blocks; <0 if end-of-file
C   --      (if OPTION)
C   --   NVARHI - IN - the number of history variables; <0 if end-of-file
C   --      (if OPTION)
C   --   NVARGL - IN - the number of global variables; <0 if end-of-file
C   --      (if OPTION)
C   --   NVARNP - IN - the number of nodal variables; <0 if end-of-file
C   --      (if OPTION)
C   --   NVAREL - IN - the number of element variables; <0 if end-of-file
C   --      (if OPTION)
C   --   NAMECO - IN - the names of the coordinates; max size = 6 (if OPTION)
C   --      (if OPTION)
C   --   NAMELB - IN - the names of the element block types; max size = 256
C   --      (if OPTION)
C   --   VNAMES - IN - the names of the variables; max size = 256 (if OPTION)
C   --   NAMEHV - IN - the history variable names (if OPTION)
C   --   NAMEGV - IN - the global variable names (if OPTION)
C   --   NAMENV - IN - the nodal variable names (if OPTION)
C   --   NAMEEV - IN - the element variable names (if OPTION)
C   --   ISEVOK - IN - the element block variable truth table; (if OPTION)
C   --      variable i of block j exists iff ISEVOK(j,i)
C   --   EXODUS - IN - false if GENESIS file, true if EXODUS file so far
C   --   * - return statement if message error

      include 'params.blk'
      CHARACTER*(*) OPTION
      CHARACTER*(*) NAMECO(*)
      CHARACTER*(*) NAMELB(*)
      CHARACTER*(*) NAMEHV(*)
      CHARACTER*(*) NAMEGV(*)
      CHARACTER*(*) NAMENV(*)
      CHARACTER*(*) NAMEEV(*)
      LOGICAL ISEVOK(NELBLK,NVAREL)
      LOGICAL EXODUS

      INTEGER PUTCR, PUTLR, PUTIR
      CHARACTER*8 TAG
      LOGICAL ISOK
      INTEGER INTPCK(12)
      CHARACTER*(MXSTLN) STRPCK(6+256+256)

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'H') .GT. 0)) THEN
         N = 0
         INTPCK(N+1) = NNDIM
         N = N + 1
         INTPCK(N+1) = NNELB
         N = N + 1
         INTPCK(N+1) = NVARHI
         N = N + 1
         INTPCK(N+1) = NVARGL
         N = N + 1
         INTPCK(N+1) = NVARNP
         N = N + 1
         INTPCK(N+1) = NVAREL
         N = N + 1
         IF (EXODUS) THEN
            INTPCK(N+1) = 1
         ELSE
            INTPCK(N+1) = 0
         END IF
         N = N + 1

         TAG = 'NAMES'
         L = N
         IERR = PUTIR (TAG, INTPCK, L)
         CALL CHKPUT (IERR, TAG, L, ISOK)
         IF (.NOT. ISOK) RETURN 1
      END IF

      N = 0
      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'C') .GT. 0)) THEN
         CALL CPYSTR (NNDIM, NAMECO, STRPCK(N+1))
         N = N + NNDIM
      END IF
      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'B') .GT. 0)) THEN
         CALL CPYSTR (NNELB, NAMELB, STRPCK(N+1))
         N = N + NNELB
      END IF
      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'V') .GT. 0)) THEN
         IF (EXODUS) THEN
            CALL CPYSTR (NVARHI, NAMEHV, STRPCK(N+1))
            N = N + NVARHI
            CALL CPYSTR (NVARGL, NAMEGV, STRPCK(N+1))
            N = N + NVARGL
            CALL CPYSTR (NVARNP, NAMENV, STRPCK(N+1))
            N = N + NVARNP
            CALL CPYSTR (NVAREL, NAMEEV, STRPCK(N+1))
            N = N + NVAREL
         END IF
      END IF

      IF (N .GT. 0) THEN
         TAG = 'NAMES'
         L = N * 8
         IERR = PUTCR (TAG, STRPCK, L)
         CALL CHKPUT (IERR, TAG, L, ISOK)
         IF (.NOT. ISOK) RETURN 1
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'T') .GT. 0)) THEN
         IF (EXODUS .AND. (NELBLK .GT. 0) .AND. (NVAREL .GT. 0)) THEN
            TAG = 'NAMES'
            L = NELBLK * NVAREL
            IERR = PUTLR (TAG, ISEVOK, L)
            CALL CHKPUT (IERR, TAG, L, ISOK)
            IF (.NOT. ISOK) RETURN 1
         END IF
      END IF

      RETURN
      END
