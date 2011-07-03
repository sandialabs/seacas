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

C $Id: rcvnam.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: rcvnam.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:21  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:56:03  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE RCVNAM (OPTION, NDIM, NELBLK,
     &   NNDIM, NNELB, NVARHI, NVARGL, NVARNP, NVAREL,
     &   NAMECO, NAMELB, NAMES, IXHV, IXGV, IXNV, IXEV,
     &   A, KIEVOK, EXODUS, *)
C=======================================================================

C   --*** RCVNAM *** (BLOT) Receive database names
C   --   Written by Amy Gilkey - revised 05/19/88
C   --
C   --RCVNAM receives the names of the coordinates, the element block types,
C   --and the database variables as a message.  All names are converted to
C   --uppercase and all embedded blanks within a name are removed.  The
C   --element block variable truth table is also received.
C   --
C   --Note that the numbers of variables are read in this routine.
C   --
C   --This routine calls DBVINI and uses DBVIX to get the variable name
C   --indices.
C   --
C   --Dynamic memory is reserved in this routine.  If there is a problem,
C   --the routine returns normally without printing an error message.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to receive all, else message options:
C   --      'H' to receive number of names
C   --      'C' to receive coordinate names
C   --      'B' to receive element block names
C   --      'V' to receive variables names
C   --      'T' to receive element block variable truth table
C   --   NDIM - IN - the number of coordinates per node
C   --   NELBLK - IN - the number of element blocks
C   --   NNDIM - OUT - the number of coordinates per node; <0 if end-of-file
C   --      (if OPTION)
C   --   NNELB - OUT - the number of element blocks; <0 if end-of-file
C   --      (if OPTION)
C   --   NVARHI - OUT - the number of history variables; <0 if end-of-file
C   --      (if OPTION)
C   --   NVARGL - OUT - the number of global variables; <0 if end-of-file
C   --      (if OPTION)
C   --   NVARNP - OUT - the number of nodal variables; <0 if end-of-file
C   --      (if OPTION)
C   --   NVAREL - OUT - the number of element variables; <0 if end-of-file
C   --      (if OPTION)
C   --   NAMECO - OUT - the names of the coordinates; max size = 6 (if OPTION)
C   --      (if OPTION)
C   --   NAMELB - OUT - the names of the element block types; max size = 256
C   --      (if OPTION)
C   --   VNAMES - OUT - the names of the variables; max size = 256 (if OPTION)
C   --   IXHV - OUT - the VNAMES index of the history variable names (if OPTION)
C   --   IXGV - OUT - the VNAMES index of the global variable names (if OPTION)
C   --   IXNV - OUT - the VNAMES index of the nodal variable names (if OPTION)
C   --   IXEV - OUT - the VNAMES index of the element variable names (if OPTION)
C   --   A - OUT - the dynamic memory base array
C   --   KIEVOK - OUT - the dynamic memory index of the element block variable
C   --      truth table; (if OPTION)
C   --      variable i of block j exists iff ISEVOK(j,i)
C   --   EXODUS - OUT - false if GENESIS file, true if EXODUS file so far
C   --   * - return statement if message error

      include 'params.blk'
      CHARACTER*(*) OPTION
      CHARACTER*(MXSTLN) NAMECO(*)
      CHARACTER*(MXSTLN) NAMELB(*)
      CHARACTER*(MXSTLN) NAMES(*)
      DIMENSION A(*)
      LOGICAL EXODUS

      INTEGER GETCR, GETLR, GETIR
      CHARACTER*8 TAGEXP, TAG
      INTEGER INTPCK(12)
      CHARACTER*(MXSTLN) STRPCK(6+256+256)
      LOGICAL ISOK

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'H') .GT. 0)) THEN
         TAGEXP = 'NAMES'
         LEXP = 7
         IERR = GETIR (TAG, INTPCK, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1

         N = 0
         NNDIM = INTPCK(N+1)
         N = N + 1
         NNELB = INTPCK(N+1)
         N = N + 1
         NVARHI = INTPCK(N+1)
         N = N + 1
         NVARGL = INTPCK(N+1)
         N = N + 1
         NVARNP = INTPCK(N+1)
         N = N + 1
         NVAREL = INTPCK(N+1)
         N = N + 1
         EXODUS = (INTPCK(N+1) .NE. 0)
         N = N + 1

C      --Initialize for DBVTYP and DBVIX
         CALL DBVINI (NVARHI, NVARGL, NVARNP, NVAREL)
      END IF

      N = 0
      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'C') .GT. 0)) THEN
         N = N + NNDIM
      END IF
      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'B') .GT. 0)) THEN
         N = N + NNELB
      END IF
      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'V') .GT. 0)) THEN
         IF (EXODUS) THEN
            N = N + NVARHI
            N = N + NVARGL
            N = N + NVARNP
            N = N + NVAREL
         END IF
      END IF

      IF (N .GT. 0) THEN
         TAGEXP = 'NAMES'
         LEXP = N * 8
         IERR = GETCR (TAG, STRPCK, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1

         N = 0
         IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'C') .GT. 0)) THEN
            CALL CPYSTR (NNDIM, STRPCK(N+1), NAMECO)
            N = N + NNDIM
         END IF
         IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'B') .GT. 0)) THEN
            CALL CPYSTR (NNELB, STRPCK(N+1), NAMELB)
            N = N + NNELB
         END IF
         IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'V') .GT. 0)) THEN
            IF (EXODUS) THEN

C            --Get the name indices
               CALL DBVIX ('H', 1, IXHV)
               CALL DBVIX ('G', 1, IXGV)
               CALL DBVIX ('N', 1, IXNV)
               CALL DBVIX ('E', 1, IXEV)

               CALL CPYSTR (NVARHI, STRPCK(N+1), NAMES(IXHV))
               N = N + NVARHI
               CALL CPYSTR (NVARGL, STRPCK(N+1), NAMES(IXGV))
               N = N + NVARGL
               CALL CPYSTR (NVARNP, STRPCK(N+1), NAMES(IXNV))
               N = N + NVARNP
               CALL CPYSTR (NVAREL, STRPCK(N+1), NAMES(IXEV))
               N = N + NVAREL
            END IF
         END IF
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'T') .GT. 0)) THEN
         IF (EXODUS) THEN
            CALL MDRSRV ('ISEVOK', KIEVOK, NELBLK * NVAREL)
            CALL MDSTAT (NERR, MEM)
            IF (NERR .GT. 0) GOTO 100

            IF ((NELBLK .GT. 0) .AND. (NVAREL .GT. 0)) THEN
               TAGEXP = 'NAMES'
               LEXP = NELBLK * NVAREL
               IERR = GETLR (TAG, A(KIEVOK), LEXP, L)
               CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
               IF (.NOT. ISOK) RETURN 1
            END IF
         END IF
      END IF

  100 CONTINUE
      RETURN
      END
