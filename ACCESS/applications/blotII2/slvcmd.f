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

C $Id: slvcmd.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: slvcmd.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:12:32  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:57:28  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE SLVCMD (A, LENF, NLNKF, KLINKF, KXN, KYN, KZN,
     &   KIF2EL, KNPSUR, NEWELB, IELBST, RFIELD, *)
C=======================================================================

C   --*** SLVCMD *** (BLOT) Receive and process a command message
C   --   Written by Amy Gilkey - revised 05/17/88
C   --
C   --SLVCMD receives a message in the form of a command.  Normally the
C   --message requests information which is then sent to the master.
C   --
C   --Parameters:
C   --   A - IN - the dynamic memory base array
C   --   LENF - IN/OUT - the cumulative face counts by element block
C   --      LENF(0) is always 0
C   --      LENF(1..NELBLK) is the end of the surface faces of element block (i)
C   --      LENF(NELBLK+1) is the end of the interior faces
C   --      LENF(NELBLK+2) is the end of the faces that are dead
C   --      LENF(NELBLK+3) is the end of the faces outside a cut
C   --      LENF(NELBLK+4) is the end of the faces in a non-selected block
C   --   NLNKF - IN - the number of nodes per face
C   --   KLINKF - IN/OUT - the index of LINKF - the connectivity for all faces
C   --   KXN, KYN, KZN - IN - the indices of XN, YN, ZN -
C   --      the nodal coordinates (ZN for 3D only)
C   --   KIF2EL - IN/OUT - the index of IF2EL - the element number of each face
C   --   KNPSUR - IN/OUT - the index of NPSURF - the indices of the surface
C   --      nodes
C   --   NEWELB - IN/OUT - the new element blocks flag:
C   --      0 = no new element blocks (set elsewhere)
C   --      1 = new selected element blocks
C   --      2 = new displayed element blocks (implies new selected blocks)
C   --   IELBST - IN/OUT - the element block status:
C   --      -1 = OFF, 0 = ON, but not selected, 1 = selected
C   --   RFIELD - SCRATCH - for the request message

      include 'dbnums.blk'
      include 'd3nums.blk'
      include 'dbnumgq.blk'
      include 'mshopt.blk'
      include 'cutopt.blk'

      common /dualpr/ proces
      character*8 proces

      DIMENSION A(*)
      INTEGER LENF(0:NELBLK+4)
      INTEGER NLNKF(NELBLK)
      INTEGER NEWELB
      INTEGER IELBST(NELBLK)

      INTEGER GETRR
      REAL RFIELD(*)

      CHARACTER*8 VERB
      LOGICAL SEL
      LOGICAL LDUM
      LOGICAL ISOK
      CHARACTER TYP
      CHARACTER CDUM

      LOGICAL FIRST
      SAVE FIRST
C      --FIRST - true iff first time through routine

      DATA FIRST / .TRUE. /

      IF (FIRST) THEN
         FIRST = .FALSE.
         NEWELB = 0
         DO 100 I = 1, NELBLK
            IELBST(I) = 1
  100    CONTINUE
         NALVAR = 0
         DEADNP = .FALSE.
         NEWCUT = .FALSE.
         ISCUT = .FALSE.
      END IF

  110 CONTINUE
      LEXP = 999
      VERB = 'COMMAND'
      IERR = GETRR (VERB, RFIELD, LEXP, NUMFLD)
      CALL CHKGET (IERR, VERB, VERB, NUMFLD, NUMFLD, ISOK)
      IF (.NOT. ISOK) RETURN 1
      IFLD = 0

      IF (VERB .EQ. ' ') THEN
         CONTINUE

C *** MESH Commands ***

      ELSE IF (VERB .EQ. 'MESHLINK') THEN
         ISTEP = INT(RFIELD(IFLD+1))
         IFLD = IFLD + 1

         NALVAR = INT(RFIELD(IFLD+1))
         IFLD = IFLD + 1
         IF (NALVAR .GT. 0) THEN
            DEADNP = (NINT (RFIELD(IFLD+1)) .NE. 0)
         ELSE
            DEADNP = .FALSE.
         END IF
         IFLD = IFLD + 1

         NEWELB = INT(RFIELD(IFLD+1))
         IFLD = IFLD + 1

         DO 120 I = 1, NELBLK
            IELBST(I) = INT(RFIELD(IFLD+1))
            IFLD = IFLD + 1
  120    CONTINUE

         NEWCUT = (NINT (RFIELD(IFLD+1)) .NE. 0)
         IFLD = IFLD + 1

         ISCUT = (NINT (RFIELD(IFLD+1)) .NE. 0)
         IFLD = IFLD + 1
         IF (ISCUT) THEN
            CALL CPYREA (3, RFIELD(IFLD+1), CUTPT)
            IFLD = IFLD + 3
            CALL CPYREA (3, RFIELD(IFLD+1), CUTNRM)
            IFLD = IFLD + 3
         END IF

C      --Calculate the new surface faces
         CALL MSGEOM (A, 'BLOT', ISTEP,
     &      LENF, NLNKF, KLINKF, KXN, KYN, KZN,
     &      KIF2EL, NEWELB, IELBST, LDUM, LDUM)

C      --Find the dead nodes, if needed
         IF (DEADNP) THEN
            CALL MDRSRV ('IDN2B', KDN2B, NUMNPF)
            IF (IS3DIM) THEN
               CALL MDFIND ('IF2EL2', KIF2E2, IDUM)
            ELSE
               KIF2E2 = 1
               KE2ELB = 1
            END IF
            CALL MDFIND ('IE2ELB', KE2ELB, IDUM)
            CALL MDSTAT (NERR, MEM)
            IF (NERR .GT. 0) GOTO 180

            CALL MAKD2B (LENF, NLNKF, A(KLINKF), IELBST,
     &         A(KIF2EL), A(KIF2E2), A(KE2ELB), A(KDN2B))
         END IF

C      --Compute NPSURF nodes for 3D
         CALL MDLONG ('NPSURF', KNPSUR, 0)
         CALL MDLONG ('NPSURF', KNPSUR, NUMNPF)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL MAKSUR (LENF, NLNKF, A(KLINKF),
     &      DEADNP, A(KDN2B), A(KNPSUR))

         CALL MDLONG ('NPSURF', KNPSUR, NNPSUR)

C      --Send the new surface faces
         CALL CNTLNK (NELBLK, LENF, NLNKF, N, IDUM)
         CALL MDRSRV ('SCR', KPACK, N)
         CALL MDRSRV ('NPIX', KNPIX, NUMNP)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180
         CALL SNDMLK ('*', NELBLK, LENF, NLNKF, A(KIF2EL), A(KLINKF),
     &      NNPSUR, A(KNPSUR), A(KNPIX), A(KPACK), *180)
         CALL MDDEL ('SCR')
         CALL MDDEL ('NPIX')

C      --Send the new surface face node numbers and coordinates
         CALL MDRSRV ('SCR', KPACK, NDIM*NNPSUR)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180
         CALL SNDMXY ('HNC', NNPSUR, A(KNPSUR), NDIM,
     &      A(KXN), A(KYN), A(KZN), DEADNP, IDUM, IDUM, A(KPACK), *180)
         CALL MDDEL ('SCR')

         IF (DEADNP) THEN
            CALL MDRSRV ('SCR', KPACK, NNPSUR)
            CALL MDSTAT (NERR, MEM)
            IF (NERR .GT. 0) GOTO 180
            CALL SNDMXY ('D', NNPSUR, A(KNPSUR), NDIM, RDUM, RDUM, RDUM,
     &         DEADNP, A(KDN2B), A(KPACK), RDUM, *180)
            CALL MDDEL ('SCR')
            CALL MDDEL ('IDN2B')
         END IF

      ELSE IF (VERB .EQ. 'MESHVAR') THEN
         ISTEP = INT(RFIELD(IFLD+1))
         IFLD = IFLD + 1
         NUMVAR = INT(RFIELD(IFLD+1))
         IFLD = IFLD + 1

         MAXLEN = 0
         CALL MDRSRV ('SCRVAR', KVAR, MAXLEN)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         DO 130 NVAR = 1, NUMVAR
            IXVAR = INT(RFIELD(IFLD+1))
            IFLD = IFLD + 1
            CALL DBVTYP (IXVAR, TYP, IDUM)
            IF (TYP .EQ. 'H') THEN
               LENVAR = NVARHI
            ELSE IF (TYP .EQ. 'G') THEN
               LENVAR = NVARGL
            ELSE IF (TYP .EQ. 'N') THEN
               LENVAR = NUMNP
            ELSE IF (TYP .EQ. 'E') THEN
               LENVAR = NUMEL
            END IF

            IF (MAXLEN .LT. LENVAR) THEN
               MAXLEN = MAX (MAXLEN, LENVAR)
               CALL MDLONG ('SCRVAR', KVAR, 0)
               CALL MDLONG ('SCRVAR', KVAR, MAXLEN)
               CALL MDSTAT (NERR, MEM)
               IF (NERR .GT. 0) GOTO 180
            END IF

            CALL GETVAR (A, IXVAR, -999, ISTEP, LENVAR, A(KVAR))

            IF (ISTEP .GE. 1) THEN
               IF ((TYP .EQ. 'H') .OR. (TYP .EQ. 'G')) THEN
                  CALL SNDVAR ('*', LENVAR, A(KVAR), *160)
               ELSE IF (TYP .EQ. 'N') THEN
                  CALL MDRSRV ('SCR', KPACK, NNPSUR)
                  CALL MDSTAT (NERR, MEM)
                  IF (NERR .GT. 0) GOTO 180
                  CALL SNDMVR ('*', TYP,
     &               NNPSUR, A(KNPSUR), IDUM, IDUM, IDUM, A(KVAR),
     &               A(KPACK), *160)
                  CALL MDDEL ('SCR')
               ELSE IF (TYP .EQ. 'E') THEN
                  CALL SNDMVR ('*', TYP,
     &               IDUM, IDUM, NELBLK, LENF, A(KIF2EL), A(KVAR),
     &               A(KPACK), *160)
               END IF
            END IF
  130    CONTINUE

         CALL MDDEL ('SCRVAR')

C *** LIST Commands ***

      ELSE IF (VERB .EQ. 'COORDINA') THEN

         CALL SNDXYZ ('*', NDIM, NUMNP, A(KXN), A(KYN), A(KZN), *180)

      ELSE IF (VERB .EQ. 'MAP') THEN
         CALL MDFIND ('MAPEL', KMAPEL, IDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL SNDMAP ('*', NUMEL, A(KMAPEL), *180)

      ELSE IF ((VERB .EQ. 'LINK') .OR. (VERB .EQ. 'CONNECTI')) THEN

         CALL MDFIND ('LENE', KLENE, IDUM)
         CALL MDFIND ('NUMLNK', KNLNKE, IDUM)
         CALL MDFIND ('LINK', KLINKE, IDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL SNDELB ('C', NELBLK,
     &      IDUM, A(KLENE), A(KNLNKE), IDUM, A(KLINKE), RDUM,
     &      IDUM, *180)

      ELSE IF (VERB .EQ. 'ATTRIBUT') THEN

         CALL MDFIND ('LENE', KLENE, IDUM)
         CALL MDFIND ('NUMATR', KNATR, IDUM)
         CALL MDFIND ('ATRIB', KATRIB, IDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL SNDELB ('A', NELBLK,
     &      IDUM, A(KLENE), IDUM, A(KNATR), IDUM, A(KATRIB),
     &      IDUM, *180)

      ELSE IF (VERB .EQ. 'NNODES') THEN

         CALL MDFIND ('IXNNPS', KIXNNS, IDUM)
         CALL MDFIND ('LTNNPS', KLTNNS, IDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL SNDNPS ('N', NUMNPS, LNPSNL,
     &      IDUM, IDUM, A(KIXNNS), A(KLTNNS), RDUM, IDUM, *180)

      ELSE IF (VERB .EQ. 'NFACTORS') THEN

         CALL MDFIND ('IXNNPS', KIXNNS, IDUM)
         CALL MDFIND ('FACNPS', KFACNS, IDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL SNDNPS ('F', NUMNPS, LNPSNL,
     &      IDUM, IDUM, A(KIXNNS), IDUM, A(KFACNS), IDUM, *180)

      ELSE IF (VERB .EQ. 'SELEMS') THEN

         CALL MDFIND ('IXEESS', KIXESS, IDUM)
         CALL MDFIND ('LTEESS', KLTESS, IDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL SNDESS ('E', NUMESS, LESSEL, LESSNL,
     &      IDUM, IDUM, IDUM, A(KIXESS), IDUM,
     &      A(KLTESS), IDUM, RDUM, IDUM, *180)

      ELSE IF (VERB .EQ. 'SNODES') THEN

         CALL MDFIND ('IXNESS', KIXNSS, IDUM)
         CALL MDFIND ('LTNESS', KLTNSS, IDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL SNDESS ('N', NUMESS, LESSEL, LESSNL,
     &      IDUM, IDUM, IDUM, IDUM, A(KIXNSS),
     &      IDUM, A(KLTNSS), RDUM, IDUM, *180)

      ELSE IF (VERB .EQ. 'SFACTORS') THEN

         CALL MDFIND ('IXNESS', KIXNSS, IDUM)
         CALL MDFIND ('FACESS', KFACSS, IDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         CALL SNDESS ('F', NUMESS, LESSEL, LESSNL,
     &      IDUM, IDUM, IDUM, IDUM, A(KIXNSS),
     &      IDUM, IDUM, A(KFACSS), IDUM, *180)

      ELSE IF (VERB .EQ. 'VARIABLE') THEN
         ISTEP = INT(RFIELD(IFLD+1))
         IFLD = IFLD + 1
         NUMVAR = INT(RFIELD(IFLD+1))
         IFLD = IFLD + 1

         MAXLEN = 0
         CALL MDRSRV ('SCRVAR', KVAR, MAXLEN)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

         DO 140 NVAR = 1, NUMVAR
            IXVAR = INT(RFIELD(IFLD+1))
            IFLD = IFLD + 1
            CALL DBVTYP (IXVAR, TYP, IDUM)
            IF (TYP .EQ. 'H') THEN
               LENVAR = NVARHI
            ELSE IF (TYP .EQ. 'G') THEN
               LENVAR = NVARGL
            ELSE IF (TYP .EQ. 'N') THEN
               LENVAR = NUMNP
            ELSE IF (TYP .EQ. 'E') THEN
               LENVAR = NUMEL
            END IF

            IF (MAXLEN .LT. LENVAR) THEN
               MAXLEN = MAX (MAXLEN, LENVAR)
               CALL MDLONG ('SCRVAR', KVAR, 0)
               CALL MDLONG ('SCRVAR', KVAR, MAXLEN)
               CALL MDSTAT (NERR, MEM)
               IF (NERR .GT. 0) GOTO 180
            END IF

            CALL GETVAR (A, IXVAR, -999, ISTEP, LENVAR, A(KVAR))
            IF (ISTEP .GE. 1) THEN
               CALL SNDVAR ('*', LENVAR, A(KVAR), *160)
            END IF
  140    CONTINUE

         CALL MDDEL ('SCRVAR')

      ELSE IF (VERB .EQ. 'SCALE') THEN
         IXVAR = INT(RFIELD(IFLD+1))
         IFLD = IFLD + 1
         CALL DBVTYP (IXVAR, TYP, IDUM)
         IF (TYP .EQ. 'E') THEN
            NALV = INT(RFIELD(IFLD+1))
            IFLD = IFLD + 1
            SEL = (NINT (RFIELD(IFLD+1)) .NE. 0)
            IFLD = IFLD + 1
            IF (SEL) THEN
               DO 150 I = 1, NELBLK
                  IELBST(I) = INT(RFIELD(IFLD+1))
                  IFLD = IFLD + 1
  150          CONTINUE
            END IF
            NALV = 0
            SEL = .FALSE.
         END IF

         CALL SCALER (A, 0, CDUM, IXVAR,
     &      SEL, IELBST, NALV, RDUM, RDUM)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 180

C *** GENERAL Commands ***

      ELSE IF (VERB .EQ. 'WAIT') THEN
         GOTO 170

      ELSE IF (VERB .EQ. 'EXIT') THEN
         GOTO 180

      ELSE
         CALL PRTERR ('CMDERR', 'Unknown command ' // VERB)
      END IF

  160 CONTINUE
      GOTO 110

  170 CONTINUE
      RETURN

  180 CONTINUE
      RETURN 1
      END
