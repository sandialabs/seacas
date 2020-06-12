C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

c
C
C
      SUBROUTINE CNTCRN (MXND, MXCORN, MLN, LNODES, LCORN, NCORN,
     &   NLOOP, N1, ERR)
C***********************************************************************
C
C  SUBROUTINE CNTCRN = COUNTS THE CURRENT DESIGNATED CORNER LENGTHS
C
C***********************************************************************
C
      DIMENSION LNODES (MLN, MXND), LCORN (MXCORN)
C
      LOGICAL ERR
C
      ERR = .FALSE.
C
C  COUNT THE CURRENT CORNERS STARTING AT THE I COUNTER
C
      NODE = N1
      NOLD = N1
      KOUNT = 0
      NCORN = 0
      KKC = 0
      KOUNTC = 0
      LASTC = 0
  100 CONTINUE
      KOUNT = KOUNT + 1
      IF (KOUNT .GT. NLOOP) THEN
         CALL MESAGE ('PROBLEM IN CNTCRN WITH UNCLOSED LOOP')
         ERR = .TRUE.
         GOTO 110
      ENDIF
C
C  A NEW CORNER NODE HAS BEEN FOUND
C
      IF (LNODES (1, NODE) .EQ. 1) THEN
C
C  ADD UP THE NUMBER OF NODES FROM THE LAST "NON-SIDE"
C
         NCORN = NCORN + 1
         IF (NCORN .LE. MXCORN) LCORN(NCORN) = NODE
         IF (NCORN .GT. 1) THEN
            LNODES (7, LASTC) = KOUNTC + 1
         ELSE
            KKC = KOUNTC + 1
         ENDIF
         LASTC = NODE
         KOUNTC = 0
C
C  THIS IS A SIDE - JUST CONTINUE
C
      ELSE
         KOUNTC = KOUNTC + 1
      ENDIF
C
C  CHECK FOR COMPLETION OF THE LOOP
C
      NODE = LNODES (3, NODE)
      IF (NODE .NE. NOLD) GOTO 100
C
C  GET THE FIRST CORNER'S DISTANCE FROM PREVIOUS CORNER CORRECT
C
      LNODES (7, LASTC) = KKC + KOUNTC
C
  110 CONTINUE
C
      RETURN
C
      END
