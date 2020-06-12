C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

c
C
C
      SUBROUTINE B4BAD (MXND, MLN, XN, YN, LXK, KXL, NXL, LXN, LNODES,
     &   ANGLE, I1, I2, J1, J2, NLOOP, KOUNTL, BOK, ERR)
C***********************************************************************
C
C  SUBROUTINE BCROSS = CHECKS TO MAKE SURE THAT A BOUNDARY IS NOT
C                      BECOMING A PERMANENT CROSS
C
C***********************************************************************
C
      DIMENSION NXL (2, 3*MXND), LXN (4, MXND)
      DIMENSION LXK (4, MXND), KXL (2, 3*MXND)
      DIMENSION LNODES (MLN, MXND), ANGLE (MXND)
      DIMENSION NODE (4)
C
      LOGICAL BOK, ERR
C
      BOK = .TRUE.
      ERR = .FALSE.
C
C  GET THE NODES THAT FORM THE REMAINING ELEMENT
C
      IF (KOUNTL .EQ. 4) THEN
         NODE(1) = I2
         NODE(2) = LNODES (3, NODE(1))
         NODE(3) = LNODES (3, NODE(2))
         NODE(4) = LNODES (3, NODE(3))
      ELSEIF (NLOOP - KOUNTL - 2 .EQ. 4) THEN
         NODE(1) = I1
         NODE(2) = LNODES (3, J2)
         NODE(3) = LNODES (3, NODE(2))
         NODE(4) = LNODES (3, NODE(3))
      ELSE
         GOTO 110
      ENDIF
C
C  NOW CHECK ALL THE NODES TO SEE IF THEY ARE ON THE BOUNDARY
C  AND CAN BE CLASSIFIED AS CORNERS
C
      DO 100 I = 1, 4
         IF ( (LXN (2, NODE (I)) .LT. 0) .AND.
     &      (LNODES (6, NODE (I)) .GE. 3) ) THEN
            BOK = .FALSE.
            GOTO 110
         ENDIF
  100 CONTINUE
C
  110 CONTINUE
C
      RETURN
C
      END
