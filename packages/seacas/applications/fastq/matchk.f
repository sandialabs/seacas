C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

C
C
C
      LOGICAL FUNCTION MATCHK (MXND, I1, I2, J1, J2, LXN)
C***********************************************************************
C
C  FUNCTION MATCHK = CHECKS THE CURRENT COLAPSED LINES TO SEE IF THEY
C                    CAN BE JOINED WITHOUT AFFECTING THE BOUNDARY.
C                    I1 & I2 MAY END UP SWITCHED WITH J1 & J2.
C
C***********************************************************************
C
      DIMENSION LXN (4, MXND)
C
      IF ( (LXN (2, I1) .LT. 0) .OR. (LXN (2, I2) .LT. 0) .OR.
     &   (LXN (2, J1) .LT. 0) .OR. (LXN (2, J2) .LT. 0) ) THEN
C
C  FIRST CHECK FOR COMPLETELY HOOKED BOUNDARY LINES.
C
         IF ((LXN (2, J1) .LT. 0) .AND. (LXN (2, J2) .LT. 0)) THEN
            MATCHK = .FALSE.
         ELSEIF ( ((LXN (2, I1) .LT. 0) .AND. (LXN (2, J2) .LT. 0)) .OR.
     &      ((LXN (2, I2) .LT. 0) .AND. (LXN (2, J1) .LT. 0)))
     &      THEN
            MATCHK = .FALSE.
         ELSE
            MATCHK = .TRUE.
         ENDIF
      ELSE
         MATCHK = .TRUE.
      ENDIF
C
      RETURN
C
      END
