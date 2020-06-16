C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

C
C
      REAL FUNCTION NICKS (ANGLE, LXN)
C***********************************************************************
C
C  FUNCTION NICKS = RETURNS THE PENALTY FOR A BAD ANGLE AND A BAD
C                   CONNECTIVITY ON A SIDE
C
C***********************************************************************
C
      DIMENSION LXN(4)
C
      PI = ATAN2(0.0, -1.0)
      ADIFF = ANGLE - PI
C
C  PENALIZE A LARGE ANGLE MORE THAN A SMALL ANGLE
C
      IF (ADIFF .LT. 0.) THEN
         ADIFF = - ADIFF
      ELSE
         ADIFF = ADIFF * 1.2
      ENDIF
C
C  IF THE ANGLE HAS 3 LINES ATTACHED
C  A REGULAR NODE WOULD BE FORMED - PENALIZE IT LIGHTLY
C  USE THIS SAME PENALTY FOR BOUNDARY NODES
C
      IF ( ( (LXN (4) .EQ. 0) .AND. (LXN (3) .NE. 0) ) .OR.
     &   (LXN (2) .LT. 0) ) THEN
         NICKS = ADIFF
C
C  IF THE ANGLE HAS 2 LINES ATTACHED
C  THEN A THREE DEGREE IRREGULAR NODE WOULD BE FORMED
C  PENALIZE IT MORE STRINGENTLY
C
      ELSEIF (LXN (3) .EQ. 0) THEN
         NICKS = ADIFF * 1.3
C
C  IF THE ANGLE HAS 4 LINES ATTACHED
C  THEN A FIVE DEGREE IRREGULAR NODE WOULD BE FORMED
C  PENALIZE IT LESS STRINGENTLY
C
      ELSEIF (LXN (4) .GT. 0) THEN
         NICKS = ADIFF * 1.15
C
C  IF THE ANGLE HAS MORE THAN 4 LINES ATTACHED
C  THEN A FIVE+ DEGREE IRREGULAR NODE WOULD BE FORMED (HIGHLY UNLIKELY)
C  PENALIZE IT SEVERELY
C
      ELSEIF (LXN (3) .EQ. 0) THEN
         NICKS = ADIFF * 1.6
      else
c         write (*,*) 'Undefined Option in nicks',
c     $        adiff, lxn(1), lxn(2), lxn(3), lxn(4)
         nicks = adiff * 2.0
      ENDIF
C
      RETURN
C
      END
