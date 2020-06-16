C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

C
C
C
      FUNCTION IOCCUR (N, L, NEW)
C***********************************************************************
C
C  FUNCTION IOCCUR = CHECKS TO SEE IF NEW OCCURS IN  (L (I), I=1, N)
C
C***********************************************************************
C
C     RETURN 0 IF NEW DOES NOT OCCUR IN  (L (I), I=1, N)
C     RETURN 1 IF IT DOES
C
C***********************************************************************
C
      DIMENSION L (N)
C
      IF (N .LT. 1) THEN
         IOCCUR = 0
         RETURN
      ENDIF
C
      DO 100 I = 1, N
         IF (L (I) .EQ. NEW) THEN
            IOCCUR = 1
            RETURN
         ENDIF
  100 CONTINUE
      IOCCUR = 0
      RETURN
      END
