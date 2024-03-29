C    Copyright(C) 1999-2020, 2023 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C
C    See packages/seacas/LICENSE for details
      SUBROUTINE SRCHC(IVEC,ILBIN,IUBIN,NUM,ICOD,LOC)

C     THIS SUBROUTINE SEARCHES ROW ILB THROUGH ROW IUB OF A
C     NUMERICALLY ORDERED CHARACTER COLUMN VECTOR FOR THE OCCURRENCE
C     THE VALUE NUM.
C     IF NUM IS FOUND ICOD IS SET TO UNITY AND LOC IS THE ROW NUMBER
C     WHERE NUM RESIDES. IF NUM IS NOT FOUND ICOD IS ZERO AND LOC IS
C     THE ROW NUMBER WHERE NUM WOULD RESIDE IF IT WERE IN THE NUMER-
C     ICALLY ORDER LIST.   BOB LUST

C     THIS SUBROUTINE HAS BEEN CHANGED FROM BOB LUST'S VERSION
C     AND NOW ASSUMES THAT THERE IS NO MORE THAN ONE MATCH
C     IN THE ORDERED LIST 'IVEC'.  BILL MILLS-CURRAN  JAN. 1, 1983

C     IVEC    ORDERED CHARACTER LIST (SINGLE COLUMN)

C     ILBIN   LOW NUMBERED ROW OF SEARCH RANGE

C     IUBIN   HIGH NUMBERED ROW OF SEARCH RANGE

C     NUM     VALUE TO BE LOCATED IN IVEC

C     ICOD    RETURN CODE  0 = NO MATCH   1 = MATCH

C     LOC     LOCATION IN IVEC FOR NUM

      CHARACTER*(*) IVEC(1),NUM

      ILB = ILBIN
      IUB = IUBIN
      ICOD=0
      IF (IUB .LT. ILB) THEN
         LOC = 1
         RETURN
      END IF

C     CHECK TO SEE IF NUM IS AT EITHER END OF LIST

      IF(IVEC(ILB).GT.NUM)THEN
         LOC=ILB
         RETURN
      ELSE IF(IVEC(IUB).LT.NUM) THEN
         LOC=IUB+1
         RETURN
      END IF

C     NUM IS INTERNAL TO IVEC

  100 MID=(ILB+IUB)/2
      IF(MID.LE.ILB)GO TO 110

C     SEARCH RANGE IS MORE THAN 2

      IF(IVEC(MID).LT.NUM) THEN

C        UPPER PART OF LIST

         ILB=MID
         GO TO 100
      ELSE IF(IVEC(MID).GT.NUM) THEN

C        LOWER PART OF LIST

         IUB=MID
         GO TO 100
      ELSE

C        MATCH HAS OCCURRED AT "MID"

         ICOD=1
         LOC=MID
         RETURN
      END IF
  110 CONTINUE

C     SEARCH RANGE IS 2 OR LESS

      IF(NUM.EQ.IVEC(ILB)) THEN

C        MATCH AT "ILB"

         ICOD=1
         LOC=ILB
         RETURN
      ELSE IF(NUM.EQ.IVEC(IUB)) THEN

C        MATCH AT "IUB"

         ICOD=1
         LOC=IUB
         RETURN
      ELSE

C        NO MATCH IN LIST.
C        LOCATION FOR NEW ENTRY IS "IUB".

         LOC=IUB
         RETURN
      END IF
      END
