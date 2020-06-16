C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

c
C
C
      SUBROUTINE PUTCRS (X, Y, OLDCUR)
C***********************************************************************
C
C     SUBROUTINE PUTCRS = PLACES THE CROSSHAIRS AT THE CURRENT LOCATION
C
C***********************************************************************
C
C
      DIMENSION IDUM(2)
C
      LOGICAL OLDCUR
C
      CHARACTER DUMMY*16
C
C  SELECT DECIMAL MODE
C
      DUMMY = CHAR(27)
      DUMMY(2:4) = 'OR1'
      WRITE(*,*)DUMMY
C
C  PLACE THE CROSSHAIRS AT THE RIGHT LOCATION
C
      CALL MP2PT(1, X, Y, X1, Y1, IDUM)
      IX = INT(X1*4151.)
      IY = INT(Y1*4151.)
      DUMMY(1:1) = CHAR(27)
      DUMMY(2:2) = 'P'
      WRITE(DUMMY(3:8), '(I6)')IX
      DUMMY(9:9) = ','
      WRITE(DUMMY(10:15), '(I6)')IY
      DUMMY(16:16) = ','
      WRITE(*,*)DUMMY
C
C  DESELECT DECIMAL MODE
C
      DUMMY = CHAR(27)
      DUMMY(2:4) = 'OR0'
      WRITE(*,*)DUMMY
C
      IF(.NOT.OLDCUR)THEN
C
C  ACTIVATE THE CROSSHAIRS
C
         DUMMY = CHAR(27)
         DUMMY(2:3) = 'G1'
         WRITE(*,*)DUMMY
         OLDCUR = .TRUE.
      ENDIF
C
      WRITE(*, '(A)')' '//CHAR(27)//'[2J'
      RETURN
C
      END
