C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

C
C
C
      SUBROUTINE SETLOP (MXND, MLN, NLOOP, LNODES, NODE, IVALUE, ERR)
C***********************************************************************
C
C  SUBROUTINE SETLOP = MARKS ALL THE NODES IN THE LOOP AS DESIGNATED
C
C***********************************************************************
C
      DIMENSION LNODES (MLN, MXND)
C
      LOGICAL ERR
C
      ERR = .FALSE.
C
      KOUNT = 0
      INOW = NODE
C
  100 CONTINUE
      INOW = LNODES (3, INOW)
      LNODES (1, INOW) = IVALUE
C
      IF (INOW .EQ. NODE) RETURN
C
      KOUNT = KOUNT + 1
      IF (KOUNT .GT. NLOOP) THEN
         CALL MESAGE('PROBLEMS IN SETLOP WITH LOOP NOT CLOSING')
         ERR = .TRUE.
         GOTO 110
      ENDIF
      GOTO 100
C
  110 CONTINUE
C
      RETURN
C
      END
