C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

C
C
C
      SUBROUTINE FLAGD (MDIM, N, LINK, INUM, FLAG)
C***********************************************************************
C
C  SUBROUTINE FLAGD = FLAGS THE DATA TO BE PLOTTED
C
C***********************************************************************
C
      DIMENSION LINK(2,MDIM), INUM(MDIM)
C
      LOGICAL FLAG, ADDLNK
C
      ADDLNK = .FALSE.
C
      DO 100 I = 1, N
         CALL LTSORT (MDIM, LINK, I, II, ADDLNK)
         IF (II .GT. 0) THEN
            IF (FLAG) THEN
               INUM(II) = -IABS (INUM (II))
            ELSE
               INUM(II) = IABS (INUM (II))
            ENDIF
         ENDIF
  100 CONTINUE
      RETURN
      END
