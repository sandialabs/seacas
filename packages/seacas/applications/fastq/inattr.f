C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

C
C
C
      SUBROUTINE INATTR (MS, MR, MA, N17, N23, JJ, RIN, IFOUND, ATTRIB,
     &   LINKM, NOROOM)
C***********************************************************************
C
C  SUBROUTINE INATTR = INPUTS MATERIAL ATTRIBUTES INTO THE DATABASE
C
C***********************************************************************
C
      DIMENSION ATTRIB (MA, MR+MS), LINKM (2,  (MS+MR))
C
      LOGICAL NOROOM, ADDLNK
C
      NOROOM = .TRUE.
      ADDLNK = .FALSE.
C
C  UPDATE THE COUNTER IF NEEDED
C
      IF (JJ.GT.N23)N23 = JJ
C
C  ADD THE ATTRIBUTES INTO THE DATABASE
C
      N17 = N17 + 1
      J = N17
      IF (J .GT. (MS + MR))RETURN
      CALL LTSORT (MS + MR, LINKM, JJ, IPNTR, ADDLNK)
      ADDLNK = .TRUE.
      IF (IPNTR .LE. 0)THEN
         J = -J
         MINUSJ  =  -J
         CALL LTSORT (MS + MR, LINKM, JJ, MINUSJ, ADDLNK)
         J = IABS (MINUSJ)
      ELSE
         CALL LTSORT (MS + MR, LINKM, JJ, J, ADDLNK)
      ENDIF
      IF (IFOUND .GT. MA)THEN
         IEND = MA
         WRITE (*, 10000)IFOUND, MA
      ELSE
         IEND = IFOUND
      ENDIF
      DO 100 I = 1, IEND
         ATTRIB (J, I) = RIN (I)
  100 CONTINUE
C
      NOROOM = .FALSE.
C
      RETURN
C
10000 FORMAT (' FOR MATERIAL NUMBER:', I5,
     &   ' NUMBER OF ATTRIBUTES READ:',  I5, /,
     &   '                               EXCEEDS MAX ALLOWED OF:',
     &   I5)
C
      END
