C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

c
C
C
      SUBROUTINE DMESS (DEV1, TEXT)
C***********************************************************************
C
C  SUBROUTINE DMESS = PRINTS A ONE LINE MESSAGE AT THE BOTTOM OF THE
C                       SCREEN
C
C***********************************************************************
C
      CHARACTER*(*) TEXT, DEV1*3
C
      CALL MESAGE (TEXT)
      RETURN
C
      END
