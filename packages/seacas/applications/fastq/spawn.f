C    Copyright(C) 1999-2020 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C    
C    See packages/seacas/LICENSE for details

C
C
      SUBROUTINE SPAWN (VAXVMS)
C***********************************************************************
C
C  SUBROUTINE SPAWN = SPAWNS A PROCESS FOR ESCAPE OUT OF FASTQ
C
C***********************************************************************
C
C  VARIABLES USED:
C     VAXVMS = .TRUE. IF RUNNING ON A VAXVMS SYSTEM
C
C***********************************************************************
C
      LOGICAL VAXVMS
C
      IF (VAXVMS) THEN
         continue
      ELSE
         CALL MESAGE ('SPAWNING POSSIBLE ONLY ON VAXVMS SYSTEM')
      ENDIF
C
      END
