C    Copyright(C) 1999-2020, 2025 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C
C    See packages/seacas/LICENSE for details
C=======================================================================
      REAL FUNCTION FMYAMOD (PARM1, PARM2)
C=======================================================================

C   --*** MYAMOD *** (ALGEBRA) amod function
C   --
C   --   PARM1, PARM2 - IN - the maximum parameters

      FMYAMOD = MOD(PARM1, PARM2)

      RETURN
      END
