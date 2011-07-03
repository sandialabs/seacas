C    Copyright(C) 2008 Sandia Corporation.  Under the terms of Contract
C    DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C    certain rights in this software
C    
C    Redistribution and use in source and binary forms, with or without
C    modification, are permitted provided that the following conditions are
C    met:
C    
C    * Redistributions of source code must retain the above copyright
C       notice, this list of conditions and the following disclaimer.
C              
C    * Redistributions in binary form must reproduce the above
C      copyright notice, this list of conditions and the following
C      disclaimer in the documentation and/or other materials provided
C      with the distribution.
C                            
C    * Neither the name of Sandia Corporation nor the names of its
C      contributors may be used to endorse or promote products derived
C      from this software without specific prior written permission.
C                                                    
C    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
C    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
C    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
C    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
C    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
C    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
C    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
C    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
C    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
C    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
C    

C=======================================================================
      SUBROUTINE INITIM (MAXTIM, NSTEPS, TIMES, TMIN, TMAX,
     &                   DELT, NINTV, NPTIMS, IPTIMS)
C=======================================================================
C $Id: initim.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: initim.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:26  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.4  2008/03/14 13:45:27  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.3  1995/10/03 21:36:17  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:21:09  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:42:43  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:24:35  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:24:34  gdsjaar
c Initial revision
c 

C   --*** INITIM *** (TIMSEL) Initialize time step parameters
C   --   Written by Amy Gilkey - revised 11/12/87
C   --
C   --INITIM initializes the time step parameters.  The initialization
C   --is dependent on the MAXTIM parameter.
C   --
C   --Parameters:
C   --   MAXTIM - IN - the initialization parameter
C   --      =0 - select all times
C   --      >0 - select NINTV interval times (up to number of steps - 1)
C   --           with interval starting at TMIN + offset
C   --      <0 - select -NINTV interval times (up to number of steps)
C   --           with interval starting at TMIN
C   --   NSTEPS - IN -  the number of time steps
C   --   TIMES  - IN -  the database times
C   --   TMIN   - OUT - the minimum selected time; set to minimum time
C   --   TMAX   - OUT - the maximum selected time; set to maximum time
C   --   DELT   - OUT - the interval between selected times
C   --                  (<0 = selected times)
C   --                  set to 0 (for all times) or interval
C   --   NINTV  - IN/OUT - the number of times between tmin and tmax to select
C   --                     (negative for zero interval);
C   --   NPTIMS - OUT - the number of selected times
C   --   IPTIMS - OUT - the selected time step numbers

C   --Routines Called:
C   --   MINMAX - (ETCLIB) Find minimum and maximum value
C   --   MINMXL - (ETCLIB) Find minimum and maximum value of selected values
C   --   NUMEQL - (ETCLIB) Count the number of equal values

      INTEGER MAXTIM
      INTEGER NSTEPS
      REAL TIMES(*)
      REAL TMIN
      REAL TMAX
      INTEGER IPTIMS(*)

       CALL MINMAX (NSTEPS, TIMES, TMIN, TMAX)

       NSTEPX = NSTEPS

      IF (MAXTIM .NE. 0) THEN
         IF (MAXTIM .GT. 0) THEN
            NINTV = MIN (MAXTIM, NSTEPX-1)
         ELSE
            NINTV = - MIN (MAXTIM, NSTEPX)
         END IF
         DELT = 999.0

      ELSE IF (MAXTIM .EQ. 0) THEN
         DELT = 0.0
         NINTV = 0
      END IF

      NPTIMS = -999
      CALL CALTIM (TMIN, TMAX, DELT, NINTV,
     &             NSTEPS, TIMES, NPTIMS, IPTIMS)

      RETURN
      END
