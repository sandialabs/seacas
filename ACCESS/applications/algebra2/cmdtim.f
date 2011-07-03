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
      SUBROUTINE CMDTIM (INLINE, VERB, IFLD, INTYP, CFIELD, IFIELD,
     &   RFIELD,NSTEPS, TIMES, TMIN, TMAX,
     &   DELT, NINTV, NPTIMS, IPTIMS)
C=======================================================================
C $Id: cmdtim.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: cmdtim.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:25  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.5  2008/03/14 13:45:25  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.4  1997/09/08 17:18:40  gdsjaar
C Minor cleanup of some exodusII related issues.  The cleanups fall in
C two distinct areas:
C 1. Handling of 32 character names
C 2. Nonexistence of History variables (or special treatment...)
C
C In the first area, changes were made in several areas to permit the
C user to reference and create variable names with >8 characters. The
C reading and writing of these worked correctly, but users couldn't
C enter names >8 characters from the command line
C
C In the second area: In exodusII, there is not really the concept of
C history variables as they existed in exodusI. If needed, a separate
C exodus file is written with a different timestep frequency.  In the
C original version of algebra, TIME and any variable that was a function
C of TIME was a history variable. This was carried over into algebra2
C except that history variables are not written to the database, so the
C user couldn't do any functions involving time.  This has been changed
C to use GLOBAL variables instead of HISTORY.
C
C Minor change: Variable names are uppercased as they are read in from
C the exodus database. This fixes problem with not being able to access
C variables with lowercase names.
C
C Revision 1.3  1995/10/03 21:36:03  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:20:42  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:41:59  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:23:44  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:23:43  gdsjaar
c Initial revision
c 

C   --*** CMDTIM *** (TIMSEL) Process time step parameter command
C   --   Written by Amy Gilkey - revised 05/18/88
C   --
C   --CMDTIM processes a time step parameter command.  The commands are:
C   --   TMIN     - sets the minimum selected time TMIN
C   --   TMAX     - sets the maximum selected time TMAX
C   --   DELTIME  - sets the selected time interval DELT
C   --   ZINTV    - sets the number of selected times NINTV
C   --              with interval starting at TMIN
C   --   NINTV    - sets the number of selected times NINTV
C   --              with interval starting at TMIN + offset
C   --   ALLTIMES - sets DELT = 0 for all times
C   --   TIMES    - selects time steps by time
C   --   STEPS    - selects time steps by number
C   --
C   --Parameters:
C   --   INLINE - IN/OUT - the parsed input lines for the log file
C   --   VERB   - IN/OUT - the time step parameter command verb; set for SHOW
C   --   IFLD   - IN/OUT - the field number
C   --   INTYP  - IN - the input types from the free field reader
C   --   CFIELD - IN - the character fields
C   --   IFIELD - IN - the integer fields
C   --   RFIELD - IN - the real fields
C   --   NSTEPS - IN - the number of time steps
C   --   TIMES  - IN - the database times
C   --   TMIN   - IN/OUT - the minimum selected time
C   --   TMAX   - IN/OUT - the maximum selected time
C   --   DELT   - IN/OUT - the interval between selected times
C   --                     (<0 = selected times)
C   --   NINTV  - IN/OUT - the number of times between tmin and tmax to select
C   --                     (negative for zero interval)
C   --   NPTIMS - IN/OUT - the number of selected times
C   --   IPTIMS - IN/OUT - the selected time step numbers

C   --Routines Called:
C   --   LOCREA - (ETCLIB) Find closest value
C   --   MINMAX - (ETCLIB) Find minimum and maximum value
C   --   MINMXL - (ETCLIB) Find minimum and maximum value of selected values
C   --   NUMEQL - (ETCLIB) Count the number of equal values

      CHARACTER*(*) INLINE(*)
      CHARACTER*(*) VERB
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)
      INTEGER IFIELD(*)
      REAL RFIELD(*)
      REAL TIMES(*)
      INTEGER IPTIMS(*)

      LOGICAL FFEXST, FFMATC
      LOGICAL ISTIME
      CHARACTER*5 STR5

      NSTEPX = NSTEPS

      IF ((VERB .EQ. 'TMIN') .OR. (VERB .EQ. 'TMAX')) THEN
         CALL FFADDC (VERB, INLINE(1))
         CALL MINMAX (NSTEPS, TIMES, TIMMIN, TIMMAX)
         IF (VERB .EQ. 'TMIN') THEN
            CALL FFREAL (IFLD, INTYP, RFIELD,
     &         'minimum time', TIMMIN, X, *120)
            TMIN = X
            CALL FFADDR (TMIN, INLINE(1))
         ELSE
            CALL FFREAL (IFLD, INTYP, RFIELD,
     &         'maximum time', TIMMAX, X, *120)
            TMAX = X
            CALL FFADDR (TMAX, INLINE(1))
         END IF
         IF (TMIN .GT. TMAX) CALL PRTERR ('CMDWARN',
     &      'Minimum time is greater than maximum time')
         IF (DELT .LT. 0) THEN
            NINTV = 0
            DELT = 0.0
         END IF

      ELSE IF ((VERB .EQ. 'NINTV') .OR. (VERB .EQ. 'ZINTV')) THEN
         CALL FFADDC (VERB, INLINE(1))
         IF (VERB .EQ. 'NINTV') THEN
            NT = MIN (10, NSTEPX-1)
         ELSE
            NT = MIN (10, NSTEPX)
         END IF
         CALL FFINTG (IFLD, INTYP, IFIELD,
     &      'number of times', NT, N, *120)
         NINTV = N
         CALL FFADDI (NINTV, INLINE(1))
         IF (VERB .EQ. 'NINTV') THEN
            NINTV = N
         ELSE
            NINTV = -N
         END IF
         DELT = 999.0

      ELSE IF (VERB .EQ. 'DELTIME') THEN
         CALL FFADDC (VERB, INLINE(1))
         NT = - MIN (10, NSTEPX)
         IF (NT .LT. -1) THEN
            DDELT = (TMAX - TMIN) / (-NT-1)
            IF (DDELT .LE. 0.) DDELT = 1.0
         ELSE
            DDELT = 0.0
         END IF
         CALL FFREAL (IFLD, INTYP, RFIELD,
     &      'delta time', DDELT, X, *120)
         DELT = X
         CALL FFADDR (DELT, INLINE(1))
         NINTV = 0

      ELSE IF (VERB .EQ. 'ALLTIMES') THEN
         CALL FFADDC (VERB, INLINE(1))
         DELT = 0.0
         NINTV = 0

      ELSE IF ((VERB .EQ. 'TIMES') .OR. (VERB .EQ. 'STEPS')) THEN
         CALL FFADDC (VERB, INLINE(1))
         ISTIME = (VERB .EQ. 'TIMES')
         VERB = 'DELTIME'
         IF (DELT .GE. 0.0) THEN
            NINTV = NPTIMS
            DELT = -1.0
         END IF

C      --Reset is assumed, unless ADD is first parameter
         IF (.NOT. FFMATC (IFLD, INTYP, CFIELD, 'ADD', 1)) NINTV = 0

  100    CONTINUE
         IF (FFEXST (IFLD, INTYP)) THEN
            IF (ISTIME) THEN
               CALL FFREAL (IFLD, INTYP, RFIELD,
     &            'time, ignored', 0.0, T, *110)
               CALL FFADDR (T, INLINE(1))
               ISTEP = LOCREA (T, NSTEPS, TIMES)
            ELSE
               CALL FFINTG (IFLD, INTYP, IFIELD,
     &            'step, ignored', 0, ISTEP, *110)
               IF ((ISTEP .LE. 0) .OR. (ISTEP .GT. NSTEPS)) THEN
                  CALL INTSTR (1, 0, ISTEP, STR5, LSTR)
                  CALL PRTERR ('CMDERR', 'Step ' // STR5(:LSTR)
     &               // ' does not exist, ignored')
                  GOTO 110
               END IF
               CALL FFADDI (ISTEP, INLINE(1))
            END IF

            IF (LOCINT (ISTEP, NINTV, IPTIMS) .LE. 0) THEN
               NINTV = NINTV + 1
               IPTIMS(NINTV) = ISTEP
            END IF
  110       CONTINUE
            GOTO 100
         END IF
      END IF

      NPTIMS = -999
      CALL CALTIM (TMIN, TMAX, DELT, NINTV,
     &   NSTEPS, TIMES, NPTIMS, IPTIMS)

  120 CONTINUE
      RETURN
      END
