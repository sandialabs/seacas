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
      SUBROUTINE LNKVAR (*)
C=======================================================================
C $Id: lnkvar.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: lnkvar.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:26  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.5  2008/03/14 13:45:27  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.4  1997/09/08 17:18:47  gdsjaar
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
C Revision 1.3  1995/10/03 21:36:21  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:21:12  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:42:51  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.2  1993/02/18  16:20:46  gdsjaar
c Upped number of equations from 100 to 512.  Changed version number to 2.03
c
c Revision 1.1.1.1  1990/11/09  16:24:47  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:24:46  gdsjaar
c Initial revision
c 

C   --*** LNKVAR *** (ALGEBRA) Link equation references with variables
C   --   Written by Amy Gilkey - revised 11/30/87
C   --
C   --LNKVAR links the variables in the equation with the /VAR../ arrays.
C   --Each equation variable (from /ENT../) is found in the /VAR../
C   --arrays, and the /VAR../ index is stored in the INXENT array.
C   --Note that since the /VAR../ arrays were constructed from the
C   --equation variables, each variable will be found.
C   --
C   --Parameters:
C   --   * - return statement if an error is found; message is printed
C   --
C   --Common Variables:
C   --   Sets INXENT, VALENT of /ENT../
C   --   Uses NAMENT, TYPENT of /ENT../
C   --   Uses NAMVAR, TYPVAR, IDVAR, ISTVAR, NUMINP, IXLHS of /VAR../

      include 'params.blk'
      include 'numeqn.blk'
      include 'ent.blk'
      include 'var.blk'
      
      PARAMETER (ICURTM = 1, ILSTTM = 2, IONETM = 3)

      NUMLHS = MAXVAR - IXLHS + 1

      DO 110 NEQN = 1, NUMEQN
         DO 100 NENT = 1, NUMENT(NEQN)
            IF (TYPENT(NENT,NEQN) .EQ. 'V') THEN

C            --If searching for a LHS variable (INXENT < 0),
C            --   then search the assigned variables, NAMVAR(IXLHS..MAXVAR)
C            --   else search the input variables, NAMVAR(1..NUMINP)

               IF (INXENT(NENT,NEQN) .LT. 0) THEN
                  IV = LOCSTR (NAMENT(NENT,NEQN),
     &               NUMLHS, NAMVAR(IXLHS))
                  IF (IV .GT. 0) IV = IXLHS + IV - 1
               ELSE
                  IV = LOCSTR (NAMENT(NENT,NEQN), NUMINP, NAMVAR(1))
               END IF

               IF (IV .LE. 0) THEN
                  CALL PRTERR ('PROGRAM', 'Variable "'
     &               // NAMENT(NENT,NEQN) // '" cannot be found')
                  GOTO 120
               END IF

               INXENT(NENT,NEQN) = IV

               IF ((TYPVAR(IV) .EQ. 'T')
     &            .OR. (TYPVAR(IV) .EQ. 'G')) THEN
c               IF ((TYPVAR(IV) .EQ. 'T')
c     &            .OR. (TYPVAR(IV) .EQ. 'H')
c     &            .OR. (TYPVAR(IV) .EQ. 'G')) THEN
                  IF (NAMVAR(IV) .EQ. '.GLOBAL') THEN
c                  IF ((NAMVAR(IV) .EQ. '.HISTORY')
c     &               .OR. (NAMVAR(IV) .EQ. '.GLOBAL')) THEN
                     VALENT(NENT,NEQN) =
     &                  VALENT(NENT,NEQN) + IDVAR(IV) - 1
                  ELSE
                     VALENT(NENT,NEQN) = IDVAR(IV)
                  END IF
               END IF

            END IF
  100    CONTINUE
  110 CONTINUE
      RETURN

  120 CONTINUE
      RETURN 1
      END
