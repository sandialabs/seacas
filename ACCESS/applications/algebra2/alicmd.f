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
      SUBROUTINE ALICMD (INLINE, INTYP, CFIELD, IFIELD, NAMES, *)
C=======================================================================
C $Id: alicmd.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: alicmd.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:25  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.6  2008/03/14 13:45:25  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.5  1997/09/08 17:18:38  gdsjaar
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
C Revision 1.4  1996/02/05 21:57:31  caforsy
C Changed data statements so program would run on both the SGI and IBM.
C Variables in named common blocks cannot be set using data statements.
C
C Revision 1.3  1995/10/03 21:35:55  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:20:34  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:41:42  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:23:24  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:23:22  gdsjaar
c Initial revision
c 

C   --*** ALICMD *** (ALGEBRA) Perform ALIAS command
C   --   Written by Amy Gilkey - revised 12/02/87
C   --
C   --ALICMD processes the input ALIAS command.  It adds the alias and
C   --the equivalent variables to the /ALIAS../ arrays.
C   --
C   --Parameters:
C   --   INLINE - IN/OUT - the parsed input lines for the log file
C   --   INTYP - IN - the field types
C   --   CFIELD - IN - the character fields
C   --   IFIELD - IN - the integer fields
C   --   NAMES - IN - the names of the global, nodal, and element variables
C   --   * - return statement if command not executed
C   --
C   --Common Variables:
C   --   Sets NUMALI, NAMALI, NIXALI, IXALI of /ALIAS../
C   --   Uses NVARHI, NVARGL, NVARNP, NVAREL of /DBNUMS/

      include 'params.blk'
      include 'alias.blk'
      include 'dbnums.blk'

      CHARACTER*(*) INLINE
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)
      INTEGER IFIELD(*)
      CHARACTER*(mxstln) NAMES(*)

      LOGICAL FFEXST, FFNUMB
      CHARACTER*(mxstln) NAME
      CHARACTER TYPNAM, TYP

c      DATA NUMALI / 0 /
      NUMALI = 0 

      IF (.NOT. FFEXST (1, INTYP)) THEN
         CALL PRTERR ('CMDERR', 'No options on ALIAS command')
         GOTO 130
      END IF

      IF (NUMALI .GE. MAXALI) THEN
         CALL PRTERR ('CMDERR', 'ALIAS table full, command ignored')
         GOTO 130
      END IF

      NVNAMS = NVARHI + NVARGL + NVARNP + NVAREL

C   --Get the tensor name

      IFLD = 1
      CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', NAME)

      IVAR = LOCSTR (NAME, NVNAMS, NAMES)
      IF (IVAR .GT. 0) THEN
         CALL PRTERR ('CMDERR', 'ALIAS name must be unique')
         GOTO 130
      END IF

      IVAR = LOCSTR (NAME, NUMALI, NAMALI)
      IF (IVAR .GT. 0) THEN
         CALL PRTERR ('CMDERR', 'ALIAS name must be unique')
         GOTO 130
      END IF

      IALI = NUMALI + 1
      NAMALI(IALI) = NAME
      CALL FFADDC (NAME, INLINE)

C   --Get and find the first variable name

      CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', NAME)
      CALL FFADDC (NAME, INLINE)

      IVAR = LOCSTR (NAME, NVNAMS, NAMES)
      IF (IVAR .GT. 0) THEN
         NIXALI(IALI) = 1
         IXALI(1,IALI) = IVAR
         CALL DBVTYP (IVAR, TYPNAM, IDUM)
      ELSE
         CALL PRTERR ('CMDERR',
     &      'Invalid variable name "' // NAME(:LENSTR(NAME)) // '"')
         GOTO 130
      END IF

C   --If a number follows, assign the next n variables

      IF (FFNUMB (IFLD, INTYP)) THEN
         CALL FFINTG (IFLD, INTYP, IFIELD,
     &      'number of variables', 1, NIXALI(IALI), *130)
         CALL FFADDI (NIXALI(IALI), INLINE)

         IF (NIXALI(IALI) .GT. MAXALN) THEN
            CALL PRTERR ('CMDERR', 'Too many variables to alias')
            GOTO 130
         END IF

         I = IVAR + NIXALI(IALI) - 1
         CALL DBVTYP (I, TYP, IDUM)
         IF (TYPNAM .NE. TYP) THEN
            CALL PRTERR ('CMDERR', 'Variables do not exist')
            GOTO 130
         END IF

         DO 100 I = 1, NIXALI(IALI)
            IXALI(I,IALI) = IVAR + I - 1
  100    CONTINUE

C   --Otherwise assign the named variables

      ELSE

  110    CONTINUE
         IF (FFEXST (IFLD, INTYP)) THEN

            CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', NAME)

            IVAR = LOCSTR (NAME, NVNAMS, NAMES)
            IF (IVAR .GT. 0) THEN
               NIXALI(IALI) = NIXALI(IALI) + 1
               IF (NIXALI(IALI) .GT. MAXALN) THEN
                  CALL PRTERR ('CMDERR', 'Too many variables to alias')
                  GOTO 130
               END IF

               IXALI(NIXALI(IALI),IALI) = IVAR

               CALL DBVTYP (IVAR, TYP, IDUM)
               IF (TYPNAM .NE. TYP) THEN
                  CALL PRTERR ('CMDERR',
     &               'Variables must be of the same type')
                  GOTO 120
               END IF

            ELSE
               CALL PRTERR ('CMDERR',
     &            'Invalid variable name' // NAME(:LENSTR(NAME)))
               GOTO 120
            END IF

            CALL FFADDC (NAME, INLINE)
  120       CONTINUE
            GOTO 110
         END IF
      END IF

      NUMALI = IALI

      RETURN

  130 CONTINUE
      RETURN 1
      END
