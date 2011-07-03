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
      SUBROUTINE ADDVAR (NAMEQV, NVAR, TYPEQV, IDEQV, NINP, ILHS)
C=======================================================================
C $Id: addvar.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: addvar.f,v $
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
C Revision 1.4  1997/09/08 17:18:36  gdsjaar
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
C Revision 1.3  1995/10/03 21:35:54  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:20:31  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:41:37  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:23:17  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:21:47  gdsjaar
c Initial revision
c 

C   --*** ADDVAR *** (ALGEBRA) Add set of variables to list
C   --   Written by Amy Gilkey - revised 12/14/87
C   --
C   --ADDVAR adds all non-repeated input (INP) variables from a set of
C   --variables to the /VAR../ arrays as both input variables and assigned
C   --(LHS) variables.  History variables are all assigned the input
C   --variable name ".HISTORY".  Global variables are all assigned the input
C   --variable name ".GLOBAL".
C   --
C   --Parameters:
C   --   NAMEQV - IN - the names of the variables in the set
C   --   NVAR - IN - the number of variables in the set
C   --   TYPEQV - IN - the type of all variables in the set
C   --   IDEQV - IN - the index of the first variable in the set;
C   --      following are consecutive
C   --   NINP - IN - the number of input variables in /VAR../ that need
C   --      to be checked
C   --   ILHS - IN - the starting index of the assigned variables in /VAR../
C   --      that need to be checked
C   --
C   --Common Variables:
C   --   Sets NUMINP, IXLHS, NAMVAR, TYPVAR, IDVAR, ISTVAR, IEVVAR of /VAR../

      PARAMETER (ICURTM = 1, ILSTTM = 2, IONETM = 3)
      include 'params.blk'
      include 'var.blk'
      
      CHARACTER*(MXSTLN) NAMEQV(*)
      CHARACTER TYPEQV
      CHARACTER*(mxstln) NAME

      NLHS = MAXVAR - ILHS + 1

      DO 100 IVAR = 1, NVAR

         NAME = NAMEQV(IVAR)

C      --Skip if already an assigned variable

         IF (LOCSTR (NAME, NLHS, NAMVAR(ILHS)) .LE. 0) THEN

C         --If not already an input variable, insert it in the input variables

c            IF (((TYPEQV .NE. 'H') .AND. (TYPEQV .NE. 'G'))
c     &         .OR. (IVAR .EQ. 1)) THEN
            IF ((TYPEQV .NE. 'G') .OR. (IVAR .EQ. 1)) THEN
C            --Only insert first history variable with name .HISTORY
c               IF (TYPEQV .EQ. 'H') NAME = '.HISTORY'

C            --Only insert first global variable with name .GLOBAL
               IF (TYPEQV .EQ. 'G') NAME = '.GLOBAL'

               IF (LOCSTR (NAME, NINP, NAMVAR) .LE. 0) THEN
                  NUMINP = NUMINP + 1
                  IF (NUMINP .LT. IXLHS) THEN
                     NAMVAR(NUMINP) = NAME
                     TYPVAR(NUMINP) = TYPEQV
                     IDVAR(NUMINP) = IDEQV + IVAR - 1
                     ISTVAR(ICURTM,NUMINP) = 1
                     ISTVAR(ILSTTM,NUMINP) = 0
                     ISTVAR(IONETM,NUMINP) = 0
                     IF (TYPEQV .EQ. 'E') THEN
                        IEVVAR(NUMINP) = IDEQV + IVAR - 1
                     ELSE
                        IEVVAR(NUMINP) = -999
                     END IF
                  END IF
               END IF
            END IF

C         --Insert into the LHS array as saved variable

            IXLHS = IXLHS - 1
            IF (NUMINP .LT. IXLHS) THEN
               NAMVAR(IXLHS) = NAMEQV(IVAR)
C               --Do not use NAME here since the global name is wrong
               TYPVAR(IXLHS) = TYPEQV
               IDVAR(IXLHS) = -999
               ISTVAR(ICURTM,IXLHS) = -2
               ISTVAR(ILSTTM,IXLHS) = 0
               ISTVAR(IONETM,IXLHS) = 0
               IF (TYPEQV .EQ. 'E') THEN
                  IEVVAR(IXLHS) = IDEQV + IVAR - 1
               ELSE
                  IEVVAR(IXLHS) = -999
               END IF
            END IF
         END IF
  100 CONTINUE

      RETURN
      END
