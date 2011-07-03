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
      SUBROUTINE DELCMD (INLINE, INTYP, CFIELD, *)
C=======================================================================
C $Id: delcmd.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: delcmd.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:26  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.7  2008/03/14 13:45:27  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.6  1998/04/28 17:36:30  gdsjaar
C Added 'logical matstr' declaration
C
C Revision 1.5  1998/04/22 21:26:00  gdsjaar
C Added 'delete qa|information|qainfo' command to delete the qa and/or
C information records from the output database
C
C Revision 1.4  1997/09/08 17:18:43  gdsjaar
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
C Revision 1.3  1995/10/03 21:36:11  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:20:58  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:42:24  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:24:12  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:24:10  gdsjaar
c Initial revision
c 

C   --*** DELCMD *** (ALGEBRA) Perform DELETE command
C   --   Written by Amy Gilkey - revised 11/23/87
C   --
C   --DELCMD processes the input DELETE command.  It sets the ISTVAR
C   --flag in the /VAR../ arrays for all deleted variables.
C   --
C   --Parameters:
C   --   INLINE - IN/OUT - the parsed input lines for the log file
C   --   INTYP - IN - the field types
C   --   CFIELD - IN - the character fields
C   --   * - return statement if command not executed
C   --
C   --Common Variables:
C   --   Uses IXLHS, NAMVAR of /VAR../
C   --   Sets ISTVAR of /VAR../

      PARAMETER (ICURTM = 1, ILSTTM = 2, IONETM = 3)
      include 'params.blk'
      include 'var.blk'
C     database type, num_of qa and info records
      include 'dbnumq.blk'

      CHARACTER*(*) INLINE
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)

      LOGICAL FFEXST, MATSTR
      CHARACTER*(mxstln) NAME

      NUMLHS = MAXVAR - IXLHS + 1

      IFLD = 1
  100 CONTINUE
      IF (FFEXST (IFLD, INTYP)) THEN

         CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', NAME)

         IF (MATSTR (NAME, 'QAINFO', 6)) THEN
           NQAREC = 0
           NINFO  = 0
         ELSE IF (MATSTR (NAME, 'QA', 2)) THEN
           NQAREC = 0
         ELSE IF (MATSTR (NAME, 'INFORMATION', 11)) THEN
           NINFO  = 0
         ELSE
           IVAR = LOCSTR (NAME, NUMLHS, NAMVAR(IXLHS))
           IF (IVAR .EQ. 0) THEN
             CALL PRTERR ('CMDERR',
     &         '"' // NAME(:LENSTR(NAME)) // '" not defined, ignored')
             GOTO 110
           END IF
           ISTVAR(ICURTM,IVAR+IXLHS-1) = -1
         END IF
         CALL FFADDC (NAME, INLINE)


  110    CONTINUE
         GOTO 100
      END IF

      RETURN

  120 CONTINUE
      RETURN 1
      END
