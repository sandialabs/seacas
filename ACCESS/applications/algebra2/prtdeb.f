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
      SUBROUTINE PRTDEB (OPTION, NUM)
C=======================================================================
C $Id: prtdeb.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: prtdeb.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:27  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.5  2008/03/14 13:45:28  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.4  1997/09/08 17:18:51  gdsjaar
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
C Revision 1.3  1995/10/03 21:36:32  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:21:25  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:43:22  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.2  1993/02/18  16:20:50  gdsjaar
c Upped number of equations from 100 to 512.  Changed version number to 2.03
c
c Revision 1.1.1.1  1990/11/09  16:25:21  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:25:20  gdsjaar
c Initial revision
c 

C   --*** PRTDEB *** (ALGEBRA) Print debug information
C   --   Written by Amy Gilkey - revised 12/14/87
C   --
C   --PRTDEB prints out the contents of the common requested.
C   --
C   --Parameters:
C   --   OPTION - IN - the information requested
C   --   NUM - IN - dependent on OPTION
C   --
C   --Common Variables:
C   --   Uses NUMEQN, NUMENT, NAMENT, TYPENT, INXENT, VALENT, VSZENT of /ENT../
C   --   Uses NUMINP, IXLHS, NAMVAR, TYPVAR, IDVAR, ISTVAR of /VAR../

      include 'params.blk'
      include 'numeqn.blk'
      include 'ent.blk'
      include 'var.blk'
      PARAMETER (ICURTM = 1, ILSTTM = 2, IONETM = 3)

      CHARACTER*(*) OPTION
      CHARACTER TYPE

      IF (OPTION .EQ. 'EQUATION') THEN
         TYPE = 'E'
      ELSE IF (OPTION .EQ. 'VARIABLE') then
         TYPE = 'V'
      ELSE
         WRITE (*, *)
         WRITE (*, 10040) 'DEBUG - Unknown OPTION ', OPTION
         TYPE = ' '
      END IF

      IF (TYPE .EQ. 'E') THEN
         IF (NUM .LE. 0) THEN
            ISTART = 1
            IEND = NUMEQN
         ELSE
            ISTART = NUMEQN
            IEND = NUMEQN
         END IF
         DO 110 NEQN = ISTART, IEND
            WRITE (*, *)
            WRITE (*, 10050, IOSTAT=IDUM) 'Equation ', NEQN
            WRITE (*, 10000)
            DO 100 I = 1, NUMENT(NEQN)
               WRITE (*, 10010, IOSTAT=IDUM) I,
     &            NAMENT(I,NEQN), TYPENT(I,NEQN),
     &            INXENT(I,NEQN), VALENT(I,NEQN),
     &            ITMENT(I,NEQN), IEVENT(I,NEQN), VSZENT(I,NEQN)
10000           FORMAT (4X, ' #', 3X, 'name    ', 3X, 'type',
     &            3X, 'index', 3X, '     value',
     &            3X, 'time', 3X, 'evok', 3X, 'size')
10010           FORMAT (4X, I2, 3X, A, 3X, 3X, A,
     &            3X, I5, 3X, F10.3,
     &            3X, I4, 3X, I4, 3X, 3X, A)
  100       CONTINUE
  110    CONTINUE

      ELSE IF (TYPE .EQ. 'V') THEN
         WRITE (*, *)
         WRITE (*, 10050) 'INPUT variables'
         WRITE (*, 10020)
         DO 120 I = 1, NUMINP
            WRITE (*, 10030, IOSTAT=IDUM) I,
     &         NAMVAR(I), TYPVAR(I), IDVAR(I), (ISTVAR(K,I), K=1,3),
     &         IEVVAR(I)
10020        FORMAT (4X, '  #', 3X, 'name    ', 3X, 'type',
     &         3X, '   id', 3X, '       store   ', 3X, 'evok')
10030        FORMAT (4X, I3, 3X, A, 3X, 3X, A,
     &         3X, I5, 3X, 3I5, 3X, I4)
  120    CONTINUE
         WRITE (*, 10050) 'LHS variables'
         WRITE (*, 10020)
         DO 130 I = IXLHS, MAXVAR
            WRITE (*, 10030, IOSTAT=IDUM) I,
     &         NAMVAR(I), TYPVAR(I), IDVAR(I), (ISTVAR(K,I), K=1,3),
     &         IEVVAR(I)
  130    CONTINUE
      END IF

      IF (TYPE .NE. ' ') WRITE (*, *)

      RETURN
10040  FORMAT (1X, 5A)
10050  FORMAT (1X, A, I5)
      END
