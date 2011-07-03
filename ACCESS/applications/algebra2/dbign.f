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
      SUBROUTINE DBIGN (NDB, NUMESS, IDESS, NNESS, IXNESS,
     &                  LTNESS, LTNNN)
C=======================================================================
C$Id: dbign.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C$Log: dbign.f,v $
CRevision 1.1  2008/10/31 05:04:00  gdsjaar
CMoved the applications into an applications subdirectory.
C
CRevision 1.1  2008/06/18 16:00:25  gdsjaar
CInitial adding of files to sourceforge
C
CRevision 1.4  2008/03/14 13:45:26  gdsjaar
CAdded copyright information to top of all files.
C
CALGEBRA2 is now open-sourced under BSD license
C
CRevision 1.3  1997/09/08 17:18:41  gdsjaar
CMinor cleanup of some exodusII related issues.  The cleanups fall in
Ctwo distinct areas:
C1. Handling of 32 character names
C2. Nonexistence of History variables (or special treatment...)
C
CIn the first area, changes were made in several areas to permit the
Cuser to reference and create variable names with >8 characters. The
Creading and writing of these worked correctly, but users couldn't
Center names >8 characters from the command line
C
CIn the second area: In exodusII, there is not really the concept of
Chistory variables as they existed in exodusI. If needed, a separate
Cexodus file is written with a different timestep frequency.  In the
Coriginal version of algebra, TIME and any variable that was a function
Cof TIME was a history variable. This was carried over into algebra2
Cexcept that history variables are not written to the database, so the
Cuser couldn't do any functions involving time.  This has been changed
Cto use GLOBAL variables instead of HISTORY.
C
CMinor change: Variable names are uppercased as they are read in from
Cthe exodus database. This fixes problem with not being able to access
Cvariables with lowercase names.
C
CRevision 1.2  1996/02/12 21:28:39  caforsy
CRemoved redundant checks for ExodusII API calls
C
CRevision 1.1  1995/10/03 21:43:27  mksmith
CAdding files new to algII for algebra2
C

C   --*** DBIGN *** Get node from the side sets
C   --   Written 9/10/95 for ExodusIIv2
C   --
C   --Parameters:
C   --   NDB    - IN  - the database number
C   --   NUMESS - IN  - number of side sets
C   --   IDESS  - OUT - array of side set IDS
C   --   NNESS  - OUT - array of the number of nodes for each side set
C   --   IXNESS - OUT - array of indices into LTNESS - 1st node each set 
C   --   LTNESS - OUT - array of nodes for all side sets
C   --   LTNNN  - OUT _ array of number of nodes for each side in a side sets

      INTEGER NDB
      INTEGER NUMESS
      INTEGER IDESS(*)
      INTEGER NNESS(*)
      INTEGER IXNESS(*)
      INTEGER LTNESS(*)
      INTEGER LTNNN(*)

C     Offset into element list of current side set
      ISOFF  = 0
C     Node count for current side set
      NODCNT = 0
      DO 100 I = 1, NUMESS
C        Set index of the first node for each side set in LTNESS
         IXNESS(I) = NODCNT + 1
         CAll EXGSP(NDB, IDESS(I), NSIDE, NDIST, IERR)
C        NSIDE - number of sides in side set IDESS(I)
C        NDIST - number of distribution factors in side set IDESS(I)

         CAll EXGSSN(NDB, IDESS(I), LTNNN(ISOFF+1),
     &               LTNESS(NODCNT+1), IERR)
C        LTNNN(ISOFF+1) - number of nodes for each side in side set IDESS(I)
C        LTNESS(NODCNT+1) - nodes for current set

C        Calculate node count sum for the current side set
         NCSUM = 0
         DO 90 J = 0, NSIDE-1
            NCSUM = NCSUM + LTNNN(ISOFF+1+J)
  90     CONTINUE
         NNESS(I) = NCSUM
         NODCNT = NODCNT + NCSUM
         ISOFF = ISOFF + NSIDE
 100  CONTINUE   

      RETURN
      END
