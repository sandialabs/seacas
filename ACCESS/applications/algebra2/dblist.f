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
      SUBROUTINE DBLIST (TYPE, A, NAMECO, NAMELB, NAMES,
     &                   TIMES, IDELB, QAREC, INFREC, MERR)
C=======================================================================
c      SUBROUTINE DBLIST (TYPE, A,
c     &   NAMECO, NAMELB, NAMES, TIMES, WHOTIM, IDELB, QAREC, INFREC)
C $Id: dblist.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: dblist.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:26  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.6  2008/03/14 13:45:26  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.5  1997/09/08 17:18:43  gdsjaar
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
C Revision 1.4  1996/02/14 18:13:26  caforsy
C Fixed error after mdcomp(). Need to find time step array indices.(IPTIMS)
C
C Revision 1.3  1995/10/03 21:36:08  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:20:51  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:42:22  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:24:09  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:24:08  gdsjaar
c Initial revision
c 

C   --*** DBLIST *** (ALGEBRA) Display database information
C   --   Written by Amy Gilkey - revised 01/07/88
C   --
C   --DBLIST prints out the requested information.
C   --
C   --Parameters:
C   --   TYPE   - IN - the type of LIST requested (none, VARS)
C   --   A      - IN - the dynamic memory base array
C   --   NAMECO - IN - the coordinate names
C   --   NAMELB - IN - the element block names
C   --   NAMES  - IN - the global, nodal, and element variable names
C   --   TIMES  - IN - the database time steps
C   --   IDELB  - IN - the element block IDs
C   --   QAREC  - IN - the QA records containing:
C   --           (1) - the analysis code name
C   --           (2) - the analysis code QA descriptor
C   --           (3) - the analysis date
C   --           (4) - the analysis time
C   --   INFREC - IN - the information records
C   --
C   --Common Variables:
C   --   Uses NDBIN of /DBASE/
C   --   Uses TITLE of /DBTITL/
C   --   Uses NDIM, NUMNP, NUMEL, NELBLK, NVARNP, NVAREL, NVARGL
C   --      of /DBNUMS/
C   --   Uses NQAREC, NINFO of /DBNUMQ/

      include 'params.blk'
      include 'dbase.blk'
      include 'dbtitl.blk'
      include 'dbnums.blk'
      include 'dbnumg.blk'
      include 'dbnumq.blk'

      CHARACTER*(*) TYPE
      DIMENSION A(*)
      CHARACTER*(MXSTLN) NAMECO(*)
      CHARACTER*(MXSTLN) NAMELB(*)
      CHARACTER*(MXSTLN) NAMES(*)
      REAL TIMES(*)
      INTEGER IDELB(*)
      CHARACTER*(MXSTLN) QAREC(4,*)
      CHARACTER*(MXLNLN) INFREC(*)

      CHARACTER*(mxstln) SHOTYP
      CHARACTER*(MXLNLN) STRING
      LOGICAL LDUM
      CHARACTER CDUM

      CHARACTER*(mxstln) SHOTBL(8)
      INTEGER MERR
      SAVE SHOTBL
C      --SHOTBL - the DBLIST type table

      MERR = 0

C   --DBLIST type table follows.  Remember to change the dimensioned size when
C   --changing the table.
      DATA SHOTBL /
     1  'VARS                            ',
     *  'BLOCKS                          ',
     *  'MATERIAL                        ',
     *  'QA                              ',
     *  'NAMES                           ',
     2  'STEPS                           ',
     *  'TIMES                           ',
     3  '                                ' /

      CALL ABRSTR (SHOTYP, TYPE, SHOTBL)
      IF (SHOTYP .EQ. ' ') SHOTYP = TYPE

      IF (SHOTYP .EQ. 'VARS') THEN
          CALL DBPINI ('TISV', NDBIN, TITLE,
     &         NDIM, NUMNP, NUMEL, NELBLK,
     &         NUMNPS, LNPSNL, LNPSDF, NUMESS, LESSEL, LESSNL,
     &         LESSDF, NVARGL, NVARNP, NVAREL)

      ELSE IF ((SHOTYP .EQ. 'BLOCKS')
     &    .OR. (SHOTYP .EQ. 'MATERIAL')) THEN
          CALL MDFIND ('NUMELB', KNELB, IDUM)
          CALL MDFIND ('NUMLNK', KNLNK, IDUM)
          CALL MDFIND ('NUMATR', KNATR, IDUM)
          CALL MDSTAT (NERR, MEM)
          IF (NERR .GT. 0) THEN
             CALL MEMERR
             MERR = 1
          END IF
          CALL DBPELB ('N', NELBLK, IDELB,
     &         A(KNELB), A(KNLNK), A(KNATR),
     &         NAMELB, IDUM, CDUM, LDUM, IDUM)

      ELSE IF (SHOTYP .EQ. 'QA') THEN
          CALL DBPQA ('*', NQAREC, QAREC, NINFO, INFREC)

      ELSE IF (SHOTYP .EQ. 'NAMES') THEN
          WRITE (*, *)
          WRITE (STRING, 10000) (NAMECO(I), I=1,NDIM)
10000     FORMAT (6(' ',A8))
          CALL SQZSTR (STRING, LSTR)
          WRITE (*, 10010) 'Coordinate names: ', STRING(:LSTR)
10010     FORMAT (1X, 10A)

          CALL DBVIX ('G', 1, IGV)
          CALL DBVIX ('N', 1, INV)
          CALL DBVIX ('E', 1, IEV)
          CALL DBPNAM ('*', NVARGL, NVARNP, NVAREL,
     &         NAMES(IGV), NAMES(INV),NAMES(IEV))

      ELSE IF (SHOTYP .EQ. 'STEPS') THEN
          CALL DBPTIM ('NM', NSTEPS, TIMES)

      ELSE IF (SHOTYP .EQ. 'TIMES') THEN
          CALL DBPTIM ('NT', NSTEPS, TIMES)

      ELSE
          CALL SHOCMD ('LIST Options:', SHOTBL)
      END IF

      RETURN
      END
