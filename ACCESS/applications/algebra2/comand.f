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
      SUBROUTINE COMAND (A, INLINE, INTYP, CFIELD, IFIELD, RFIELD,
     &   NAMECO, NAMELB, NAMES, TIMES, IPTIMS,
     &   IDELB, VISELB, SELELB,
     &   QAREC, INFREC, RETVRB, MERR)
C=======================================================================
C $Id: comand.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: comand.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:25  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.7  2008/03/14 13:45:25  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.6  1997/09/08 17:18:40  gdsjaar
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
C Revision 1.5  1995/12/10 15:46:41  caforsy
C Integrated call to rpterr and mdcomp/mccomp calls
C
C Revision 1.4  1995/11/03 17:06:59  caforsy
C Generated error if ZOOM command limits were not of the correct format
C (xmin < xmax, ymin < ymax, zmin < zmax)
C
C Revision 1.3  1995/10/03 21:36:04  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:20:44  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:42:04  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.2  1993/02/18  16:20:42  gdsjaar
c Upped number of equations from 100 to 512.  Changed version number to 2.03
c
c Revision 1.1.1.1  1990/11/09  16:23:51  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:23:49  gdsjaar
c Initial revision
c 

C   --*** COMAND *** (ALGEBRA) Process command
C   --   Written by Amy Gilkey - revised 05/18/88
C   --
C   --COMAND processes an ALGEBRA command.  The commands are:
C   --
C   --   TITLE                     Change database title
C   --
C   --   SAVE     {N,E,G,A,var,...} Save input database variable
C   --   DELETE   {var,var,...}    Delete assigned variable
C   --   ALIAS    {name,var,n}     Assign alias to vector/tensor variables
C   --
C   --   TMIN     {TMIN}           Set minimum selected time
C   --   TMAX     {TMAX}           Set maximum selected time
C   --   DELTIME  {DELT}           Set selected time interval
C   --   NINTV    {NINTV}          Change selected time interval
C   --   ZINTV    {NINTV}          Change selected time interval(zero interval)
C   --   ALLTIMES                  Select all times
C   --   TIMES    {t1,t2...}       Select specified times
C   --   STEPS    {n1,n2...}       Select specified steps
C   --
C   --   ZOOM     {x1,x2,y1,y2,z1,z2} Set zoomed mesh limits
C   --   VISIBLE  {id1,...}        Set element blocks to be written
C   --   BLOCKS   {id1,...}        Set selected element blocks
C   --   MATERIAL {id1,...}        Set selected element blocks
C   --
C   --   LIST     {option}         Display database information
C   --   SHOW     {option}         Display parameter setting
C   --   HELP     {option}         Help on ALGEBRA
C   --
C   --   END                       Exit to evaluate equations (also EXIT)
C   --   QUIT                      Exit but do not write database
C   --
C   --Parameters:
C   --   A         - IN - the dynamic memory base array
C   --   INLINE    - IN/OUT - the parsed input lines for the log file
C   --   INTYP     - IN - the input types from the free field reader
C   --   CFIELD    - IN - the character fields
C   --   IFIELD    - IN - the integer fields
C   --   RFIELD    - IN - the real fields
C   --   NAMECO    - IN - the coordinate names
C   --   NAMELB    - IN - the element block names
C   --   NAMES     - IN - the global, nodal, and element variable names
C   --   TIMES     - IN - the database time steps
C   --   IPTIMS    - IN/OUT - the selected times steps
C   --   IDELB     - IN - the element block IDs
C   --   VISELB(i) - IN/OUT - true iff element block i is to be written
C   --   SELELB(i) - IN/OUT - true iff element block i is selected
C   --   QAREC     - IN - the QA records containing:
C   --              (1) - the analysis code name
C   --              (2) - the analysis code QA descriptor
C   --              (3) - the analysis date
C   --              (4) - the analysis time
C   --   INFREC    - IN - the information records
C   --   RETVRB    - OUT - returned action verb (END, QUIT, PRINT, LOG)
C   --
C   --Common Variables:
C   --   Uses NDIM, NSTEPS of /DBNUMS/
C   --   Sets TITLEO of /DBTITL/
C   --   Sets NPTIMS, TMIN, TMAX, DELT, NINTV, WHONLY of /TIMES/
C   --   Sets ISZOOM, ZMLIM of /ZOOM/
C   --   Uses FNCNAM of /FNCTBL/

      include 'params.blk'
      include 'dbnums.blk'
      include 'dbtitl.blk'
      include 'times.blk'
      include 'zoom.blk'
      include 'fnctbc.blk'
      
      common /debugc/ cdebug
      common /debugn/ idebug
      character*(mxstln) cdebug

      LOGICAL FFEXST, FFNUMB, MATSTR

      DIMENSION A(*)
      CHARACTER*(*) INLINE(*)
      INTEGER       INTYP(*)
      CHARACTER*(*) CFIELD(*)
      INTEGER       IFIELD(*)
      REAL          RFIELD(*)
      CHARACTER*(MXSTLN) NAMECO(*)
      CHARACTER*(MXSTLN) NAMELB(*)
      CHARACTER*(MXSTLN) NAMES(*)
      REAL TIMES(*)
      INTEGER IPTIMS(*)
      INTEGER IDELB(*)
      LOGICAL VISELB(NELBLK)
      LOGICAL SELELB(NELBLK)
      CHARACTER*(MXSTLN) QAREC(4,*)
      CHARACTER*(MXLNLN) INFREC(*)
      CHARACTER*(*) RETVRB
      CHARACTER*1 OPTION

      CHARACTER*(mxstln) VERB, WORD
      CHARACTER*(mxstln) HLPTYP
      LOGICAL ISON
      LOGICAL HELP
      LOGICAL XLIM, YLIM, ZLIM
      INTEGER MERR

      CHARACTER*(mxstln) CMDTBL(24)
      SAVE CMDTBL
C      --CMDTBL - the commands table

      CHARACTER*(mxstln) HLPTBL(4)
      SAVE HLPTBL
C      --HLPTBL - the HELP type table

      MERR = 0

C   --Command table follows.  Remember to change the dimensioned size when
C   --changing the table.
      DATA CMDTBL /
     1  'TITLE                           ',
     2  'SAVE                            ',
     *  'DELETE                          ',
     *  'ALIAS                           ',
     3  'TMIN                            ',
     *  'TMAX                            ',
     *  'DELTIME                         ',
     *  'NINTV                           ',
     *  'ZINTV                           ',
     4  'ALLTIMES                        ',
     *  'TIMES                           ',
     *  'STEPS                           ',
     5  'ZOOM                            ',
     *  'VISIBLE                         ',
     *  'BLOCKS                          ',
     *  'MATERIAL                        ',
     6  'LOG                             ',
     7  'LIST                            ',
     *  'SHOW                            ',
     *  'HELP                            ',
     8  'END                             ',
     *  'EXIT                            ',
     *  'QUIT                            ',
     9  '                                ' /

C   --HELP type table follows.  Remember to change the dimensioned size when
C   --changing the table.
      DATA HLPTBL /
     &  'RULES                           ',
     *  'COMMANDS                        ',
     *  'FUNCTION                        ',
     &  '                                ' /

      RETVRB = ' '

      IFLD = 1
      CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
      CALL ABRSTR (VERB, WORD, CMDTBL)
      IF (VERB .EQ. ' ') VERB = WORD

      IF (VERB .EQ. 'TITLE') THEN
         CALL FFADDC (VERB, INLINE(1))
         CALL GETINP (0, 0, 'TITLE> ', TITLEO, IOSTAT)
         INLINE(2) = TITLEO

      ELSE IF (VERB .EQ. 'ALIAS') THEN
         CALL FFADDC (VERB, INLINE(1))
         CALL ALICMD (INLINE, INTYP(IFLD), CFIELD(IFLD), IFIELD(IFLD),
     &      NAMES, *150)

      ELSE IF (VERB .EQ. 'SAVE') THEN
         CALL FFADDC (VERB, INLINE(1))
         CALL SAVCMD (INLINE, INTYP(IFLD), CFIELD(IFLD), NAMES, *150)

      ELSE IF (VERB .EQ. 'DELETE') THEN
         CALL FFADDC (VERB, INLINE(1))
         CALL DELCMD (INLINE, INTYP(IFLD), CFIELD(IFLD), *150)

      ELSE IF ((VERB .EQ. 'TMIN') .OR. (VERB .EQ. 'TMAX')
     &   .OR. (VERB .EQ. 'DELTIME')
     &   .OR. (VERB .EQ. 'NINTV') .OR. (VERB .EQ. 'ZINTV')
     &   .OR. (VERB .EQ. 'ALLTIMES')
     &   .OR. (VERB .EQ. 'TIMES') .OR. (VERB .EQ. 'STEPS')) THEN
         CALL CMDTIM (INLINE, VERB, IFLD, INTYP, CFIELD, IFIELD,
     &                RFIELD, NSTEPS, TIMES, TMIN, TMAX, DELT,
     &                NINTV, NPTIMS, IPTIMS)  
      ELSE IF (VERB .EQ. 'ZOOM') THEN
         CALL FFADDC (VERB, INLINE(1))
         CALL FFNEED (IFLD, INTYP, 'R', MIN(3,NDIM)*2,
     &      'zoom mesh limits for all dimensions', *150)
         DO 100 I = 1, MIN(3,NDIM)*2
            CALL FFREAL (IFLD, INTYP, RFIELD,
     &         'zoom mesh limit', 0.0, ZMLIM(I), *150)
            CALL FFADDR (ZMLIM(I), INLINE(1))
  100    CONTINUE
C        Set logical variables which determine if zoom mesh limits
C        are of the correct format (minlimit < maxlimit)
         XLIM = (ZMLIM(1) .LE. ZMLIM(2))
         YLIM = (ZMLIM(3) .LE. ZMLIM(4))
         IF (NDIM .EQ. 3) THEN
            ZLIM = (ZMLIM(5) .LE. ZMLIM(6))
         ELSE
            ZLIM = .TRUE.
         ENDIF
         IF ((.NOT. XLIM) .OR. (.NOT. YLIM) .OR.
     &       (.NOT. ZLIM)) THEN
C           Zoom limits are incorrect
            CALL PRTERR ('CMDERR', 
     &      'Zoom limits are incorrect. (minlimit > maxlimit)')
            INLINE(1) = ' '
         ELSE
            ISZOOM = .TRUE.
         END IF
      ELSE IF (VERB .EQ. 'VISIBLE') THEN
         CALL FFADDC (VERB, INLINE(1))

         IF (.NOT. FFEXST (IFLD, INTYP)) THEN

C         --Select all element blocks if no parameters

            CALL INILOG (NELBLK, .TRUE., VISELB)
            OPTION = '+'

         ELSE IF (.NOT. FFNUMB (IFLD, INTYP)) THEN

C         --Strip off ADD or DELETE option
            CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
            IF (MATSTR (WORD, 'ADD', 1)) THEN
               CALL FFADDC ('ADD', INLINE(1))
               OPTION = '+'
            ELSE IF (MATSTR (WORD, 'DELETE', 1)) THEN
               CALL FFADDC ('DELETE', INLINE(1))
               OPTION = '-'
            ELSE
               CALL PRTERR ('CMDERR',
     &            'Expected "ADD" or "DELETE" or element block ID')
               GOTO 160
            END IF

         ELSE

C         --De-select all element blocks so only listed blocks are selected
            CALL INILOG (NELBLK, .FALSE., VISELB)
            OPTION = '+'
         END IF

  110    CONTINUE
         IF (FFEXST (IFLD, INTYP)) THEN
            CALL FFINTG (IFLD, INTYP, IFIELD,
     &         'element block ID', 0, ID, *120)
            IELB = LOCINT (ID, NELBLK, IDELB)
            IF (IELB .GT. 0) THEN
               CALL FFADDI (ID, INLINE(1))
               VISELB(IELB) = (OPTION .EQ. '+')
            ELSE
               CALL INTSTR (1, 0, ID, WORD, LSTR)
               CALL PRTERR ('CMDERR', 'Element block ID ' //
     &            WORD(:LSTR) // ' does not exist, ignored')
            END IF
  120       CONTINUE
            GOTO 110
         END IF

      ELSE IF ((VERB .EQ. 'BLOCKS') .OR. (VERB .EQ. 'MATERIAL')) THEN
         CALL FFADDC (VERB, INLINE(1))
         IF (VERB .EQ. 'MATERIAL') VERB = 'BLOCKS'

         IF (.NOT. FFEXST (IFLD, INTYP)) THEN

C         --Select all element blocks if no parameters
            CALL INILOG (NELBLK, .TRUE., SELELB)
            OPTION = '+'

         ELSE IF (.NOT. FFNUMB (IFLD, INTYP)) THEN

C         --Strip off ADD or DELETE option
            CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
            IF (MATSTR (WORD, 'ADD', 1)) THEN
               CALL FFADDC ('ADD', INLINE(1))
               OPTION = '+'
            ELSE IF (MATSTR (WORD, 'DELETE', 1)) THEN
               CALL FFADDC ('DELETE', INLINE(1))
               OPTION = '-'
            ELSE
               CALL PRTERR ('CMDERR',
     &            'Expected "ADD" or "DELETE" or element block ID')
               GOTO 160
            END IF

         ELSE

C         --De-select all element blocks so only listed blocks are selected
            CALL INILOG (NELBLK, .FALSE., SELELB)
            OPTION = '+'
         END IF

  130    CONTINUE
         IF (FFEXST (IFLD, INTYP)) THEN
            CALL FFINTG (IFLD, INTYP, IFIELD,
     &         'element block ID', 0, ID, *140)
            IELB = LOCINT (ID, NELBLK, IDELB)
            IF (IELB .GT. 0) THEN
               CALL FFADDI (ID, INLINE(1))
               SELELB(IELB) = (OPTION .EQ. '+')
            ELSE
               CALL INTSTR (1, 0, ID, WORD, LSTR)
               CALL PRTERR ('CMDERR', 'Element block ID ' //
     &            WORD(:LSTR) // ' does not exist, ignored')
            END IF
  140       CONTINUE
            GOTO 130
         END IF

         RETVRB = 'BLOCKS'

      ELSE IF ((VERB .EQ. 'LOG') .or. (verb .eq. 'SAVELOG')) THEN
         if (verb .eq. 'SAVELOG') then
            call prterr ('CMDSPEC', 'Please use the LOG command')
            verb = 'LOG'
         end if
         VERB = ' '
         RETVRB = 'LOG'

      ELSE IF (VERB .EQ. 'LIST') THEN
         VERB = ' '
         CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
         CALL DBLIST (WORD, A, NAMECO, NAMELB, NAMES,
     &                TIMES, IDELB, QAREC, INFREC, MERR)
         IF (MERR .EQ. 1) RETURN

      ELSE IF (VERB .EQ. 'SHOW') THEN
         VERB = ' '
         CALL FFCHAR (IFLD, INTYP, CFIELD, ' ', WORD)
         CALL SHOW (WORD, CMDTBL, NAMECO, NAMELB, NAMES,
     &        TIMES, IPTIMS, IDELB, VISELB, SELELB)

         IF (WORD .EQ. ' ') RETVRB = 'PRINT'

      ELSE IF (VERB .EQ. 'HELP') THEN
         VERB = ' '
         CALL ABRSTR (HLPTYP, WORD, HLPTBL)
         ISON = HELP ('ALGEBRA', HLPTYP, ' ')
         IF (.NOT. ISON) THEN
            IF (HLPTYP .EQ. 'RULES') THEN
               CONTINUE
            ELSE IF (HLPTYP .EQ. 'COMMANDS') THEN
               CALL SHOCMD ('COMMANDS', CMDTBL)
            ELSE IF (HLPTYP .EQ. 'FUNCTION') THEN
               CALL SHOCMD ('Available Functions:', FNCNAM)
            ELSE
               CALL SHOCMD ('HELP Options:', HLPTBL)
            END IF
         END IF

         RETVRB = 'PRINT'

      ELSE IF ((VERB .EQ. 'END') .OR. (VERB .EQ. 'EXIT')) THEN
         VERB = ' '
         CALL FFADDC ('END', INLINE(1))
         RETVRB = 'END'

      ELSE IF (VERB .EQ. 'QUIT') THEN
         VERB = ' '
         CALL FFADDC ('QUIT', INLINE(1))
         RETVRB = 'QUIT'

C   --This command allows selectable debugging
      else if (verb .eq. 'DEBUG') then
         verb = ' '
         call ffchar (ifld, intyp, cfield, ' ', cdebug)
         if (matstr (cdebug, 'EQUATION', 3)) then
            call prtdeb ('EQUATION', 0)
         else if (matstr (cdebug, 'VARIABLE', 3)) then
            call prtdeb ('VARIABLE', 0)
         else if (matstr (cdebug, 'ALL', 3)) then
            call prtdeb ('VARIABLE', 0)
            call prtdeb ('EQUATION', -1)
         end if

      ELSE
         CALL PRTERR ('CMDERR', '"' // VERB(:LENSTR(VERB))
     &      // '" is an invalid command')
         VERB = ' '
         GOTO 150
      END IF

      GOTO 160

  150 CONTINUE
      INLINE(1) = ' '

  160 CONTINUE
      IF (VERB .NE. ' ') THEN
         CALL SHOW (VERB, CMDTBL, NAMECO, NAMELB, NAMES,
     &      TIMES, IPTIMS, IDELB, VISELB, SELELB)
      END IF

      RETURN
      END
