C Copyright(C) 1999-2022 National Technology & Engineering Solutions
C of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C NTESS, the U.S. Government retains certain rights in this software.
C
C See packages/seacas/LICENSE for details

C=======================================================================
      PROGRAM TXTEXO
C=======================================================================

#if defined(__INTEL_COMPILER)
cDEC$ OPTIMIZE:0
#endif
C     --*** TXTEXO *** (TXTEXO) TEXT to EXODUS translator
C     --TXTEXO reads a text file and writes an EXODUS database file.
C     --Expects the input text file on unit 20, the output database on unit 12.

      include 'exodusII.inc'
      INCLUDE 'argparse.inc'
      CHARACTER*(MXSTLN) QAINFO(6)

      CHARACTER*80 TITLE

      CHARACTER*2048 FILNAM, SCRATCH

      LOGICAL EXODUS

      DIMENSION A(1)
      INTEGER IA(1)
      EQUIVALENCE (A(1), IA(1))
      CHARACTER*1 C(1)
C     --A - the dynamic memory base array

      CHARACTER*32 STRA, STRB
      CHARACTER*8 STR8

C     Program Information
C.
      QAINFO(1) = 'txtexo                          '
      QAINFO(2) = '2021/01/28                      '
      QAINFO(3) = ' 2.02                           '
      QAINFO(4) = '                                '
      QAINFO(5) = '                                '
      QAINFO(6) = '                                '

      CALL STRTUP (QAINFO)

      CALL BANNER (0, QAINFO,
     &     'TEXT FILE TO EXODUSII DATABASE TRANSLATOR',
     &     ' ', ' ')
      CALL PRTERR ('CMDSPEC',
     $     'Please use ncdump/ncgen instead of exotxt/txtexo.')

      CALL MDINIT (A)
      CALL MCINIT (C)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

C     --Open the database and write the initial variables
      NTXT = 20
      NDB = 12

      NARG = argument_count()
      if (narg .lt. 2) then
        CALL PRTERR ('FATAL', 'Filename not specified.')
        CALL PRTERR ('CMDSPEC', 'Syntax is: "txtexo text_file db_file"')
        CALL PRTERR ('CMDSPEC',
     *    'Documentation: https://sandialabs.github.io' //
     $       '/seacas-docs/sphinx/html/index.html#txtexo')
        GOTO 140
      else if (narg .gt. 2) then
        CALL PRTERR ('FATAL', 'Too many arguments specified.')
        CALL PRTERR ('CMDSPEC', 'Syntax is: "txtexo text_file db_file"')
        CALL PRTERR ('CMDSPEC',
     *    'Documentation: https://sandialabs.github.io' //
     $       '/seacas-docs/sphinx/html/index.html#txtexo')
        GOTO 140
      end if

      CALL get_argument(1,FILNAM, LFIL)
      open(unit=ntxt, file=filnam(:lfil), status='old', iostat=ierr)
      IF (IERR .NE. 0) THEN
        SCRATCH = 'Text file "'//FILNAM(:LFIL)//'" does not exist.'
        CALL PRTERR ('FATAL', SCRATCH(:LENSTR(SCRATCH)))
        GOTO 140
      END IF
      EXODUS = .FALSE.

      CALL get_argument(2,FILNAM, LFIL)
      CMPSIZ = 0
      IOWS   = iowdsz()
      ndb = excre(filnam(:lfil), EXCLOB, CMPSIZ, IOWS, IERR)
      if (ierr .lt. 0) then
        SCRATCH = 'Could not create "'//FILNAM(:LFIL)//'"'
        CALL PRTERR ('FATAL', SCRATCH(:LENSTR(SCRATCH)))
        call exopts (EXVRBS, ierr)
        call exerr('txtexo', 'Error from excre', ierr)
        go to 140
      endif

C     --Read the initial variables

      CALL RDINIT (NTXT, VERS, TITLE, NDIM, NUMNP, NUMEL, NELBLK,
     &     NUMNPS, LNPSNL, LNPSDF, NUMESS, LESSEL, LESSNL, LESSDF,
     *     NAMLEN, *140)

      call exmxnm(ndb, namlen, ierr)

      call expini (ndb, title, ndim, numnp, numel, nelblk, numnps,
     &     numess, ierr)

      CALL DBPINI ('TIS', NDB, TITLE, NDIM, NUMNP, NUMEL, NELBLK,
     &     NUMNPS, LNPSNL, NUMESS, LESSEL, LESSNL,
     &     IDUM, IDUM, IDUM, IDUM)

C     --Read the coordinates

      CALL MDRSRV ('XN', KXN, NUMNP)
      IF (NDIM .GE. 2) CALL MDRSRV ('YN', KYN, NUMNP)
      IF (NDIM .GE. 3) CALL MDRSRV ('ZN', KZN, NUMNP)

      CALL MCRSRV ('NAMECO', KNACOR, NAMLEN*NDIM)

      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      CALL RWXYZ (NTXT, NDB, NDIM, NUMNP, A(KXN), A(KYN), A(KZN),
     *  C(KNACOR), NAMLEN, *140)

      CALL MDDEL ('XN')
      IF (NDIM .GE. 2) CALL MDDEL ('YN')
      IF (NDIM .GE. 3) CALL MDDEL ('ZN')
      CALL MCDEL ('NAMECO')

C     --Read the element and node maps

      CALL MDRSRV ('MAP', KMAP, MAX(NUMEL,NUMNP))
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

C     ... Node number map
      CALL RDMAP (NTXT, NUMNP, IA(KMAP), *140)
      call expnnm (ndb, ia(kmap), ierr)

C     ... Element number map
      CALL RDMAP (NTXT, NUMEL, IA(KMAP), *140)
      call expenm (ndb, ia(kmap), ierr)

C     ... Element order map
      CALL RDMAP (NTXT, NUMEL, IA(KMAP), *140)
      call expmap (ndb, ia(kmap), ierr)

      CALL MDDEL ('MAP')

C     --Read the element blocks

      CALL MDRSRV ('NUMELB', KNELB, NELBLK)
      CALL MDRSRV ('IDELB', KIDLB, NELBLK)
      CALL MDRSRV ('LINK', KLINK, 0)
      CALL MDRSRV ('ATRIB', KATRIB, 0)
      call mcrsrv ('NAMELB', KNMLB, mxstln*nelblk)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      DO 100 IELB = 1, NELBLK
        CALL RDELB (NTXT, IELB, ia(KIDLB+IELB-1), iA(KNELB+IELB-1),
     &    NUMLNK, NUMATR, c(knmlb), A, KLINK, KATRIB, *140)
        CALL MDSTAT (NERR, MEM)
        IF (NERR .GT. 0) GOTO 130
        CALL DBOELB (NDB, IELB, IELB,
     &       IA(KIDLB+IELB-1), iA(KNELB+IELB-1), NUMLNK, NUMATR,
     &       iA(KLINK), c(knmlb), A(KATRIB))

        CALL MDLONG ('LINK', KLINK, 0)
        CALL MDLONG ('ATRIB', KATRIB, 0)
 100  CONTINUE

      CALL MDDEL ('LINK')
      CALL MDDEL ('ATRIB')
      call mcdel ('NAMELB')

      NEL = INTADD (NELBLK, iA(KNELB))
      IF (NEL .NE. NUMEL) THEN
         CALL INTSTR (1, 0, NEL, STRA, LSTRA)
         CALL INTSTR (1, 0, NUMEL, STRB, LSTRB)
         CALL PRTERR ('WARNING',
     &        'NUMBER OF ELEMENTS IN BLOCK = ' // STRA(:LSTRA)
     &        // ' does not match TOTAL = ' // STRB(:LSTRB))
      END IF

C     --Read the node sets

      if (numnps .gt. 0) then
         CALL MDRSRV ('IDNPS',  KIDNS,  NUMNPS)
         CALL MDRSRV ('NNNPS',  KNNNS,  NUMNPS)
         CALL MDRSRV ('NDNPS',  KNDNPS, NUMNPS)
         CALL MDRSRV ('IXNNPS', KIXNNS, NUMNPS)
         CALL MDRSRV ('IXDNPS', KIXDNS, NUMNPS)
         CALL MDRSRV ('LSTNPS', KLSTNS, LNPSNL)
         CALL MDRSRV ('FACNPS', KFACNS, LNPSNL)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 130

         CALL RDNPS (NTXT, NUMNPS, LNPSNL, LNPSDF,
     &        iA(KIDNS), iA(KNNNS), iA(KNDNPS), iA(KIXNNS), iA(KIXDNS),
     &        iA(KLSTNS), A(KFACNS), *140)
         call expcns (ndb, ia(kidns), ia(knnns), ia(kndnps),
     &        ia(kixnns), ia(kixdns), ia(klstns), a(kfacns), ierr)

         CALL MDDEL ('NNNPS')
         CALL MDDEL ('NDNPS')
         CALL MDDEL ('IXNNPS')
         CALL MDDEL ('IXDNPS')
         CALL MDDEL ('LSTNPS')
         CALL MDDEL ('FACNPS')
      end if

C     --Read the side sets

      if (numess .gt. 0) then
         CALL MDRSRV ('IDESS', KIDSS, NUMESS)
         CALL MDRSRV ('NEESS', KNESS, NUMESS)
         CALL MDRSRV ('NNESS', KNNSS, NUMESS)
         CALL MDRSRV ('NDESS',  KNDSS,  NUMESS)
         CALL MDRSRV ('IXEESS', KIXESS, NUMESS)
         CALL MDRSRV ('IXNESS', KIXNSS, NUMESS)
         CALL MDRSRV ('IXDESS', KIXDSS, NUMESS)
         CALL MDRSRV ('LTEESS', KLTESS, LESSEL)
         CALL MDRSRV ('LTNESS', KLTNSS, LESSNL)
         CALL MDRSRV ('LTSESS', KLTSSS, LESSEL)
         CALL MDRSRV ('FACESS', KFACSS, LESSNL)
         CALL MDSTAT (NERR, MEM)
         IF (NERR .GT. 0) GOTO 130

         CALL RDESS (NTXT, NUMESS, LESSEL, LESSNL, LESSDF,
     &     iA(KIDSS), iA(KNESS), ia(knnss), iA(KNDSS), iA(KIXESS),
     *     iA(KIXDSS), iA(KLTESS), iA(KLTSSS), A(KFACSS), *140)
         call expcss (ndb, ia(kidss), ia(kness), ia(kndss), ia(kixess),
     &     ia(kixdss), ia(kltess), ia(kltsss), a(kfacss), ierr)

         CALL MDDEL ('NEESS')
         CALL MDDEL ('NNESS')
         CALL MDDEL ('NDESS')
         CALL MDDEL ('IXEESS')
         CALL MDDEL ('IXNESS')
         CALL MDDEL ('IXDESS')
         CALL MDDEL ('LTEESS')
         CALL MDDEL ('LTNESS')
         CALL MDDEL ('LTSESS')
         CALL MDDEL ('FACESS')
      end if

C     --Read the properties
      call rwpval(ntxt, ndb, a, ia, c, nelblk, numnps, numess,
     &  ia(kidlb), ia(kidns), ia(kidss), *140)

      if (numnps .gt. 0) CALL MDDEL ('IDNPS')
      if (numess .gt. 0) CALL MDDEL ('IDESS')

C     --Read the QA records
      CALL RWQA (NTXT, NDB, C, QAINFO, *140)

C     --Read the database names

      CALL RWNAME (NTXT, NDB, NELBLK, NVARGL, NVARNP, NVAREL,
     &  A, C, KIEVOK, EXODUS, NAMLEN, *140)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      IF (EXODUS) THEN
         CALL DBPINI ('V', NTXT, TITLE, NDIM, NUMNP, NUMEL, NELBLK,
     &        NUMNPS, LNPSNL, NUMESS, LESSEL, LESSNL,
     &        0, NVARGL, NVARNP, NVAREL)
      END IF

C Put truth table
      IF (.NOT. EXODUS) GOTO 140

C     --Read the database time steps

      NSTEPS = 0
      maxvar = max(nvargl, nvarnp*numnp, nvarel*numel)
      CALL MDRSRV ('VAR', KVAR, maxvar)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      WRITE (*, *)
      WRITE (*, *)              !#VAX

 110  CONTINUE
      IF (.TRUE.) THEN
         NSTEPS = NSTEPS + 1
         CALL RWSTEP (NTXT, NDB, NSTEPS, ia(kidlb),
     &     NVARGL, NVARNP, NUMNP, NVAREL, NELBLK, iA(KNELB), iA(KIEVOK),
     &     TIME, A(KVAR), *120)

         WRITE (*, 10000) NSTEPS
10000    FORMAT (I8, ' time steps processed')
         GOTO 110
      END IF

 120  CONTINUE
      call mddel('IDELB')
      call mddel('NUMELB')
      call mddel('ISEVOK')
      call mddel('VAR')

      NSTEPS = NSTEPS-1
      CALL INTSTR (1, 0, NSTEPS, STR8, LSTR)
      WRITE (*, 10010) STR8(:LSTR)
10010 FORMAT (/, 4X, A,
     &     ' time steps have been written to the database')

      GOTO 140

 130  CONTINUE
      CALL MEMERR
      GOTO 140

 140  CONTINUE

      CLOSE (NTXT, IOSTAT=IDUM)
      if (ndb .ne. 12 .and. ndb .gt. 0) call exclos(ndb, ierr)

      call addlog (QAINFO(1)(:lenstr(QAINFO(1))))
      CALL PRTERR ('CMDSPEC',
     $     'Please use ncdump/ncgen instead of exotxt/txtexo.')
      CALL WRAPUP (QAINFO(1))

      END
C.
      subroutine wrcon (ndb, cornam, ierr, namlen)
      character*(namlen) cornam(*)

      call expcon (ndb, cornam, ierr)
      return
      end
