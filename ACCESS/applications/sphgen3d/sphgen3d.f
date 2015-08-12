      PROGRAM SPHGEN
C $Id: sphgen3d.f,v 1.4 1999/02/18 23:03:49 gdsjaar Exp $

C---------------------------------------------------------------------
C  DISCRIPTION: program to convert a genises mesh with 8 node
C               hex to a 3d sph mesh, this version is for use with
C               PRONTO3D/SPH
C
C  Writen by D. S. Preece and L. M. Taylo
C  Modified by S.W. Attaway 2/15/89
C  Modified by S.W. Attaway 7/9/90
C  Modified by S.W. Attaway 2/6/91
C  Modified by S.W. Attaway 2/22/91
C  modified by s.w. attaway 1-27-92 added nsets for each element block
CC  Modified by S.W. Attaway 9/93
C---------------------------------------------------------------------
C
      include 'exodusII.inc'
      include 'argparse.inc'

      DIMENSION A(1),IA(1)
      EQUIVALENCE (A(1),IA(1))
      CHARACTER*1 C(1)
      CHARACTER*(MXSTLN) NAMEBL
      CHARACTER*(MXLNLN) HEAD
      CHARACTER*2048 FILE, FILOUT, SCRATCH
      CHARACTER*(MXSTLN) QAINFO(4),NAMCOR(3)
      PARAMETER (NSPC=3)

      DATA NAMCOR/'X','Y','Z'/
      DATA NAMEBL/'SPHERE  '/
      DATA QAINFO/ 'sphgen3d','2.1','20110616','08:20:20'/
C
      NDBIN  = 8
      NDBOUT = 9

      CALL EXDATE( QAINFO(3) )
      CALL EXTIME( QAINFO(4) )
      CALL EXCPUS( CPUOLD )
C
      CALL MDINIT( A )
      CALL MCINIT( C )
C
C .. Get filename from command line.  If not specified, emit error message
      NARG = argument_count()
      if (narg .lt. 2) then
        CALL PRTERR ('FATAL', 'Filenames not specified.')
        CALL PRTERR ('FATAL',
     *    'Syntax is: "sphgen3d hex_file_in sph_file_out"')
        GOTO 60
      else if (narg .gt. 2) then
        CALL PRTERR ('FATAL', 'Too many arguments specified.')
        CALL PRTERR ('FATAL',
     *    'Syntax is: "sphgen3d hex_file_in sph_file_out"')
        GOTO 60
      end if

C---- Open unit 8 - Input exodus database
      CMPSIZ = 0
      IOWS   = 0
      CALL get_argument(1,FILE, LNAM)
      NDBIN = exopen(file(:lnam), EXREAD, CMPSIZ, IOWS, vers, IERR)
      IF (IERR .NE. 0) THEN
        SCRATCH = 'Database "'//FILE(:LNAM)//'" does not exist.'
        CALL PRTERR ('FATAL', SCRATCH(:LENSTR(SCRATCH)))
        goto 60
      END IF
      
C---- Open unit 9 - exodus output file
c     output file word size is specifed in environment variable EXT05
      FILOUT = ' '
      CALL get_argument(2,FILOUT, LFIL)
      iows = iowdsz()
      CMPSIZ = 0
      ndbout = excre(filout(:lfil), EXCLOB, CMPSIZ, IOWS, IERR)
      if (ierr .lt. 0) then
        SCRATCH = 'Problems creating database "'//FILOUT(:LFIL)//'".'
        CALL PRTERR ('FATAL', SCRATCH(:LENSTR(SCRATCH)))
        goto 60
      endif

C---- Title and Element mesh size data -
      call exgini(ndbin, head, ndim, ndel, numel, nelblk,
     *  nbcnod, nbcsid, ierr)
C
      if (nbcnod .gt. 0) then
        call PRTERR('CMDSPEC', 
     * 'All nodesets will be replaced by element block nodesets')
      end if

      CALL MDRSRV( 'IDELB' ,LIDLB,  NELBLK)
      CALL MDRSRV( 'LINKE' ,LLINKE, 8*NUMEL )
      CALL MDRSRV( 'LNKSCR',LLNKSC, NUMEL)
      CALL MDRSRV( 'KONMAT',LKONMA, 4*NELBLK )
      CALL MDRSRV( 'COORD' ,LCOORD, NDEL*NSPC )
      CALL MDRSRV( 'CS'    ,LCS,    NUMEL*NSPC )
      CALL MDRSRV( 'RAD'   ,LRAD,   2*NUMEL )
      CALL MDSTAT (NERR, MEM)
      if (nerr .gt. 0) then
        STOP 'Memory allocation failure'
      end if
C
      CALL MSHDAT( NDBIN, NDBOUT,IA(LIDLB),
     &  NDEL,NUMEL,NELBLK,NSPC,IA(LKONMA),IA(LLINKE),IA(LLNKSC),
     *  A(LCOORD),A(LCS),A(LRAD),HEAD,
     *  namebl,NAMCOR,QAINFO )
C
 60   continue
      call addlog (QAINFO(1)(:lenstr(QAINFO(1))))
      CALL WRAPUP (QAINFO(1))
      END

      SUBROUTINE MSHDAT(NDBIN, NDBOUT, IDELB, 
     &  NDEL,NUMEL,NELBLK,NSPC,KONMAT,LINKE,LNKSCR,
     *  COORD,CS,RAD,HEAD,NAMEBL,NAMCOR,QAINFO)
C
      include 'exodusII.inc'
      INTEGER IDELB(NELBLK)
      DIMENSION KONMAT(4,NELBLK),LINKE(8,NUMEL),COORD(NDEL,NSPC),
     *          CS(NUMEL,NSPC),RAD(2,NUMEL)
      INTEGER LNKSCR(NUMEL)
      CHARACTER*(MXLNLN) HEAD
      CHARACTER*(MXSTLN) NAMEBL,NAMCOR(3),QAINFO(4)
C
C---- Nodal Coordinates
C
      call exgcor(ndbin, coord(1,1), coord(1,2), coord(1,3), ierr)
C
C---- Element Block Data
C
      call exgebi(ndbin, idelb, ierr)
      IEND = 0
      DO 10 N = 1,NELBLK
C
C     Element Block Parameters -
        call exgelb(ndbin, idelb(n), namebl, numelb, nelnod, numatr,
     &    ierr)
         ISTRT = IEND + 1
         IEND  = IEND + NUMELB
         KONMAT(1,N) = idelb(n)
         KONMAT(2,N) = ISTRT
         KONMAT(3,N) = IEND
         KONMAT(4,N) = NUMELB
C
C     Element Connectivity -
         call exgelc(ndbin, idelb(n), linke(1,istrt), ierr)
C
   10 CONTINUE
c      iseed = 111111111
      DO 20 N = 1,NUMEL
         X1 = COORD(LINKE(1,N),1)
         X2 = COORD(LINKE(2,N),1)
         X3 = COORD(LINKE(3,N),1)
         X4 = COORD(LINKE(4,N),1)
         X5 = COORD(LINKE(5,N),1)
         X6 = COORD(LINKE(6,N),1)
         X7 = COORD(LINKE(7,N),1)
         X8 = COORD(LINKE(8,N),1)

         Y1 = COORD(LINKE(1,N),2)
         Y2 = COORD(LINKE(2,N),2)
         Y3 = COORD(LINKE(3,N),2)
         Y4 = COORD(LINKE(4,N),2)
         Y5 = COORD(LINKE(5,N),2)
         Y6 = COORD(LINKE(6,N),2)
         Y7 = COORD(LINKE(7,N),2)
         Y8 = COORD(LINKE(8,N),2)

         z1 = COORD(LINKE(1,N),3)
         z2 = COORD(LINKE(2,N),3)
         z3 = COORD(LINKE(3,N),3)
         z4 = COORD(LINKE(4,N),3)
         z5 = COORD(LINKE(5,N),3)
         z6 = COORD(LINKE(6,N),3)
         z7 = COORD(LINKE(7,N),3)
         z8 = COORD(LINKE(8,N),3)


         XC = .125 * ( X1 + X2 + X3 + X4 + x5 + x6 + x7 + x8 )
         YC = .125 * ( Y1 + Y2 + Y3 + Y4 + y5 + y6 + y7 + y8 )
         zC = .125 * ( z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 )

         Z24 = Z2 - Z4
         Z52 = Z5 - Z2
         Z45 = Z4 - Z5
         G1 = ( Y2*(Z6-Z3-Z45) + Y3*Z24 + Y4*(Z3-Z8-Z52)
     *                   + Y5*(Z8-Z6-Z24) + Y6*Z52 + Y8*Z45 ) / 12.
         Z31 = Z3 - Z1
         Z63 = Z6 - Z3
         Z16 = Z1 - Z6
         G2 = ( Y3*(Z7-Z4-Z16) + Y4*Z31 + Y1*(Z4-Z5-Z63)
     *                   + Y6*(Z5-Z7-Z31) + Y7*Z63 + Y5*Z16 ) / 12.
         Z42 = Z4 - Z2
         Z74 = Z7 - Z4
         Z27 = Z2 - Z7
         G3 = ( Y4*(Z8-Z1-Z27) + Y1*Z42 + Y2*(Z1-Z6-Z74)
     *                   + Y7*(Z6-Z8-Z42) + Y8*Z74 + Y6*Z27 ) / 12.
         Z13 = Z1 - Z3
         Z81 = Z8 - Z1
         Z38 = Z3 - Z8
         G4 = ( Y1*(Z5-Z2-Z38) + Y2*Z13 + Y3*(Z2-Z7-Z81)
     *                   + Y8*(Z7-Z5-Z13) + Y5*Z81 + Y7*Z38 ) / 12.
         Z86 = Z8 - Z6
         Z18 = Z1 - Z8
         Z61 = Z6 - Z1
         G5 = ( Y8*(Z4-Z7-Z61) + Y7*Z86 + Y6*(Z7-Z2-Z18)
     *                   + Y1*(Z2-Z4-Z86) + Y4*Z18 + Y2*Z61 ) / 12.
         Z57 = Z5 - Z7
         Z25 = Z2 - Z5
         Z72 = Z7 - Z2
         G6 = ( Y5*(Z1-Z8-Z72) + Y8*Z57 + Y7*(Z8-Z3-Z25)
     *                   + Y2*(Z3-Z1-Z57) + Y1*Z25 + Y3*Z72 ) / 12.
         Z68 = Z6 - Z8
         Z36 = Z3 - Z6
         Z83 = Z8 - Z3
         G7 = ( Y6*(Z2-Z5-Z83) + Y5*Z68 + Y8*(Z5-Z4-Z36)
     *                   + Y3*(Z4-Z2-Z68) + Y2*Z36 + Y4*Z83 ) / 12.
         Z75 = Z7 - Z5
         Z47 = Z4 - Z7
         Z54 = Z5 - Z4
         G8 = ( Y7*(Z3-Z6-Z54) + Y6*Z75 + Y5*(Z6-Z1-Z47)
     *                   + Y4*(Z1-Z3-Z75) + Y3*Z47 + Y1*Z54 ) / 12. 
        VOLUME = x1 * G1
     *          + x2 * G2
     *          + x3 * G3
     *          + x4 * G4
     *          + x5 * G5
     *          + x6 * G6
     *          + x7 * G7
     *          + x8 * G8

         rad(2,n) = volume
         RAD(1,n) = volume**(1./3.)/2.

C note that if a random pertabation is used, then the compile time option
C +E1 must be used on the HP
c         CS(N,1) = XC + ran(iseed) * rad(1,n)/20.
c         CS(N,2) = YC + ran(iseed) * rad(1,n)/20.

         CS(N,1) = XC 
         CS(N,2) = YC
         cs(n,3) = zc 
   20 CONTINUE
C
C number of nodesets = number of element blocks (each block gets it on sideset)

       numnps = nelblk
       call expini(ndbout, head, 3, numel, numel, nelblk, numnps, 0,
     &   ierr)
C ... Write coordinates
       call expcor(ndbout, cs(1,1), cs(1,2), cs(1,3), ierr)
       call expcon(ndbout, namcor, ierr)

C ... Write element blocks
       NAMEBL = 'SPHERE'
       do 25 i=1, numel
         lnkscr(i) = i
 25    continue

      DO 30 N = 1,NELBLK
C
C     Element Block Parameters -
        call expelb(ndbout, konmat(1,n), namebl, konmat(4,n), 1, 2,
     &    ierr)
C ... Link array is single node equal to element number        
        call expelc(ndbout, konmat(1,n), lnkscr(konmat(2,n)), ierr)
        call expeat(ndbout, konmat(1,n), rad(1,konmat(2,n)), ierr)
   30 CONTINUE

C Node sets
C node set id (set equal to block id)
C node set counts (number of elements in a block)
C node list == element connectivity list
C ... Hijack the rad array to store distribution factors...
      call inirea(numel, 1.0, rad)
      do 40 i=1, numnps
        call expnp (ndbout, konmat(1,i), konmat(4,i), konmat(4,i), ierr)
        call expns (ndbout, konmat(1,i), lnkscr(konmat(2,i)),      ierr)
        call expnsd(ndbout, konmat(1,i), rad(konmat(2,i),1),       ierr)
 40   continue
C
      NQAREC = 1
      call expqa(ndbout, nqarec, qatitl, ierr)
C
      call exclos(ndbin, ierr)
      call exclos(ndbout, ierr)

      RETURN
C
      END





