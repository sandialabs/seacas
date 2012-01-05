C -*- Mode: fortran -*-
C=======================================================================
      PROGRAM GEN3D
C=======================================================================

C   $Id: gen3d.f,v 1.2 1990/10/01 15:25:22 gdsjaar Exp $
C   $Log: gen3d.f,v $
C   Revision 1.2  1990/10/01 15:25:22  gdsjaar
C   -Fixed improper array reference in SPLXYZ 
C   -Renamed SPLINE common block to SPLBLK to eliminate conflict with
C   SPLINE subroutine.
C   -Removed Copyright inclusion from gen3d.f
C   -Removed SPAWN from comand.f
C
c Revision 1.1.1.1  90/08/20  12:21:39  gdsjaar
c Gen3D Mesh Generation Program
c 
c Revision 1.1  90/08/20  12:21:37  gdsjaar
c Initial revision
c 

C   --*** GEN3D *** (GEN3D) GENESIS 2D to 3D Program
C   --   Written by Amy Gilkey - revised 04/27/88
C   --
C   --GEN3D inputs a 2D GENESIS database and outputs a 3D GENESIS database.
C   --The input mesh is either translated along the Z coordinate or rotated
C   --around a Y axis.  The user specifies the number of translations or
C   --rotations.  For rotations, the total number of degrees the 2D mesh
C   --is to be rotated is also user-specified.  The mesh may be rotated
C   --around an edge of the input mesh (creating part of a cylinder-shaped
C   --mesh) or around a line outside the input mesh (creating part of a
C   --cylinder-shaped mesh with a hole in a middle).
C   --
C   --Expected input:
C   --   o The commands on the standard input device.
C   --   o The 2D GENESIS database on unit 9
C   --     (must have 4 nodes per element,
C   --     cannot have more than 256 element blocks).
C   --
C   --Output:
C   --   o A listing of the input database information and any errors
C   --     found on the standard output device.
C   --   o The 3D GENESIS database on unit 10.

C      INCLUDE 'xaccess$root:[libraries]copyright.for'

C   --Developed at Sandia National Laboratories.
C   --
C   --Current author and code sponsor: Gregory D. Sjaardema
C   --
C   --Revision History:
C   --   04/86 Created (Amy Gilkey)
C   --
C   --Source is in FORTRAN 77
C   --
C   --External software used:
C   --   SUPES package (dynamic memory, free-field reader, FORTRAN extensions)
C   --
C   --Runs on VAX VMS !#VAX
C#CTSSC   --Runs on CRAY CTSS

C   --Documentation:
C   --   "User's Manual for GEN3D"

      INCLUDE 'progqa.blk'
      INCLUDE 'dbase.blk'
      INCLUDE 'dbtitl.blk'
      INCLUDE 'dbnums.blk'
      INCLUDE 'dbnum3.blk'
      INCLUDE 'params.blk'
      INCLUDE 'xyzoff.blk'
      INCLUDE 'xyzrot.blk'
      INCLUDE 'xyzmir.blk'
      INCLUDE 'twist.blk'

      CHARACTER*80 SCRSTR

      PARAMETER (MAXQA = 256, MAXINF = 256)
      CHARACTER*8 QAREC(4,MAXQA)
      CHARACTER*80 INFREC(MAXINF)
C      --QAREC - the QA records
C      --INFREC - the information records

      CHARACTER*8 NAMECO(6)
      CHARACTER*8 NAMELB(256)
C      --NAMECO - the coordinate names
C      --NAMELB - the element block names

      CHARACTER BLKTYP(256)

      LOGICAL NOEOF
      LOGICAL LDUM
      CHARACTER CDUM

      DIMENSION A(1)
      INTEGER IA(1)
      EQUIVALENCE (A(1), IA(1))
C      --A - the dynamic numeric memory base array

      INTEGER IDNSET(0:10,2)
      INTEGER IDESET(0:10,2)

      INCLUDE 'qainfo.blk'

      CALL STRTUP (QAINFO)

      WRITE (*, 70)
      WRITE (*, 80)
      CALL BANNER (0, QAINFO,
     &   'A GENESIS DATABASE 2D TO 3D CONVERSION PROGRAM',
     &   ' ', ' ')

      CALL MDINIT (A)
d     call mddebg (6) !#VAX
d     call mddebg (99) !#VAX
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Open the input database and read the initial variables

      NDBIN  = 9
      NDBOUT = 10
      NDSPL  = 12

      CALL OPNFIL (NDBIN, 'I', 'U', 0, IERR)
      IF (IERR .NE. 0) THEN
         CALL PRTERR ('FATAL', 'Database does not exist')
         GOTO 60
      END IF

      CALL DBIINI (NDBIN, '*', NVERS, TITLE, NDIM, NUMNP, NUMEL, NELBLK,
     &   NUMNPS, LNPSNL, NUMESS, LESSEL, LESSNL, *60)

      CALL DBPINI ('NTIS', NDBIN, TITLE, NDIM, NUMNP, NUMEL, NELBLK,
     &   NUMNPS, LNPSNL, NUMESS, LESSEL, LESSNL,
     &   IDUM, IDUM, IDUM, IDUM)

      IF (NDIM .NE. 2) THEN
         CALL PRTERR ('FATAL', 'Number of dimensions must be 2')
         GOTO 60
      END IF
      IF (NELBLK .GT. 256) THEN
         CALL PRTERR ('FATAL', 'Number of element blocks > 256'
     &      // ' - See code sponsor')
         GOTO 60
      END IF

C   --Reserve memory for the 2D information

      CALL MDRSRV ('XN', KXN, NUMNP)
      CALL MDRSRV ('YN', KYN, NUMNP)

      CALL MDRSRV ('MAPEL', KMAPEL, NUMEL)

      CALL MDRSRV ('LINK', KLINK, 4 * NUMEL)

      CALL MDRSRV ('IDELB', KIDELB, NELBLK)
      CALL MDRSRV ('NUMELB', KNELB, NELBLK)
      CALL MDRSRV ('NUMLNK', KNLNK, NELBLK)
      CALL MDRSRV ('NUMATR', KNATR, NELBLK)

      CALL MDRSRV ('IDNPS',  KIDNS, NUMNPS)
      CALL MDRSRV ('NNNPS', KNNNS, NUMNPS)
      CALL MDRSRV ('IXNNPS', KIXNNS, NUMNPS)
      CALL MDRSRV ('LTNNPS', KLTNNS, LNPSNL)
      CALL MDRSRV ('FACNPS', KFACNS, LNPSNL)

      CALL MDRSRV ('IDESS', KIDSS, NUMESS)
      CALL MDRSRV ('NEESS', KNESS, NUMESS)
      CALL MDRSRV ('NNESS', KNNSS, NUMESS)
      CALL MDRSRV ('IXEESS', KIXESS, NUMESS)
      CALL MDRSRV ('IXNESS', KIXNSS, NUMESS)
      CALL MDRSRV ('LTEESS', KLTESS, LESSEL)
      CALL MDRSRV ('LTNESS', KLTNSS, LESSNL)
      CALL MDRSRV ('FACESS', KFACSS, LESSNL)

      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Read 2D information from the database and close file

      CALL DBIXYZ (NDBIN, '*', NDIM, NUMNP, A(KXN), A(KYN), RDUM, *60)

      CALL DBIMAP (NDBIN, '*', NUMEL, A(KMAPEL), *60)

C#????      CALL DBIELB (NDBIN, '*', 1, NELBLK,
C#????     &   A(KIDELB), A(KNELB), A(KNLNK), A(KNATR),
C#????     &   A(KLINK), KATRIB, *150)
      CALL RDELB (A, A(KIDELB), A(KNELB), A(KNLNK), A(KNATR),
     &   A(KLINK), KATRIB, *60)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

      CALL DBINPS (NDBIN, '*', NUMNPS, LNPSNL,
     &   A(KIDNS), A(KNNNS), A(KIXNNS), A(KLTNNS), A(KFACNS), *60)

      CALL DBIESS (NDBIN, '*', NUMESS, LESSEL, LESSNL,
     &   A(KIDSS), A(KNESS), A(KNNSS), A(KIXESS), A(KIXNSS),
     &   A(KLTESS), A(KLTNSS), A(KFACSS), *60)

      NOEOF = .TRUE.
      CALL DBIQA (NDBIN, '*', MAXQA, MAXINF,
     &   NQAREC, QAREC, NINFO, INFREC, NOEOF, *10)
   10 CONTINUE

      IF ((NQAREC .GT. 0) .OR. (NINFO .GT. 0)) THEN
         CALL DBPQA ('*', MIN(NQAREC,MAXQA),  QAREC, 
     &                    MIN(NINFO, MAXINF), INFREC)
      END IF

      CALL INISTR (NDIM, ' ', NAMECO)
      CALL INISTR (256, ' ', NAMELB)

      IF (NOEOF) THEN
         CALL DBINAM (NDBIN, 'CB', NDIM, NELBLK,
     &      NNDIM, NNELB, NVARHI, NVARGL, NVARNP, NVAREL,
     &      NAMECO, NAMELB, CDUM, IDUM, IDUM, IDUM, IDUM,
     &      A, IDUM, LDUM, *20)
   20    CONTINUE
      END IF


C   --Read in runtime parameters

      CALL MDRSRV ('IBPARM', KIBPAR, 4 * NELBLK)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

      CALL COMAND (A(KIDNS), A(KIDSS), IDNSET, IDESET,
     &   BLKTYP, A(KIBPAR), A(KIDELB), A(KNELB), A(KNLNK),
     &   A(KXN), A(KYN), A, *60)

C   --Get the new numbers for the elements and nodes

      CALL MDRSRV ('IXEL', KIXEL, NUMEL)
      CALL MDRSRV ('INCEL', KINCEL, NUMEL)
      CALL MDRSRV ('NREL', KNREL, NUMEL)
      CALL MDRSRV ('IELCOL', KIECOL, NUMEL)
      CALL MDRSRV ('IXNP', KIXNP, NUMNP)
      CALL MDRSRV ('NRNP', KNRNP, NUMNP)
      CALL MDRSRV ('NPCEN', KNPCEN, NUMCOL * 2 * NUMEL)
      CALL MDRSRV ('IELROW', KELROW, NUMEL)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

      CALL RENUMB (A, BLKTYP, A(KNELB), A(KLINK), A(KXN), A(KYN),
     &   A(KIXEL), A(KINCEL), A(KNREL), A(KIECOL), A(KIXNP), A(KNRNP),
     &   A(KNPCEN), A(KELROW))

      CALL MDDEL ('IELROW')
      CALL MDLONG ('NPCEN', KNPCEN, NUMCOL * NUMROW)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Get the node sets

      LNPSNO = INTADD (NUMNPS, A(KNNNS)) * NNREPL
      CALL MDRSRV ('NNNP3', KNNN3, NUMNPS)
      CALL MDRSRV ('IXNNP3', KIXNN3, NUMNPS)
      CALL MDRSRV ('LTNNP3', KLTNN3, LNPSNO)
      CALL MDRSRV ('FACNP3', KFACN3, LNPSNO)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

      CALL NEWNPS (IDNSET(0,1), IDNSET(0,2),
     &   A(KIDNS), A(KNNNS), A(KNNN3), A(KIXNNS), A(KIXNN3),
     &   A(KLTNNS), A(KLTNN3), A(KFACNS), A(KFACN3),
     &   A(KIXNP), A(KNRNP))

      CALL MDDEL ('NNNPS')
      CALL MDDEL ('IXNNPS')
      CALL MDDEL ('LTNNPS')
      CALL MDDEL ('FACNPS')
      CALL MDLONG ('LTNNP3', KLTNN3, LNPSNO)
      CALL MDLONG ('FACNP3', KFACN3, LNPSNO)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Get the side sets, and the front and back side sets

      NSSET = IDESET(0,1)+IDESET(0,2)
      IF (NSSET .GT. 0) THEN
         CALL MDRSRV ('ISSFRO', KISFRO, NUMEL)
         CALL MDRSRV ('ISSBCK', KISBCK, NUMEL)
         CALL MDRSRV ('NSSFRO', KNSFRO, 4*NUMEL)
         CALL MDRSRV ('NSSBCK', KNSBCK, 4*NUMEL)
      ELSE
         KISFRO = 1
         KISBCK = 1
         KNSFRO = 1
         KNSBCK = 1
      END IF

      LESSEO = INTADD (NUMESS, A(KNESS)) * NEREPL
      LESSNO = INTADD (NUMESS, A(KNNSS)) * NEREPL * 4
      CALL MDRSRV ('NEES3', KNES3, NUMESS)
      CALL MDRSRV ('NNES3', KNNS3, NUMESS)
      CALL MDRSRV ('IXEES3', KIXES3, NUMESS)
      CALL MDRSRV ('IXNES3', KIXNS3, NUMESS)
      CALL MDRSRV ('LTEES3', KLTES3, LESSEO)
      CALL MDRSRV ('LTNES3', KLTNS3, LESSNO)
      CALL MDRSRV ('FACES3', KFACS3, LESSNO)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

      CALL NEWESS
     &   (IDESET(0,1), IDESET(0,2),
     &   A(KLINK), A(KISFRO), A(KISBCK), NSSUR, A(KNSFRO), A(KNSBCK),
     &   A(KIDSS), A(KNESS), A(KNES3), A(KNNSS), A(KNNS3),
     &   A(KIXESS), A(KIXES3), A(KIXNSS), A(KIXNS3),
     &   A(KLTESS), A(KLTES3),
     &   A(KLTNSS), A(KLTNS3), A(KFACSS), A(KFACS3),
     &   A(KIXEL), A(KINCEL), A(KNREL), A(KIECOL), A(KIXNP), A(KNRNP))

      CALL MDDEL ('NEESS')
      CALL MDDEL ('NNESS')
      CALL MDDEL ('IXEESS')
      CALL MDDEL ('IXNESS')
      CALL MDDEL ('LTEESS')
      CALL MDDEL ('LTNESS')
      CALL MDDEL ('FACESS')
      CALL MDLONG ('LTEES3', KLTES3, LESSEO)
      CALL MDLONG ('LTNES3', KLTNS3, LESSNO)
      CALL MDLONG ('FACES3', KFACS3, LESSNO)
      IF (NSSET .GT. 0) THEN
         CALL MDLONG ('NSSFRO', KNSFRO, NSSUR)
         CALL MDLONG ('NSSBCK', KNSBCK, NSSUR)
      END IF
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Open the output database

      CALL OPNFIL (NDBOUT, 'O', 'U', 0, IERR)

C   --Write the initial variables

      CALL NEWINI (IDNSET(0,1)+IDNSET(0,2), IDESET(0,1)+IDESET(0,2),
     &   NSSUR, BLKTYP, A(KIBPAR))
      CALL DBOINI (NDBOUT, TITLE, NDIM3, NUMNP3, NUMEL3, NELBL3,
     &   NNPS3, LNPSN3, NESS3, LESSE3, LESSN3)

      CALL DBPINI ('NTIS', NDBOUT, TITLE, NDIM3, NUMNP3, NUMEL3, NELBL3,
     &   NNPS3, LNPSN3, NESS3, LESSE3, LESSN3,
     &   IDUM, IDUM, IDUM, IDUM)

C   --Write the coordinates

      CALL MDRSRV ('ZCORD', KZCORD, NNREPL)
      CALL MDRSRV ('SINANG', KSINA, NNREPL)
      CALL MDRSRV ('COSANG', KCOSA, NNREPL)

      CALL MDRSRV ('XN3', KXN3, NUMNP3)
      CALL MDRSRV ('YN3', KYN3, NUMNP3)
      CALL MDRSRV ('ZN3', KZN3, NUMNP3)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

      CALL NEWXYZ (A(KXN), A(KYN), A(KXN3), A(KYN3), A(KZN3),
     &   A(KIXNP), A(KNRNP), A(KNPCEN), A(KZCORD), A(KSINA), A(KCOSA),
     &   A )
      CALL DBOXYZ (NDBOUT, NDIM3, NUMNP3, A(KXN3), A(KYN3), A(KZN3))

      CALL MDDEL ('ZCORD')
      CALL MDDEL ('SINANG')
      CALL MDDEL ('COSANG')

      CALL MDDEL ('XN')
      CALL MDDEL ('YN')
      CALL MDDEL ('XN3')
      CALL MDDEL ('YN3')
      CALL MDDEL ('ZN3')
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Write the element order map

      CALL MDRSRV ('MAPEL3', KMAPEL3, NUMEL3)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

      CALL NEWMAP (A(KMAPEL), A(KMAPEL3),
     &   A(KIXEL), A(KINCEL), A(KNREL), A(KIECOL))
      CALL DBOMAP (NDBOUT, NUMEL3, A(KMAPEL3))

      CALL MDDEL ('MAPEL')
      CALL MDDEL ('MAPEL3')
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Write the element block

      CALL WRELB (A, BLKTYP, A(KIBPAR),
     &   A(KIDELB), A(KNELB), A(KNLNK), A(KNATR),
     &   A(KLINK), A(KATRIB),
     &   A(KIXEL), A(KINCEL), A(KNREL), A(KIECOL), A(KIXNP), A(KNRNP))

      CALL MDDEL ('LINK')
      CALL MDDEL ('ATRIB')
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Write the node sets

      CALL WRNPS (IDNSET(0,1), IDNSET(0,2),
     &   A(KIDNS), A(KNNN3), A(KIXNN3), A(KLTNN3), A(KFACN3),
     &   A(KIXNP), A(KNRNP))

      CALL MDDEL ('IDNPS')
      CALL MDDEL ('NNNP3')
      CALL MDDEL ('IXNNP3')
      CALL MDDEL ('LTNNP3')
      CALL MDDEL ('FACNP3')
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Fixup sides sets if mirrored
      IF (XMIRR * YMIRR * ZMIRR .LT. 0.0) THEN
         CALL MIRSS (IDESET(0,1), IDESET(0,2),
     &      NSSUR, A(KNSFRO), A(KNSBCK), A(KLTNS3))
      END IF
C   --Write the side sets

      CALL WRESS (IDESET(0,1), IDESET(0,2),
     &   A(KISFRO), A(KISBCK), NSSUR, A(KNSFRO), A(KNSBCK),
     &   A(KIDSS), A(KNES3), A(KNNS3),
     &   A(KIXES3), A(KIXNS3), A(KLTES3), A(KLTNS3), A(KFACS3))

      IF (NSSET .GT. 0) THEN
         CALL MDDEL ('ISSFRO')
         CALL MDDEL ('ISSBCK')
         CALL MDDEL ('NSSFRO')
         CALL MDDEL ('NSSBCK')
      END IF
      CALL MDDEL ('IDESS')
      CALL MDDEL ('NEES3')
      CALL MDDEL ('NNES3')
      CALL MDDEL ('IXEES3')
      CALL MDDEL ('IXNES3')
      CALL MDDEL ('LTEES3')
      CALL MDDEL ('LTNES3')
      CALL MDDEL ('FACES3')
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 40

C   --Write the QA records

      IF (NOEOF .OR. (NQAREC .GE. 0) .OR. (NINFO .GT. 0)) THEN
         IF ((NQAREC .GT. 0) .AND. (NQAREC .LT. MAXQA)) THEN
            NQAREC = NQAREC + 1
            QAREC(1,NQAREC) = QAINFO(1)
            QAREC(2,NQAREC) = QAINFO(3)
            QAREC(3,NQAREC) = QAINFO(5)
            QAREC(4,NQAREC) = QAINFO(6)
         END IF
C
C ... Add info block 
C
C---ELIMINATES FASTQ ADVERTISEMENT
         IF (NINFO .LT. MAXINF) THEN
c         NINFO = NINFO + 1 
         NINFO = 1
         INFREC(NINFO) = ' Gen3D:'
         LINFO = LENSTR(INFREC(NINFO))+2
         INQUIRE (NDBIN, NAME=SCRSTR)
         INFREC(NINFO)(LINFO:) = SCRSTR
         END IF

         CALL DBOQA (NDBOUT, NQAREC, QAREC, NINFO, INFREC)

C      --Write the database names

         IF ((NNDIM .GT. 0) .OR. (NNELB .GT. 0)) THEN
            IF (NNDIM .GT. 0) THEN
               NNDIM = NDIM3
               NAMECO(1) = 'X'
               NAMECO(2) = 'Y'
               NAMECO(3) = 'Z'
            END IF
            IF (NNELB .GT. 0) THEN
               DO 30 I = 1, NELBL3
                  IF (NAMELB(I) .EQ. 'QUAD' .OR. NAMELB(I) .EQ. ' ')
     *                NAMELB(I) = 'HEX'
   30          CONTINUE
            END IF

            CALL DBONAM (NDBOUT, NNDIM, NELBL3, -999, -999, -999, -999,
     &         NAMECO, NAMELB,
     &         CDUM, CDUM, CDUM, CDUM, LDUM)
         END IF
      END IF

      GOTO 50

   40 CONTINUE
      CALL MEMERR
      GOTO 50

   50 CONTINUE
      CLOSE (NDBIN, IOSTAT=IDUM)
      CLOSE (NDBOUT, IOSTAT=IDUM)

   60 CONTINUE
      CALL WRAPUP (QAINFO(1))

   70 FORMAT (/
     &   16X,'  GGGGGG    EEEEEEEEEE  NN      NN   3333333   ',
     $     ' DDDDDDD  ', /
     &   15X,' GGGGGGGG   EEEEEEEEEE  NN      NN  333333333  ',
     $     ' DDDDDDDD ', /
     &   14X,'GG      GG  EE          NNN     NN  33      33 ',
     $     ' DD     DD', /
     &   13X,'GG          EE          NNNN    NN          33 ',
     $     ' DD     DD')
   80 FORMAT (
     &   12X,'GG          EEEEEEEE    NN NN   NN      33333  ',
     $     ' DD     DD', /
     &   11X,'GG    GGGG  EEEEEEEE    NN  NN  NN      33333  ',
     $     ' DD     DD', /
     &   10X,'GG    GGGG  EE          NN   NN NN          33 ',
     $     ' DD     DD', /
     &    9X,'GG      GG  EE          NN    NNNN  33      33 ',
     $     ' DD     DD', /
     &    8X,' GGGGGGGG   EEEEEEEEEE  NN     NNN  333333333  ',
     $     ' DDDDDDDD ', /
     &    7X,'  GGGGGG    EEEEEEEEEE  NN      NN   3333333   ',
     $     ' DDDDDDD  ')
      END
