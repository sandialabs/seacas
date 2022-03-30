C    Copyright(C) 1999-2020, 2022 National Technology & Engineering Solutions
C    of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
C    NTESS, the U.S. Government retains certain rights in this software.
C
C    See packages/seacas/LICENSE for details

C ... Each cavity is a single sideset id, but calculates volume of each cavity
C     simultaneously and then outputs all data at the end.  Only reads the 
C     displacements a single time for all cavities instead of once per cavity

      SUBROUTINE MULTI_CAVITY (A, CRD, IDESS, NEESS, NNESS, IPEESS, 
     *   IPNESS, LTEESS, LTNESS, FACESS, DISP, NUMNP, NDIM, NUMESS,
     *   TIME, ITMSEL, TITLE, CENT, CENTER)

      include 'nu_io.blk'
      DIMENSION A(*), CRD(NUMNP,NDIM), IDESS(*), NEESS(*),
     *   NNESS(*), IPEESS(*), IPNESS(*), LTEESS(*), LTNESS(*),
     *   FACESS(*), TIME(*), DISP(NUMNP,NDIM), CENT(3)
      LOGICAL ITMSEL(*)
      CHARACTER*80 TITLE
      include 'nu_logs.blk'
      include 'nu_ptim.blk'
      include 'nu_cav.blk'
      LOGICAL ERROR, CENTER

      CALL GETCAV (ERROR, IDESS, NUMESS)
      IF (ERROR) RETURN

C ... Allocate some memory for use in storing time-related output
C     Experimental for use with cavity
      call mdrsrv('TIMESCR', ITIMSCR, NSTEP * NUMCAV)
      call mdrsrv('TVOL', ITVOL, NUMCAV)

      DO NCAV = 1, NUMCAV
         IFLG = IFND(NCAV)
         IPTR = IPNESS(IFLG)
         IF (NDIM .EQ. 3) THEN
            CALL VOL3D( CRD, LTNESS(IPTR), NEESS(IFLG), VOLUME,
     *         NDIM, NUMESS, CENT, NUMNP, CENTER)
         ELSE
            CALL VOL2D( CRD, LTNESS(IPTR), NEESS(IFLG), VOLUME,
     *         NDIM, NUMESS, AXI, CENT, NUMNP, CENTER)
         END IF
         a(itvol + ncav - 1) = volume
      END DO

C ... REWIND EXODUS FILE TO BEGINNING OF TIMESTEPS

      IF (EXODUS .AND. ISDIS) THEN
         TIMEL = STMIN
         CALL GETDSP (CRD, DISP, NDIM, NUMNP, TIME, ITMSEL, 'R', ISTAT)
         IF (ISTAT .NE. 0) GO TO 140
         istep = 1
   90    CONTINUE
         CALL GETDSP (CRD, DISP, NDIM, NUMNP, TIME, ITMSEL, 'S', ISTAT)
         IF (ISTAT .NE. 0) GO TO 140
         DELVOL = 0.0
         DO NCAV = 1, NUMCAV
            IFLG = IFND(NCAV)
            IPTR = IPNESS(IFLG)

C     NOTE: Positive delcav = shrink in cavity volume

            IF (NDIM .EQ. 3) THEN
               CALL DVOL3D(CRD, DISP, LTNESS(IPTR),
     *            NEESS(IFLG), DELCAV, NDIM, NUMNP)
            ELSE
               CALL DVOL2D(CRD, DISP, LTNESS(IPTR),
     *            NEESS(IFLG), DELCAV, NDIM, AXI, NUMNP)
            END IF

            a(itimscr + ((ncav-1)*nstep + istep) - 1) = delcav
         END DO
         istep = istep + 1
c$$$         DELDEL = DELVOL - DELLAS
c$$$         IF (TREAD .EQ. TIMEL) THEN
c$$$            DO IO=IOMIN, IOMAX
c$$$               WRITE (IO, 130) TREAD, TVOL-DELVOL, -DELVOL,
c$$$     *            -DELDEL
c$$$            END DO
c$$$         ELSE
c$$$            DELRAT = DELDEL / (TREAD - TIMEL)
c$$$            DO IO=IOMIN, IOMAX
c$$$               WRITE (IO, 130) TREAD, TVOL-DELVOL, -DELVOL,
c$$$     *            -DELDEL, -DELRAT
c$$$            END DO
c$$$         END IF
c$$$         DELLAS = DELVOL
c$$$         TIMEL  = TREAD
         GO TO 90
      END IF
  140 CONTINUE

      DELLAS = 0.0
      write (*,*) '------------- new version ------------------'
      do ncav = 1, numcav
         tvol = a(itvol + ncav - 1)
      DO IO=IOMIN, IOMAX
         write (IO, 30) icav(ncav)
         WRITE (IO,60) tvol
         WRITE (IO, 80)
      END DO
      do istep = 1, nstep
         delvol = a(itimscr + ((ncav-1)*nstep + istep) - 1)
         deldel = delvol - dellas
         if (i .gt. 1) then
            DELRAT = DELDEL / (time(istep) - time(istep-1))
         else
            delrat = 0.0
         end if
         IF (i .eq. 1) THEN
            DO IO=IOMIN, IOMAX
               WRITE (IO, 130) TIME(ISTEP), TVOL-DELVOL, -DELVOL,
     *            -DELDEL
            END DO
         ELSE
            DELRAT = DELDEL / (TREAD - TIMEL)
            DO IO=IOMIN, IOMAX
               WRITE (IO, 130) TIME(ISTEP), TVOL-DELVOL, -DELVOL,
     *            -DELDEL, -DELRAT
            END DO
         end if
         dellas = delvol
      end do
      end do
      CALL INIINT (NUMCAV, 0, ICAV)
      NUMCAV = 0

   30 FORMAT (/' Cavity Flag(s): ',8I8)
   40 FORMAT ( ' Apex at X =',1PE15.8,', Y =',1PE15.8)
   50 FORMAT ( ' Apex at X =',1PE15.8,', Y =',1PE15.8,', Z =',1PE15.8)
   60 FORMAT (/' Undeformed Volume of Cavity is ',1PE15.8)
   80    FORMAT (/,
     *      4X,'                 Cavity           Total',
     *     '            Timestep         Rate of',/
     *      4X,'Time             Volume           Change',
     *     '           Change           Change',/
     *      4X,'----             ------           ------',
     *     '           --------         -------')
  130    FORMAT (1X,5(1PE15.8,2X))

      call mddel('TIMESCR')
      call mddel('TVOL')

      RETURN
      END
