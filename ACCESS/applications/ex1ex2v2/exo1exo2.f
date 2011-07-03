C Copyright (C) 2009 Sandia Corporation.  Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C certain rights in this software
C 
C Redistribution and use in source and binary forms, with or without
C modification, are permitted provided that the following conditions are
C met:
C 
C     * Redistributions of source code must retain the above copyright
C       notice, this list of conditions and the following disclaimer.
C 
C     * Redistributions in binary form must reproduce the above
C       copyright notice, this list of conditions and the following
C       disclaimer in the documentation and/or other materials provided
C       with the distribution.
C 
C     * Neither the name of Sandia Corporation nor the names of its
C       contributors may be used to endorse or promote products derived
C       from this software without specific prior written permission.
C 
C THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
C "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
C LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
C A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
C OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
C SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
C LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
C DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
C THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
C OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
C 

C=======================================================================
      PROGRAM EXO1EXO2
C=======================================================================

C   --*** EXO1EXO2 *** EXODUS I to EXODUS II translator
C   --   Written by Lynn Clements  
C        12/22/93 - V.R. Yarberry
C	 Updated to EXODUS II V 2.0 specs
C   --
C   --EXO1EXO2 reads an EXODUS I database and writes an EXODUS II database 
C   --

      include 'exodusII.inc'
      include 'netcdf.inc'

      CHARACTER*8 QAINFO(6)

      CHARACTER*80 TITLE

      PARAMETER (MAXQA = 100, MAXINF = 100)
      CHARACTER*8 QAREC(4,MAXQA)
      CHARACTER*80 INFO(MAXINF)
      CHARACTER*8 NAMECO(6)
      CHARACTER*8 NAMELB(256)
      CHARACTER*8 NAMES(256)
      character exofil*250, netfil*250, hfil*250

      integer cpuws,wsout

      DIMENSION A(1)
      DIMENSION IA(1)
      EQUIVALENCE (A(1), IA(1))
C      --A - the dynamic memory base array

      CHARACTER*8 STR8
      character*80 ws
      LOGICAL EXODUS
      LOGICAL WHOTIM

      DATA (QAINFO(I), I=1,3) / 'EXO1EXO2', '03/17/94', 'V 2.01  ' /
      data iin,iout/5,6/
      data nsteps /0/
      data cpuws,wsout /0,0/

      CALL STRTUP (QAINFO)

      CALL BANNER (0, QAINFO,
     &   'EXODUS I TO EXODUS II FILE TRANSLATOR',
     &   ' ', ' ')
      call exinq (netid, EXLBVR, idummy, exlibversion, name, nerr)
      write(*,*)'ExodusII Library version ',
     1          exlibversion

C   --Open the input and output files

      NRES = 25
      NDB = 11
      NET = 20

       

c10    write (iout,*) 'Enter name of EXODUS I file:'
c      read (iin, '(a)') exofil
      call exname (ndb, exofil, lnam)
      write(*,*)'Input filename: ',exofil(1:lnam)
      CALL OPNFIL (NDB, 'I', 'U', 0, IERR)
       IF (IERR .NE. 0) THEN
         CALL PRTERR ('FATAL', 'Database does not exist')
         GOTO 140
      END IF

c20    write (iout, *) 'Enter name of EXODUS II file to create:'
c     read (iin, '(a)') netfil
      call exname (net, netfil, lnam)
      write(*,*)'Output filename: ',netfil(1:lnam)

c24    write (iout, *) 'Enter output file word size: (4 or 8) '
c     read (iin, '(i)') wsout
c     output file word size is specifed in environment variable EXT00
      call exname (nres, ws, llen)
      read(ws,'(i1)',ERR=25)wsout
      if (wsout .ne. 4 .and. wsout .ne. 8) then
         CALL PRTERR ('FATAL', 'Invalid output word size')
         STOP
      endif
      goto 26
25    write(*,*)
     1	'Output word size not specified, default (4) will be used'
      wsout=4

26    continue
      write(*,*)'Output word size: ',wsout
      CALL MDINIT (A)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130
      CALL MDFILL(-1)

C   --Read the initial variables from exodusI database

      CALL DBIINI (NDB, '*', NVERS, TITLE, NDIM, NUMNP, NUMEL, NELBLK,
     &   NUMNPS, LNPSNL, NUMESS, LESSEL, LESSNL, *150)

      call ncpopt(0)
      call exopts(1,ierr)

c
c     create the a netcdf file
c
      idexo = excre (netfil(1:lnam), EXCLOB, cpuws, wsout, ierr)
      if (ierr .ne. 0) then
        call exerr('exo1exo2','Error from excre', ierr)
        go to 140
      end if

c
c     write initial variables to netcdf file
c
      call expini (idexo, title, ndim, numnp, numel, nelblk, numnps,
     &    numess, ierr)
      if (ierr .ne. 0) then
        call exerr ('exo1exo2',' Error from expini', ierr)
        goto 150
      end if


      CALL DBPINI ('NTIS', NDB, TITLE, NDIM, NUMNP, NUMEL, NELBLK,
     &   NUMNPS, LNPSNL, NUMESS, LESSEL, LESSNL,
     &   IDUM, IDUM, IDUM, IDUM)

C   --Read the coordinates from the exodusI database

      CALL MDRSRV ('XN', KXN, NUMNP)
      CALL MDRSRV ('YN', KYN, NUMNP)
      IF (NDIM .GE. 3) CALL MDRSRV ('ZN', KZN, NUMNP)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      CALL DBIXYZ (NDB, '*', NDIM, NUMNP, A(KXN), A(KYN), A(KZN), *150)
c
c     write the coordinates to the regular netcdf file
c
      call expcor (idexo, a(kxn), a(kyn), a(kzn), ierr)

      if (ierr .ne. 0) then
        call exerr ('exo1exo2','Error from expcor', ierr)
        goto 150
      end if
      CALL MDDEL ('XN')
      CALL MDDEL ('YN')
      IF (NDIM .GE. 3) CALL MDDEL ('ZN')
C   --Read the element order map from the exodusI database

      CALL MDRSRV ('MAPEL', KMAPEL, NUMEL)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      CALL DBIMAP (NDB, '*', NUMEL, A(KMAPEL), *150)
c
c     write the element order map to the regular netcdf file
c
      call expmap (idexo, a(kmapel), ierr)
      if (ierr .ne. 0) then
        call exerr ('exo1exo2','Error from expmap', ierr)
        goto 150
      end if
      CALL MDDEL ('MAPEL')

C   --Read the element blocks

      CALL MDRSRV ('NUMELB', KNELB, NELBLK)
      CALL MDRSRV ('LINK', KLINK, 0)
      CALL MDRSRV ('ATRIB', KATRIB, 0)
      call MDRSRV ('IDELB', KIDELB, NELBLK)
      call MDRSRV ('NUMLNK', KNMLNK, NELBLK)
      call MDRSRV ('NUMATR', KNMATR, NELBLK)
      CALL MDSTAT (IERR, MEM)
      IF (IERR .GT. 0) goto 150

      CALL DBIELB (NDB, '*', 1, nelblk, A(KIDELB), A(KNELB), A(KNMLNK), 
     &   A(KNMATR), A, KLINK, KATRIB, *150)
      CALL MDSTAT (IERR, MEM)
      IF (IERR .GT. 0) goto 150

C   --Read the nodal points sets

      CALL MDRSRV ('IDNPS',  KIDNS, NUMNPS)
      CALL MDRSRV ('NNNPS', KNNNS, NUMNPS)
      CALL MDRSRV ('IXNNPS', KIXNNS, NUMNPS)
      CALL MDRSRV ('LSTNPS', KLSTNS, LNPSNL)
      CALL MDRSRV ('FACNPS', KFACNS, LNPSNL)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      CALL DBINPS (NDB, '*', NUMNPS, LNPSNL,
     &   A(KIDNS), A(KNNNS), A(KIXNNS), A(KLSTNS), A(KFACNS), *150)


C   --Read the element side sets

      CALL MDRSRV ('IDESS', KIDSS, NUMESS)
      CALL MDRSRV ('NEESS', KNESS, NUMESS)
      CALL MDRSRV ('NNESS', KNNSS, NUMESS)
      CALL MDRSRV ('IXEESS', KIXESS, NUMESS)
      CALL MDRSRV ('IXNESS', KIXNSS, NUMESS)
      CALL MDRSRV ('LTEESS', KLTESS, LESSEL)
      CALL MDRSRV ('LTNESS', KLTNSS, LESSNL)
      CALL MDRSRV ('FACESS', KFACSS, LESSNL)
      CALL MDRSRV ('SACESS', KLTSSS, LESSEL)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      CALL DBIESS (NDB, '*', NUMESS, LESSEL, LESSNL,
     &   A(KIDSS), A(KNESS), A(KNNSS), A(KIXESS), A(KIXNSS),
     &   A(KLTESS), A(KLTNSS), A(KFACSS), *150)


C   --Read the QA and info records

      CALL DBIQA (NDB, '*', MAXQA, MAXINF, NQAREC, QAREC, NINFO, INFO,
     &   EXODUS, *150)
      IF (.NOT. EXODUS) GOTO 150


c**********************************************************************
C   --Read the database names

      CALL DBINAM (NDB, '*', NDIM, NELBLK,
     &   NNDIM, NNELB, NVARHI, NVARGL, NVARNP, NVAREL,
     &   NAMECO, NAMELB, NAMES, IXHV, IXGV, IXNV, IXEV,
     &   A, KIEVOK, EXODUS, *150)
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130

      IF (EXODUS) THEN
         CALL DBPINI ('V', NDB, TITLE, NDIM, NUMNP, NUMEL, NELBLK,
     &      NUMNPS, LNPSNL, NUMESS, LESSEL, LESSNL,
     &      NVARHI, NVARGL, NVARNP, NVAREL)
      END IF

c*********
      ioff = 0
      DO 100 IELB = 1, NELBLK
c
c          write element block parameters to the netcdf file
c
         call expelb (IDEXO, A(KIDELB+IELB-1), namelb(IELB),
     1      A(KNELB+IELB-1),
     2      A(KNMLNK+IELB-1), A(KNMATR+IELB-1), IERR) 
         IF (IERR .NE. 0) 
     1		CALL exerr('exo1exo2','Error from expelb',ierr)
c
c          write block attributes to the netcdf file
c
         IF (IA(KNMATR+IELB-1) .GT. 0) THEN
           call expeat (IDEXO, A(KIDELB+IELB-1), A(KATRIB+ioff), IERR) 
           IF (IERR .NE. 0) 
     1		CALL exerr ('rdelb','Error from expeat', IERR)
         end if

         ioff = ioff + ia(knmatr + ielb-1) * ia(knelb + ielb-1)

c         CALL MDLONG ('LINK', KLINK, 0)
c         CALL MDLONG ('ATRIB', KATRIB, 0)
  100 CONTINUE

      iptr = klink
      do 101 ielb = 1, nelblk
c
c          write the element block connectivity to the netcdf file
c	     skipping null element blocks
c
	if (A(KNELB+IELB-1) .eq. 0) then
	  write(*,*)'Null element block: ',ielb
        else
          call expelc (idexo, a(kidelb+ielb-1), a(iptr), ierr)
          if (ierr .ne. 0) then
            call exerr ('exo1exo2','Error from expelc', ierr)
            goto 150
          end if
        end if

        iptr = iptr + ( ia(knmlnk+ielb-1) * ia(knelb+ielb-1) )

101   continue

c
c     write out the nodal point sets to the regular netcdf file
c
c	Note: For exodus I data, dist factors always exist.

      if (numnps .gt. 0) then
        call expcns (idexo, a(kidns), a(knnns), a(knnns), a(kixnns),
     &      a(kixnns), a(klstns), a(kfacns), ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expcns', ierr)
          goto 150
        end if
      endif

c     write element side sets
c
c	Note: Exodus II V2.0 represents a major change for side sets:
c		They are represented as side IDs - not node IDs and
c		must be translated.
      if (numess .gt. 0) then
        call excn2s (idexo, a(kness), a(knnss), a(kixess),
     1		a(kixnss), a(kltess), a(kltnss), a(kltsss), ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from excn2s', ierr)
          goto 150
        end if
c
        call expcss (idexo, a(kidss), a(kness), a(knnss), a(kixess),
     &      a(kixnss), a(kltess), a(kltsss), a(kfacss), ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expcss', ierr)
          goto 150
        end if
      endif

      call mddel ('LINK')
      call mddel ('NUMLNK')
      CALL MDDEL ('ATRIB')
      CALL MDDEL ('NUMATR')

      CALL MDDEL ('IDNPS')
      CALL MDDEL ('NNNPS')
      CALL MDDEL ('IXNNPS')
      CALL MDDEL ('LSTNPS')
      CALL MDDEL ('FACNPS')

      CALL MDDEL ('IDESS')
      CALL MDDEL ('NEESS')
      CALL MDDEL ('NNESS')
      CALL MDDEL ('IXEESS')
      CALL MDDEL ('IXNESS')
      CALL MDDEL ('LTEESS')
      CALL MDDEL ('LTNESS')
      CALL MDDEL ('FACESS')
      CALL MDDEL ('SACESS')
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130
c
c      write the QA records
c
      IF (NQAREC .GT. 0) then
        call expqa (idexo, NQAREC, QAREC, ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expqa', ierr)
          goto 150
        end if
      end if
c
c       write the info records
c
      if (NINFO .gt. 0) then
        call expinf (idexo, ninfo, info, ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expinf', ierr)
          goto 150
        end if
      end if

c**********************************************************************
c
c        write coordinate names
c
      call expcon (idexo, nameco, ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expcon', ierr)
          goto 150
        end if
c
      if (.not. EXODUS) goto 150
c
c        if there are any history variables
c
      if (nvarhi .gt. 0) then
c
c        create EXODUS II history file
c
        ilen = index (netfil, ' ')
        hfil =netfil(1:ilen-1)//'h'
        ihdexo = excre (hfil, EXCLOB, cpuws, wsout, ierr) 
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from excre', ierr)
          goto 150
        end if
c
c
c        write the number of history variables (as global variables)
c
        call expvp (ihdexo, 'G', nvarhi, ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expvp', ierr)
          goto 140
        end if
c
c        write the history variable names (as global variable names)
c
        call expvan (ihdexo, 'G', nvarhi, names(ixhv), ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expvan', ierr)
          goto 140
        end if
      end if
c
c       write the number of global variables
c
      if (nvargl .gt. 0) then
        call expvp (idexo, 'G', nvargl, ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expvp', ierr)
          goto 140
        end if
c
c       write the global variable names
c
        call expvan (idexo, 'G', nvargl, names(ixgv), ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expvan', ierr)
          goto 140
        end if
      end if
c
c       write the number of nodal variables
c
      if (nvarnp .gt. 0) then
        call expvp (idexo, 'N', nvarnp, ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expvp', ierr)
          goto 140
        end if
c
c       write the nodal variable names
c
        call expvan (idexo, 'N', nvarnp, names(ixnv), ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expvan', ierr)
          goto 140
        end if
      end if
c
c       write the number of element variables
c
      if (nvarel .gt. 0) then 
        call expvp (idexo, 'E', nvarel, ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expvp', ierr)
          goto 140
        end if
c
c       write the element variable names
c
        call expvan (idexo, 'E', nvarel, names(ixev), ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from exvan', ierr)
          goto 140
        end if
      end if
c
c       write the element variable truth table
c
      call mdrsrv ('ebids', kebids, nelblk)
      call exgebi (idexo, a(kebids), ierr)
      if (ierr .ne. 0) then
        call exerr ('exo1exo2','Error from exgebi', ierr)
        goto 140
      end if

      if (nvarel .gt. 0) then
        call expvtt(idexo, nelblk, nvarel, a(kievok), ierr)
        if (ierr .ne. 0) then
          call exerr ('exo1exo2','Error from expvtt', ierr)
          goto 140
        end if
      end if

      call mddel ('ebids')

      IF (.NOT. EXODUS) GOTO 140

C   --Read the database time steps

      CALL MDRSRV ('VARHI', KVARHI, NVARHI)
      CALL MDRSRV ('VARGL', KVARGL, NVARGL)
      CALL MDRSRV ('VARNP', KVARNP, NVARNP * NUMNP)
      CALL MDRSRV ('VAREL', KVAREL, NVAREL * NUMEL)
c      CALL MDRSRV ('VARTMP', KVARTMP, max(NUMNP,NUMEL))
      CALL MDSTAT (NERR, MEM)
      IF (NERR .GT. 0) GOTO 130


      nwstep = 0
      nhstep = 0

  110 CONTINUE
      IF (.TRUE.) THEN

         CALL DBISTE (NDB, '*', NhSTEP+1,
     &      NVARHI, NVARGL, NVARNP, NUMNP, NVAREL, NELBLK,
     &      a(knelb), A(KIEVOK), TIME, WHOTIM, A(KVARHI), 
     &      A(KVARGL), A(KVARNP), A(KVAREL), *120)

         nhstep = nhstep + 1
         if (whotim) then
           nwstep = nwstep+1
           write (*,'(4x, "processing whole time step ", i4)') nwstep
c
c            write global variables
c
           if (nvargl .gt. 0) then
             call expgv (idexo, nwstep, nvargl, a(kvargl), ierr)
             if (ierr .ne. 0) then
               call exerr ('exo1exo2','Error from expgv', ierr)
               goto 140
             end if
           end if
c
c            write nodal variable values
c
           if (nvarnp .gt. 0) then
             do 111 i= 1,nvarnp  
               call expnv (idexo, nwstep, i, numnp, 
     &           a(kvarnp+(i-1)*numnp), ierr)
               if (ierr .ne. 0) then
                 call exerr ('exo1exo2','Error from expnv', ierr)
                 goto 140
               end if
111          continue
           end if
c
c            write element variable values
c
           if (nvarel .gt. 0) then
             call putev (idexo, nwstep, nelblk, nvarel, 
     &         a(knelb), a(kvarel), a(kidelb), a(kievok), ierr)
               if (ierr .ne. 0) then
                 call exerr ('exo1exo2','Error from putev', ierr)
                 goto 140
               end if
           end if
c
c            write whole time step
c
           call exptim (idexo, nwstep, time, ierr)
           if (ierr .ne. 0) then
             call exerr ('exo1exo2','Error from exptim', ierr)
             goto 140
           end if


         end if
c
c           write history variable values (as global variables)
c
         if (nvarhi .gt. 0) then
           call expgv (ihdexo, nhstep, nvarhi, a(kvarhi), ierr)
           if (ierr .ne. 0) then
             call exerr ('exo1exo2','Error from expgv', ierr)
             goto 140
           end if
c
c            write history time step
c
           call exptim (ihdexo, nhstep, time, ierr)
           if (ierr .ne. 0) then
             call exerr ('exo1exo2','Error from exptim', ierr)
             goto 140
           end if
         end if

         GOTO 110
      END IF

  120 CONTINUE

      call mddel ('IDELB')

      WRITE (STR8, '(I8)', IOSTAT=K) NwSTEP
      CALL SQZSTR (STR8, LSTR)
      WRITE (*, 10010) STR8(:LSTR)
10010  FORMAT (/, 4X, A,
     &   ' time steps have been written to the file')

      GOTO 140

  130 CONTINUE
      CALL MEMERR

  140 CONTINUE



      if (nvarhi .gt. 0) then
        call exclos (ihdexo, ierr)
      endif

  150 call exclos (idexo, ierr)

      IF (NDB .NE. 0) CLOSE (NDB, IOSTAT=K)


      CALL WRAPUP (QAINFO(1))

      STOP
      END
