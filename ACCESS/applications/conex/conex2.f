C    Copyright(C) 2007 Sandia Corporation.  Under the terms of Contract
C    DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C    certain rights in this software
C    
C    Redistribution and use in source and binary forms, with or without
C    modification, are permitted provided that the following conditions are
C    met:
C    
C        * Redistributions of source code must retain the above copyright
C          notice, this list of conditions and the following disclaimer.
C    
C        * Redistributions in binary form must reproduce the above
C          copyright notice, this list of conditions and the following
C          disclaimer in the documentation and/or other materials provided
C          with the distribution.
C    
C        * Neither the name of Sandia Corporation nor the names of its
C          contributors may be used to endorse or promote products derived
C          from this software without specific prior written permission.
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
C $Id: conex2.f,v 1.11 2009/03/25 14:41:41 gdsjaar Exp $
C************************************************************************
      PROGRAM CONEX2
C************************************************************************
C       C O N E X - A code for sequentially appending EXODUS data bases
C       Main Routine
C       G. D. Sjaardema, 1521,  01/30/88
C       Modified for ExodusII database
C       Christi Forsythe, 9118,  01/18/96
C
C DESCRIPTION:
C       CONEX is a program for sequentially appending compatible
C       post processing data bases written in the EXODUSII format.
C       The command line accepts the name of the first file and
C       the name of the results file. The code prompts for each
C       subsequent data base to append and also allows the user to
C       select specific time steps to copy to the results file
C       given that the time is greater than any time existing in 
C       the current output file.   The subsequent database
C       files must all be exodusII format.  Data bases are
C       checked for compatibility.
C
C FILE INPUT: master_filename results_filename
C
C************************************************************************
C     Command Line Input
C     % conex masterfile resultfile
C     script will test masterfile for ExodusII format
C     translate code to exodusII format
C     run conex2
C************************************************************************
C
C************************************************************************
C Header Information to check
C
C     Fatal Error if values are not equal between the Master and New files
C       Initialization
C     I NDIM       number of coordinates per node
C     I NUMNP      number of nodes
C     I NUMEL      number of elements
C     I NELBLK     number of element blocks
C       Element Block
C     C NAMELB(*)  type of elements in element block
C     I NUMELB(*)  number of elements in element block
C     I NUMLNK(*)  number of nodes per element in element block
C     I NUMATR(*)  number of attributes per element in element block
C       Results Variables
C     I IEVTT      element variable truth table
C     I NVARGL     number of global variables
C     I NVARNP     number of nodal variables
C     I NVAREL     number of element variables
C     
C     Warnings if values are not equal between the Master and New files
C     I NUMNPS     number of node sets
C     I NUMESS     number of side sets
C     I LNPSNL     length of node set node list
C     I LNPSDF     length of node set distribution factors list
C     I LESSNL     length of side set node list
C     I LESSEL     length of side set element list
C     I LESSDF     length of side set distribution factors list
C
C************************************************************************
C     Data that is transfered directly from the Mater to the results file
C       Initialization
C     C TITLE      database title
C     R TIME       time step values between tmin and tmax
C       QA and Information Records
C     I NQAREC     number of QA records
C     C QAREC(4,*) QA records (MXSTLN)
C     I NINFO      number of information records
C     C INFO(*)    information records (MXLNLN)
C       Nodal Coordinates
C     R XN(*)      X coordinates of nodes
C     R YN(*)      Y coordinates of nodes
C     R ZN(*)      Z coordinates of nodes
C     C NAMECO(*)  names of coordinate arrays
C       Element Block
C     I IDELB(*)   element block ID's
C     I MAPNOD(*)  node number map
C     I MAPELN(*)  element number map
C     I MAPEL(*)   element order map
C     I LINK(*,*)  element block connectivity (NUMLNK, NUMELB)
C     R ATRIB(*,*) attributes for element block (NUMATR, NUMELB)
C       Node Sets
C     I IDNPSS(*)  node set ID's
C     I NNNPS(*)   number of nodes for each node set
C     I NDNPS(*)   number of distribution factors for each set
C     I IXNNPS(*)  indices in LTNNPS - 1st node for each set
C     I IXDNPS(*)  indices in FACNPS - 1st distribution factor for each set
C     I LTNNPS(*)  contains nodes for all node sets
C     R FACNPS(*)  contains distribution factors for all sets
C       Side Sets
C     I IDESSS(*)  side set ID's
C     I NSESS(*)   number of sides for each side set
C     I NDESS(*)   number of distribution factors for each side set
C     I IXEESS(*)  indices in LTEESS - 1st element for each set
C     I IXDESS(*)  indices in FACESS - 1st distribution factor for each set
C     I LTEESS(*)  contains elements for all side sets
C     I LTSESS(*)  contains sides for all side sets
C     R FACESS(*)  contains distribution factors for all side sets
C     
C
C************************************************************************

C************************************************************************
C     Include Files
C************************************************************************
      include 'exodusII.inc'

      include 'exsets.blk'
      include 'exinit.blk'
      include 'exrvar.blk'
      include 'exprop.blk'
      include 'ebptr.blk'
      include 'exqain.blk'
      include 'namptr.blk'
      INCLUDE 'argparse.inc'

C************************************************************************
C     Program variables
C************************************************************************
C     QA - program information
      character*(mxstln) qainfo(6)

C     done  - true indicates to terminate the program
C             flase indicates the program is not finished
C     okay  - true indicates that the files are compatible for appending
C             false indicates that the files are not compatible for appending
      logical done, okay
      logical batch

C     mfile - name of master file
C     nfile - name of new file to append
C     rfile - name of results file
      character*2048 mfile, nfile, rfile, scratch

C     tminm - minimum time step value in master file
C     tmaxm - maximum time step value in master file
C     tminn - minimum time step value in new appending file
C     tmaxn - maximum time step value in new appending file
C     rtmin - minimum time step value in results file
C     rtmax - maximum time step value in results file
      real tminm, tmaxm, tminn, tmaxn, rtmin, rtmax

C     Time variables
C     nsteps - number of time steps in master file
C     newstp - number of time steps in new appending file
C     nstepr - number of time steps written to results file
      integer nsteps, newstp, nstepr

C     ndbm - file id for master file
C     ndbn - file id for new appending file
C     ndbo - file id for results file
      integer ndbm, ndbn, ndbo

C     variables used in exodusII calls
C     ierr   - error number form exodusII API calls
C     cpuws  - cpu word size master file
C     iows   - io word size for master file
C     ncpuws - cpu word size for new appending file
C     niows  - io word size for new appending file
C     vers   - version number for master file
C     versn  - version number for new appending file
C     mopen  - indicates that master file is open
C     nopen  - indicates that new appending file is open
C     ropen  - indicates that results file is open
C     mdebug - flag for debugging dynamic memory problems
      integer ierr, cpuws, iows, ncpuws, niows
      character*(MXSTLN) cnum
      real vers, versn
      logical mopen, nopen, ropen
      logical mdebug

C     dynamic memory
C     nerr - number of accummulated error for numeric dynamic memory
C     cerr - number of accummulated errors for character dynamic memory
      integer nerr, cerr
      dimension   a(1)
      character*1 c(1)

C************************************************************************
C     Program Information
C************************************************************************
      call version (qainfo)

      call strtup (qainfo)
      call bannr2 (MXLNLN, qainfo(1), 0)
      call banner (0, qainfo, 'A code for sequentially appending',
     &             'Exodus II databases', ' ')
      call cpyrgt (0, '2008')
      write (*,*)

C************************************************************************
C     Initialize variables; memory manager
C************************************************************************
      call mdinit (a)
      call mcinit (c)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         go to 200
      end if   

C************************************************************************
C     program variable initialization
C************************************************************************
      ndbm   = 11
      ndbo   = 12
      done   = .false.
      ropen  = .false.
      mopen  = .false.
      nopen  = .false.
      mdebug = .false.
      nsteps = 0
      newstp = 0
      tminm  = 0.0
      tmaxm  = 0.0
      tminn  = 0.0
      tmaxn  = 0.0

C************************************************************************
C     Debug memory
C************************************************************************
      if (mdebug) then
         call mlist()
      end if

C************************************************************************
C     exodusII variable initialization
C************************************************************************
      cpuws = 0
      iows  = 0
      vers  = 0.0
      call exopts(EXABRT, ierr)

C .. Check number of arguments on command line.
      NARG = argument_count()
      if (narg .lt. 2) then
        CALL PRTERR ('FATAL', 'Filenames not specified.')
        CALL PRTERR ('FATAL',
     *    'Syntax is: "conex first_file_in file_out"')
        GOTO 210
      else if (narg .gt. 2) then
        CALL PRTERR ('FATAL', 'Too many arguments specified.')
        CALL PRTERR ('FATAL',
     *    'Syntax is: "conex first_file_in file_out"')
        GOTO 210
      end if

C************************************************************************
C     Open Master file read; time step data
C************************************************************************
C     Open Master file
      CALL get_argument(1,mfile, lfil)
      ndbm = exopen(mfile(:lfil), EXREAD, cpuws, iows, vers, ierr)
      IF (IERR .NE. 0) THEN
        SCRATCH = 'Database "'//mfile(:lfil)//'" does not exist.'
        CALL PRTERR ('FATAL', SCRATCH(:LENSTR(SCRATCH)))
        GOTO 210
      END IF
      mopen = .true.

C************************************************************************
C     Read Initialization parameters. Inquire parameters 
C************************************************************************
C     read initialization parameters for genesis file
      call init (ndbm, title, ndim, numnp, numel, nelblk,
     &           numnps, numess, lnpsnl, lnpsdf, lessnl,
     &           lessel, lessdf, nqarec, ninfo, nvargl,
     &           nvarnp, nvarel, numebp, numnsp, numssp)

C************************************************************************
C     Read time parameters.  Reserve memory to read time values
C************************************************************************
C     read number of time steps
      call exinq(ndbm, EXTIMS, nsteps, rnum, cnum, ierr)
C     reserve dynamic memory for time step array and time step values array
C     STEPSM - time STEPS in Master. Inicates if time step is written to output
C     = 1 if corresponding time value is written to output file
C     = 0 if corresponding time value is not written to output file
      call mdrsrv ('STEPSM', kstepm, nsteps)
C     TIMESM - TIMES in Master
      call mdrsrv ('TIMESM', ktimem, nsteps)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         go to 200
      end if

C     initialize values in STEPSM and TIMESM arrays
C     Default Behavior - time step values must be >= rtmax before
C     being written to output
      call iniint (nsteps, 0, a(kstepm))
      call inirea (nsteps, 0.0, a(ktimem))

C     read time values from master file
      call exgatm(ndbm, a(ktimem), ierr)

C     initiablize tmin/tmax variables for master file
      tminm = a(ktimem)
      tmaxm = a(ktimem + nsteps -1)

C************************************************************************
C     Reserve memory to store time values in result file
C************************************************************************
      nstepr = 0
      irtime = 0
      irstep = 0
      rtmin  = -100.0
      rtmax  = -100.0
      call mdrsrv ('RTIME', irtime, nstepr)
C     RSTEP - store index into time arrays to be written to output
      call mdrsrv ('RSTEP', irstep, nstepr)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         go to 200
      end if

C************************************************************************
C     Pre-process time steps
C************************************************************************
C     This subroutine will mark the acceptable times to be added
C     to the results file based on the maximum time in the results
C     file.
      call pretim (nsteps, a(ktimem), a(kstepm), tminm, tmaxm, rtmax)

C************************************************************************
C     Command Mode - prompt user for manipulation of master file data
C************************************************************************
      call rdcomd(nstepr, a(irtime), a(irstep), rtmin, rtmax,
     &            nsteps, a(ktimem), a(kstepm), tminm, tmaxm, 
     &            done, ioerr)
      if (ioerr .eq. 1) go to 200


C************************************************************************
C     Read Header of Master file
C************************************************************************
      call rdhead(ndbm, a, c, ioerr)
      if (ioerr .eq. 1) go to 200

C************************************************************************
C     open results file
C************************************************************************
      CALL get_argument(2,rfile, lfil)
      ndbo = excre(rfile(:lfil), EXCLOB, cpuws, iows, ierr)
      IF (IERR .NE. 0) THEN
        SCRATCH = 'Problems creating database "'//rfile(:LFIL)//'".'
        CALL PRTERR ('FATAL', SCRATCH(:LENSTR(SCRATCH)))
        goto 210
      END IF
      ropen = .true.
C     Write Master File Header and Transfer data to Results file
      call cphead (ndbo, ndbm, a, a, c, qainfo, ioerr)
 
C************************************************************************
C     write master time results to master_tmax
C************************************************************************
      call mdfind ('IDELB', kidelb, nelblk)
      call mdlong ('RTIME', irtime, nstepr + nsteps)
      call mdlong ('RSTEP', irstep, nsteps)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         go to 200
      end if

C     Write Time Data to Results file
      call wtmres (ndbo, nsteps, a, a(kstepm), a(ktimem), nstepr,
     &             a(irstep), a(irtime), rtmin, rtmax, ntout, ioerr)
      if (ioerr .eq. 1) go to 200
      call mdlong ('RTIME', irtime, nstepr)
      call mdlong ('RSTEP', irstep, nstepr)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         go to 200
      end if

C************************************************************************
C     write selected results to results file
C************************************************************************
      call cpres (ndbm, ndbo, a, a(kstepm), a(irstep), a(irtime),
     &            a(kidelb), a(knelb), ntout, a(kevtt), ioerr)

C************************************************************************
C     Process new files
C************************************************************************
C     fortran while loop
 100  if (.not. done) then

C        Prompt User for New File
C        -Exit program if input is blank: done= .true.
C                                         goto LABEND
 300     continue
         call prompt('Input name of NEW database to add: ',
     &               nfile, done)
         if (done) go to 200

C        Open New file
         len = lenstr(nfile)
         ncpuws = 0
         niows = 0
         versn = 0.0
C        Check if exodus II file
         ndbn = exopen(nfile(:len), EXREAD, ncpuws, niows, versn, ierr)
         if (ndbn .lt. 0) then
            write(*,'(a,a)')'ERROR opening file ',nfile(:len)
            write(*,1020) 'Possible Errors:',
     &                    'File may not exist',
     &                    'File may not be an exodusII file'
 1020       format(a,/,2(17x,a,/))
            go to 300
         end if
         nopen = .true.

C        read number of time steps in new file
         call exinq(ndbn, EXTIMS, newstp, rnum, cnum, ierr)

C        reserve dynamic memory for time step array and time step values array
C        STEPSN - time STEPS in New file
         call mdrsrv ('STEPSN', kstepn, newstp)
C        TIMESN - TIME step valueS in New file
         call mdrsrv ('TIMESN', ktimen, newstp)
         call mdstat (nerr, mem)
         if (nerr .gt. 0) then
            call memerr
            go to 200
         end if

C        initialize values in STEPSN and TIMESN arrays
         call iniint (newstp, 0, a(kstepn))
         call inirea (newstp, 0.0, a(ktimen))

C        read new file times
         call exgatm(ndbn, a(ktimen), ierr)

C        initiablize tmin/tmax variables for new file
         tminn = a(ktimen)
         tmaxn = a(ktimen+newstp-1)

C        Pre-process time steps
         call pretim (newstp, a(ktimen), a(kstepn), tminn, tmaxn, rtmax)

C        Command Mode
         call rdcomd(nstepr, a(irtime), a(irstep), rtmin, rtmax,
     &               newstp, a(ktimen), a(kstepn), tminn, tmaxn,
     &               done, ioerr)

C************************************************************************
C        Read Header of New file
C        Compare Header information of master and new files
C        -Print fatal errors; Print warning messages
C        -if compare is successful okay = .true. else okay = .false.
         okay = .true.
         call cmpinp(ndbn, a, c, okay, ioerr)
         if (ioerr .gt. 0) go to 200
C************************************************************************
         if (okay) then
C              write global, nodal and element results from master file
               call mdlong ('RTIME', irtime, nstepr + newstp)
               call mdlong ('RSTEP', irstep, newstp)
               call mdstat (nerr, mem)
               if (nerr .gt. 0) then
                  call memerr
                  go to 200
               end if
               call wtmres (ndbo, newstp, a, a(kstepn), a(ktimen),
     &                      nstepr, a(irstep), a(irtime),
     &                      rtmin, rtmax, ntout, ioerr)
               if (ioerr .eq. 1) go to 200
               call mdlong ('RTIME', irtime, nstepr)
               call mdlong ('RSTEP', irstep, nstepr)
               call mdfind ('IDTMP', idtmp, nelblk)
               call mdstat (nerr, mem)
               if (nerr .gt. 0) then
                  call memerr
                  go to 200
               end if

C              write global, nodal and element results from new file
               call cpres (ndbn, ndbo, a, a(kstepn), a(irstep),
     &                     a(irtime), a(idtmp), a(knelb),
     &                     ntout, a(kevtt), ioerr)

         else if (.not. okay) then
C            print message that files are not compatible
             write(6, 1000)
 1000        format(/,'ERROR: Database Compatibility Tests Failed',/
     $             '**************************************************',
     $            /)
         else
C            default (okay is undefined) memory error 
             write(6, 1010)
 1010        format('Unknown Error: okay undefined')

         end if

C        delete memory
         call mddel ('TIMESN')
         call mddel ('STEPSN')
         call mddel ('IDTMP')
C        compress
         call mdcomp()
         call mccomp()
         call mcstat (cerr, mem)
         call mdstat (nerr, mem)
         if ((nerr .gt. 0) .or. (cerr .gt. 0)) then
            call memerr
            go to 200
         end if

C        find all pointers
         call fndptr(ioerr)
         if (ioerr .eq. 1) go to 200
         call mdfind ('STEPSM', kstepm, nsteps)
         call mdfind ('TIMESM', ktimem, nsteps)
         call mdfind ('RSTEP', irstep, nstepr)
         call mdfind ('RTIME', irtime, nstepr)
         call mdstat (nerr, mem)
         if (nerr .gt. 0) then
            call memerr
            go to 200
         end if

C        close new file
         call exclos (ndbn, ierr)
         nopen = .false.

C        continue with fortran while loop
         if (okay .or. .not. batch()) then
C ... If the compatability checks failed and conex is being 
C     run in batch, error out.  If being run interactively, 
C     continue since the user can decide how to continue.
           goto 100
         end if
      end if

 200  continue

C     delete dynamic memory
      call mddel ('STEPSM')
      call mddel ('TIMESM')
      call mddel ('RSTEP')
      call mddel ('RTIME')
      call mddel ('NUMELB')
      call mddel ('NUMLNK')
      call mddel ('NUMATR')
      call mcdel ('NAMELB')
      call mddel ('IEVTT')
      if (nvargl .gt. 0) call mcdel ('GLOB')
      if (nvarnp .gt. 0) call mcdel ('NODE')
      if (nvarel .gt. 0) call mcdel ('ELEM')
      call mcstat (cerr, mem)
      call mdstat (nerr, mem)
      if ((nerr .gt. 0) .or. (cerr .gt. 0)) call memerr

C     close any open files
 210  continue
      if (ropen) call exclos(ndbo, ierr)

      if (mopen) call exclos(ndbm, ierr)

      if (nopen) call exclos(ndbn, ierr)

C     end program
      call addlog (QAINFO(1)(:lenstr(QAINFO(1))))
      call wrapup(qainfo(1))

      if (okay) then
        stop
      else
        stop 'COMPATABILITY ERROR'
      end if
      end

      subroutine mlist()

      call mdlist(6)

      return
      end

