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
C $Id: rdcomd.f,v 1.6 2008/03/14 13:50:53 gdsjaar Exp $
C***********************************************************************
      subroutine rdcomd (ntsr, rtime, rstep, rtmin, rtmax, nsteps,
     &                   times, steps, tmin, tmax, done, ioerr)
C***********************************************************************
C
C     rdcomd - read user command
C
C PARAMETERS:
C     nrts   - IN  - number of time steps in results file
C     rtime  - IN  - array of time step values in results file
C     rstep  - IN  - array of time steps
C     rtmin  - IN  - minimum time value in results file
C     rtmax  - IN  - maximum time value in results file
C     nsteps - IN  - number of time steps
C     times  - IN  - array of time step values
C     steps  - IN  - array of time steps
C     tmin   - I/O - minimum time selected
C     tmax   - I/O - maximum time selected
C     done   - I/O - flag that indicates to end the program
C     ioerr  - OUT - error flag for memory or io errors
C
C ASSUMPTIONS:
C     All times steps are selected - steps(i)= 1 , i=1, nsteps
C     tmin = times(1)
C     tmax = times(nsteps)
C     if nsteps = 0 return
C
C***********************************************************************

      include 'params.blk'

      parameter (mfield = 80)
      integer ntsr
      real rtime(*)
      integer rstep(*)
      real rtmin, rtmax
      integer nsteps
      real times(*)
      integer steps(*)
      real tmin, tmax
      logical done
      integer ioerr

C     local variables
      integer nfield
      integer kvalue(mfield)
      character*8 cvalue(mfield)
      integer ivalue(mfield)
      real rvalue(mfield)
      logical readin
      character*8 tmpstr
      character*8 cmdtbl(13)
      character*8 verb, word, ctemp
      integer itemp, isteps(mfield)
      real rtemp

      data cmdtbl /
     1     'CONT    ', 'QUIT    ', 'EXIT    ', 'END     ',
     2     'TMIN    ', 'TMAX    ', 'TIMES   ', 'STEPS   ',
     3     'LIST    ', 'HELP    ', 'ALL     ', 'RESULTS ',
     4     '        ' /

      ioerr = 0
C     need to design strategy if nsteps=0
      if (nsteps .eq. 0) return

C     initialize local variables
      nfield = 0
      readin = .true.

C     initialize temporary arrays
      call iniint(mfield, 0, isteps)

C     print out list of commands
      call wrtcmd ()
C     fortran while loop
  100 continue
      if (readin) then
         call iniint(mfield, -1, kvalue)
         call inistr(mfield, ' ', cvalue)
         call iniint(mfield, 0, ivalue)
         call inirea(mfield, 0.0, rvalue)
         call frefld (0, 0, 'CONEX> ', mfield, iostat, nfield, kvalue,
     &                cvalue, ivalue, rvalue)
C        The supes free format reader does not work the same on
C        all platforms.  Must be careful when parsing user input.
         itemp = -1
         rtemp = 0.0
         ctemp = '        '
C        1st field should be a character string
         if (kvalue(1) .ge. 0) then
            word = cvalue(1)
         else
            word = '        '
         end if
         call abrstr (verb, word, cmdtbl)
         if (verb .eq. ' ') then
            write(*,'(a,/,a)')
     &         'Error: abbreviation or command not found',
     &         'Please enter command again'
            go to 100
         end if
         tmpstr = verb

C***********************************************************************
         if (nfield .eq. 0) then
C***********************************************************************
C           no input from user
            go to 100

C***********************************************************************
         else if (nfield .eq. 1) then
C***********************************************************************
C           check for continue flag
            if (tmpstr .eq. 'CONT') then
C              nothing is set with this command
               readin = .false.
               go to 100
C           check for exit conditions
            else if ((tmpstr .eq. 'QUIT') .or. (tmpstr .eq. 'EXIT') .or.
     &         (tmpstr .eq. 'END')) then
               readin = .false.
               done = .true.
               go to 100
            else if (tmpstr .eq. 'ALL') then
               call pretim (nsteps, times, steps, tmin, tmax, rtmax)
               if (nsteps .gt. 100) then
                 call list(-1, nsteps, times, steps, tmin, tmax)
               else
                 call list(0, nsteps, times, steps, tmin, tmax)
               end if
               readin = .false.
               go to 100
C           check for incomplete input and print out help
            else if (tmpstr .eq. 'TMIN') then
               write (*, 1000)'TMIN real_value',
     &         'real_value is minimum time to write to results file'
            else if (tmpstr .eq. 'TMAX') then
               write (*, 1000)'TMAX real_value',
     &         'real_value is maximum time to write to results file'
            else if (tmpstr .eq. 'STEPS') then
               write (*, 1000)'STEPS integer_list',
     &         'integer_list is list of times step indices'
            else if (tmpstr .eq. 'TIMES') then
               write (*, 1000)'TIMES real_list',
     &         'real_list is list of time step values'
               write (*, *)
               write (*, 1000)'ALL',
     &  'select all time values > largest time value in results file'
            else if (tmpstr .eq. 'LIST') then
               write (*, 1000)'LIST options',
     &         'options: TIMES STEPS RESULTS'
            else if (tmpstr .eq. 'HELP') then
               call wrtcmd ()
            else
               write (*, 1010) 'Syntax Error'
c               go to 100
            end if

C***********************************************************************
         else if (nfield .eq. 2) then
C***********************************************************************
C           initialize temporary variables
            itemp = -1
            rtemp = 0.0
            ctemp = '        '
            kval2 = kvalue(2)
            if (kval2 .eq. -1) then
C              null field
            else if (kval2 .eq. 0) then
C              non-numeric field - CVALUE specified
               ctemp = cvalue(2)
            else if (kval2 .eq. 1) then
C              real numeric field - cvalue and rvalue specified
               rtemp = rvalue(2)
               ctemp = cvalue(2)
            else if (kval2 .eq. 2) then
C              integer numeric field - cvalue, rvalue, and ivalue specified
               itemp = ivalue(2)
               ctemp = cvalue(2)
               rtemp = rvalue(2)
            else
C              default - error in kvalue
            end if 

C***********************************************************************
C           Process TMIN and TMAX command
C***********************************************************************
            if ((tmpstr .eq. 'TMIN').or.(tmpstr .eq. 'TMAX')) then
C              Next value should be a real value
               if ((kval2 .eq. 1) .or. (kval2 .eq.2)) then
C                 second field is a real number
                  if (tmpstr .eq. 'TMIN') then
                     if (rtemp .gt. rtmax) then
                        tmin = rtemp
                     else
                        write (*,'(a,f10.5)')
     &                  'The value of TMIN must be > ', rtmax
                     end if
                  end if
                  if (tmpstr .eq. 'TMAX') then
                     if (rtemp .gt. rtmax) then
                        tmax = rtemp
                     else
                        write (*,'(a,f10.5)')
     &                  'The value of TMAX must be > ', rtmax
                     end if 
                  end if
                  do 110 i = 1, nsteps
                     if ((times(i) .ge. tmin) .and.
     &                   (times(i) .le. tmax)) then
C                       select time step
                        steps(i) = 1
                     else
C                       set time step as not selected
                        steps(i) = 0
                     end if
 110              continue
               if (nsteps .gt. 100) then
                 call list(-1, nsteps, times, steps, tmin, tmax)
               else
                 call list(0, nsteps, times, steps, tmin, tmax)
               end if
               else
C                 error
                  write (*, 1010) 'Syntax Error'
               end if

C***********************************************************************
C           Process TIMES command
C***********************************************************************
            else if (tmpstr .eq. 'TIMES') then
               if ((kval2 .eq. 1).or.(kval2 .eq. 2)) then
C                 real value
                  if (rtemp .gt. rtmax) then
                     do 120 i = 1, nsteps
                        if (times(i) .eq. rtemp) then
                           steps(i) = 1
                        else
                           steps(i) = 0
                        end if
  120                continue
C                    Reset tmin
                     tmin = rtemp
C                    Reset tmax
                     tmax = rtemp
                     if (nsteps .gt. 100) then
                       call list(-1, nsteps, times, steps, tmin, tmax)
                     else
                       call list(0, nsteps, times, steps, tmin, tmax)
                     end if
                   else
                     write (*,'(a,f10.5,/,2(f10.5))')
     &                     'Cannot select time value ', rtemp,
     &                     rtemp,' <= ',rtmax
                  end if
               else
C                 error
                  write (*, *) 'Syntax Error'
               end if

C***********************************************************************
C           Process STEPS command
C***********************************************************************
            else if (tmpstr .eq. 'STEPS') then
C              Next value should be an integer
               if (kval2 .eq. 2) then
                  if (times(itemp) .gt. rtmax) then
C                    itemp is index to time step selected
                     do 130 i = 1, nsteps
                        if (i .eq. itemp) then
                           steps(i) = 1
                        else
                           steps(i) = 0
                        end if
 130                 continue
C     Reset tmin and tmax
                     tmin = times(itemp)
                     tmax = times(itemp)
                  else
                     write (*,'(a,f10.5,/,a, f10.5,a,f10.5)')
     &                     'Cannot select time step ', i,
     &                     'step(i) = ',times(i),' <= ',rtmax
                  end if
               else
                  write(*, 1010) 'Syntax Error'
               end if
               if (nsteps .gt. 100) then
                 call list(-1, nsteps, times, steps, tmin, tmax)
               else
                 call list(0, nsteps, times, steps, tmin, tmax)
               end if
C***********************************************************************
C           Process LIST command
C***********************************************************************
            else if (tmpstr .eq. 'LIST') then
C              Next value should be a character string
               if (kval2 .ge. 0) then
                  ctemp = cvalue(2)
                  call abrstr (verb, ctemp, cmdtbl)
                  if (verb .eq. ' ') verb = ctemp
               end if
               if ((verb .eq. 'TIMES').or. (verb .eq. 'STEPS')) then
                  call list(0, nsteps, times, steps, tmin, tmax)
               else if (verb .eq. 'RESULTS') then
                  if (ntsr .eq. 0) then
                     write (*,*)'No time results written to result file'
                  else
                     call iniint(ntsr, 1, rstep)
                     call list (1, ntsr, rtime, rstep, rtmin, rtmax)
                  end if
               else
                  write (*, 1010)'Syntax Error'
                  write (*, 1015)'LIST option',
     &                           'option: TIMES STEPS RESULTS'
               end if
            else
C              Error in parsing string
               write (*, 1010) 'Syntax Error'
            end if

C***********************************************************************
         else if (nfield .gt. 2) then
C***********************************************************************
            if (nfield .gt. mfield) then
               write (*, *) 'Too Many fields'
               go to 100
            end if

C***********************************************************************
C           Process STEPS command
C***********************************************************************
            if (tmpstr .eq. 'STEPS') then
C              initialize temporary array
               call iniint(mfield, 0, isteps)
C              check syntax of input - fields 2-nfield should be integers
               icount = 0
               do 140 i = 2, nfield
                  if ((kvalue(i) .eq. 1) .or.
     &                (kvalue(i) .eq. 2)) then
C                    integer value Okay
C                    store integer steps in temporary isteps array
                     icount = icount + 1
                     isteps(icount) = ivalue(i)
                  else
C                    non numeric value
                     write (*,*) 'Syntax Error'
                     go to 100
                  end if
 140           continue

C              icount   = number of steps to save
C              nfield-1 = number of fields in the user command
C                         -1 for the 'STEPS' command
C                       = number of steps to be saved
               if (icount .ne. (nfield-1)) then
                  write(*,*)'STEPS WARNING'
                  ioerr = 1
                  return
               end if

C              Initialize steps array to 0's
               call iniint(nsteps, 0, steps)
C              need to calculate tmin and tmax again
               tmin = times(nsteps)
               tmax = times(1)
C              Set all steps

               do 145 i = 1, icount
                  rtmp = times(isteps(i))
                  if (rtmp .gt. rtmax) then
C                    okay to save time step
                     steps(isteps(i)) = 1
C                    Reset tmin
                     if (rtmp .lt. tmin) tmin = rtmp
C                    Reset tmax
                     if (rtmp .gt. tmax) tmax = rtmp
                  else
                     write (*,'(a,i4,/,f10.5,a,f10.5)')
     &                     'Cannot select time step ', isteps(i),
     &                     rtmp,' <= ',rtmax
                  end if
 145           continue

               call list(0, nsteps, times, steps, tmin, tmax)

C***********************************************************************
C           Process TIMES command
C***********************************************************************
            else if (tmpstr .eq. 'TIMES') then
               call iniint(nsteps, 0, steps)
               call iniint(mfield, 0, isteps)

C              check syntax of input - fields 2-nfield should be reals
               icount = 0
               do 150 i = 2, nfield
                  if ((kvalue(i) .eq. 1) .or.
     &                (kvalue(i) .eq. 2)) then
C                    integer value Okay
C                    store integer steps in temporary isteps array
                     icount = icount + 1
                     rtemp = rvalue(i)
C                    set steps array from rvalues
                     do 152 j = 1, nsteps
                        if (times(j) .eq. rtemp) then
                           steps(j) = 1
                           isteps(icount) = j
                        end if
 152                 continue
                  else
C                    non numeric value
                     write (*,*) 'Syntax Error'
                     go to 100
                  end if
 150           continue

C              icount   = number of steps to save
C              nfield-1 = number of fields in the user command
C                         -1 for the 'STEPS' command
C                       = number of steps to be saved
               if (icount .ne. (nfield-1)) then
                  write(*,*)'TIMES ERROR'
                  ioerr = 1
                  return
               end if


C              need to calculate tmin and tmax again
               tmin = times(nsteps)
               tmax = times(1)

C              Set all steps
               do 155 i = 1, icount
                  rtmp = times(isteps(i))
                  if (rtmp .gt. rtmax) then
C                    okay to save time step
                     steps(isteps(i)) = 1
C                    Reset tmin
                     if (rtmp .lt. tmin) tmin = rtmp
C                    Reset tmax
                     if (rtmp .gt. tmax) tmax = rtmp
                  else
                     write (*,'(a,f10.5,/,f10.5,a,f10.5)')
     &                     'Cannot select time value ', rtmp,
     &                     rtmp,' <= ',rtmax
                  end if
 155           continue
               call list(0, nsteps, times, steps, tmin, tmax)

            else
C              default error message
               write (*, *) 'Syntax Error'
            end if
C***********************************************************************
         else
C***********************************************************************
C           error nfield undefined of nfield < 0
C           print error message
            ioerr = 1
            return
         end if

C        fortran while loop
         write (*, *)
         go to 100
      end if
 1000 format (a,/,a)
 1010 format (a)
 1015 format (a,/,a)
 1020 format (a,I10,a)
 1025 format (a, f10.5, a)

      return
      end


C***********************************************************************
C     Utility subroutines for rdcomd.f
C***********************************************************************
      subroutine wrtcmd()
      write (*,*) 'COMMANDS FOR CONEX2'
      write (*,*) '-------------------'
      write (*,1030) 
     &      'ALL          : Select times > largest result time',
     &      '               Continue with program',
     &      'CONT         : Continue with program',
     &      'TMIN         : Select minimum time for output',
     &      'TMAX         : Select maximum time for output',
     &      'TIMES        : Select times for output',
     &      'STEPS        : Select time steps for output',
     &      'LIST TIMES   : List database time steps and values',
     &      'LIST STEPS   : List database time steps and values',
     &      'LIST RESULTS : List times written to result file',
     &      'EXIT         : Terminate program',
     &      'END          : Terminate program',
     &      'QUIT         : Terminate program',
     &      'HELP         : Print this list'
      write (*,'(/,/)')
 1030 format (14(5x,a,/))
      return
      end


