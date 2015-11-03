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
C $Id: utils.f,v 1.6 2008/03/14 13:50:54 gdsjaar Exp $
C************************************************************************
C      Utility subroutines
C************************************************************************

C************************************************************************
      subroutine idxrea (idx, rvalue, isize, rarray, tol)
C************************************************************************
C     Find real value in array - return array index
C     idx = 0 if value not found
C     idx > 0 returns index into rarray
C     rvalue - IN - real value to search for
C     isize  - IN - size of real value array
C     rarray - IN - real value array
C     tol    - IN - allowable tolerance between 2 real values

      real rvalue
      integer isize
      real rarray(isize)
      real tol

      integer ptr

      idx = 0
      ptr = 1
 100  continue
      if (ptr .gt. isize) return
      if (abs(rvalue - rarray(ptr)) .lt. tol) then
C        rvalue = rarray(ptr)
         idx = ptr
         return
      else
         ptr = ptr + 1
         go to 100
      end if

      return
      end

C************************************************************************
      subroutine wtmres (ndbo, nstep, a, istep, times, nstepr,
     &                   rstep, rtime, rtmin, rtmax, ntsout, ioerr)
C************************************************************************
C     write time values to result file
C     reset steps and rstep to indicate indice into times array that are
C     written to output file 
      integer ndbo, nstep, nstepr, ntsout, ioerr
      dimension a(1)
      integer istep(*), rstep(*)
      real times(*), rtime(*)
      real rtmin, rtmax

      ioerr  = 0
      ntsout = 0
C     reserve scratch array to reorder istep array
      call mdrsrv ('SCR', iscr, nstep)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         ioerr = 1
         return
      end if
      call iniint(nstep, 0, a(iscr))

      do 100 i = 1, nstep
         if (istep(i) .eq. 1) then
C           check if time value is already in file
            rval = times(i)
            idx = 0
            if (nstepr .eq. 0) then
C              no need to search since no results are written to output file
            else if (nstepr .gt. 0) then
               rtol = (times(nstep) - times(1)) * 0.01 / nstep
C              find index of time that will be overwritten
               call idxrea(idx, rval, nstepr, rtime, rtol)
            else
C              nstepr < 0
               write (*,*) 'ERROR in wtmres: nstepr < 0'
               ioerr = 1
               return
            end if
            if (idx .gt. 0) then
C               time value exists in results database file
C               print warning to user
                write (*,'(a,1pe16.5,a)')'Results for time = ', 
     &                rval, ' will be overwritten'
            end if
C           increment number of results counter
            nstepr = nstepr + 1
C           write time to results file
            call exptim (ndbo, nstepr, rval, ierr)
C           copy time to results time step array
            rtime(nstepr) = rval
C           increment number of time steps that are written for this file
            ntsout = ntsout + 1
C           assign index to results step array
            rstep(ntsout) = nstepr
            call iniint(1, i, a(iscr+ntsout-1))
         end if
 100  continue
      call cpyint(ntsout, a(iscr), istep)

      call mddel('SCR')
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         ioerr = 1
      end if
      if (ntsout .gt. 0) then
         rtmin = rtime(1)
         rtmax = rtime(nstepr)

         write (*,*)'Please wait while conex2 processes the data'
      end if
      return
      end

C************************************************************************
      subroutine pretim (nstep, time, steps, tmin, tmax, rtmax)
C************************************************************************
C     This subroutine is a preprocessor for the time values
C     It will select the acceptable time to add to the results
C     file based on the maximum time value in the results file

      integer nstep, steps(*)
      real time(*), tmin, tmax, rtmax
      integer ptr

      do 100 i = 1, nstep
         if (time(i) .gt. rtmax) then
            steps(i) = 1
         else
            steps(i) = 0
         end if
 100  continue

      ptr = 1
 125  continue
      if (ptr .gt. nstep) then
C        no time steps are selected from this file
         tmin = -1.0
         go to 150
      end if
      if (steps(ptr) .eq. 1) then
C        first '1' encountered working forwards in steps array
C        set tmin and continue
         tmin = time(ptr)
      else
         ptr = ptr + 1
         go to 125
      end if
 150  continue

      ptr = nstep
 175  continue
      if (ptr .eq. 0) then
C        no time steps are selected from this file
         tmax = -1.0
         go to 200
      end if
      if (steps(ptr) .eq. 1) then
C        first '1' encountered working backwards in steps array
C        set tmax and continue
         tmax = time(ptr)
      else
         ptr = ptr - 1
         go to 175
      end if
 200  continue

      return
      end

C************************************************************************
      subroutine cpres (ndbi, ndbo, a, istep, irstep, rtime, 
     &                  idelb, nelb, ntout, ievtt, ioerr)
C************************************************************************

      integer ndbi, ndbo, ntout, ioerr
      dimension a(1)
      integer istep(*), irstep(*)
      real rtime(*)
      integer idelb(*), nelb(*)

C     Copy result data from file id ndbi to ndbo
      include 'params.blk'
      include 'exinit.blk'
      include 'exrvar.blk'

      integer ievtt(nvarel, nelblk)

      if ((nvargl .gt. 0) .or. (nvarnp .gt. 0) .or.
     &   (nvarel .gt. 0)) then
C        reserve memory for results variables
         if (nvargl .gt. 0)
     &      call mdrsrv ('VALGV', ivgl, nvargl)
         if (nvarnp .gt. 0)
     &      call mdrsrv ('VALNP', ivnp, numnp)
         if (nvarel .gt. 0) then
            imax = 0
            do 225 i = 1, nelblk
               if (nelb(i) .gt. imax) imax = nelb(i)
 225        continue
            call mdrsrv ('VALEL', ivel, imax)
         end if
         call mdstat (nerr, merr)
         if (nerr .ne. 0) then
            call memerr()
            ioerr = 1
            return
         end if
         write (*,*) 'Time steps values written to results file:'
         do 10 i = 1, ntout
C           get time step for ndbi
            ird = istep(i)
C           get time step for ndbo
            iwt = irstep(i)

C           print time step value written to output file
            write(*,2000)iwt,') ',rtime(iwt)

C           Global Variable Results
            if (nvargl .gt. 0) then
C              read data from ndbi
               call exggv (ndbi, ird, nvargl, a(ivgl), ierr)
C              write data to ndbo
               call expgv (ndbo, iwt, nvargl, a(ivgl), ierr)
            end if

C           Nodal Variable Results
            if (nvarnp .gt. 0) then
               do 150 ix = 1, nvarnp
C                 read data from ndbi
                  call exgnv (ndbi, ird, ix, numnp, a(ivnp), ierr)
C                 write data to ndbo
                  call expnv (ndbo, iwt, ix, numnp, a(ivnp), ierr)
 150           continue
            end if

C           Element Variable Results
            if (nvarel .gt. 0) then
               do 275 ix = 1, nvarel
                  do 250 ieb = 1, nelblk
                     if (ievtt(ix, ieb) .eq. 1) then
C                       read data from ndbi
                        call exgev (ndbi, ird, ix, idelb(ieb),
     &                              nelb(ieb), a(ivel), ierr)
C                       write data to ndbo
                        call expev (ndbo, iwt, ix, idelb(ieb),
     &                              nelb(ieb), a(ivel), ierr)
                     end if
 250              continue
 275           continue
            end if
 10      continue
         if (ntout .eq. 0) write (*,*) 'No results are written'
         if (nvargl .gt. 0)
     &      call mddel('VALGV')
         if (nvarnp .gt. 0)
     &      call mddel ('VALNP')
         if (nvarel .gt. 0)
     &      call mddel ('VALEL')
         call mdstat (nerr, merr)
         if (nerr .ne. 0) then
            call memerr()
            ioerr = 1
            return
         end if
      else
         write (*,*) 'No results are written'
      end if
      write(*,*)
 2000 format(5x, i6, a2, 1pe16.5)

      return
      end
