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
C $Id: list.f,v 1.5 2008/03/14 13:50:52 gdsjaar Exp $
C=======================================================================
      subroutine list (option, nsteps, times, steps, tmin, tmax)
C=======================================================================
C   list display the time step and associated time values.
C   C.A. Forsythe  1/30/06
C
C   Parameters:
C     option  - IN - 0: print selected time steps 1: print all
C                    -1 print summary only.
C     nsteps  - IN - number of time steps
C     times   - IN - array of time values
C     tmin    - IN - minimum time in database or selected
C     tmax    - IN - maximum time in database or selected

      integer option
      integer nsteps
      real times(nsteps)
      integer steps(nsteps)
      real tmin, tmax

      integer nsel 
      nsel = 0

      if (option .eq. 0 .or. option .eq. 1) then
        do 10 i = 1, nsteps
          if (option .eq. 0) then
            if (steps(i) .ge. 1) then
              write (*, 1000) i,') ', times(i), 'Selected'
              nsel = nsel + 1
            else if (steps(i) .eq. 0) then
              write (*, 1000) i,') ', times(i), 'Not Selected'
            else
              write (*, 1005) 'ERROR: step ',i,' = ',steps(i)
            end if
          else if (option .eq. 1) then
            write (*,1002) i,') ', times(i)
          end if
 10     continue
      else
        do 20 i = 1, nsteps
          if (steps(i) .ge. 1) nsel = nsel + 1
 20     continue
      end if
      
      write (*, 1010) 'minimum time                     = ', tmin
      write (*, 1010) 'maximum time                     = ', tmax
      write (*, 1015) 'number of time steps on database = ', nsteps
      write (*, 1015) 'number of time steps selected    = ', nsel

      if (tmin .gt. tmax)
     &   write (*, '(10x, a)') '**WARNING**: tmin > tmax'

 1000 format(5x, i6, a2, 1pe16.5,2x, a)
 1002 format(5x, i6, a2, 1pe16.5)
 1005 format(5x, a, i6, a, i4)
 1010 format(10x, a, 1pe16.5)
 1015 format(10x, a, 4x, I6)

      return
      end

