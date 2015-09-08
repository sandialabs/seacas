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
C $Id: prompt.f,v 1.4 2008/03/14 13:50:53 gdsjaar Exp $
C************************************************************************
      subroutine prompt (pmsg, filnam, done)
C************************************************************************
C
C     prompt
C     C.A. Forsythe 01/18/96
C
C DESCRIPTION: 
C     prompt user for input.  This includes the master file name
C     and the new file name to be added.  A description of the
C     input format will be printed
C     Input Format: filname rvalue
C
C PARAMETERS
C     pmsg   - IN  - prompt message printed for user
C     filnam - OUT - file name
C     done   - OUT - flag to indicated end of program
C
C************************************************************************

      include 'params.blk'

C     subroutine arguments
      character*(*) pmsg
      character*(MXLNLN) filnam
      logical done
C     local variables
      character*(MXLNLN) tmpstr
      integer len

C     clear strings
      do 10 i = 1, MXLNLN
         filnam(i:i) = ' '
         tmpstr(i:i) = ' '
 10   continue

      write(*, 1000)pmsg
 1000 format(a,
     & /, 5x, 'NOTE: filename is case sensitive',
     & /, 5x, 'Type exit, quit, or end to terminate program')

 20   continue
      call getinp (0, 0, 'FILENAME> ', filnam, iostat)

C     Check for no input condition
      IF (lenstr(filnam) .EQ. 1 .AND. filnam(1:1) .EQ. ' ') THEN
C        carriage return - typo error by user
         go to 20
      end if

C     check for exit condition
      write(tmpstr, '(a)') filnam
      call exupcs(tmpstr)
      
      call sqzstr(tmpstr, len)
      if ((tmpstr(1:4) .eq. 'QUIT') .or.
     &    (tmpstr(1:4) .eq. 'EXIT') .or.
     &    (tmpstr(1:3) .eq.  'END')) then
         done = .true.
         write (*,'(a)') 'Terminate Program'
      else
C        debug
         write(*,'(a,a)')'Filename = ', filnam
      end if


      return
      end




