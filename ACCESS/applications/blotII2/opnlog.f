C Copyright(C) 2009 Sandia Corporation. Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C certain rights in this software.
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

C $Id: opnlog.f,v 1.1 2009/03/25 04:47:53 gdsjaar Exp $
C $Log: opnlog.f,v $
C Revision 1.1  2009/03/25 04:47:53  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.3  2005/12/14 14:39:23  gdsjaar
C Check whether the log file can be written to.  If opening a database
C in another users area, it may be able to open the log file if the file
C already exists in that area, but then fails when trying to write to
C it. This fixes it so if the write fails, it continues execution, but
C doesn't try to write anymore.
C
C Revision 1.2  2004/08/25 16:28:56  gdsjaar
C Fixed so the variable summary information is not written to the log file if the log file cannot be opened.
C
C Revision 1.1  1994/04/07 20:06:24  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.1  1993/09/27  22:35:30  gdsjaar
c Fixed opnlog to work with exodusII files.
c
c Revision 1.3  1993/09/15  20:41:56  gdsjaar
c Removed names of graphic devices from log output
c
c Revision 1.2  1990/12/14  08:54:24  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE OPNLOG (LOGU)
C=======================================================================

C   --*** OPNLOG *** (BLOT) Open log file and write header
C   --   Written by Amy Gilkey - revised 01/11/88
C   --
C   --OPNLOG opens the log file and writes the command line as the header
C   --for the log file.
C   --
C   --Parameters:
C   --   NLOG - IN/OUT - the log file number; returned <= if log file
C   --      cannot be opened
C   --
C   --Common Variables:
C   --   Uses NDB of /DBASE/
C   --   Uses QAINFO of /PROGQA/

      include 'progqa.blk'
      include 'dbase.blk'
      include 'dbname.blk'

      CHARACTER*256 INLINE
      CHARACTER*256 STR
      LOGICAL ISON

      NLOG = LOGU
      CALL OPNFIL (NLOG, 'O', 'L', 0, IERR)
      IF (IERR .NE. 0) THEN
         CALL PRTERR ('WARNING', 'Log file cannot be opened')
         NLOG = -1
         GOTO 100
      END IF

      INLINE = '$$$ ' // QAINFO(1)
      L = LENSTR (INLINE) + 1
      
      IF (L .LT. LEN (INLINE)) INLINE(L+1:) = FILNAM
      L = LENSTR (INLINE) + 1

      CALL GRGPAR ('DEVICE', 1, ISON, STR)
      IF (ISON) THEN
         IF (L .LT. LEN (INLINE)) INLINE(L+1:) = STR
      ELSE
         IF (L .LT. LEN (INLINE)) INLINE(L+1:) = '""'
      END IF
      L = LENSTR (INLINE) + 1

      CALL GRGPAR ('DEVICE', 2, ISON, STR)
      IF (ISON) THEN
         IF (L .LT. LEN (INLINE)) INLINE(L+1:) = STR
      ELSE
         IF (L .LT. LEN (INLINE)) INLINE(L+1:) = '""'
      END IF
      L = LENSTR (INLINE) + 1

      WRITE (NLOG, '(A)', IOSTAT=IERR) INLINE(:L-1)
      if (ierr .ne. 0) then
         CALL PRTERR ('WARNING', 'Log file cannot be written')
         NLOG = -1
         GOTO 100
      end if
  100 CONTINUE
      LOGU = NLOG
      RETURN
      END
