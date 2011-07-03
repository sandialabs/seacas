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

C $Id: chkput.f,v 1.1 2009/03/25 04:47:53 gdsjaar Exp $
C $Log: chkput.f,v $
C Revision 1.1  2009/03/25 04:47:53  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.2  1996/06/21 16:07:00  caforsy
C Ran ftnchek and removed unused variables.  Reformat output for list
C var, list global, and list name.
C
C Revision 1.1  1994/04/07 19:55:34  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:48:02  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE CHKPUT (IERR, TAG, L, OK)
C=======================================================================

C   --*** CHKPUT *** (BLOT) Check sent message
C   --   Written by Amy Gilkey - revised 05/13/88
C   --
C   --CHKPUT checks the error indicator of the sent message.
C   --
C   --Parameters:
C   --   IERR - IN - the error indicator (0 = no error)
C   --   TAG  - IN - the message tag
C   --   L    - IN - the message length
C   --   OK   - IN - returns true iff no error

      CHARACTER*8 TAG
      LOGICAL OK

      OK = .TRUE.
      IF (IERR .NE. 0) THEN
         WRITE (*, 10000, IOSTAT=IDUM) IERR, TAG(:LENSTR(TAG)), L
10000    FORMAT (1X, '*** MESSAGE PUT ERROR: ', I5,
     &           ', sent ', A, ', length ', I5)
         OK = .FALSE.
      END IF

      RETURN
      END
