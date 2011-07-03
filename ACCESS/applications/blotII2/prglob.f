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

C $Id: prglob.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: prglob.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.3  2008/10/23 17:27:50  gdsjaar
C Handle output of reals with 3-digit exponents 1.0e-100 was printing as 1.0-100
C
C Revision 1.2  1996/06/21 16:07:12  caforsy
C Ran ftnchek and removed unused variables.  Reformat output for list
C var, list global, and list name.
C
C Revision 1.1  1994/04/07 20:07:35  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:55:05  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE PRGLOB (OPTION, NOUT, NVARGL, LISGV, NAMEGV, VARGL)
C=======================================================================

C   --*** PRGLOB *** (BLOT) Display current database global variables
C   --   Written by Amy Gilkey - revised 11/05/87
C   --
C   --PRGLOB displays the global data for a time step.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to print all, else print options
C   --   NOUT - IN - the output file, <=0 for standard
C   --   NVARGL - IN - the number of global variables
C   --   LISGV - IN - the indices of the selected global variables
C   --   NAMEGV - IN - the names of the global variables
C   --   VARGL - IN - the global variables for the time step

      include 'params.blk'
      CHARACTER*(*) OPTION
      INTEGER LISGV(0:*)
      CHARACTER*(MXSTLN) NAMEGV(*)
      REAL VARGL(*)

      IF (NOUT .GT. 0) WRITE (NOUT, 10000)

      IF (NOUT .GT. 0) THEN
         WRITE (NOUT, 10010) (NAMEGV(LISGV(I)), I=1,LISGV(0))
      ELSE
         WRITE (*, 10010) (NAMEGV(LISGV(I)), I=1,LISGV(0))
      END IF

      IF (NOUT .GT. 0) THEN
         WRITE (NOUT, 10020, IOSTAT=IDUM)
     &      (VARGL(LISGV(I)), I=1,LISGV(0))
      ELSE
         WRITE (*, 10020, IOSTAT=IDUM)
     &      (VARGL(LISGV(I)), I=1,LISGV(0))
      END IF

      RETURN

10000  FORMAT (/, 1X, 'GLOBAL TIME STEP VARIABLES')
10010  FORMAT (/, 1X, :, 2(2X, A32), :, /, (1X, 2(2X, A32)))

10020  FORMAT (/, 1X, 'Globals', /, 2(2X, 1PE15.7E3, 18X), :, /,
     &   (2(2X, 1PE15.7E3, 18X)))

      END
