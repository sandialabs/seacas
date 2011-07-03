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

C $Id: splabn.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: splabn.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.2  1999/03/09 19:41:42  gdsjaar
C Fixed missing parameter definition of MSHBOR in blotII2.f
C
C Cleaned up parameters and common blocks in other routines.
C
C Revision 1.1  1994/04/07 20:14:29  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:58:19  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE SPLABN (IPVAR, TIME, NENUM, NAMES,
     &   PLTITL, TXLAB, TYLAB)
C=======================================================================

C   --*** SPLABN *** (SPLOT) Get neutral file plot labels
C   --   Written by Amy Gilkey - revised 03/10/86
C   --
C   --SPLABN makes up the plot titles and labels for the neutral file.
C   --
C   --Parameters:
C   --   IPVAR - IN - the /SPVARS/ index of the starting plot variable
C   --   TIME - IN - the plot time
C   --   NENUM - IN - the node/element numbers
C   --   NAMES - IN - the variable names
C   --   PLTITL - OUT - the plot title describing the curves to be
C   --      plotted (e.g. "TIME vs SIGXX at ELEMENT 30")
C   --   TXLAB, TYLAB - OUT - the X and Y axis labels, either the
C   --      user-input labels or the plot variable descriptions
C   --
C   --Common Variables:
C   --   Uses NODVAR, NNENUM of /SELNE/
C   --   Uses ISVID of /SPVARS/
C   --   Uses XLAB, YLAB of /XYLAB/

      include 'params.blk'
      include 'dbnums.blk'
      include 'selne.blk'
      include 'spvars.blk'
      include 'xylab.blk'

      INTEGER NENUM(NNENUM)
      CHARACTER*(*) NAMES(*)
      CHARACTER*(*) PLTITL
      CHARACTER*(*) TXLAB, TYLAB

      CHARACTER*20 STRNUM
      CHARACTER*20 STRTIM
      CHARACTER*(MXSTLN) NAM

C   --Get the plot legend

      NAM = NAMES(ISVID(IPVAR))

      WRITE (STRNUM, 10000, IOSTAT=IDUM) NENUM(1), NENUM(NNENUM)
10000  FORMAT (I6, '..', I6)
      CALL PCKSTR (1, STRNUM)

      CALL NUMSTR (1, 4, TIME, STRTIM, LSTR)

      IF (NODVAR) THEN
         PLTITL = 'DISTANCE vs ' // NAM(:LENSTR(NAM))
     &      // ' NODES ' // STRNUM(:LENSTR(STRNUM))
     &      // ' at TIME ' // STRTIM(:LSTR)
      ELSE
         PLTITL = 'DISTANCE vs ' // NAM(:LENSTR(NAM))
     &      // ' ELEMENTS ' // STRNUM(:LENSTR(STRNUM))
     &      // ' at TIME ' // STRTIM(:LSTR)
      END IF

C   --Get the axis labels

      IF (XLAB .NE. ' ') THEN
         TXLAB = XLAB
      ELSE
         TXLAB = 'DISTANCE'
      END IF

      IF (YLAB .NE. ' ') THEN
         TYLAB = YLAB
      ELSE
         TYLAB = NAM
      END IF

      RETURN
      END
