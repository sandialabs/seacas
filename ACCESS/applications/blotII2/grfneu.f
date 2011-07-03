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

C $Id: grfneu.f,v 1.1 2009/03/25 04:47:53 gdsjaar Exp $
C $Log: grfneu.f,v $
C Revision 1.1  2009/03/25 04:47:53  gdsjaar
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
C Revision 1.2  1994/07/21 15:27:29  gdsjaar
C Increased curve name string size
C
c Revision 1.1  1994/04/07  20:02:21  gdsjaar
c Initial checkin of ACCESS/graphics/blotII2
c
c Revision 1.1  1994/01/14  19:47:30  gdsjaar
c Added capability to write both grafaid and xmgr neutral files.
c
c Revision 1.2  1990/12/14  08:59:33  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE GRFNEU (NPTS, XPTS, YPTS, PLTITL, TXLAB, TYLAB)
C=======================================================================

C   --*** WRTNEU *** (XYPLOT) Write curve to neutral file
C   --   Written by Amy Gilkey - revised 04/21/88
C   --
C   --WRTNEU writes the data for a curve to a neutral file.  The first
C   --time the routine is called, the neutral file is opened.
C   --
C   --The format of the neutral file is described in "GRAFAID Code User
C   --Manual" under Neutral File Format.
C   --
C   --Parameters:
C   --   NPTS - IN - the number of points on the curve
C   --   XPTS, YPTS - IN - the points on the curve
C   --   PLTITL - IN - the plot title describing the curve
C   --      (e.g. "TIME vs SIGXX at ELEMENT 30")
C   --   TXLAB, TYLAB - IN - the X and Y axis labels, either the
C   --      user-input labels or the plot variable descriptions
C   --
C   --Common Variables:
C   --   Uses TITLE, CREATE, MODIFY, DRAW of /DBTITL/
C   --   Uses DOQA, CAPTN of /LEGOPT/
C   --   Uses XMIN, XMAX, YMIN, YMAX of /LIMITS/
C   --   Uses NEU, NUMCRV, INCCRV, CRVNAM of /NEUTR./
C   --   Uses and sets NEUOPN of /NEUTR./

      include 'dbtitl.blk'
      include 'legopt.blk'
      include 'xylim.blk'
      include 'neutr.blk'

      REAL XPTS(NPTS), YPTS(NPTS)
      CHARACTER*(*) PLTITL
      CHARACTER*(*) TXLAB, TYLAB

      CHARACTER*4 XTYP
      CHARACTER*4 AXTYP
      CHARACTER AUX
      CHARACTER*80 CURVE

      DATA AUX /'F'/
      DATA AXTYP /'NOLO'/

      IF (.NOT. GRFOPN) THEN

C      --Open the neutral file and write the title line

         CALL OPNFIL (NEUGRF, 'O', 'L', 0, IERR)
         IF (IERR .NE. 0) THEN
            CALL PRTERR ('CMDERR', 'Neutral file cannot be opened')
            GOTO 170
         END IF
         GRFOPN = .TRUE.

         WRITE (NEUGRF, 10000) DRAW(1)(:LENSTR(DRAW(1))),
     &      CREATE(1)(:LENSTR(CREATE(1))), CREATE(3), CREATE(4),
     &      MODIFY(1)(:LENSTR(MODIFY(1))), MODIFY(3), MODIFY(4)
10000     FORMAT (A, ': CREATED BY ', A, ' ', A, ' ', A,
     &      ', MODIFIED BY ', A, ' ', A, ' ', A)
      END IF

C   --Figure out if the X points are monotonic or not

      XTYP = 'MONO'
      DO 100 I = 2, NPTS
         IF (XPTS(I-1) .GE. XPTS(I)) THEN
            XTYP = 'NONM'
            GOTO 110
         END IF
  100 CONTINUE
  110 CONTINUE

C   --Get the curve name

      WRITE (CURVE, '(A32, I7)', IOSTAT=IDUM) CRVNAM, NUMCRV
      CALL PCKSTR (1, CURVE)
      NUMCRV = NUMCRV + INCCRV

      WRITE (*, 10090) 'Writing ', PLTITL(:LENSTR(PLTITL))

C   --Write the begin curve record with the curve name

      WRITE (NEUGRF, 10010) 'BEGIN CURVE', CURVE
10010  FORMAT (A, ',', A15)

C   --Write the title lines

      DO 120 IEND = 3, 1, -1
         IF (CAPTN(IEND,2) .NE. ' ') GOTO 130
  120 CONTINUE
  130 CONTINUE
      IF (DOQA(2)) THEN
         WRITE (NEUGRF, 10020) 1+MAX(IEND,1), TITLE
         IF (IEND .GT. 0) THEN
            DO 140 I = 1, IEND
               WRITE (NEUGRF, 10030) CAPTN(I,2)
  140       CONTINUE
         ELSE
            WRITE (NEUGRF, 10030) PLTITL
         END IF
      ELSE IF (IEND .GT. 0) THEN
         WRITE (NEUGRF, 10020) IEND, CAPTN(1,2)
         DO 150 I = 2, IEND
            WRITE (NEUGRF, 10030) CAPTN(I,2)
  150    CONTINUE
      ELSE
         WRITE (NEUGRF, 10020) 1, PLTITL
      END IF
10020  FORMAT (I1, ',', A80)
10030  FORMAT (A80)

C   --Write the X and Y labels

      WRITE (NEUGRF, 10040) TXLAB
      WRITE (NEUGRF, 10040) TYLAB
10040  FORMAT (A40)

C   --Write the min/max, the number of points, and the auxiliary data flag

      WRITE (NEUGRF, 10050) XMIN, XMAX, YMIN, YMAX, NPTS, AUX
10050  FORMAT (4 (1PE15.7E3, ','), I5, ',', A1)
      WRITE (NEUGRF, 10060) AXTYP, XTYP, ' '
10060  FORMAT (A4, ',', A4, ',', A4)

C   --Write the data points

      DO 160 I = 1, NPTS
         WRITE (NEUGRF, 10070) XPTS(I), YPTS(I)
10070     FORMAT (2 (1PE15.7E3, :, ','))
  160 CONTINUE

C   --Write the end curve record with the curve name

      WRITE (NEUGRF, 10080) 'END CURVE', CURVE
10080  FORMAT (A, ',', A)

  170 CONTINUE
      RETURN
10090  FORMAT (1X, 5A)
      END
