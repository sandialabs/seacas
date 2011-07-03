C Copyright (C) 2009 Sandia Corporation.  Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C certain rights in this software
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
C 
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
C 

C $Id: pltstm.f,v 1.1 2009/06/10 18:23:55 gdsjaar Exp $ 
C $Log: pltstm.f,v $
C Revision 1.1  2009/06/10 18:23:55  gdsjaar
C Open source and copyright assertion
C
C Title:	SVDI Ver. 1.1
C SCR#:	126.2
C Type:	Open Source Software
C
C The Department of Energy (DOE), by memo dated 06/10/2009, has granted
C Sandia permission to assert its copyright in software entitled "SVDI
C Ver. 1.1".
C
C In accordance with the DOE's permission:
C
C All copies of "SVDI Ver. 1.1" that are released under an Open Source
C Software (OSS) license should be marked: Copyright (year first
C published) Sandia Corporation. Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C certain rights in this software.
C
C The above notice should appear before the OSS license, and the above
C notice and the OSS license are the ONLY terms that should appear in
C the software or the associated web site. No other disclaimers or
C notices should be used.
C
C The primary vehicle for distribution of "SVDI Ver. 1.1" should be the
C OSS license and associated web site. However, if "SVDI Ver. 1.1" is to
C be released without an OSS license it should be marked: Copyright
C (year first published) Sandia Corporation. Under the terms of Contract
C DE-AC04-94AL85000, there is a non-exclusive license for use of this
C work by or on behalf of the U.S. Government. Export of this program
C may require a license from the United States Government.
C
C At a minimum, this copyright notice should be on the package
C containing the program. Preferably, the copyright notice should also
C be incorporated into the source code and be displayed on at least the
C first screen that comes up when the software is run. In addition, the
C following complete language should also be included in the copyright
C notice:
C
C NOTICE:
C
C 	For five (5) years from 06/10/2009, the United States
C 	Government is granted for itself and others acting on its
C 	behalf a paid-up, nonexclusive, irrevocable worldwide license
C 	in this data to reproduce, prepare derivative works, and
C 	perform publicly and display publicly, by or on behalf of the
C 	Government. There is provision for the possible extension of
C 	the term of this license. Subsequent to that period or any
C 	extension granted, the United States Government is granted for
C 	itself and others acting on its behalf a paid-up,
C 	nonexclusive, irrevocable worldwide license in this data to
C 	reproduce, prepare derivative works, distribute copies to the
C 	public, perform publicly and siplay publicly, and to permit
C 	others to do so. The specific term of the license can be
C 	identified by inquiry made to Sandia Corportation or DOE.
C
C 	NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES
C 	DEPARTMENT OF ENERGY, NOR SANDIA CORPORATION, NOR ANY OF THEIR
C 	EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES
C 	ANY LEGAL RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR
C 	USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR PROCESS
C 	DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE
C 	PRIVATELY OWNED RIGHTS.
C
C 	Any licensee of this software has the obligation and
C 	responsibility to abide by the applicable export control laws,
C 	regulations, and general prohibitions relating to the export
C 	of technical data. Failure to obtain an export control license
C 	or other authority from the Government may result in criminal
C 	liability under U.S. laws.
C
C (End of Notice)
C
C This alternate language should only be used if the software is to be
C distributed without the OSS license. If there is any question at all
C as to which notice is appropriate, please contact Jason D. Martinez at
C (505) 844-8067.
C
C This software has been assigned SCR# 126.2. This number is a Sandia
C internal tracking number and is a useful reference when contacting the
C Intellectual Property Center or Licensing and IP Management. However,
C since this software is to be released as OSS, the software is deemed
C to be Publicly Available.
C
C Revision 1.1  1993/07/16 16:49:35  gdsjaar
C Changed plt to library rather than single source file.
C 
C=======================================================================
      LOGICAL FUNCTION PLTSTM(INDX,BUFF)
      REAL DEVCAP(23)
      REAL DEFOUT(7)
      COMMON /STATUS/DEVCAP,DEFOUT
      REAL DEVP(5)
      COMMON /DEVICE/DEVP
      REAL COLP(3)
      REAL PALETT(3,16)
      COMMON /COLOR/COLP,PALETT
      REAL TEXTP(40)
      COMMON /TEXT/TEXTP
      REAL VECTP(5)
      REAL XCUR
      REAL YCUR
      COMMON /VECTRC/VECTP,XCUR,YCUR
      INTEGER IDEX(200,2)
      INTEGER NVECT(200,2)
      REAL XSIZE(200,2)
      REAL YSIZE(200,2)
      REAL X0(2300,2)
      REAL Y0(2300,2)
      REAL X1(2300,2)
      REAL Y1(2300,2)
      COMMON /FONT/IDEX,NVECT,XSIZE,YSIZE,X0,Y0,X1,Y1
      REAL GRAPHP(100)
      COMMON /GRAPH/GRAPHP
      COMMON /MAPPAR/MAPP(11)
      REAL MAPP
      COMMON /STORAG/MEMORY(1000)
      DIMENSION BUFF(*)
      CHARACTER*16 IERROR
      REAL LEFT
      CHARACTER*6 SUBNAM
      DATA SUBNAM/'PLTSTM'/

      PLTSTM = .FALSE.
      IF (INDX.EQ.0) THEN
         CALL PLTRSM

      ELSE IF (INDX.EQ.1) THEN
         IF (BUFF(1).EQ.0.) THEN
            CALL PLTFLU
            CALL SIORPT(SUBNAM,'You cannot set the X scale factor to 0.'
     *                  ,2)
            RETURN

         END IF

         MAPP(1) = BUFF(1)

      ELSE IF (INDX.EQ.2) THEN
         IF (BUFF(1).EQ.0.) THEN
            CALL PLTFLU
            CALL SIORPT(SUBNAM,'You cannot set the Y scale factor to 0.'
     *                  ,2)
            RETURN

         END IF

         MAPP(2) = BUFF(1)

      ELSE IF (INDX.EQ.3) THEN
         MAPP(3) = BUFF(1)

      ELSE IF (INDX.EQ.4) THEN
         MAPP(4) = BUFF(1)

      ELSE IF (INDX.EQ.5) THEN
         MAPP(5) = BUFF(1)

      ELSE IF (INDX.EQ.6) THEN
         LEFT = BUFF(1)
         RIGHT = BUFF(2)
         BOTTOM = BUFF(3)
         TOP = BUFF(4)
         IF (LEFT.GE.RIGHT) THEN
            CALL PLTFLU
            CALL SIORPT(SUBNAM,
     *  'You cannot specify the left viewport edge >= to the right edge'
     *                  ,2)
            RETURN

         END IF

         IF (TOP.LE.BOTTOM) THEN
            CALL PLTFLU
            CALL SIORPT(SUBNAM,
     *  'You cannot specify the top viewport edge <= to the bottom edge'
     *                  ,2)
            RETURN

         END IF

         IF (TOP.LT.0 .OR. TOP.GT.DEVP(5)) THEN
            CALL PLTFLU
            CALL SIORPT(SUBNAM,'Top viewport specification out of range'
     *                  ,2)
            RETURN

         END IF

         IF (BOTTOM.LT.0 .OR. BOTTOM.GT.DEVP(5)) THEN
            CALL PLTFLU
            CALL SIORPT(SUBNAM,
     *                  'Bottom viewport specification out of range',2)
            RETURN

         END IF

         IF (LEFT.LT.0 .OR. LEFT.GT.DEVP(4)) THEN
            CALL PLTFLU
            CALL SIORPT(SUBNAM,
     *                  'Left viewport specification out of range',2)
            RETURN

         END IF

         IF (RIGHT.LT.0 .OR. RIGHT.GT.DEVP(4)) THEN
            CALL PLTFLU
            CALL SIORPT(SUBNAM,
     *                  'Right viewport specification out of range',2)
            RETURN

         END IF

         MAPP(6) = LEFT
         MAPP(8) = RIGHT
         MAPP(7) = BOTTOM
         MAPP(9) = TOP

      ELSE
         CALL CHRIC(INDX,IERROR,L)
         CALL PLTFLU
         CALL SIORPT('PLTSTM','Illegal index '//IERROR(1:L)//'.',2)
         RETURN

      END IF

      PLTSTM = .TRUE.
      MAPP(10) = 0.
      IF (MAPP(1).NE.1. .OR. MAPP(2).NE.1. .OR. MAPP(3).NE.0. .OR.
     *    MAPP(4).NE.0. .OR. MAPP(5).NE.0.) THEN
         MAPP(10) = 1.
      END IF

      IF (MAPP(6).NE.0. .OR. MAPP(8).NE.DEVP(4) .OR. MAPP(7).NE.0. .OR.
     *    MAPP(9).NE.DEVP(5)) THEN
         MAPP(10) = 1.
      END IF

      RETURN

      END
