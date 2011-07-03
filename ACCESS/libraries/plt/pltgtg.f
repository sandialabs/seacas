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

C $Id: pltgtg.f,v 1.1 2009/06/10 18:23:55 gdsjaar Exp $ 
C $Log: pltgtg.f,v $
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
C Revision 1.1  1993/07/16 16:48:19  gdsjaar
C Changed plt to library rather than single source file.
C 
C=======================================================================
      LOGICAL FUNCTION PLTGTG(INDX,BUFF)
      CHARACTER*16 IERROR
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
      REAL BUFF(*)

      PLTGTG = .TRUE.
      IF (INDX.EQ.1) THEN
         BUFF(1) = GRAPHP(1)

      ELSE IF (INDX.EQ.2) THEN
         BUFF(1) = GRAPHP(2)

      ELSE IF (INDX.EQ.3) THEN
         BUFF(1) = GRAPHP(3)

      ELSE IF (INDX.EQ.4) THEN
         BUFF(1) = GRAPHP(4)

      ELSE IF (INDX.EQ.5) THEN
         BUFF(1) = GRAPHP(5)

      ELSE IF (INDX.EQ.6) THEN
         BUFF(1) = GRAPHP(38)

      ELSE IF (INDX.EQ.7) THEN
         IF (GRAPHP(6).EQ.1.) THEN
            BUFF(1) = GRAPHP(47) - 4.

         ELSE
            BUFF(1) = 0.
         END IF

      ELSE IF (INDX.EQ.8) THEN
         BUFF(1) = GRAPHP(23)

      ELSE IF (INDX.EQ.9) THEN
         BUFF(1) = GRAPHP(21)

      ELSE IF (INDX.EQ.10) THEN
         BUFF(1) = GRAPHP(37)

      ELSE IF (INDX.EQ.11) THEN
         IF (BUFF(1).EQ.3.) THEN
            BUFF(2) = GRAPHP(24)
            BUFF(3) = GRAPHP(25)
            BUFF(4) = GRAPHP(26)
            BUFF(5) = GRAPHP(27)
            BUFF(6) = GRAPHP(28)
            BUFF(7) = GRAPHP(29)
            BUFF(8) = GRAPHP(30)
            BUFF(9) = GRAPHP(31)
         END IF

         IF (BUFF(1).EQ.4.) THEN
            BUFF(2) = GRAPHP(78)
            BUFF(3) = GRAPHP(79)
            BUFF(4) = GRAPHP(80)
            BUFF(5) = GRAPHP(81)
            BUFF(6) = GRAPHP(82)
            BUFF(7) = GRAPHP(83)
            BUFF(8) = GRAPHP(84)
            BUFF(9) = GRAPHP(85)
            BUFF(10) = GRAPHP(86)
            BUFF(11) = GRAPHP(87)
         END IF

         BUFF(1) = GRAPHP(22)

      ELSE IF (INDX.EQ.12) THEN
         BUFF(1) = GRAPHP(32)

      ELSE IF (INDX.EQ.13) THEN
         BUFF(1) = GRAPHP(91)

      ELSE IF (INDX.EQ.14) THEN
         BUFF(1) = GRAPHP(90)

      ELSE IF (INDX.EQ.15) THEN
         BUFF(1) = GRAPHP(35)

      ELSE IF (INDX.EQ.16) THEN
         BUFF(1) = GRAPHP(36)

      ELSE IF (INDX.EQ.17) THEN
         BUFF(1) = GRAPHP(39)

      ELSE IF (INDX.EQ.18) THEN
         BUFF(1) = GRAPHP(40)

      ELSE IF (INDX.EQ.19) THEN
         BUFF(1) = GRAPHP(41)

      ELSE IF (INDX.EQ.20) THEN
         BUFF(1) = GRAPHP(42)

      ELSE IF (INDX.EQ.21) THEN
         BUFF(1) = GRAPHP(92)

      ELSE IF (INDX.EQ.22) THEN
         BUFF(1) = GRAPHP(44)

      ELSE IF (INDX.EQ.23) THEN
         BUFF(1) = GRAPHP(45)

      ELSE IF (INDX.EQ.47) THEN
         BUFF(1) = GRAPHP(88)

      ELSE IF (INDX.EQ.48) THEN
         BUFF(1) = GRAPHP(89)

      ELSE IF (INDX.EQ.24) THEN
         BUFF(1) = GRAPHP(46)

      ELSE IF (INDX.EQ.27) THEN
         DO 2280 I = 0,13
            BUFF(I+1) = GRAPHP(7+I)
 2280    CONTINUE

      ELSE IF (INDX.EQ.28) THEN
         BUFF(1) = GRAPHP(62)

      ELSE IF (INDX.EQ.29) THEN
         BUFF(1) = GRAPHP(63)

      ELSE IF (INDX.EQ.30) THEN
         BUFF(1) = GRAPHP(64)

      ELSE IF (INDX.EQ.31) THEN
         BUFF(1) = GRAPHP(65)

      ELSE IF (INDX.EQ.32) THEN
         BUFF(1) = GRAPHP(66)

      ELSE IF (INDX.EQ.33) THEN
         BUFF(1) = GRAPHP(67)

      ELSE IF (INDX.EQ.34) THEN
         BUFF(1) = GRAPHP(68)

      ELSE IF (INDX.EQ.35) THEN
         BUFF(1) = GRAPHP(69)

      ELSE IF (INDX.EQ.36) THEN
         BUFF(1) = GRAPHP(70)

      ELSE IF (INDX.EQ.37) THEN
         BUFF(1) = GRAPHP(71)

      ELSE IF (INDX.EQ.38) THEN
         BUFF(1) = GRAPHP(72)

      ELSE IF (INDX.EQ.39) THEN
         BUFF(1) = GRAPHP(73)

      ELSE IF (INDX.EQ.43) THEN
         BUFF(1) = GRAPHP(74)

      ELSE IF (INDX.EQ.44) THEN
         BUFF(1) = GRAPHP(75)

      ELSE IF (INDX.EQ.45) THEN
         BUFF(1) = GRAPHP(76)

      ELSE IF (INDX.EQ.46) THEN
         BUFF(1) = GRAPHP(77)

      ELSE
         CALL CHRIC(INDX,IERROR,L)
         CALL PLTFLU
         CALL SIORPT('PLTGTG','Illegal index '//IERROR(1:L)//'.',2)
         PLTGTG = .FALSE.
         RETURN

      END IF

      RETURN

      END
