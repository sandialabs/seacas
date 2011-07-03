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

C $Id: pltstg.f,v 1.1 2009/06/10 18:23:55 gdsjaar Exp $ 
C $Log: pltstg.f,v $
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
C Revision 1.1  1993/07/16 16:49:34  gdsjaar
C Changed plt to library rather than single source file.
C 
C=======================================================================
      LOGICAL FUNCTION PLTSTG(INDX,BUFF)
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

      PLTSTG = .TRUE.
      IF (INDX.EQ.0) THEN
         CALL PLTRSG

      ELSE IF (INDX.EQ.1) THEN
         GRAPHP(1) = BUFF(1)

      ELSE IF (INDX.EQ.2) THEN
         GRAPHP(2) = BUFF(1)

      ELSE IF (INDX.EQ.3) THEN
         GRAPHP(3) = BUFF(1)

      ELSE IF (INDX.EQ.4) THEN
         GRAPHP(4) = BUFF(1)

      ELSE IF (INDX.EQ.5) THEN
         GRAPHP(5) = BUFF(1)

      ELSE IF (INDX.EQ.6) THEN
         GRAPHP(38) = BUFF(1)

      ELSE IF (INDX.EQ.7) THEN
         IF (BUFF(1).EQ.0.) THEN
            GRAPHP(6) = 0.

         ELSE
            GRAPHP(6) = 1.
            GRAPHP(47) = BUFF(1) + 4.
         END IF

      ELSE IF (INDX.EQ.8) THEN
         IF (BUFF(1).LE.0.) THEN
            CALL PLTFLU
            CALL SIORPT('PLTSTG',
     *                  'Symbol increment must be greater than zero.',2)
            PLTSTG = .FALSE.
            RETURN

         END IF

         GRAPHP(23) = BUFF(1)

      ELSE IF (INDX.EQ.9) THEN
         GRAPHP(21) = BUFF(1)

      ELSE IF (INDX.EQ.10) THEN
         GRAPHP(37) = BUFF(1)

      ELSE IF (INDX.EQ.11) THEN
         IF (BUFF(1).EQ.1.) THEN
            GRAPHP(22) = BUFF(1)

         ELSE IF (BUFF(1).EQ.2.) THEN
            GRAPHP(22) = BUFF(1)

         ELSE IF (BUFF(1).EQ.3.) THEN
            IF (BUFF(2).EQ.BUFF(3)) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG','XMIN cannot be equal to XMAX.',2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF (BUFF(6).EQ.BUFF(7)) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG','YMIN cannot be equal to YMAX.',2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF (BUFF(4).EQ.0.) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG',
     *                  'Number of major x intervals cannot equal zero.'
     *                     ,2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF (BUFF(8).EQ.0.) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG',
     *                  'Number of major y intervals cannot equal zero.'
     *                     ,2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            GRAPHP(22) = BUFF(1)
            DO 2220 I = 0,7
               GRAPHP(I+24) = BUFF(I+2)
 2220       CONTINUE

         ELSE IF (BUFF(1).EQ.4.) THEN
            IF (BUFF(4).EQ.BUFF(3)) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG',
     *                     'XMAX cannot equal first nice X number.',2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF (BUFF(5).EQ.0.) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG','X interval cannot equal zero.',2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF (BUFF(9).EQ.BUFF(8)) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG',
     *                     'YMAX cannot equal first nice Y number.',2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF (BUFF(10).EQ.0.) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG','Y interval cannot equal zero.',2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF ((BUFF(3).LT.BUFF(2).AND.BUFF(4).GT.BUFF(2)) .OR.
     *          (BUFF(3).GT.BUFF(2).AND.BUFF(4).LT.BUFF(3))) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG',
     *                  'Invalid specification of XMIN, XNICE and XMAX.'
     *                     ,2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF ((BUFF(8).LT.BUFF(7).AND.BUFF(9).GT.BUFF(7)) .OR.
     *          (BUFF(8).GT.BUFF(7).AND.BUFF(9).LT.BUFF(8))) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG',
     *                  'Invalid specification of YMIN, YNICE and YMAX.'
     *                     ,2)
               PLTSTG = .FALSE.
               RETURN

            END IF

            IF (BUFF(4).LT.BUFF(2) .AND. BUFF(5).GT.0.) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG',
     *                     'Setting X interval negative as XMAX < XMIN.'
     *                     ,2)
               BUFF(5) = -BUFF(5)
            END IF

            IF (BUFF(9).LT.BUFF(7) .AND. BUFF(10).GT.0.) THEN
               CALL PLTFLU
               CALL SIORPT('PLTSTG',
     *                     'Setting Y interval negative as YMAX < YMIN.'
     *                     ,2)
               BUFF(10) = -BUFF(10)
            END IF

            GRAPHP(22) = BUFF(1)
            DO 2240 I = 0,9
               GRAPHP(I+78) = BUFF(I+2)
 2240       CONTINUE

         ELSE
            CALL CHRRVC(BUFF(1),IERROR,L)
            CALL PLTFLU
            CALL SIORPT('PLTSTG','Illegal buffer value '//IERROR(1:L)//
     *                  ' in graph scaling type.',2)
            PLTSTG = .FALSE.
            RETURN

         END IF

      ELSE IF (INDX.EQ.12) THEN
         GRAPHP(32) = BUFF(1)

      ELSE IF (INDX.EQ.13) THEN
         GRAPHP(91) = BUFF(1)

      ELSE IF (INDX.EQ.14) THEN
         GRAPHP(90) = BUFF(1)

      ELSE IF (INDX.EQ.15) THEN
         GRAPHP(35) = BUFF(1)

      ELSE IF (INDX.EQ.16) THEN
         GRAPHP(36) = BUFF(1)

      ELSE IF (INDX.EQ.17) THEN
         GRAPHP(39) = BUFF(1)

      ELSE IF (INDX.EQ.18) THEN
         GRAPHP(40) = BUFF(1)

      ELSE IF (INDX.EQ.19) THEN
         GRAPHP(41) = BUFF(1)

      ELSE IF (INDX.EQ.20) THEN
         GRAPHP(42) = BUFF(1)

      ELSE IF (INDX.EQ.21) THEN
         GRAPHP(92) = BUFF(1)

      ELSE IF (INDX.EQ.22) THEN
         GRAPHP(44) = BUFF(1)

      ELSE IF (INDX.EQ.23) THEN
         GRAPHP(45) = BUFF(1)

      ELSE IF (INDX.EQ.47) THEN
         GRAPHP(88) = BUFF(1)

      ELSE IF (INDX.EQ.48) THEN
         GRAPHP(89) = BUFF(1)

      ELSE IF (INDX.EQ.24) THEN
         GRAPHP(46) = BUFF(1)

      ELSE IF (INDX.EQ.25) THEN
         GRAPHP(48) = BUFF(1)

      ELSE IF (INDX.EQ.26) THEN
         GRAPHP(49) = BUFF(1)

      ELSE IF (INDX.EQ.27) THEN
         DO 2260 I = 0,13
            GRAPHP(7+I) = BUFF(I+1)
 2260    CONTINUE

      ELSE IF (INDX.EQ.28) THEN
         GRAPHP(62) = BUFF(1)

      ELSE IF (INDX.EQ.29) THEN
         GRAPHP(63) = BUFF(1)

      ELSE IF (INDX.EQ.30) THEN
         GRAPHP(64) = BUFF(1)

      ELSE IF (INDX.EQ.31) THEN
         GRAPHP(65) = BUFF(1)

      ELSE IF (INDX.EQ.32) THEN
         GRAPHP(66) = BUFF(1)

      ELSE IF (INDX.EQ.33) THEN
         GRAPHP(67) = BUFF(1)

      ELSE IF (INDX.EQ.34) THEN
         GRAPHP(68) = BUFF(1)

      ELSE IF (INDX.EQ.35) THEN
         GRAPHP(69) = BUFF(1)

      ELSE IF (INDX.EQ.36) THEN
         GRAPHP(70) = BUFF(1)

      ELSE IF (INDX.EQ.37) THEN
         GRAPHP(71) = BUFF(1)

      ELSE IF (INDX.EQ.38) THEN
         GRAPHP(72) = BUFF(1)

      ELSE IF (INDX.EQ.39) THEN
         GRAPHP(73) = BUFF(1)

      ELSE IF (INDX.EQ.43) THEN
         GRAPHP(74) = BUFF(1)

      ELSE IF (INDX.EQ.44) THEN
         GRAPHP(75) = BUFF(1)

      ELSE IF (INDX.EQ.45) THEN
         GRAPHP(76) = BUFF(1)

      ELSE IF (INDX.EQ.46) THEN
         GRAPHP(77) = BUFF(1)

      ELSE
         CALL CHRIC(INDX,IERROR,L)
         CALL PLTFLU
         CALL SIORPT('PLTSTG','Illegal index '//IERROR(1:L)//'.',2)
         PLTSTG = .FALSE.
         RETURN

      END IF

      RETURN

      END
