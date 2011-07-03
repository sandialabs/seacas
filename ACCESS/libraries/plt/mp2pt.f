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

C $Id: mp2pt.f,v 1.1 2009/06/10 18:23:55 gdsjaar Exp $ 
C $Log: mp2pt.f,v $
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
C Revision 1.1  1993/07/16 16:47:03  gdsjaar
C Changed plt to library rather than single source file.
C 
C=======================================================================
      SUBROUTINE MP2PT(N,X0,Y0,PX,PY,MASK)
      COMMON /MAP/MODEL(4,4),VIEW(4,4),PROJ(4,4),CPNEAR,CPFAR,VWPORT(4),
     *       MVP(4,4),VP(4,4),CPLINE(2,2,10),CPPLAN(2,3,10),PEYE(3),
     *       PLOOK(3),ETWIST,NCPLIN,NCPLAN,TMAT1(4,4),TMAT2(4,4),
     *       TMAT3(4,4),TVEC1(4),TVEC2(4),TVEC3(4),TVEC4(4),TARR1(32),
     *       TARR2(32),TARR3(32),TARR4(32),TARR5(32),TARR6(32),
     *       TARR7(32),TARR8(32)
      REAL MODEL,MVP
      LOGICAL NOCLIP
      DIMENSION C1(2),C2(2),X0(*),Y0(*),PX(*),PY(*),MASK(*)

      VCX = (VWPORT(1)+VWPORT(2))/2.
      VSX = (VWPORT(2)-VWPORT(1))/2.
      VCY = (VWPORT(3)+VWPORT(4))/2.
      VSY = (VWPORT(4)-VWPORT(3))/2.
      NOCLIP = (NCPLIN.EQ.0)
      KM = 0
      J = 0
 2060 IF (.NOT. (J.LT.N)) GO TO 2070
      JN = MIN(N-J,32)
      J1 = J
      KM = KM + 1
      J = J + JN
      MASK(KM) = -1
      IF (NOCLIP) THEN
         CALL MPMUL2(JN,X0(1+J1),Y0(1+J1),MVP,TARR1,TARR2,TARR3,TARR4)

      ELSE
         CALL MPMUL2(JN,X0(1+J1),Y0(1+J1),MODEL,TARR1,TARR2,TARR3,TARR4)
         DO 2080 K = 1,NCPLIN
            C1(1) = CPLINE(1,1,K)
            C1(2) = CPLINE(1,2,K)
            C2(1) = CPLINE(2,1,K)
            C2(2) = CPLINE(2,2,K)
            CALL PLTCP2(JN,MASK(KM),TARR1,TARR2,C1,C2)
 2080    CONTINUE
         CALL MPMUL4(JN,MASK(KM),TARR1,TARR2,TARR3,TARR4,VP,TARR1,TARR2,
     *               TARR3,TARR4)
      END IF

      DO 2100 I = 1,JN
         TARR1(I) = (TARR1(I)/TARR4(I))*VSX + VCX
         TARR2(I) = (TARR2(I)/TARR4(I))*VSY + VCY
 2100 CONTINUE
      C1(1) = VWPORT(1)
      C1(2) = VWPORT(3)
      C2(1) = VWPORT(2)
      C2(2) = VWPORT(4)
      CALL PLTVWP(C1,C2,JN,MASK(KM),TARR1,TARR2)
      DO 2120 I = 1,JN
         PX(J1+I) = TARR1(I)
         PY(J1+I) = TARR2(I)
 2120 CONTINUE
      GO TO 2060

 2070 CONTINUE
      RETURN

      END
