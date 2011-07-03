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

C $Id: pltsbm.f,v 1.1 2009/06/10 18:23:55 gdsjaar Exp $ 
C $Log: pltsbm.f,v $
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
C Revision 1.3  1993/07/19 17:06:43  gdsjaar
C Changed hex constants back to preceding X, --needed on cray. Works
C either way on other systems.
C
c Revision 1.2  1993/07/16  17:33:21  gdsjaar
c Integer constant too big on sun, replaced it with hexadecimal notation
c
c Revision 1.1  1993/07/16  16:49:28  gdsjaar
c Changed plt to library rather than single source file.
c 
C=======================================================================
      SUBROUTINE PLTSBM(N,MASK,X,Y,SYMB)
      DIMENSION X(*),Y(*),MASK(*)
      CHARACTER*(*) SYMB
      INTEGER IZBIT(32)
      DATA IZBIT/1,          2,          4,          8,
     *          16,         32,         64,        128,
     *         256,        512,       1024,       2048,
     *        4096,       8192,      16384,      32768,
     *       65536,     131072,     262144,     524288,
     *     1048576,    2097152,    4194304,    8388608,
     *    16777216,   33554432,   67108864,  134217728,
     *   268435456,  536870912, 1073741824, X'80000000'/

      J = 0
      KM = 0
 2170 IF (.NOT. (J.LT.N)) GO TO 2180
      JN = MIN(N-J,32)
      KM = KM + 1
      M = MASK(KM)
      J1 = J
      J = J + JN
      IF (M.EQ.0) THEN
         GO TO 2170

      END IF

      DO 2190 K = 1,JN
         IF (IAND(M,IZBIT(K)).NE.0) THEN
            CALL PLTXTS(X(J1+K),Y(J1+K),SYMB)
         END IF

 2190 CONTINUE
      GO TO 2170

 2180 CONTINUE
      RETURN

      END
