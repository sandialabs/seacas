C    Copyright(C) 2008 Sandia Corporation.  Under the terms of Contract
C    DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C    certain rights in this software
C    
C    Redistribution and use in source and binary forms, with or without
C    modification, are permitted provided that the following conditions are
C    met:
C    
C    * Redistributions of source code must retain the above copyright
C       notice, this list of conditions and the following disclaimer.
C              
C    * Redistributions in binary form must reproduce the above
C      copyright notice, this list of conditions and the following
C      disclaimer in the documentation and/or other materials provided
C      with the distribution.
C                            
C    * Neither the name of Sandia Corporation nor the names of its
C      contributors may be used to endorse or promote products derived
C      from this software without specific prior written permission.
C                                                    
C    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
C    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
C    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
C    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
C    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
C    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
C    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
C    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
C    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
C    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
C    
C $Id: princ3.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: princ3.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:27  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.5  2008/03/14 13:45:28  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.4  1997/08/26 13:25:48  gdsjaar
C Removed unused PI parameter (also testing cvs on cray)
C
C Revision 1.3  1995/10/03 21:36:31  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:21:24  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:43:19  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.4  1991/12/02  17:43:43  gdsjaar
c Fixed problem with divide by zero
c
c Revision 1.3  1991/04/17  14:46:19  gdsjaar
c Fixed bug in P? determination, removed SIGN trick
c
c Revision 1.2  1991/04/16  22:43:55  gdsjaar
c Added some constants that were omitted from original
c
c Revision 1.1  1991/04/16  22:34:44  gdsjaar
c Initial revision
c

      SUBROUTINE PRINC3(SK1, SK2, SK3, SK4, SK5, SK6, EV, INFO)
      
      REAL EV(3)
      REAL I1, I2, I3
      INTEGER INFO

      PARAMETER (THIRD = 0.33333 33333 33333 33333 33333)
      SQRT3  = SQRT(3.0)

      INFO = 0
C
C Find principal trial stresses and directions -
C
      I1 = ( SK1 + SK2 + SK3 )
      I2 = ( (SK1-SK2)**2 + (SK1-SK3)**2 + (SK2-SK3)**2 ) / 6.0
     *     + SK4**2 + SK5**2 + SK6**2
      
      S1 = ( (SK1 - SK2) + (SK1 - SK3) ) * THIRD
      S2 = (-(SK1 - SK2) + (SK2 - SK3) ) * THIRD
      S3 = (-(SK1 - SK3) - (SK2 - SK3) ) * THIRD
      
      I3 = S1 * S2 * S3 + 2.*SK4 * SK5 * SK6
     *     - S1 * SK5**2 - S2 *  SK6**2
     *     - S3 * SK4**2
      
C     Calculate constants for Malvern Method  (p92) 
 
C .... This sign trick does not work on Ultrix (and others?) with IEEE
C      since IEEE has a signed 0...........
CC      FI2 = I2 + (SIGN (0.5, I2) + SIGN (0.5, -I2))

      IF (I2 .EQ. 0.0) THEN
         FI2 = 1.0
      ELSE
         FI2 = I2
      END IF

      COS3AL = SQRT3 * 1.5 * I3 / FI2 / SQRT(FI2) 
      COS3AL = SIGN( MIN( 1.0, ABS(COS3AL) ),COS3AL )
C     
C     ... TRIG FUNCTION USED
C     
      CALPHA = COS( ACOS(COS3AL) / 3.0)
      SALPHA = SQRT(1.0 - CALPHA**2)
      
      T  = SQRT3 * SQRT(I2)
      P1 = (I1 + T *  2. * CALPHA             ) * THIRD 
      P2 = (I1 - T * (CALPHA + SALPHA * SQRT3)) * THIRD 
      P3 = (I1 - T * (CALPHA - SALPHA * SQRT3)) * THIRD 

C ... Sort Into Correct Position (ev1 < ev2 < ev3)

      EV(1) = MIN(P1, P2, P3)
      EV(3) = MAX(P1, P2, P3)

C ... This is bad from a round-off perspective, but we don't use the
C     value in algebra.  Be warned if use where you need an accurate
C     ev(2).....

      EV(2) = P1 + P2 + P3 - EV(1) - EV(3)
      
      RETURN
      END
