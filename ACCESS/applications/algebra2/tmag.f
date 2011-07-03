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

C=======================================================================
      SUBROUTINE TMAG (PARTYP, LENARY, NELBLK, IXELB, ISEVOK,
     &                 SIG1, SIG2, SIG3, SIG12, SIG23, SIG31, 
     &                 NUMIX, IXNODE, IXELEM, RESULT)
C=======================================================================
C $Id: tmag.f,v 1.1 2008/10/31 05:04:01 gdsjaar Exp $
C $Log: tmag.f,v $
C Revision 1.1  2008/10/31 05:04:01  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:27  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.5  2008/03/14 13:45:30  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.4  1996/02/26 16:57:59  caforsy
C Changed exopts call to abort on exodus failure; Fixed tmag subroutine
C to work with zoomed mesh; Added comments to subroutines that compute
C functions.
C
C Revision 1.3  1995/10/03 21:36:48  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:21:44  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:44:10  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:26:15  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:26:14  gdsjaar
c Initial revision
c 

C   --*** TMAG *** (ALGEBRA) Calculate tensor magnitude
C   --   Written by Amy Gilkey - revised 12/03/87
C   --
C   --TMAG calculates the tensor magnitudes.
C   --
C   --Parameters:
C   --   PARTYP - IN - the parameter type (element must be handled differently)
C   --   LENARY - IN - the length of the parameter arrays
C   --   NELBLK - IN - the number of element blocks
C   --   IXELB  - IN - the cumulative element counts for each element block
C   --   ISEVOK - IN - the element variable truth table for each element block
C   --   SIG1, SIG2, SIG3, SIG12, SIG23, SIG31 - IN - the tensor components
C   --   NUMIX  - IN  - the number of selected values; <0 if all
C   --   IXNODE - IN  - the indices of the selected nodes (only if NUMIX>=0)
C   --   IXELEM - IN  - the indices of the selected elements (only if NUMIX>=0)
C   --   RESULT - OUT - the returned tensor magnitudes

      CHARACTER PARTYP
      INTEGER IXELB(0:NELBLK)
      LOGICAL ISEVOK(*)
      REAL SIG1(*), SIG2(*), SIG3(*), SIG12(*), SIG23(*), SIG31(*)
      INTEGER NUMIX
      INTEGER IXNODE(*)
      INTEGER IXELEM(*)
      REAL RESULT(*)

      IF (NUMIX .GE. 0) THEN
C        COMPUTE TMAG ONLY FOR ELEMENTS/NODES THAT EXIST
         IF (PARTYP .NE. 'E') THEN
            DO 50 I = 1, NUMIX
               J = IXNODE(I)
               RESULT(J) = SQRT ((SIG1(J)-SIG2(J))**2
     &            + (SIG2(J)-SIG3(J))**2
     &            + (SIG3(J)-SIG1(J))**2
     &            + 6 * (SIG12(J)**2 + SIG23(J)**2 + SIG31(J)**2))
   50       CONTINUE
         ELSE
            DO 75 IELB = 1, NELBLK
               IF (ISEVOK(IELB)) THEN
                  DO 95 I = IXELB(IELB-1)+1, IXELB(IELB)
                     J = IXELEM(I)
                     RESULT(J) = SQRT ((SIG1(J)-SIG2(J))**2
     &                  + (SIG2(J)-SIG3(J))**2
     &                  + (SIG3(J)-SIG1(J))**2
     &                  + 6 * (SIG12(J)**2 + SIG23(J)**2 + SIG31(J)**2))
   95             CONTINUE
               END IF
   75       CONTINUE
         END IF
      ELSE
C        NUMIX < 0; All Values are selected
C        NUMEL = NUMELO; NUMNP = NUMNPO; IXELB(J) = IXELBO(J) J=1,NELBLK
         IF (PARTYP .NE. 'E') THEN
            DO 100 J = 1, LENARY
               RESULT(J) = SQRT ((SIG1(J)-SIG2(J))**2
     &            + (SIG2(J)-SIG3(J))**2
     &            + (SIG3(J)-SIG1(J))**2
     &            + 6 * (SIG12(J)**2 + SIG23(J)**2 + SIG31(J)**2))
  100       CONTINUE
         ELSE
            DO 120 IELB = 1, NELBLK
               IF (ISEVOK(IELB)) THEN
                  DO 110 J = IXELB(IELB-1)+1, IXELB(IELB)
                     RESULT(J) = SQRT ((SIG1(J)-SIG2(J))**2
     &                  + (SIG2(J)-SIG3(J))**2
     &                  + (SIG3(J)-SIG1(J))**2
     &                  + 6 * (SIG12(J)**2 + SIG23(J)**2 + SIG31(J)**2))
  110             CONTINUE
               END IF
  120       CONTINUE
         END IF
      END IF

      RETURN
      END
