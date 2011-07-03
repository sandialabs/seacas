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
      SUBROUTINE MAKEVO (NELBLK, ISEVOK, IEV1, IEV2, IEVOUT, IEVSEL)
C=======================================================================
C $Id: makevo.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: makevo.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:26  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.4  2008/03/14 13:45:28  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.3  1995/10/03 21:36:24  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:21:16  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:43:00  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:24:58  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:24:57  gdsjaar
c Initial revision
c 

C   --*** MAKEVO *** (ALGEBRA) Make up ISEVOK entry
C   --   Written by Amy Gilkey - revised 05/12/88
C   --
C   --MAKEVO makes up an ISEVOK entry based on two input entries.  The output
C   --entry is true iff the two input entries are true.  A flag indicates
C   --if the output entry is equal to one of the input entries, but the
C   --output entry is still set.
C   --
C   --Parameters:
C   --   NELBLK - IN - the number of element blocks
C   --   ISEVOK - IN/OUT - the variable truth table
C   --   IEV1, IEV2 - IN - the ISEVOK indices of the two input entries;
C   --      either index may be 0, but not both
C   --   IEVOUT - OUT - the ISEVOK index of the output entry
C   --   IEVSEL - OUT - the ISEVOK index of the input entry that is equal
C   --      to the output entry or the index of the output entry if
C   --      neither entry is equal

      LOGICAL ISEVOK(NELBLK,*)

      LOGICAL OK1, OK2

      OK1 = (IEV1 .GE. 1)
      OK2 = (IEV2 .GE. 1)

      IF ((IEV1 .GE. 1) .AND. (IEV2 .GE. 1)) THEN
         DO 100 IELB = 1, NELBLK
            IF (ISEVOK(IELB,IEV1) .EQV. ISEVOK(IELB,IEV2)) THEN
               ISEVOK(IELB,IEVOUT) = ISEVOK(IELB,IEV1)
            ELSE
               IF (ISEVOK(IELB,IEV1)) THEN
                  OK1 = .FALSE.
               ELSE
                  OK2 = .FALSE.
               END IF
               ISEVOK(IELB,IEVOUT) = .FALSE.
            END IF
  100    CONTINUE
      ELSE IF (IEV1 .GE. 1) THEN
         DO 110 IELB = 1, NELBLK
            ISEVOK(IELB,IEVOUT) = ISEVOK(IELB,IEV1)
  110    CONTINUE
      ELSE IF (IEV2 .GE. 1) THEN
         DO 120 IELB = 1, NELBLK
            ISEVOK(IELB,IEVOUT) = ISEVOK(IELB,IEV2)
  120    CONTINUE
      ELSE
         DO 130 IELB = 1, NELBLK
            ISEVOK(IELB,IEVOUT) = .TRUE.
  130    CONTINUE
      END IF

      IF (OK1) THEN
         IEVSEL = IEV1
      ELSE IF (OK2) THEN
         IEVSEL = IEV2
      ELSE
         IEVSEL = IEVOUT
      END IF

      RETURN
      END
