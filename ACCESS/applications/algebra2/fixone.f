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
      SUBROUTINE FIXONE (MAXNE, VARVAL)
C=======================================================================
C $Id: fixone.f,v 1.1 2008/10/31 05:04:00 gdsjaar Exp $
C $Log: fixone.f,v $
C Revision 1.1  2008/10/31 05:04:00  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:26  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.4  2008/03/14 13:45:27  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.3  1995/10/03 21:36:17  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.2  1993/08/02 14:21:07  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:42:41  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:24:33  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:24:31  gdsjaar
c Initial revision
c 

C   --*** FIXONE *** (ALGEBRA) Handle first time step variables
C   --   Written by Amy Gilkey - revised 12/10/87
C   --
C   --FIXONE is called after the first time step is read in.  It copies
C   --the current variables to the last and first time step variables
C   --(if needed).  It also corrects the current variable pointer if
C   --only the first time step for a variable is needed.
C   --
C   --Parameters:
C   --   MAXNE  - IN     - the VARVAL dimension
C   --   VARVAL - IN/OUT - the input data and copied data
C   --
C   --Common Variables:
C   --   Uses ISTVAR of /VAR../
C   --   Uses NVARNP, NVAREL of /DBNUMS/

      PARAMETER (ICURTM = 1, ILSTTM = 2, IONETM = 3)
      
      include 'var.blk'

      REAL VARVAL(MAXNE,*)

      DO 100 IVAR = 1, NUMINP
            IF (ISTVAR(IONETM,IVAR) .GT. 0) THEN
               IF (ISTVAR(ICURTM,IVAR) .LT. 0) THEN
                  ISTVAR(ICURTM,IVAR) = 0
               ELSE
                  IFROM = ISTVAR(ICURTM,IVAR)
                  ITO   = ISTVAR(IONETM,IVAR)
                  CALL CPYVAR (TYPVAR(IVAR), MAXNE,
     &                         VARVAL(1,IFROM), VARVAL(1,ITO))
               END IF
            END IF
            IF (ISTVAR(ILSTTM,IVAR) .GT. 0) THEN
               IFROM = ISTVAR(ICURTM,IVAR)
               ITO = ISTVAR(ILSTTM,IVAR)
               CALL CPYVAR (TYPVAR(IVAR), MAXNE,
     &                      VARVAL(1,IFROM), VARVAL(1,ITO))
            END IF
  100 CONTINUE

      DO 120 IVAR = IXLHS, MAXVAR
            DO 110 ITM = 1, 3
               IF (ITM .NE. ICURTM) THEN
                  IF (ISTVAR(ITM,IVAR) .GT. 0) THEN
                     IFROM = ISTVAR(ICURTM,IVAR)
                     ITO = ISTVAR(ITM,IVAR)
                     CALL CPYVAR (TYPVAR(IVAR), MAXNE,
     &                            VARVAL(1,IFROM), VARVAL(1,ITO))
                  END IF
               END IF
  110       CONTINUE
  120 CONTINUE

      RETURN
      END
