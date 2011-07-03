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
      SUBROUTINE STORE (ISTEP, TYP, IBEGIN, IEND, NWRDS,
     &   NUMELB, IDELB, ISEVOK, VISELB, MAXNE, VARVAL, MERR)
C=======================================================================
C $Id: store.f,v 1.1 2008/10/31 05:04:01 gdsjaar Exp $
C $Log: store.f,v $
C Revision 1.1  2008/10/31 05:04:01  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:27  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.8  2008/03/14 13:45:29  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.7  1996/02/12 21:28:50  caforsy
C Removed redundant checks for ExodusII API calls
C
C Revision 1.6  1995/12/10 15:46:55  caforsy
C Integrated call to rpterr and mdcomp/mccomp calls
C
C Revision 1.5  1995/10/26 20:37:48  caforsy
C Dimension element variable truth table array and tried to speed up
C the reading of the element variables from the input file.
C
C Revision 1.4  1995/10/03  21:36:46  mksmith
C Removing old algII files
C Replacing modified algII files
C
C Revision 1.3  1993/08/02 16:54:22  gdsjaar
C Fixed getting of element variables
C
C Revision 1.2  1993/08/02  14:21:42  gdsjaar
C Split out common blocks into separare files. Miscellaneous bug fixes
C on several files.  First public release version.
C
c Revision 1.1  1993/07/30  21:44:05  gdsjaar
c Initial checkin of ACCESS/translate/algII
c
c Revision 1.1.1.1  1990/11/09  16:26:09  gdsjaar
c Algebra - Unix Version
c
c Revision 1.1  90/11/09  16:26:07  gdsjaar
c Initial revision
c 

C   --*** STORE *** (ALGEBRA) Store database variable data
C   --   Written by Amy Gilkey - revised 08/16/88
C   --
C   --STORE reads each database variable and stores it, if it is wanted,
C   --into the proper location of VARVAL.  The /VAR../ variables between
C   --IBEGIN and IEND are assumed to be ordered on IDVAR, with no
C   --repetitions.
C   --
C   --Parameters:
C   --   ISTEP  - IN  - the time step number
C   --   TYP    - IN  - the variable types
C   --   IBEGIN - IN  - the starting /VAR../ index of the variables
C   --   IEND   - IN  - the ending /VAR../ index of the variables
C   --   NWRDS  - IN  - the number of words to be read for each variable
C   --   NUMELB - IN  - the number of elements per block
C   --   ISEVOK - IN  - the element block variable truth table;
C   --                  variable i of block j exists iff ISEVOK(j,i)
C   --   MAXNE  - IN  - the VARVAL dimension (max of NUMEL and NUMNP)
C   --   VARVAL - OUT - the returned needed input variables
C   --   MERR   - OUT - error flag C   --
C   --Common Variables:
C   --   Uses IDVAR, ISTVAR of /VAR../
C   --   Uses NDBIN of /DBASE/

      PARAMETER (ICURTM = 1, ILSTTM = 2, IONETM = 3)
      include 'var.blk'
      include 'dbase.blk'
      include 'dbnums.blk'

      CHARACTER TYP
      INTEGER NUMELB(*)
      INTEGER IDELB(*)
      LOGICAL ISEVOK(NELBLK,NVAREL)
      LOGICAL VISELB(NELBLK)
      REAL VARVAL(MAXNE,*)
      INTEGER MERR
      MERR = 0


C     Read and store all global variables (if needed for the current step)
      IF (TYP .EQ. 'G') THEN
         IF (ISTVAR(ICURTM,IBEGIN) .NE. 0) THEN
            NSTO = ISTVAR(ICURTM,IBEGIN)
            ID = IDVAR(IBEGIN)
            call exggv(ndbin, istep, nvargl, varval(id,nsto), ierr)
         END IF
C     Read and store all needed nodal variables for the current step
      ELSE IF (TYP .EQ. 'N') THEN
         DO 100 NVAR = IBEGIN, IEND
            IF (ISTVAR(ICURTM,NVAR) .NE. 0) THEN
               NSTO = IABS (ISTVAR(ICURTM,NVAR))
               call exgnv(ndbin, istep, idvar(nvar), nwrds,
     &                    varval(1,nsto), ierr)
            END IF
  100    CONTINUE
C     Read and store all needed element variables for the current step
      ELSE IF (TYP .EQ. 'E') THEN
         IEL = 1
         DO 130 IELB = 1, NELBLK
            IDEB = IDELB(IELB)
            NELEM = NUMELB(IELB)
            IF (VISELB(IELB)) THEN
              DO 120 NVAR = IBEGIN, IEND
               INVAR = IDVAR(NVAR)
               IF (ISTVAR(ICURTM,NVAR) .NE. 0) THEN
                  NSTO = IABS (ISTVAR(ICURTM,NVAR))
                  IF (ISEVOK(IELB,INVAR)) THEN
                    call exgev(ndbin, istep, INVAR,
     &                IDEB, NELEM, varval(iel,nsto), ierr)
                  ELSE
C                  --Make sure values for undefined elements are zero
                     DO 110 I = IEL, IEL+NUMELB(IELB) - 1
                        VARVAL(I,NSTO) = 0.0
  110                CONTINUE
                  END IF
               END IF
  120         CONTINUE
            END IF
            IEL = IEL + NELEM
  130    CONTINUE
      ELSE
         WRITE(*,10)
     &   'Unknown Type in Subroutine Store: TYP =  ',typ
 10      format(A,A)
         MERR = 1
      END IF

      RETURN
      END
