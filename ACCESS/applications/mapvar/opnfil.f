C Copyright (c) 2007 Sandia Corporation. Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Governement
C retains certain rights in this software.
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

C=======================================================================
*DECK,OPNFIL
      SUBROUTINE OPNFIL
C
C     ******************************************************************
C
C     SUBROUTINE TO OPEN REQUIRED FILES
C
C     Calls subroutine ERROR
C
C     Called by MAPVAR
C
C     ******************************************************************
C
      CHARACTER*132 FILNAM
C
      COMMON /EX2TP/  NTP2EX,NTP3EX,NTP4EX
      COMMON /NTPDAT/ IFILES(5)
      COMMON /TAPES/  NOUT,NTPOUT,NTP2,NTP3,NTP4
      include 'exodusII.inc'
C
C     ******************************************************************
C
C     OPENING OF INPUT/OUTPUT, SCRATCH AND DATA FILES
C
C     TEXT OUTPUT FILE
C
      IFILES(1)=1
      IUNIT=NTPOUT
      CALL EXNAME (NTPOUT,FILNAM,LN)
      OPEN (UNIT=NTPOUT, FILE=FILNAM, STATUS='unknown', 
     &      FORM='formatted', ERR=10)
C
C     EXODUS DATA FILE - MESH-A (MESH & SOLUTION)
C
      IFILES(3)=1
      IUNIT=NTP2
      CALL EXNAME (NTP2,FILNAM,LN)
      icpuws = 0
      iows1 = 0
      ntp2ex = exopen(FILNAM(1:LN),EXREAD,icpuws,iows1,vers,ierr)
      if (ierr .lt. 0)then
      WRITE(NOUT,11)IERR,NTP2,FILNAM(1:LN),NTP2EX
      WRITE(NTPOUT,11)IERR,NTP2,FILNAM(1:LN),NTP2EX
      GO TO 10
      end if
 11   format(' In opnfil - error opening mesh + solution file',/,
     &' error number ',i5,' tape number ',i5,' filename ',a20,/,
     &' ntp2ex = ',i5)
C
C     GENESIS DATA FILE - MESH-B (MESH)
C
      IFILES(4)=1
      IUNIT=NTP3
      CALL EXNAME (NTP3,FILNAM,LN)
      icpuws = 0
      iows2 = 0
      ntp3ex = exopen(FILNAM(1:LN),EXREAD,icpuws,iows2,vers,ierr)
      if (ierr .lt. 0)then
      WRITE(NOUT,12)IERR,NTP3,FILNAM(1:LN),NTP3EX
      WRITE(NTPOUT,12)IERR,NTP3,FILNAM(1:LN),NTP3EX
      GO TO 10
      end if
 12   format(' In opnfil - error opening genesis file',/,
     &' error number ',i5,' tape number ',i5,' filename ',a20,/,
     &' ntp3ex = ',i5)
C
C     EXODUS DATA FILE - MESH-C (MESH & INTERPOLATED SOLUTION)
C
      IFILES(5)=1
      IUNIT=NTP4
      CALL EXNAME (NTP4,FILNAM,LN)
C ... Set iows for created file to default floating point word size
C     on this machine (as returned by previous call to exopen)      
      iows3 = icpuws
      icpuws = 0
      ntp4ex = excre(FILNAM(1:LN),EXCLOB,icpuws,iows3,ierr)
      if (ierr .lt. 0)then
      WRITE(NOUT,13)IERR,NTP4,FILNAM(1:LN),NTP4EX
      WRITE(NTPOUT,13)IERR,NTP4,FILNAM(1:LN),NTP4EX
      GO TO 10
      end if
 13   format(' In opnfil - error opening interpolation file',/,
     &' error number ',i5,' tape number ',i5,' filename ',a20,/,
     &' ntp4ex = ',i5)
C
      RETURN
C
   10 CONTINUE
      CALL ERROR ('OPNFIL','ERROR OPENING FILE','UNIT NUMBER',IUNIT,    
     1' ',0,' ',' ',1)
      END
