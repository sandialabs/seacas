C    Copyright(C) 2007 Sandia Corporation.  Under the terms of Contract
C    DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C    certain rights in this software
C    
C    Redistribution and use in source and binary forms, with or without
C    modification, are permitted provided that the following conditions are
C    met:
C    
C        * Redistributions of source code must retain the above copyright
C          notice, this list of conditions and the following disclaimer.
C    
C        * Redistributions in binary form must reproduce the above
C          copyright notice, this list of conditions and the following
C          disclaimer in the documentation and/or other materials provided
C          with the distribution.
C    
C        * Neither the name of Sandia Corporation nor the names of its
C          contributors may be used to endorse or promote products derived
C          from this software without specific prior written permission.
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
C $Id: ewelbk.f,v 1.3 2008/03/14 13:50:48 gdsjaar Exp $
C=======================================================================
      subroutine ewelbk (ndb, nelblk, idelb, numelb, numlnk, numatr,
     &                   namelb, link, atrib)
C=======================================================================

C   --ewelbk
C   --   Exodus Write ELement BlocK
C   --   Renamed and Modified by C.A. Forsythe 01/25/96
C   --
C   --ewelbk writes the element block information to the file ndb
C   --
C   --Parameters:
C     ndb    - IN  - database file id
C     nelblk - IN  - number of element blocks
C     idelb  - IN  - element blocks ID
C     numelb - IN  - number of elements in each element block
C     numlnk - IN  - number of nodes per element in each block
C     ielnk  - IN  - size of connectivity array
C     ieatr  - IN  - size of attribute array

      include 'params.blk'

      integer ndb
      integer nelblk
      integer idelb(nelblk)
      integer numelb(nelblk)
      integer numlnk(nelblk)
      integer numatr(nelblk)
      character*(MXSTLN) namelb(nelblk)
      integer link(*)
      real atrib(*)

C     local variabes
      integer ideb, nelb, natr, nlnk
      character*(MXSTLN) nameb
      integer islnk, ilnk
      integer isatr, iatr

      islnk = 0
      ilnk  = 0
      isatr = 0
      iatr  = 0

      do 100 ie = 1, nelblk
         ideb  = idelb(ie)
         nelb  = numelb(ie)
         nlnk  = numlnk(ie)
         natr  = numatr(ie)
         nameb = namelb(ie)
C        compute connectivity index
         islnk = ilnk + 1
         ilnk  = ilnk + (nlnk * nelb)
C        compute attribute index
         isatr = iatr + 1
         iatr  = iatr + (natr * nelb)

C        write element block data
         call expelb (ndb, ideb, nameb, nelb, nlnk, natr, ierr)

C        write connectivity array
         call expelc (ndb, ideb, link(islnk), ierr)

C        write attribute array
         if (natr .gt. 0) then
            call expeat (ndb, ideb, atrib(isatr), ierr)
         end if

 100  continue
      

      RETURN
      END

