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
C $Id: rdhead.f,v 1.4 2008/03/14 13:50:53 gdsjaar Exp $
C************************************************************************
      subroutine rdhead(ndb, a, c, merr)
C************************************************************************
C
C     rdhead
C     C.A. Forsythe 01/19/95
C
C DESCRIPTION: read element block data, names of result variable,
C              and the element varaible truth table
C
C PARAMETERS:
C     ndb   - IN  - file id
C     a     - I/O - dynamic memory base array for numeric values
C     c     - I/O - dynamic memory base array for character values
C     merr  - OUT - dynamic memory error flag
C
C************************************************************************

      include 'params.blk'
      include 'exinit.blk'
      include 'ebptr.blk'
      include 'exrvar.blk'
      include 'namptr.blk'

C     file id
      integer ndb
C     declare dynamic arrays
      dimension a(1)
      character*1 c(1)
C     memory error flag
      integer merr

C     local variables
      integer nerr, cerr

C     connectivity and attribute arrays
      ilink  = 0
      iatrib = 0


      if (nelblk .gt. 0) then
c        reserve memory to read element block data
         call mdget (4*nelblk)
         call mcget (nelblk * mxstln)
C        idelb(*)element block id
         call mdrsrv ('IDELB', kidelb, nelblk)
C        numelb(*) number of elements in each block
         call mdrsrv ('NUMELB', knelb, nelblk)
C        numlnk(*) number of nodes/element block
         call mdrsrv ('NUMLNK', knlnk, nelblk)
C        numatr(*) number of attributes/element block
         call mdrsrv ('NUMATR', knatr, nelblk)
C        namelb(*) element block type
         call mcrsrv ('NAMELB', knameb, nelblk * MXSTLN)
C        link(*) element block connectivity
         call mdrsrv ('LINK', klink, 0)
C        atrib(*) element block attributes
         call mdrsrv ('ATRIB', katrib, 0)
         call mdstat (nerr, mem)
         call mcstat (cerr, mem)
         if ((nerr .gt. 0) .or. (cerr .gt. 0)) then
            call memerr()
            merr = 1
            return
         end if

         call erelbk (ndb, '*', 1, nelblk, a(kidelb), a(knelb),
     &                a(knlnk), a(knatr), c(knameb), a, 'LINK',
     &                ilink, 'ATRIB', iatrib, merr)
         if (merr .eq. 1) return

C        check for memory error
         call mdfind ('LINK', klink, ilink)
         call mdfind ('ATRIB', katrib, iatrib)
         call mdstat (nerr, mem)
         if (nerr .gt. 0) then
            call memerr
            merr = 1
            return
         end if
      end if

C     Read variable names
      if ((nvargl .gt. 0) .or. (nvarnp .gt. 0) .or.
     &   (nvarel .gt. 0)) then
C        reserve space to read names of results variable and coordinate name
         call mcget (MXSTLN*ndim + MXSTLN*(nvargl+nvarnp+nvarel))
         if (nvargl .gt. 0) then
            call mcrsrv ('GLOB', kglob, MXSTLN*nvargl)
         else
            kglob = 1
         end if
         if (nvarnp .gt. 0) then
            call mcrsrv ('NODE', knode, MXSTLN*nvarnp)
         else
            knode = 1
         end if
         if (nvarel .gt. 0) then
            call mcrsrv ('ELEM', kelem, MXSTLN*nvarel)
         else
            kelem = 1
         end if
         call mcstat (cerr, mem)
         if (cerr .gt. 0) then
            call memerr()
            merr = 1
            return
         end if
C        Exodus Read Variable NAMes
         call ervnam (ndb, nvargl, nvarnp, nvarel, c(kglob),
     &                c(knode), c(kelem))

      end if

      if ((nvarel .gt. 0) .and. (nelblk .gt. 0)) then
C        reserve space for element variable truth table
         call mdrsrv ('IEVTT', kevtt, nvarel*nelblk)
C        read the element variable truth table
         call exgvtt (ndb, nelblk, nvarel, a(kevtt), ierr)
      else
C        need to reserve space for future use
         call mdrsrv ('IEVTT', kevtt, 0)
      end if
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr()
         merr = 1
         return
      end if

      return
      end



      
