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
C $Id: cmpelb.f,v 1.6 2008/03/14 13:50:45 gdsjaar Exp $
C************************************************************************
      subroutine cmpelb (ndb, nelblk, idtmp, namelb, numelb, numlnk,
     &                   numatr, okay)
C************************************************************************
C
C     cmpelb
C     C.A. Forsythe 01/22/95
C
C DESCRIPTION: read element block data from file id passed in and
C              compare element block data with the existing eb data
C
C PARAMETERS:
C     ndb    - IN  - file id of file to read element block data
C     nelblk - IN  - number of element blocks
C     idtmp  - IN  - element block ID's of file to read element block data
C     namelb - IN  - type of element stored in element block
C     numelb - IN  - number of elements per element block
C     numlnk - IN  - number of nodes per element block
C     numatr - IN  - number of attributes per element block
C     okay   - OUT - flag indicates if mesh in files are compatible
C
C************************************************************************

      include 'params.blk'

      integer ndb
      integer nelblk
      integer idtmp(nelblk)
      character*(MXSTLN) namelb(nelblk)
      integer numelb(nelblk)
      integer numlnk(nelblk)
      integer numatr(nelblk)
      logical okay

C     local variables
      integer ielb
      character*(MXSTLN) ebtype
      integer nmelb, nmlnk, nmatr, ierr

      do 125 ielb = 1, nelblk
         call exgelb (ndb, idtmp(ielb), ebtype, nmelb,
     &                nmlnk, nmatr, ierr)
         call lowstr(ebtype, ebtype)
         if (namelb(ielb) .ne. ebtype) then
            write(6, 1000) 'Element block type names'
            write(6, 1020) namelb(ielb), ebtype
            okay = .false.
         end if
         if (numelb(ielb) .ne. nmelb) then
            write(6, 1000) 'Number of elements in each block'
            write(6, 1010) numelb(ielb), numelb
            okay = .false.
         end if
         if (numlnk(ielb) .ne. nmlnk) then
            write(6, 1000) 'Number of elements in each block'
            write(6, 1010) numlnk(ielb), numlnk
            okay = .false.
         end if
         if (numatr(ielb) .ne. nmatr) then
            write(6, 1000) 'Number of attributes in each block'
            write(6, 1010) numatr(ielb), numatr
            okay = .false.
         end if
         if (.not. okay) return
  125 continue

 1000 format('Compatibility Error: ', a)
 1005 format('Compatibility Warning: ', a)
 1010 format('Master File = ',I10, 5x, ' New File = ', I10)
 1020 format('Master File = ',A, 5x, ' New File = ', A)

      return
      end

