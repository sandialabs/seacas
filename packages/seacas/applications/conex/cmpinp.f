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
C $Id: cmpinp.f,v 1.5 2008/12/08 17:34:06 gdsjaar Exp $
C************************************************************************
      subroutine cmpinp(ndb, a, c, okay, ioerr)
C************************************************************************
C
C     cmpinp
C     C.A. Forsythe 01/22/95
C
C DESCRIPTION: read the header information from file 
C
C PARAMETERS:
C     ndb   - IN  - file id new file
C     a     - I/O - dynamic memory base array for numeric values
C     c     - I/O - dynamic memory base array for character values
C     okay  - OUT - flag indicates if mesh in files are compatible
C     ioerr - OUT - error flag for memory or io errors
C
C************************************************************************

      include 'exodusII.inc'
      include 'ebptr.blk'
      include 'exinit.blk'
      include 'exsets.blk'
      include 'exrvar.blk'
      include 'namptr.blk'
      include 'crc.blk'

C     arguments
      integer*4 mycrc
      integer ndb
C     declare dynamic arrays
      dimension a(1)
      character*1 c(1)
      logical okay
      integer ierr

C     local variables
      integer cerr, nerr
      real rnum
      character*(MXLNLN) title2
      character*(MXSTLN) cnum
C     initialization variables
      integer ndim2, numnp2, numel2, nelbl2, nmnps2, nmess2
C     node set variables
      integer lnsnl2, lnsdf2
C     side set variables
      integer lssnl2, lssel2, lssdf2
C     element block variables
      integer lidtmp

C     read initialization parameters
      call exgini(ndb, title2, ndim2, numnp2, numel2, nelbl2,
     &            nmnps2, nmess2, ierr)
      if (ndim .ne. ndim2 ) then
         write(6, 1000) 'Number of coordinates per node'
         write(6, 1010) ndim, ndim2
         okay = .false.
      end if
      if (numnp .ne. numnp2) then
         write(6,1000) 'Number of nodes'
         write(6,1010) numnp, numnp2
         okay = .false.
      end if
      if (numel .ne. numel2 ) then
         write(6, 1000) 'Number of elements'
         write(6, 1010) numel, numel2
         okay = .false.
      end if
      if (nelblk .ne. nelbl2) then
         write(6, 1000) 'Number of element blocks'
         write(6, 1010) nelblk, nelbl2
         okay = .false.
      end if

C ... Read node map, element number map, and element order map.  Compute crc of 
C     the ids and compare to stored value computed from initial database.  If not 
C     a match, print error message.
      mxsize = max(numnp, numel)
      call mdrsrv ('SCR', kmap, mxsize)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr()
         ioerr = 1
         return
      end if

C************************************************************************
C     check node number map 
C************************************************************************
C     read the node number map
      call exgnnm (ndb, a(kmap), ierr)

C ... Calculate adler crc of node map ids so can use to check other files.      
      call adler32(mycrc, a(kmap), numnp)

      if (mycrc .ne. mnodcrc) then
         write(6, 1000) 'Node number map ids'
         okay = .false.
      end if
C************************************************************************
C     check element number map 
C************************************************************************
C     read the element number map
      call exgenm (ndb, a(kmap), ierr)

C ... Calculate adler crc of element map ids so can use to check other files.      
      call adler32(mycrc, a(kmap), numel)

      if (mycrc .ne. menmcrc) then
         write(6, 1000) 'Element number map ids'
         okay = .false.
      end if
C************************************************************************
C     check element order map 
C************************************************************************
C     read the element order map
      call exgmap (ndb, a(kmap), ierr)

C ... Calculate adler crc of element order map ids so can use to check other files.      
      call adler32(mycrc, a(kmap), numel)

      if (mycrc .ne. meomcrc) then
         write(6, 1000) 'Element order map ids'
         okay = .false.
      end if
      call mddel ('SCR')

      
C     print warnings if number of node sets or side set is not the same
      if (numnps .ne. nmnps2) then
         write(6, 1005) 'Number of node sets'
         write(6, 1010) numnps, nmnps2
      end if

      if (numess .ne. nmess2) then
         write(6, 1005) 'Number of side sets'
         write(6, 1010) numess, nmess2
      end if

C     read inquire variables
C     length of node set list
      call exinq(ndb, EXNSNL, lnsnl2, rnum, cnum, ierr)
      if (lnpsnl .ne. lnsnl2) then
         write(6, 1005) 'Length of the node set node list'
         write(6, 1010) lnpsnl, lnsnl2
      end if
C     length of node set distribution factor list
      call exinq(ndb, EXNSDF, lnsdf2, rnum, cnum, ierr)
      if (lnpsdf .ne. lnsdf2) then
         write(6, 1005) 
     &   'Length of the node set distribution factors list'
         write(6, 1010) lnpsdf, lnsdf2
      end if
C     length of side set node list
      call exinq(ndb, EXSSNL, lssnl2, rnum, cnum, ierr)
      if (lessnl .ne. lssnl2) then
         write(6, 1005) 'Length of the side set node list'
         write(6, 1010) lessnl, lssnl2
      end if
C     length of the side set element list
      call exinq(ndb, EXSSEL, lssel2, rnum, cnum, ierr)
      if (lessel .ne. lssel2) then
         write(6, 1005) 'Length of the side set element list'
         write(6, 1010) lessel, lssel2
      end if
C     length of the side set distribution factors list
      call exinq(ndb, EXSSDF, lssdf2, rnum, cnum, ierr)
      if (lessdf .ne. lssdf2) then
         write(6, 1005) 
     &   'Length of the side set distribution factors list'
         write(6, 1010) lessdf, lssdf2
      end if

      call mdrsrv ('IDTMP', lidtmp, nelbl2)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         ioerr = 1
         return
      end if
      call exgebi(ndb, a(lidtmp), ierr)

      call cmpelb (ndb, nelbl2, a(lidtmp), c(knameb), a(knelb),
     &             a(knlnk), a(knatr), okay)
      if (.not. okay) return

C     read the number of global variables
      call exgvp (ndb, 'G', nglobl, ierr)
      if (nvargl .ne. nglobl) then
         write (6, 1000) 'Number of global variables'
         write (6, 1010) nvargl, nglobl
         okay = .false.
      end if

C     read the number of nodal variables
      call exgvp (ndb, 'N', nnodal, ierr)
      if (nvarnp .ne. nnodal) then
         write (6, 1000) 'Number of nodal variables'
         write (6, 1010) nvarnp, nnodal
         okay = .false.
      end if

C     read the number of element variables
      call exgvp (ndb, 'E', nelemt, ierr)
      if (nvarel .ne. nelemt) then
         write (6, 1000) 'Number of nodal variables'
         write (6, 1010) nvarel, nelemt
         okay = .false.
      end if
      if (.not. okay) return


C     reserve space to read names of results variable and coordinate name
      if (nglobl .gt. 0) then
         call mcrsrv ('NGLOB', lnglob, MXSTLN*nglobl)
      else
         call mcrsrv ('NGLOB', lnglob, 1)
      end if

      if (nnodal .gt. 0) then
         call mcrsrv ('NNODE', lnnode, MXSTLN*nnodal)
      else
         call mcrsrv ('NNODE', lnnode, 1)
      end if

      if (nelemt .gt. 0) then
         call mcrsrv ('NELEM', lnelem, MXSTLN*nelemt)
      else
         call mcrsrv ('NELEM', lnelem, 1)
      end if

      call mcstat (cerr, mem)
      if (cerr .gt. 0) then
         call memerr
         ioerr = 1
         return
      end if

      call ervnam (ndb, nglobl, nnodal, nelemt, c(lnglob),
     &             c(lnnode), c(lnelem))

C     compare names of global element and nodal variables

      call cmpnam (nvargl, nvarnp, nvarel, c(kglob), c(knode),
     &             c(kelem), nglobl, nnodal, nelemt, c(lnglob),
     &             c(lnnode), c(lnelem), okay)
      if (.not. okay) return

C     read element variable truth table and compare input
      call mdrsrv ('ITEMP', kitemp, nvarel*nelblk)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr
         ioerr = 1
         return
      end if

      if ((nelblk .gt. 0) .and. (nvarel .gt. 0)) then
         call exgvtt (ndb, nelblk, nvarel, a(kitemp), ierr)

         do 180 i = 1, nvarel*nelblk
            if (a(kevtt+i-1) .ne. a(kitemp+i-1)) then
               write (6, 1000) 'Element Variable Truth Table'
               write (6, 1010) a(kevtt+i-1), a(kitemp+i-1)
               okay = .false.
            end if
  180    continue
      end if

c     Do not delete temporary element block ID data
C     Need to keep ID's for writing result data 
c     call mddel ('IDTMP')
      call mcdel ('NGLOB')
      call mcdel ('NNODE')
      call mcdel ('NELEM')
      call mddel ('ITEMP')
      call mdstat (nerr, mem)
      call mcstat (cerr, mem)
      if ((nerr .gt. 0) .or. (cerr .gt. 0)) then
         call memerr
         ioerr = 1
         return
      end if

 1000 format('**************************************************',/
     $       '**** Compatibility Error:   ', a)
 1005 format('**   Compatibility Warning: ', a)
 1010 format('Master File = ',I10, 5x, ' New File = ', I10)

      return
      end



      subroutine cmpnam (nvargl, nvarnp, nvarel, glob, node, elem,
     &                   nglobl, nnodal, nelemt, nglob, nnode,
     &                   nelem, okay)

      include 'params.blk'

      integer nvargl, nvarnp, nvarel, nglobl, nnodal, nelemt
      character*(mxstln) glob(nvargl), node(nvarnp), elem(nvarel)
      character*(mxstln) nglob(nglobl), nnode(nnodal), nelem(nelemt)
      logical okay

      if (nvargl .gt. 0) then
         do 150 i = 1, nglobl
            if (glob(i) .ne. nglob(i)) then
               write (6, 1015) 'Names of global variables'
               write (6, 1020) glob(i), nglob(i)
               okay = .false.
            end if
  150    continue
      end if
      if (.not. okay) return

      if (nvarnp .gt. 0) then
         do 160 i = 1, nnodal
            if (node(i) .ne. nnode(i)) then
               write (6, 1015) 'Names of nodal variables'
               write (6, 1020) node(i), nnode(i)
               okay = .false.
            end if
  160    continue
      end if
      if (.not. okay) return

      if (nvarel .gt. 0) then
         do 170 i = 1, nelemt
            if (elem(i) .ne. nelem(i)) then
               write (6, 1015) 'Names of element variables'
               write (6, 1020) elem(i), nelem(i)
               okay = .false.
            end if
  170    continue
      end if
      if (.not. okay) return

 1015 format('**************************************************',/
     $       '**** Compatibility Error:   ', a)

 1020 format('Master File = ',a, /, 'New File    = ', a)

      return
      end
