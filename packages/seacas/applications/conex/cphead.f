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
C $Id: cphead.f,v 1.9 2008/03/14 13:50:47 gdsjaar Exp $
C************************************************************************
      subroutine cphead (ndbo, ndbi, a, ia, c, qainfo, ioerr)
C************************************************************************
C
C     cphead
C     C.A. Forsythe 01/25/95
C
C DESCRIPTION: Write header data from file id ndbi to ndbo 
C
C PARAMETERS:
C     ndbo  - IN  - file id of output file (write to)
C     ndbi  - IN  - file id of input file (read from)
C     a     - I/O - dynamic memory base array for numeric values
C     c     - I/O - dynamic memory base array for character values
C     ioerr - OUT - error flag for memory or io errors
C
C************************************************************************

      include 'exodusII.inc'

      include 'ebptr.blk'
      include 'namptr.blk'
      include 'exinit.blk'
      include 'exsets.blk'
      include 'exrvar.blk'
      include 'exprop.blk'
      include 'exqain.blk'
      include 'crc.blk'

      integer ndbo, ndbi
      dimension a(1)
      integer ia(1)
      character*1 c(1)
      character*(mxstln) qainfo(6)
      integer ioerr

C     local variables
      integer nerr, cerr
      real rdum
      ioerr = 0

C************************************************************************
C     write initialization variables
C************************************************************************
      call expini (ndbo, title, ndim, numnp, numel, nelblk, numnps,
     &             numess, ierr)

C************************************************************************
C     write coordinates of nodes
C************************************************************************
C     need to read coordinates first
      call mdrsrv ('X', kxn, numnp*ndim)
      call mdrsrv ('Y', kyn, numnp*ndim)
      if (ndim .eq. 3) call mdrsrv ('Z', kzn, numnp*ndim)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr()
         ioerr = 1
         return
      end if

C     read from input file
      if (ndim .eq. 2) then
         call exgcor (ndbi, a(kxn), a(kyn), rdum, ierr)
      else if (ndim .eq. 3) then
         call exgcor (ndbi, a(kxn), a(kyn), a(kzn), ierr)
      else 
C        print error message
      end if

C     write to output file
      if (ndim .eq. 2) then
         call expcor (ndbo, a(kxn), a(kyn), rdum, ierr)
      else if (ndim .eq. 3) then
         call expcor (ndbo, a(kxn), a(kyn), a(kzn), ierr)
      else
C        print error message
      end if

      call mddel ('X')
      call mddel ('Y')
      if (ndim .eq. 3) call mddel ('Z')

      
C************************************************************************
C     write node number map 
C************************************************************************
      call mdrsrv ('SCR', kmpnod, numnp)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr()
         ioerr = 1
         return
      end if

C     read the node number map
      call exgnnm (ndbi, a(kmpnod), ierr)

C     write the node number map to results file
      call expnnm (ndbo, a(kmpnod), ierr)

C ... Calculate adler crc of node map ids so can use to check other files.      
      call adler32(mnodcrc, a(kmpnod), numnp)

      call mddel ('SCR')

C************************************************************************
C     write element number map 
C************************************************************************
      call mdrsrv ('SCR', kmpeln, numel)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr()
         ioerr = 1
         return
      end if

C     read the element number map
      call exgenm (ndbi, a(kmpeln), ierr)

C     write the element number map
      call expenm (ndbo, a(kmpeln), ierr)

C ... Calculate adler crc of element map ids so can use to check other files.      
      call adler32(menmcrc, a(kmpeln), numel)

      call mddel ('SCR')


C************************************************************************
C     write element order map 
C************************************************************************
      call mdrsrv ('SCR', kmapel, numel)
      call mdstat (nerr, mem)
      if (nerr .gt. 0) then
         call memerr()
         ioerr = 1
         return
      end if

C     read the element order map
      call exgmap (ndbi, a(kmapel), ierr)

C     write the element order map
      call expmap (ndbo, a(kmapel), ierr)

C ... Calculate adler crc of element order map ids so can use to check other files.      
      call adler32(meomcrc, a(kmapel), numel)

      call mddel ('SCR')

C************************************************************************
C     write element block data
C     write connectivity array
C     write attribute array
C************************************************************************

      call ewelbk (ndbo, nelblk, a(kidelb), a(knelb), a(knlnk), 
     &             a(knatr), c(knameb), a(klink), a(katrib))
      call mddel ('LINK')
      call mddel ('ATRIB')
      if (nerr .gt. 0) then
         call memerr()
         ioerr = 1
         return
      end if

C************************************************************************
C     write concatenated node sets
C************************************************************************
      if (numnps .gt. 0) then
C        reserve memory for node set parameters
C        node set ID's
         call mdrsrv ('IDNPS', kidnps, numnps)
C        number of nodes in each node set
         call mdrsrv ('NNNPS', knnnps, numnps)
C        number of distribution factors for each set
         call mdrsrv ('NDNPS', kndnps, numnps)
C        indices in LTNNPS - 1st node for each node set
         call mdrsrv ('IXNNPS', kxnnps, numnps)
C        indices in FACNPS - 1st distribution factor for each set
         call mdrsrv ('IXDNPS', kxdnps, numnps)
C        contains nodes for all node sets
         call mdrsrv ('LTNNPS', kltnps, lnpsnl)
C        contains distribution factors for all node sets
         call mdrsrv ('FACNPS', kfcnps, lnpsdf)
         call mdstat (nerr, mem)
         if (nerr .gt. 0) then
            call memerr
            ioerr = 1
            return
         end if

C        read the concatenated node sets
         call exgcns (ndbi, a(kidnps), a(knnnps), a(kndnps), a(kxnnps),
     &                a(kxdnps), a(kltnps), a(kfcnps), ierr)

C        write the concatenated node sets
         call expcns (ndbo, a(kidnps), a(knnnps), a(kndnps), a(kxnnps),
     &                a(kxdnps), a(kltnps), a(kfcnps), ierr)

         call mddel ('IDNPS')
         call mddel ('NNNPS')
         call mddel ('NDNPS')
         call mddel ('IXNNPS')
         call mddel ('IXDNPS')
         call mddel ('LTNNPS')
         call mddel ('FACNPS')
         call mdstat (nerr, mem)
         if (nerr .gt. 0) then
            call memerr
            ioerr = 1
            return
         end if
      end if

C************************************************************************
C     write concatednated side sets
C************************************************************************
      if (numess .gt. 0) then

C        reserve memory for the side sets parameters
C        side sets ID's
         call mdrsrv ('IDESSS', kidess, numess)
C        number of sides for each side set
         call mdrsrv ('NSESS', knsess, numess)
C        number of distribution factors for each side set
         call mdrsrv ('NDESS', kndess, numess)
C        indices in LTEESS - 1st element for each side set
         call mdrsrv ('IXEESS', kxeess, numess)
C        indices in FACESS - 1st distribution factors for each side set
         call mdrsrv ('IXDESS', kxdess, numess)
C        contains elements for all side sets
         call mdrsrv ('LTEESS', kleess, lessel)
C        contains sides for all side sets
         call mdrsrv ('LTSESS', klsess, lessel)
C        contains distribution factors for all side sets
         call mdrsrv ('FACESS', kfcess, lessdf)
         call mdstat (nerr, mem)
         if (nerr .gt. 0) then
            call memerr
            ioerr = 1
            return
         end if

C        read concatednated side set data
         call exgcss (ndbi, a(kidess), a(knsess), a(kndess), a(kxeess),
     &                a(kxdess), a(kleess), a(klsess), a(kfcess), ierr)

C        write concatednated side set data
         call expcss (ndbo, a(kidess), a(knsess), a(kndess), a(kxeess),
     &                a(kxdess), a(kleess), a(klsess), a(kfcess), ierr)

         call mddel ('IDESSS')
         call mddel ('NSESS')
         call mddel ('NDESS')
         call mddel ('IXEESS')
         call mddel ('IXDESS')
         call mddel ('LTEESS')
         call mddel ('LTSESS')
         call mddel ('FACESS')
         call mdstat (nerr, mem)
         if (nerr .gt. 0) then
            call memerr
            ioerr = 1
            return
         end if

      end if

C************************************************************************
C     write qa and information records
C************************************************************************

C     reserve memory for QA and information records
      call mcget ( ((nqarec+1)*MXSTLN*4) + (ninfo*MXLNLN) )
      call mcrsrv ('QAREC', kqarec, (nqarec+1)*MXSTLN*4)
      call mcrsrv ('INFO', kinfo, ninfo*MXLNLN)
      call mcstat (cerr, mem)
      if (cerr .gt. 0) then
         call memerr
         ioerr = 1
         return
      end if

      if ((nqarec .gt. 0) .or. (ninfo .gt. 0)) then
         call erqain (ndbi, '*', nqarec, c(kqarec), ninfo, c(kinfo))
      end if
      call ewqain (ndbo, qainfo, nqarec, c(kqarec), ninfo, c(kinfo))

      call mcdel ('QAREC')
      call mcdel ('INFO')


C************************************************************************
C     write coordinate names
C************************************************************************
      call mcrsrv ('NAMECO', kncoor, MXSTLN*ndim)
      call mcstat (cerr, mem)
      if (cerr .gt. 0) then
         call memerr
         ioerr = 1
         return
      end if

      call erwcon (ndbi, ndbo, c(kncoor))

      call mcdel ('NAMECO')
      call mcstat (cerr, mem)
      if (cerr .gt. 0) then
         call memerr
         ioerr = 1
         return
      end if


C************************************************************************
C     write number of global, nodal, and element results variables
C************************************************************************
C     write the number of global variables
      call expvp (ndbo, 'G', nvargl, ierr)

C     read the number of nodal variables
      call expvp (ndbo, 'N', nvarnp, ierr)

C     read the number of element variables
      call expvp (ndbo, 'E', nvarel, ierr)

C************************************************************************
C     write names of global, nodal, and element results variables
C************************************************************************

      call ewvnam (ndbo, nvargl, nvarnp, nvarel, c(kglob),
     &             c(knode), c(kelem))
      if (ioerr .eq. 1) return

C************************************************************************
C     write element variable truth table
C************************************************************************

C     write the element variable truth table
      if ((nelblk .gt. 0) .and. (nvarel .gt. 0))
     &   call expvtt (ndbo, nelblk, nvarel, a(kevtt), ierr)

C************************************************************************
C     write element block properties
C************************************************************************
C     common /exprop/ numebp, numsnp, numssp

C     Reserve memory for element block properties names and values
      if (numebp .gt. 0) then
        call mcrsrv ('EBPNAM', iebpn, numebp * mxstln)
        call mdrsrv ('EBPVAL', iebpv, numebp * nelblk)
        call mcstat (cerr, mem)
        call mdstat (nerr, mem)
        if ((nerr .ne. 0) .or. (cerr .ne. 0)) then
          call memerr()
          ioerr = 1
          return
        end if
        
        call rwpval (ndbi, ndbo, EXEBLK, numebp, c(iebpn), ia(iebpv))
        
        call mcdel ('EBPNAM')
        call mddel ('EBPVAL')
      end if

C************************************************************************
C     write node set properties
C************************************************************************
      if (numnsp .gt. 0) then
        call mcrsrv ('NSPNAM', inspn, numnsp * mxstln)
        call mdrsrv ('NSPVAL', inspv, numnsp * numnps)
        call mcstat (cerr, mem)
        call mdstat (nerr, mem)
        if ((nerr .ne. 0) .or. (cerr .ne. 0)) then
          call memerr()
          ioerr = 1
          return
        end if
        
        call rwpval (ndbi, ndbo, EXNSET, numnsp, c(inspn), ia(inspv))
        
        call mcdel ('NSPNAM')
        call mddel ('NSPVAL')
      end if
C************************************************************************
C     read/write side set properties
C************************************************************************

      if (numssp .gt. 0) then
        call mcrsrv ('SSPNAM', isspn, numssp * mxstln)
        call mdrsrv ('SSPVAL', isspv, numssp * numess)
        call mcstat (cerr, mem)
        call mdstat (nerr, mem)
        if ((nerr .ne. 0) .or. (cerr .ne. 0)) then
          call memerr()
          ioerr = 1
          return
        end if
        
        call rwpval (ndbi, ndbo, EXSSET, numssp, c(isspn), ia(isspv))
        
        call mcdel ('SSPNAM')
        call mddel ('SSPVAL')
        call mdstat (nerr, mem)
        if (nerr .ne. 0) then
          call memerr()
          ioerr = 1
          return
        end if
      end if
      return
      end 

C************************************************************************
C     Utility subroutines for cphead.f
C************************************************************************

C************************************************************************
      subroutine  erwcon (ndbi, ndbo, nameco)
C************************************************************************
C
C     erwcon - Exodus Read/Write COordinate Names
C              C.A. Forsythe 1/26/95
C
C     Parameters:
C     ndbi   - IN  - database file to read from
C     ndbo   - I/O - database file to write to
C     nameco - IN  - array to hold coordinate names
C
      include 'params.blk'

      integer ndbi
      integer ndbo
      character*(MXSTLN) nameco(*)

C     read coordinate names form input file
      call exgcon (ndbi, nameco, ierr)

C     write coordinate names to output file
      call expcon (ndbo, nameco, ierr)

      return
      end


C************************************************************************
      subroutine rwpval(ndbi, ndbo, flag, nump, names, values)
C************************************************************************
C     This subroutine with read properties from an input file and
C     write properties to an output file
C     ndbi   - IN - file id to read from
C     ndbo   - IN - file id to write to
C     flag   - IN - flag indicating what type of properties to read/write
C     nump   - IN - number of properties
C     names  - IN - names of the properties
C     values - IN - values of the properties

      include 'params.blk'

      integer ndbi, ndbo, flag, nump
      character*(mxstln) names(*)
      integer values(*)

      names(1) = " "
      call exgpn (ndbi, flag, names, ierr)

      do 100 i = 1, nump
         call exgpa (ndbi, flag, names(i), values(i), ierr)
         call exppa (ndbo, flag, names(i), values(i), ierr)
 100  continue

      return
      end

