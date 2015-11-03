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
C $Id: init.f,v 1.3 2008/03/14 13:50:52 gdsjaar Exp $
C************************************************************************
      subroutine init (ndb, title, ndim, numnp, numel, nelblk,
     &                   numnps, numess, lnpsnl, lnpsdf, lessnl,
     &                   lessel, lessdf, nqarec, ninfo, nvargl,
     &                   nvarnp, nvarel, numebp, numnsp, numssp)
C************************************************************************
C     Read initialization parameters from the exodus file ndb

      include 'exodusII.inc'
      integer ndb, ndim, numnp, numel, nelblk
      character*(MXLNLN) title
      character*(MXSTLN) cdum
      integer numnps, lnpsnl, lnpsdf
      integer numess, lessnl, lessel, lessdf
      integer nqarec, ninfo
      integer numebp, numnsp, numssp

      call exgini(ndb, title, ndim, numnp, numel, nelblk,
     &            numnps, numess, ierr)

C     read the number of global variables
      call exgvp (ndb, 'G', nvargl, ierr)
C     read the number of nodal variables
      call exgvp (ndb, 'N', nvarnp, ierr)
C     read the number of element variables
      call exgvp (ndb, 'E', nvarel, ierr)
C     length of node set list
      call exinq(ndb, EXNSNL, lnpsnl, rnum, cdum, ierr)
C     length of node set distribution factor list
      call exinq(ndb, EXNSDF, lnpsdf, rnum, cdum, ierr)
C     length of side set node list
      call exinq(ndb, EXSSNL, lessnl, rnum, cdum, ierr)
C     length of the side set element list
      call exinq(ndb, EXSSEL, lessel, rnum, cdum, ierr)
C     length of the side set distribution factors list
      call exinq(ndb, EXSSDF, lessdf, rnum, cdum, ierr)
C     number of qa records
      call exinq(ndb, EXQA, nqarec, rdum, cdum, ierr)
C     number of information records
      call exinq(ndb, EXINFO, ninfo, rdum, cdum, ierr)
C     number of element block properties
      call exinq(ndb, EXNEBP, numebp, rdum, cdum, ierr)
C     number of node set properties
      call exinq(ndb, EXNNSP, numnsp, rdum, cdum, ierr)
C     number of side set properties
      call exinq(ndb, EXNSSP, numssp, rdum, cdum, ierr)

      return
      end
