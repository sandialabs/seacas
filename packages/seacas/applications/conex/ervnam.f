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
C $Id: ervnam.f,v 1.3 2008/03/14 13:50:48 gdsjaar Exp $
C=======================================================================
      subroutine ervnam (id, nvargl, nvarnp, nvarel, glob,
     &                   node, elem)
C=======================================================================
C
C   --ervnam - read names of the global, nodal and element variable
C   --   Renamed and modified by C.A. Forsythe 01/19/96
C   --This code originated from the subroutine
C   --*** DBINAM *** (EXOLIB) Read database names
C   --   Written by Amy Gilkey - revised 02/08/88
C   --DBINAM performed a number of different input file read base
C   --on the passed in option argument.  DBINAM was split up
C   --into a number of different subroutins
C   --
C   --
C   --Parameters:
C        id     - IN  - file ID number
C        nvargl - IN  - number of global variables
C        nvarnp - IN  - number of nodal variables
C        nvarel - IN  - number of element variables
C        glob   - OUT - array to store global variable names
C        node   - OUT - array to store nodal variable names
C        elem   - OUT - array to store element variable names
C     Wrapper for reading character string in exodusII calls
C
      include 'params.blk'

      integer id, nvargl, nvarnp, nvarel
      character*(MXSTLN) glob(nvargl), node(nvarnp), elem(nvarel)

C     read names of global variables
      if (nvargl .gt. 0)
     &   call exgvan (id, 'G', nvargl, glob, ierr)

C     read names of nodal variables
      if (nvarnp .gt. 0)
     &   call exgvan (id, 'N', nvarnp, node, ierr)

C     read names of element variables
      if (nvarel .gt. 0)
     &   call exgvan (id, 'E', nvarel, elem, ierr)

      return
      end
