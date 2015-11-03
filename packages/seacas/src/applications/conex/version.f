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
C $Id: version.f,v 1.18 2008/12/08 17:34:06 gdsjaar Exp $
C************************************************************************
      subroutine version (qainfo)
C************************************************************************

      include 'params.blk'
      character*(mxstln) qainfo(6)
      character*(mxstln) cvsrev, cvsdat

C     The CVS* variables are updated by the CVS/RCS program every time
C     this routine is commited to the repository.  Therefore, whenever 
C     there are any changes to any routines, this routine should also 
C     be committed.  The assignment to QAINFO(I) strips off the 'Revision'
C     and 'Date' strings from the CVS variables.  Kludgey but it might work.

C      --QAINFO - the current program QA information:
C      --   (1) = program name
C      --   (2) = revision date
C      --   (3) = version as "QA xx.xx" or "X  xx.xx" or "   xx.xx"
C      --   (4) = program name with version appended
C      --   (5) = date of current run
C      --   (6) = time of current run

      
      cvsrev = '$Revision: 1.18 $'
      cvsdat = '$Date: 2008/12/08 17:34:06 $'

      qainfo(1) = 'Conex2                          '
      qainfo(2) = '                                '
      qainfo(3) = '                                '
      qainfo(4) = '                                '
      qainfo(5) = '                                '
      qainfo(6) = '                                '
      qainfo(2) = cvsdat(10:17)
      qainfo(3)(2:5) = cvsrev(12:15)

      return
      end
C...............
