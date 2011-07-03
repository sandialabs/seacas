C Copyright(C) 2009 Sandia Corporation. Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C certain rights in this software.
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

C=======================================================================
      SUBROUTINE GSLIBV (STRING)
C=======================================================================
C$Id: gslibv.f,v 1.2 2009/03/25 04:47:54 gdsjaar Exp $
C$Log: gslibv.f,v $
CRevision 1.2  2009/03/25 04:47:54  gdsjaar
CAdded blotII2 source since Copyright was asserted.
C
CUpdate copyright notice in suplib.
C
CAdd blotII2 to config files.  Note that blot will not build yet since
Cit requires some libraries that are still being reviewed for copyright
Cassertion.
C
CRevision 1.2  1996/02/29 15:39:30  caforsy
CChanged string length.
C
CRevision 1.1  1996/02/26 17:47:43  caforsy
CAdded subroutine gslibv.f.  Returns current version number of suplib.
C
C
C***********************************************************************
C
C     *** GSLIBV *** Returns current version number of the suplib library
C                    Get SupLIB Version number
C     Parameters:
C     STRING - OUT - string containing version number of suplib
C
C     Version Number Format:
C           n1.n2.n3
C
C     where n1 is the major version number
C           n2 is the minor version or change capability number
C           n3 is the bug-fix number
C***********************************************************************

      CHARACTER*32 STRING

C     Version Format: 'major.minor.bug_fix'
      STRING = '1.0.0'

      RETURN
      END

