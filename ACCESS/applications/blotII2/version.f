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

C $Id: version.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $

      SUBROUTINE VERSION(QAINFO)

      include 'params.blk'

      CHARACTER*(MXSTLN) QAINFO(6)
      CHARACTER*(MXSTLN) CVSREV, CVSDAT

C     The CVS* variables are updated by the CVS/RCS program every time
C     this routine is commited to the repository.  Therefore, whenever 
C     there are any changes to any routines, this routine should also 
C     be committed.  The assignment to QAINFO(I) strips off the 'Revision'
C     and 'Date' strings from the CVS variables.  Kludgey but it might work.
      
      CVSREV = '$Revision: 1.1 $'
      CVSDAT = '$Date: 2009/03/25 04:47:54 $'

      QAINFO(1) = 'BlotII2                         '
      QAINFO(2) = '                                '
      QAINFO(3) = '                                '
      QAINFO(4) = '                                '
      LREV = LENSTR(CVSREV)
      LDAT = LENSTR(CVSDAT)
      
      if (cvsrev(1:1) .eq. '$') then
C ... Dont copy the $
         QAINFO(2) = CVSDAT(8:MIN(MXSTLN,LDAT-2))
         QAINFO(3) = CVSREV(11:MIN(MXSTLN,LREV-2))
      else
         QAINFO(2) = CVSDAT(1:MIN(MXSTLN,LDAT))
         Qainfo(3) = CVSREV(1:MIN(MXSTLN,LREV))
      end if

      RETURN
      END

C-dbielb updated
c-rename dbieb1 to dbiebi to reduce linkage confusion.
C-dynamic dimensioning of character arrays
C-fixed up writing of xmgr neutral file
c-check for number of info/qa records
c-remove wrtneu from distribution - same as blot now
c-remove duplicate def of exopen
c-fix for zero timesteps with variables
c-minimal what and where commands for elements
c-fixes for color setting and name output width
c-fix nodeset and sideset input
c-better matfac message, fix xmgr neutral file output
c-remove prompting at end of plot in certain situations
c-fix zoom before plot problem
c-removed unreachable lines.
c-added 'external blkdat' for dec alpha systems
c-more doubleescape files
c-memory related fixes (eliminate mapel for large)
c-dbinam.f converts variable names to uppercase now
c-added ndb to rdtype argument list
c-fixed nodeset/sideset distribution factor index list sizes
c-control over sphere segments added 'SPHERE ON|OFF nsegs'
c-fixed sph contour plotting when copen/cmin/cmax specified
c-fixed truth table modifications for hexshells (dbinm1.f)
c-fix common blocks, external data, times array, linux port
c-fix to array in face3d and also to maxepn
c-reduce mdlongs in dbielb
c-added feature which allows to abreviate the vector command (cmdvar.f)
c-array bounds problem in setcol
c-fix for 9-node quad in qepain.f
c-remove qainfo.blk 
c-param problem in main, clean up common blocks
c-face matching problem, changed mxepn and face3d
c-fix symsph non-saving problem (do calc each time)
c-fix control data problem with light and rainbow common blocks
c-add lcntr to save in dtlab.F
c-fix label length in Cre: Mod: Drw: label
c-fix sphere plotting -- ihidop not set for 2d and 2d vs 3d not checked
c-fix pointer bug in mssurf
c-fix dimension of sphere radius array
c-fix handling of >8 character date strings (qalab.f)
c-fixed what3 command handling
c-logical statement function problem on SGI
c-better vector suffix matching
c-background color changing should work...
c-fix save addcrv in tpcomd, spcomd, lncomd
c-minor fix to filhnd to avoid undefined characters
c-minor fix for uninit variables and missing save statement
c-minor fix for reading variables -- value was getting lost, fix range setting for constant neg val.
c-fix so doesn't write to log file if it can't be opened
c-default sphere radius to 1.0 if no element attributes
c-do not allocate map if no elements
c-Add tet capability
c-Fix for multiple blocks of tets
c-fix for problem with 64-bit and float(intarg) -- changed to 1.0*intarg
c-Add write of CSV (comma separated values) neutral file format
c-Fix some memory access problems
c-Remove comment lines from CSV written file.
c-Add RAW (CSV with no legend) format
c-Fixe opening of log file
c-Fix undefined iostat in getins
c-Fix memory problem in msmain (echo command)
c-fortran lint warnings fixed
c-Increased length of string used to store input filename from 40 to 256
c-Handle "zoom node id radius" command for nodes not visible in current view
c-Optional 'alive value' added to death on command
c-Handle output of values with 3-digit exponent 1.0e-100
c-Add default radius for sph "sphere on [radius defrad] [segments]"
c-Removed some history-only output
c-add minor support for nodeset and sideset variables
c-Revert the zoom node command checkin.  It wasn't working correctly
