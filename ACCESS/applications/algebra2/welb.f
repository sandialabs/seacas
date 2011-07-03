C    Copyright(C) 2008 Sandia Corporation.  Under the terms of Contract
C    DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
C    certain rights in this software
C    
C    Redistribution and use in source and binary forms, with or without
C    modification, are permitted provided that the following conditions are
C    met:
C    
C    * Redistributions of source code must retain the above copyright
C       notice, this list of conditions and the following disclaimer.
C              
C    * Redistributions in binary form must reproduce the above
C      copyright notice, this list of conditions and the following
C      disclaimer in the documentation and/or other materials provided
C      with the distribution.
C                            
C    * Neither the name of Sandia Corporation nor the names of its
C      contributors may be used to endorse or promote products derived
C      from this software without specific prior written permission.
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
C=======================================================================
      SUBROUTINE WELB (NDBOUT, NELBLK, VISELB, ALLELE, NAMELB,
     &           NUMLNK, NUMATR, LINK, ATRIB, NUMELB, IXELB, IXELBO,
     &           IXELEM, NEWNOD, NODIX, IDELB, A, C, MERR)
C=======================================================================
C $Id: welb.f,v 1.1 2008/10/31 05:04:01 gdsjaar Exp $
C $Log: welb.f,v $
C Revision 1.1  2008/10/31 05:04:01  gdsjaar
C Moved the applications into an applications subdirectory.
C
C Revision 1.1  2008/06/18 16:00:27  gdsjaar
C Initial adding of files to sourceforge
C
C Revision 1.5  2008/03/14 13:45:31  gdsjaar
C Added copyright information to top of all files.
C
C ALGEBRA2 is now open-sourced under BSD license
C
C Revision 1.4  1996/02/12 21:28:51  caforsy
C Removed redundant checks for ExodusII API calls
C
C Revision 1.3  1995/12/10 15:46:55  caforsy
C Integrated call to rpterr and mdcomp/mccomp calls
C
C Revision 1.2  1995/10/25 16:35:18  caforsy
C Fixed index during write of element block information
C
C Revision 1.1  1995/10/03 21:43:44  mksmith
C Adding files new to algII for algebra2
C
C
C   --The original version of this code was written by
C   --Amy Gilkey - revised 04/19/88
C   --*** RWELB *** (ALGEBRA) Read and write database element blocks
C   --RWELB was modified and renamed to WELB 8/25/95
C   --
C   --WELB writes the element block information to the database.
C   --Deleted elements are removed and nodes are renumbered.
C   --
C   --Parameters:
C   --   NDBOUT   - IN - the output database file
C   --   NELBLK   - IN - the number of element blocks
C   --   VISELB   - IN - true iff element block i is to be written
C   --   ALLELE   - IN - true iff all elements are selected
C   --   NAMELB   - IN - element type for each element block
C   --   NUMLNK   - IN - Number of nodes per element for each element block
C   --   NUMATR   - IN - Number of attributes for each element block
C   --   LINK     - IN - Connectivity array for element blocks
C   --   ATRIB    - IN - Attribute array for element blocks
C   --   NUMELB   - I/O - the number of elements in each block; set if ALLELE
C   --   IXELB    - I/O - the cumulative element counts for each
C   --                     element block; set if ALLELE
C   --   IXELBO   - I/O - the cumulative element counts for each output block;
C   --                     set if ALLELE
C   --   IXELEM   - IN - the indices of the output elements
C   --                   (iff IXELBO <> IXELB)
C   --   NEWNOD   - IN - true iff nodes are renumbered
C   --   NODIX(i) - IN - the zoom mesh index for each node (iff NEWNOD)
C   --   A        - IN - the dynamic memory array
C   --   MERR     - OUT - memory error flag

      include 'params.blk'

      INTEGER NDBOUT
      INTEGER NELBLK
      LOGICAL VISELB(NELBLK)
      LOGICAL ALLELE
      CHARACTER*(MXSTLN) NAMELB(*)
      INTEGER NUMLNK(*)
      INTEGER NUMATR(*)
      INTEGER LINK(*)
      REAL    ATRIB(*)
      INTEGER NUMELB(*)
      INTEGER IXELB(0:NELBLK)
      INTEGER IXELBO(0:NELBLK)
      INTEGER IXELEM(*)
      LOGICAL NEWNOD
      INTEGER NODIX(*)
      INTEGER IDELB(*)
      DIMENSION A(1)
      CHARACTER*1 C(1)
      INTEGER MERR

      CHARACTER*(32) NAEB
      INTEGER NELB, EBID, NLNK, NATR
      INTEGER NERR

      MERR  = 0
      ILNK  = 0
      IATR  = 0
      ISLNK = 0
      ISATR = 0

      IF (ALLELE) THEN
         IXELB(0) = 0
         IXELBO(0) = 0
      END IF

      MAXCON = NUMLNK(1)*NUMELB(1)
      MAXATR = NUMATR(1)*NUMELB(1)
      DO 90 I = 1, NELBLK
         NELB = NUMELB(I)
         NLNK = NUMLNK(I)
         NATR = NUMATR(I)
         IF (NELB*NLNK .GT. MAXCON) MAXCON = NELB*NLNK
         IF (NELB*NATR .GT. MAXATR) MAXATR = NELB*NATR
  90  CONTINUE

C     Reserve memory for scratch files
      CALL MDGET(MAXCON + MAXATR)

C     Loop from 1 to number of element blocks
      DO 100 IELB = 1, NELBLK

C           These variable must be set here in order to calculate
C           the correct indices for the LINK and ATRIB arrays
C           The problem arises when the VISIBLE command is used
C           and not all element blocks are visible
            NELB = NUMELB(IELB)
            NLNK = NUMLNK(IELB)
            NATR = NUMATR(IELB)
            ISLNK = ILNK + 1
            ILNK = ILNK + (NLNK * NELB)
            ISATR = IATR + 1
            IATR = IATR + (NATR * NELB)

C        If write all elements or write this element block
         IF (ALLELE .OR. VISELB(IELB)) THEN

            EBID = IDELB(IELB)
            NAEB = NAMELB(IELB)


            IF (ALLELE) THEN
               IXELB(IELB)  = IXELB(IELB-1)  + NELB
               IXELBO(IELB) = IXELBO(IELB-1) + NELB
            END IF

            NELBO = IXELBO(IELB) - IXELBO(IELB-1)

C           Write element block information
C           expelb(fileid, elem_blk_id, type_elem, num_elem,
C                  num_nodes_perelem, num_attrib_perelem, errorid)
            CALL EXPELB(NDBOUT, EBID, NAEB, NELBO,
     &                  NLNK, NATR, IERR)

            if ((NELBO .NE. NELB)  .or. NEWNOD) then
               CALL MDRSRV('LNSCR', KLNSCR, MAX(NELB,NELBO)*NLNK)
               CALL MDRSRV('ATRSCR', KATSCR, MAX(NELB,NELBO)*NATR)
               CALL MDSTAT (NERR, MEM)
               IF (NERR .GT. 0) then
                  call memerr
                  MERR = 1
                  return
               end if
            else
               klnscr = 1
               katscr = 1
            end if

            IEL  = IXELB(IELB-1)+1
            IELO = IXELBO(IELB-1)+1

C           Write element block connectivity array
C           Write element block attributes
            CALL WCONAT (NDBOUT, NELB, NELBO, IEL, IXELEM(IELO),
     &           NLNK, NATR, MAX(1,NLNK), MAX(1,NATR),
     &           LINK(ISLNK), ATRIB(ISATR), NAEB,
     &           EBID, NEWNOD, NODIX, 
     &           A(KLNSCR), A(KATSCR))

            IF (nelbo .ne. nelb  .or.  newnod) THEN
               CALL MDDEL ('LNSCR')
               CALL MDDEL ('ATRSCR')
               CALL MDSTAT (NERR, MEM)
               IF (NERR .GT. 0) then
                  call memerr
                  MERR = 1
                  return
               end if
            END IF

         END IF
  100 CONTINUE

      RETURN
      END


