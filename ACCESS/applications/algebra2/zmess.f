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
      SUBROUTINE ZMESS (NUMEL, NUMELO, IXELEM, NUMNP, NUMNPO, IXNODE,
     &           NUMESS, LESSEL, LESSNL, IDESS, NEESS, NNESS, IXEESS,
     &           IXNESS, LTEESS, LTSESS, LTNESS, FACESS, NDESS, NEWIX,
     &           NEWSD, IXESS, IXNSS)
C=======================================================================
C $Id: zmess.f,v 1.1 2008/10/31 05:04:01 gdsjaar Exp $
C $Log: zmess.f,v $
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
C Revision 1.4  1998/08/04 15:03:13  gdsjaar
C Added trap and error message for sideset with no nodes or elements
C
C Revision 1.3  1995/10/03 21:36:50  mksmith
C Removing old algII files
C Replacing modified algII files
C

C   --*** ZMESS *** (ALGEBRA) Compress database side sets
C   --   Written by Amy Gilkey - revised 07/13/89
C   --   Modified for ExodusII version 2 database format 10/2/95
C   --
C   --ZMESS compresses the side set information by renumbering the
C   --elements and nodes and removing deleted elements (and their nodes).
C   --
C   --Parameters:
C   --   NUMEL      - IN - the number of elements
C   --   NUMELO     - IN - the number of output elements
C   --   IXELEM     - IN - the indices of the output elements
C   --                     (iff NUMELO <> NUMEL)
C   --   NUMNP      - IN - the number of nodes
C   --   NUMNPO     - IN - the number of nodes
C   --   IXNODE     - IN - the indices of the output nodes
C   --                     (iff NUMNPO <> NUMNP)
C   --   NUMESS - IN/OUT - the number of side sets
C   --   LESSEL - IN/OUT - the length of the side sets element list
C   --   LESSNL - IN/OUT - the length of the side sets node list
C   --   IDESS  - IN/OUT - the side set ID for each set
C   --   NEESS  - IN/OUT - the number of elements for each set
C   --   NNESS  - IN/OUT - the number of nodes for each set
C   --   IXEESS - IN/OUT - the index of the first element for each set
C   --   IXNESS - IN/OUT - the index of the first node for each set
C   --   LTEESS - IN/OUT - the elements for all sets
C   --   LTSESS - IN/OUT - array of sides for all side sets
C   --   LTNESS - IN/OUT - the nodes for all sets
C   --   FACESS - IN/OUT - the distribution factors for all sets
C   --   NDESS  - IN/OUT - the number of distribution factors in each side set
C   --   NEWIX - SCRATCH - size = MAX (NUMEL, NUMNP)
C   --   NEWSD - SCRATCH - size = LESSEL
C   --   IXESS - SCRATCH - size = LESSEL
C   --   IXNSS - SCRATCH - size = LESSNL

      INTEGER NUMEL
      INTEGER NUMELO
      INTEGER IXELEM(*)
      INTEGER NUMNP
      INTEGER NUMNPO
      INTEGER IXNODE(*)
      INTEGER NUMESS
      INTEGER LESSEL
      INTEGER LESSNL
      INTEGER IDESS(*)
      INTEGER NEESS(*)
      INTEGER NNESS(*)
      INTEGER IXEESS(*)
      INTEGER IXNESS(*)
      INTEGER LTEESS(*)
      INTEGER LTSESS(*)
      INTEGER LTNESS(*)
      REAL    FACESS(*)
      INTEGER NDESS(*)
      INTEGER NEWIX(*)
      INTEGER NEWSD(*)
      INTEGER IXESS(*)
      INTEGER IXNSS(*)

      IF (NUMEL .EQ. NUMELO) RETURN

C   --Get new element number for each existing element number

      DO 100 IEL = 1, NUMEL
         NEWIX(IEL) = 0
  100 CONTINUE
      DO 110 IX = 1, NUMELO
         NEWIX(IXELEM(IX)) = IX
  110 CONTINUE
      DO 115 IX = 1, LESSEL
         NEWSD(IX) = LTSESS(IX)
 115  CONTINUE

C   --Zero out deleted elements in side sets and renumber elements while
C   --packing element list

      NLO = 0
      DO 120 NL = 1, LESSEL
         IF (NEWIX(LTEESS(NL)) .GT. 0) THEN
            NLO = NLO + 1
            LTEESS(NLO) = NEWIX(LTEESS(NL))
            LTSESS(NLO) = NEWSD(NL)
            IXESS(NL) = NLO
         ELSE
            IXESS(NL) = 0
         END IF
  120 CONTINUE
      LESSEL = NLO

C   --Get new node number for each existing node number

      DO 130 INP = 1, NUMNP
         NEWIX(INP) = 0
  130 CONTINUE
      DO 140 IX = 1, NUMNPO
         NEWIX(IXNODE(IX)) = IX
  140 CONTINUE

C   --Zero out nodes from deleted elements in side sets and renumber nodes
C   --while packing element list

      DO 150 NL = 1, LESSNL
         IXNSS(NL) = 1
  150 CONTINUE
      DO 180 IESS = 1, NUMESS
         IF (NEESS(IESS) .GT. 0) THEN
           NPE = NNESS(IESS) / NEESS(IESS)
           IF (NPE * NEESS(IESS) .NE. NNESS(IESS)) THEN
             WRITE (*, *) 'PROBLEM'
           END IF
         ELSE
           WRITE (*,155) IDESS(IESS)
 155       FORMAT(' **** ERROR: Sideset ',i5,' has no faces')
           NPE = 0
         END IF
         DO 170 IX = IXEESS(IESS), IXEESS(IESS)+NEESS(IESS)-1
            IF (IXESS(IX) .LE. 0) THEN
               N0 = IXNESS(IESS) + (IX - IXEESS(IESS)) * NPE - 1
               DO 160 I = 1, NPE
                  IXNSS(N0+I) = 0
  160          CONTINUE
            END IF
  170    CONTINUE
  180 CONTINUE
      NLO = 0
      DO 190 NL = 1, LESSNL
         IF (IXNSS(NL) .GT. 0) THEN
            NLO = NLO + 1
            LTNESS(NLO) = NEWIX(LTNESS(NL))
            FACESS(NLO) = FACESS(NL)
         END IF
  190 CONTINUE
      LESSNL = NLO

C   --Adjust side set pointers, etc

      NESSO = 0
      DO 210 IESS = 1, NUMESS
         NE = 0
         IX0 = 0
         DO 200 IX = IXEESS(IESS), IXEESS(IESS)+NEESS(IESS)-1
            IF (IXESS(IX) .GT. 0) THEN
               NE = NE + 1
               IF (IX0 .EQ. 0) IX0 = IXESS(IX)
            END IF
  200    CONTINUE
         IF (NE .GT. 0) THEN
            NESSO = NESSO + 1
            NPE = NNESS(NESSO) / NEESS(NESSO)
            IDESS(NESSO) = IDESS(IESS)
            NNESS(NESSO) = NE * NPE
            IXNESS(NESSO) = IXNESS(NESSO) - (IXEESS(NESSO) - IX0) * NPE
            NEESS(NESSO) = NE
            NDESS(NESSO) = NNESS(NESSO)
            IXEESS(NESSO) = IX0
         END IF
  210 CONTINUE
      NUMESS = NESSO

      RETURN
      END
