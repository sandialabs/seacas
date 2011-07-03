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

C $Id: rcvess.f,v 1.1 2009/03/25 04:47:54 gdsjaar Exp $
C $Log: rcvess.f,v $
C Revision 1.1  2009/03/25 04:47:54  gdsjaar
C Added blotII2 source since Copyright was asserted.
C
C Update copyright notice in suplib.
C
C Add blotII2 to config files.  Note that blot will not build yet since
C it requires some libraries that are still being reviewed for copyright
C assertion.
C
C Revision 1.1  1994/04/07 20:09:05  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:55:54  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE RCVESS (OPTION, NUMESS, LESSEL, LESSNL,
     &   IDESS, NEESS, NNESS, IXEESS, IXNESS, LTEESS, LTNESS, FACESS,
     &   INTPCK, *)
C=======================================================================

C   --*** RCVESS *** (BLOT) Receive database node sets
C   --   Written by Amy Gilkey - revised 05/09/88
C   --
C   --RCVESS receives the side set information as a message.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to receive all, else message options:
C   --      'H' to receive information about side sets
C   --      'E' to receive side set elements
C   --      'N' to receive side set nodes
C   --      'F' to receive side set factors
C   --   NUMESS - IN - the number of side sets
C   --   LESSEL - IN - the length of the side sets element list
C   --   LESSNL - IN - the length of the side sets node list
C   --   IDESS - OUT - the side set ID for each set (if OPTION)
C   --   NEESS - OUT - the number of elements for each set (if OPTION)
C   --   NNESS - OUT - the number of nodes for each set (if OPTION)
C   --   IXEESS - OUT - the index of the first element for each set (if OPTION)
C   --   IXNESS - OUT - the index of the first node for each set (if OPTION)
C   --   LTEESS - OUT - the elements for all sets (if OPTION)
C   --   LTNESS - OUT - the nodes for all sets (if OPTION)
C   --   FACESS - OUT - the distribution factors for all sets (if OPTION)
C   --   INTPCK - SCRATCH - size = 5 * NUMESS
C   --   * - return statement if message error

      CHARACTER*(*) OPTION
      INTEGER IDESS(*)
      INTEGER NEESS(*)
      INTEGER NNESS(*)
      INTEGER IXEESS(*)
      INTEGER IXNESS(*)
      INTEGER LTEESS(*)
      INTEGER LTNESS(*)
      REAL FACESS(*)
      INTEGER INTPCK(*)

      INTEGER GETIR, GETRR
      CHARACTER*8 TAGEXP, TAG
      LOGICAL ISOK

      IF (NUMESS .LE. 0) RETURN

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'H') .GT. 0)) THEN
         TAGEXP = 'SSETS'
         LEXP = 5 * NUMESS
         IERR = GETIR (TAG, INTPCK, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1

         N = 0
         CALL CPYINT (NUMESS, INTPCK(N+1), IDESS)
         N = N + NUMESS
         CALL CPYINT (NUMESS, INTPCK(N+1), NEESS)
         N = N + NUMESS
         CALL CPYINT (NUMESS, INTPCK(N+1), NNESS)
         N = N + NUMESS
         CALL CPYINT (NUMESS, INTPCK(N+1), IXEESS)
         N = N + NUMESS
         CALL CPYINT (NUMESS, INTPCK(N+1), IXNESS)
         N = N + NUMESS
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'E') .GT. 0)) THEN
         TAGEXP = 'SSETS'
         LEXP = LESSEL
         IERR = GETIR (TAG, LTEESS, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'N') .GT. 0)) THEN
         TAGEXP = 'SSETS'
         LEXP = LESSNL
         IERR = GETIR (TAG, LTNESS, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1
      END IF

      IF ((OPTION .EQ. '*') .OR. (INDEX (OPTION, 'F') .GT. 0)) THEN
         TAGEXP = 'SSETS'
         LEXP = LESSNL
         IERR = GETRR (TAG, FACESS, LEXP, L)
         CALL CHKGET (IERR, TAG, TAGEXP, L, LEXP, ISOK)
         IF (.NOT. ISOK) RETURN 1
      END IF

      RETURN
      END
