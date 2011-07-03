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
      SUBROUTINE RLGINT (INLINE, IFLD, INTYP, CFIELD, IFIELD,
     &   SELMSG, MAXVAL, ANYSEL, LOGSEL, *)
C=======================================================================

C   --*** RLGINT *** (BLOT) Parse selection command
C   --   Written by Amy Gilkey - revised 04/11/88
C   --
C   --RLGINT selects the items listed in the command.  If there are no
C   --fields, all the items are selected.  If the first field is ADD,
C   --the items are added to the items already selected, otherwise
C   --only the listed items are selected.
C   --
C   --Parameters:
C   --   INLINE - IN/OUT - the parsed input line for the log file
C   --   IFLD - IN/OUT - the number of the next entry to scan, incremented
C   --   INTYP - IN - the free-format field types
C   --      -1 = none, 0 = name, 1 = real, 2 = integer, 3 = character
C   --   CFIELD - IN - the input character fields
C   --   IFIELD - IN - the input integer fields
C   --   SELMSG - IN - the type of item for error messages
C   --   MAXVAL - IN - the maximum range value
C   --   ANYSEL - IN/OUT - true iff any items are selected
C   --   LOGSEL - OUT - LOGSEL(i) is true iff i is in the range
C   --   * - return statement if error before any items selected

      CHARACTER*(*) INLINE
      INTEGER INTYP(*)
      CHARACTER*(*) CFIELD(*)
      INTEGER IFIELD(*)
      CHARACTER*(*) SELMSG
      LOGICAL ANYSEL
      LOGICAL LOGSEL(1:*)

      LOGICAL FFEXST, FFNUMB, FFMATC
      CHARACTER*80 ERRMSG
      INTEGER IRNG(3)

      IF (.NOT. (FFEXST (IFLD, INTYP))) THEN

C      --Select all items if no fields

         ANYSEL = .TRUE.
         DO 100 I = 1, MAXVAL
            LOGSEL(I) = .TRUE.
  100    CONTINUE

      ELSE IF (FFMATC (IFLD, INTYP, CFIELD, 'OFF', 3)) THEN

C      --Select no items if OFF

         CALL FFADDC ('OFF', INLINE)
         ANYSEL = .FALSE.
         DO 110 I = 1, MAXVAL
            LOGSEL(I) = .FALSE.
  110    CONTINUE

      ELSE

C      --Reset to none selected unless ADD

         IF (FFMATC (IFLD, INTYP, CFIELD, 'ADD', 3)) THEN
            CALL FFADDC ('ADD', INLINE)
         ELSE
            IF (.NOT. FFNUMB (IFLD, INTYP)) THEN
               ERRMSG =
     &            'Expected "OFF" or "ADD" or ' // SELMSG // ' range'
               CALL PRTERR ('CMDERR', ERRMSG(:LENSTR(ERRMSG)))
               GOTO 160
            END IF
            ANYSEL = .FALSE.
            DO 120 I = 1, MAXVAL
               LOGSEL(I) = .FALSE.
  120       CONTINUE
         END IF

  130    CONTINUE
         IF (FFEXST (IFLD, INTYP)) THEN

C         --Scan numeric range

            CALL FFVRNG (IFLD, INTYP, CFIELD, IFIELD,
     &         SELMSG, MAXVAL, IRNG, *150)
            CALL FFADDV (IRNG, INLINE)

            ANYSEL = .TRUE.
            DO 140 I = IRNG(1), IRNG(2), IRNG(3)
               LOGSEL(I) = .TRUE.
  140       CONTINUE

            GOTO 130
         END IF

  150    CONTINUE
         IF (.NOT. ANYSEL) THEN
            ERRMSG = 'No ' // SELMSG // 's are selected'
            CALL PRTERR ('CMDWARN', ERRMSG(:LENSTR(ERRMSG)))
         END IF
      END IF

      RETURN

  160 CONTINUE
      RETURN 1
      END
