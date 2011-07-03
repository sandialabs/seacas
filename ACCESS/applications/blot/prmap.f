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

C $Log: prmap.f,v $
C Revision 1.3  2009/03/25 12:36:46  gdsjaar
C Add copyright and license notice to all files.
C Permission to assert copyright has been granted; blot is now open source, BSD
C
C Revision 1.2  1998/03/10 16:08:35  gdsjaar
C If the model size is greater than 1,000,000 elements, dont save the element order map. Should help a little with large problems
C
C Revision 1.1  1994/04/07 20:07:44  gdsjaar
C Initial checkin of ACCESS/graphics/blotII2
C
c Revision 1.2  1990/12/14  08:55:11  gdsjaar
c Added RCS Id and Log to all files
c
C=======================================================================
      SUBROUTINE PRMAP (OPTION, NOUT, NUMEL, MAPEL)
C=======================================================================

C   --*** PRMAP *** (BLOT) Display database element order map
C   --   Written by Amy Gilkey - revised 01/21/88
C   --
C   --PRMAP displays the element order map.
C   --
C   --Parameters:
C   --   OPTION - IN - '*' to print all, else print options:
C   --   NOUT - IN - the output file, <=0 for standard
C   --   NUMEL - IN - the number of elements
C   --   MAPEL - IN - the element order map

      CHARACTER*(*) OPTION
      INTEGER MAPEL(*)

      LOGICAL ISABRT
      CHARACTER*4 FMT
      CHARACTER*6 STRA, STRB

      LOGICAL MAPONE

      IF (NOUT .GT. 0) WRITE (NOUT, 10000)

      if (numel .ge. 1000000) then
         WRITE (NOUT, 10030) 
         return
      end if

      IF (NOUT .GT. 0) THEN
         WRITE (NOUT, 10010)
      ELSE
         WRITE (*, 10010)
      END IF

      WRITE (STRA, '(I11)', IOSTAT=IDUM) NUMEL
      CALL SQZSTR (STRA, LSTRA)
      WRITE (FMT, '(''(I'', I1, '')'')', IOSTAT=IDUM) LSTRA

C ... Check for 1-1 mapping
      mapone = .TRUE.
      do i = 1, numel
        if (mapel(i) .ne. i) then
          mapone = .FALSE.
          go to 90
        end if
      end do
 90   continue
      if (mapone) then
        IF (NOUT .GT. 0) THEN
          write (nout, 10040) 'element'
        ELSE
          write (*, 10040) 'element'
        END IF
      else
      DO 100 IEL = 1, NUMEL, 8
         IF (ISABRT ()) RETURN
         NE = MIN (IEL+7, NUMEL)
         WRITE (STRA, FMT, IOSTAT=IDUM) IEL
         WRITE (STRB, FMT, IOSTAT=IDUM) NE
         IF (NOUT .GT. 0) THEN
            WRITE (NOUT, 10020, IOSTAT=IDUM)
     &         STRA(:LSTRA), STRB(:LSTRA), (MAPEL(N), N=IEL,NE)
         ELSE
            WRITE (*, 10020, IOSTAT=IDUM)
     &         STRA(:LSTRA), STRB(:LSTRA), (MAPEL(N), N=IEL,NE)
         END IF
  100 CONTINUE
      end if
      RETURN

10000  FORMAT (/, 1X, 'ELEMENT ORDER MAP')
10010  FORMAT (/, 1X, 'Element Order Map:')
10020  FORMAT (1X, 3X, A, '..', A, 3X, 8I11)
10030  FORMAT (/, 1x, 'Element order Map not saved for large models',
     $      'Use grope if you need to see the map.')
10040  format (1x, 3x, 'Map does not modify local ',
     &   A, ' ids (X maps to X)')
      END
