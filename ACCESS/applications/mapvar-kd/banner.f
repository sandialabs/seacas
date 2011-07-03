C Copyright (c) 2007 Sandia Corporation. Under the terms of Contract
C DE-AC04-94AL85000 with Sandia Corporation, the U.S. Governement
C retains certain rights in this software.
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
C 
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
C 

C $Id: banner.f,v 1.1 2008/10/31 05:04:13 gdsjaar Exp $ 
C=======================================================================
*DECK,BANNER
      SUBROUTINE BANNER (NCOLS, LINEIN, IOUT)
C
C     ******************************************************************
C
C     SUBROUTINE TO PRINT A HEADING BANNER ON THE OUTPUT FILE
C
C     Called by MAPVAR, RDINPT
C
C     ******************************************************************
C
      IMPLICIT INTEGER(A-Z)
      PARAMETER (MAXCHR = 14)
C
      CHARACTER*8 LETTER(7,49), SECT(7,MAXCHR)
      CHARACTER*49 MATRIX
      CHARACTER*(*) LINEIN
      CHARACTER*(MAXCHR) LINE
      CHARACTER BLANK*66
C
      SAVE MATRIX, LETTER, BLANK
C
      DATA BLANK /' '/
      DATA MATRIX(1:36)  /'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'/
      DATA MATRIX(37:48) /'$()*+,/.: -='/
      DATA MATRIX(49:49) /''''/
C
      DATA (LETTER(I, 1),I=1,7) /                                       
     1 ' AAAAA  ',                                                      
     2 'AA   AA ',                                                      
     3 'AA   AA ',                                                      
     4 'AAAAAAA ',                                                      
     5 'AA   AA ',                                                      
     6 'AA   AA ',                                                      
     7 'AA   AA '/
      DATA (LETTER(I, 2),I=1,7) /                                       
     1 'BBBBBB  ',                                                      
     2 'BB   BB ',                                                      
     3 'BB   BB ',                                                      
     4 'BBBBBB  ',                                                      
     5 'BB   BB ',                                                      
     6 'BB   BB ',                                                      
     7 'BBBBBB  '/
      DATA (LETTER(I, 3),I=1,7) /                                       
     1 ' CCCCC  ',                                                      
     2 'CC   CC ',                                                      
     3 'CC      ',                                                      
     4 'CC      ',                                                      
     5 'CC      ',                                                      
     6 'CC   CC ',                                                      
     7 ' CCCCC  '/
      DATA (LETTER(I, 4),I=1,7) /                                       
     1 'DDDDDD  ',                                                      
     2 'DD   DD ',                                                      
     3 'DD   DD ',                                                      
     4 'DD   DD ',                                                      
     5 'DD   DD ',                                                      
     6 'DD   DD ',                                                      
     7 'DDDDDD  '/
      DATA (LETTER(I, 5),I=1,7) /                                       
     1 'EEEEEEE ',                                                      
     2 'EE      ',                                                      
     3 'EE      ',                                                      
     4 'EEEEE   ',                                                      
     5 'EE      ',                                                      
     6 'EE      ',                                                      
     7 'EEEEEEE '/
      DATA (LETTER(I, 6),I=1,7) /                                       
     1 'FFFFFFF ',                                                      
     2 'FF      ',                                                      
     3 'FF      ',                                                      
     4 'FFFFF   ',                                                      
     5 'FF      ',                                                      
     6 'FF      ',                                                      
     7 'FF      '/
      DATA (LETTER(I, 7),I=1,7) /                                       
     1 ' GGGGG  ',                                                      
     2 'GG   GG ',                                                      
     3 'GG      ',                                                      
     4 'GG      ',                                                      
     5 'GG  GGG ',                                                      
     6 'GG   GG ',                                                      
     7 ' GGGGG  '/
      DATA (LETTER(I, 8),I=1,7) /                                       
     1 'HH   HH ',                                                      
     2 'HH   HH ',                                                      
     3 'HH   HH ',                                                      
     4 'HHHHHHH ',                                                      
     5 'HH   HH ',                                                      
     6 'HH   HH ',                                                      
     7 'HH   HH '/
      DATA (LETTER(I, 9),I=1,7) /                                       
     1 '  IIII  ',                                                      
     2 '   II   ',                                                      
     3 '   II   ',                                                      
     4 '   II   ',                                                      
     5 '   II   ',                                                      
     6 '   II   ',                                                      
     7 '  IIII  '/
      DATA (LETTER(I,10),I=1,7) /                                       
     1 '     JJ ',                                                      
     2 '     JJ ',                                                      
     3 '     JJ ',                                                      
     4 '     JJ ',                                                      
     5 '     JJ ',                                                      
     6 'JJ   JJ ',                                                      
     7 ' JJJJJ  '/
      DATA (LETTER(I,11),I=1,7) /                                       
     1 'KK   KK ',                                                      
     2 'KK  KK  ',                                                      
     3 'KK KK   ',                                                      
     4 'KKKK    ',                                                      
     5 'KKKKK   ',                                                      
     6 'KK  KK  ',                                                      
     7 'KK   KK '/
      DATA (LETTER(I,12),I=1,7) /                                       
     1 'LL      ',                                                      
     2 'LL      ',                                                      
     3 'LL      ',                                                      
     4 'LL      ',                                                      
     5 'LL      ',                                                      
     6 'LL      ',                                                      
     7 'LLLLLLL '/
      DATA (LETTER(I,13),I=1,7) /                                       
     1 'M     M ',                                                      
     2 'MM   MM ',                                                      
     3 'MMM MMM ',                                                      
     4 'MM M MM ',                                                      
     5 'MM   MM ',                                                      
     6 'MM   MM ',                                                      
     7 'MM   MM '/
      DATA (LETTER(I,14),I=1,7) /                                       
     1 'N    NN ',                                                      
     2 'NN   NN ',                                                      
     3 'NNN  NN ',                                                      
     4 'NN N NN ',                                                      
     5 'NN  NNN ',                                                      
     6 'NN   NN ',                                                      
     7 'NN    N '/
      DATA (LETTER(I,15),I=1,7) /                                       
     1 ' OOOOO  ',                                                      
     2 'OO   OO ',                                                      
     3 'OO   OO ',                                                      
     4 'OO   OO ',                                                      
     5 'OO   OO ',                                                      
     6 'OO   OO ',                                                      
     7 ' OOOOO  '/
      DATA (LETTER(I,16),I=1,7) /                                       
     1 'PPPPPP  ',                                                      
     2 'PP   PP ',                                                      
     3 'PP   PP ',                                                      
     4 'PPPPPP  ',                                                      
     5 'PP      ',                                                      
     6 'PP      ',                                                      
     7 'PP      '/
      DATA (LETTER(I,17),I=1,7) /                                       
     1 ' QQQQQ  ',                                                      
     2 'QQ   QQ ',                                                      
     3 'QQ   QQ ',                                                      
     4 'QQ   QQ ',                                                      
     5 'QQ Q QQ ',                                                      
     6 'QQ  QQQ ',                                                      
     7 ' QQQQQQ '/
      DATA (LETTER(I,18),I=1,7) /                                       
     1 'RRRRRR  ',                                                      
     2 'RR   RR ',                                                      
     3 'RR   RR ',                                                      
     4 'RRRRRR  ',                                                      
     5 'RRRRR   ',                                                      
     6 'RR  RR  ',                                                      
     7 'RR   RR '/
      DATA (LETTER(I,19),I=1,7) /                                       
     1 ' SSSSSS ',                                                      
     2 'SS      ',                                                      
     3 'SS      ',                                                      
     4 ' SSSSS  ',                                                      
     5 '     SS ',                                                      
     6 '     SS ',                                                      
     7 'SSSSSS  '/
      DATA (LETTER(I,20),I=1,7) /                                       
     1 'TTTTTT  ',                                                      
     2 '  TT    ',                                                      
     3 '  TT    ',                                                      
     4 '  TT    ',                                                      
     5 '  TT    ',                                                      
     6 '  TT    ',                                                      
     7 '  TT    '/
      DATA (LETTER(I,21),I=1,7) /                                       
     1 'UU   UU ',                                                      
     2 'UU   UU ',                                                      
     3 'UU   UU ',                                                      
     4 'UU   UU ',                                                      
     5 'UU   UU ',                                                      
     6 'UU   UU ',                                                      
     7 ' UUUUU  '/
      DATA (LETTER(I,22),I=1,7) /                                       
     1 'V      V',                                                      
     2 ' V    V ',                                                      
     3 ' VV  VV ',                                                      
     4 '  V  V  ',                                                      
     5 '  VVVV  ',                                                      
     6 '   VV   ',                                                      
     7 '   VV   '/
      DATA (LETTER(I,23),I=1,7) /                                       
     1 'WW   WW ',                                                      
     2 'WW   WW ',                                                      
     3 'WW   WW ',                                                      
     4 'WW   WW ',                                                      
     5 'WW W WW ',                                                      
     6 'WWWWWWW ',                                                      
     7 ' WW WW  '/
      DATA (LETTER(I,24),I=1,7) /                                       
     1 'XX   XX ',                                                      
     2 ' XX XX  ',                                                      
     3 '  XXX   ',                                                      
     4 '  XXX   ',                                                      
     5 '  XXX   ',                                                      
     6 ' XX XX  ',                                                      
     7 'XX   XX '/
      DATA (LETTER(I,25),I=1,7) /                                       
     1 'YY   YY ',                                                      
     2 ' YY YY  ',                                                      
     3 '  YYY   ',                                                      
     4 '   YY   ',                                                      
     5 '   YY   ',                                                      
     6 '   YY   ',                                                      
     7 '   YY   '/
      DATA (LETTER(I,26),I=1,7) /                                       
     1 'ZZZZZZZ ',                                                      
     2 '     Z  ',                                                      
     3 '    Z   ',                                                      
     4 '   Z    ',                                                      
     5 '  Z     ',                                                      
     6 ' Z      ',                                                      
     7 'ZZZZZZZ '/
      DATA (LETTER(I,27),I=1,7) /                                       
     1 ' 000000 ',                                                      
     2 '0    00 ',                                                      
     3 '0   0 0 ',                                                      
     4 '0  0  0 ',                                                      
     5 '0 0   0 ',                                                      
     6 '00    0 ',                                                      
     7 '000000  '/
      DATA (LETTER(I,28),I=1,7) /                                       
     1 '   1    ',                                                      
     2 '  11    ',                                                      
     3 ' 1 1    ',                                                      
     4 '   1    ',                                                      
     5 '   1    ',                                                      
     6 '   1    ',                                                      
     7 ' 11111  '/
      DATA (LETTER(I,29),I=1,7) /                                       
     1 '  2222  ',                                                      
     2 ' 2    2 ',                                                      
     3 '     2  ',                                                      
     4 '    2   ',                                                      
     5 '   2    ',                                                      
     6 '  2     ',                                                      
     7 ' 222222 '/
      DATA (LETTER(I,30),I=1,7) /                                       
     1 ' 33333  ',                                                      
     2 '3     3 ',                                                      
     3 '      3 ',                                                      
     4 '    33  ',                                                      
     5 '      3 ',                                                      
     6 '3     3 ',                                                      
     7 ' 33333  '/
      DATA (LETTER(I,31),I=1,7) /                                       
     1 '    44  ',                                                      
     2 '  4  4  ',                                                      
     3 ' 4   4  ',                                                      
     4 '444444  ',                                                      
     5 '     4  ',                                                      
     6 '     4  ',                                                      
     7 '     4  '/
      DATA (LETTER(I,32),I=1,7) /                                       
     1 '555555  ',                                                      
     2 '5       ',                                                      
     3 '5       ',                                                      
     4 '55555   ',                                                      
     5 '     5  ',                                                      
     6 '     5  ',                                                      
     7 '55555   '/
      DATA (LETTER(I,33),I=1,7) /                                       
     1 ' 6666   ',                                                      
     2 '6       ',                                                      
     3 '6       ',                                                      
     4 '66666   ',                                                      
     5 '6    6  ',                                                      
     6 '6    6  ',                                                      
     7 ' 6666   '/
      DATA (LETTER(I,34),I=1,7) /                                       
     1 '7777777 ',                                                      
     2 '     7  ',                                                      
     3 '    7   ',                                                      
     4 '   7    ',                                                      
     5 '  7     ',                                                      
     6 ' 7      ',                                                      
     7 '7       '/
      DATA (LETTER(I,35),I=1,7) /                                       
     1 '  8888  ',                                                      
     2 ' 8    8 ',                                                      
     3 ' 8    8 ',                                                      
     4 '  8888  ',                                                      
     5 ' 8    8 ',                                                      
     6 ' 8    8 ',                                                      
     7 '  8888  '/
      DATA (LETTER(I,36),I=1,7) /                                       
     1 '  9999  ',                                                      
     2 ' 9    9 ',                                                      
     3 ' 9    9 ',                                                      
     4 '  99999 ',                                                      
     5 '      9 ',                                                      
     6 '      9 ',                                                      
     7 '  9999  '/
      DATA (LETTER(I,37),I=1,7) /                                       
     1 '   $    ',                                                      
     2 ' $$$$$$ ',                                                      
     3 '$  $    ',                                                      
     4 ' $$$$$  ',                                                      
     5 '   $  $ ',                                                      
     6 '$$$$$$  ',                                                      
     7 '   $    '/
      DATA (LETTER(I,38),I=1,7) /                                       
     1 '     (  ',                                                      
     2 '    (   ',                                                      
     3 '   (    ',                                                      
     4 '   (    ',                                                      
     5 '   (    ',                                                      
     6 '    (   ',                                                      
     7 '     (  '/
      DATA (LETTER(I,39),I=1,7) /                                       
     1 ' )      ',                                                      
     2 '  )     ',                                                      
     3 '   )    ',                                                      
     4 '   )    ',                                                      
     5 '   )    ',                                                      
     6 '  )     ',                                                      
     7 ' )      '/
      DATA (LETTER(I,40),I=1,7) /                                       
     1 '*     * ',                                                      
     2 ' *   *  ',                                                      
     3 '  * *   ',                                                      
     4 '******* ',                                                      
     5 '  * *   ',                                                      
     6 ' *   *  ',                                                      
     7 '*     * '/
      DATA (LETTER(I,41),I=1,7) /                                       
     1 '   +    ',                                                      
     2 '   +    ',                                                      
     3 '   +    ',                                                      
     4 '+++++++ ',                                                      
     5 '   +    ',                                                      
     6 '   +    ',                                                      
     7 '   +    '/
      DATA (LETTER(I,42),I=1,7) /                                       
     1 '        ',                                                      
     2 '        ',                                                      
     3 '        ',                                                      
     4 '        ',                                                      
     5 '  ,,    ',                                                      
     6 '   ,    ',                                                      
     7 '  ,     '/
      DATA (LETTER(I,43),I=1,7) /                                       
     1 '      / ',                                                      
     2 '     /  ',                                                      
     3 '    /   ',                                                      
     4 '   /    ',                                                      
     5 '  /     ',                                                      
     6 ' /      ',                                                      
     7 '/       '/
      DATA (LETTER(I,44),I=1,7) /                                       
     1 '        ',                                                      
     2 '        ',                                                      
     3 '        ',                                                      
     4 '        ',                                                      
     5 '        ',                                                      
     6 '  ..    ',                                                      
     7 '  ..    '/
      DATA (LETTER(I,45),I=1,7) /                                       
     1 '        ',                                                      
     2 '  ::    ',                                                      
     3 '  ::    ',                                                      
     4 '        ',                                                      
     5 '  ::    ',                                                      
     6 '  ::    ',                                                      
     7 '        '/
      DATA (LETTER(I,46),I=1,7) /                                       
     1 '        ',                                                      
     2 '        ',                                                      
     3 '        ',                                                      
     4 '        ',                                                      
     5 '        ',                                                      
     6 '        ',                                                      
     7 '        '/
      DATA (LETTER(I,47),I=1,7) /                                       
     1 '        ',                                                      
     2 '        ',                                                      
     3 '        ',                                                      
     4 '------- ',                                                      
     5 '        ',                                                      
     6 '        ',                                                      
     7 '        '/
      DATA (LETTER(I,48),I=1,7) /                                       
     1 '        ',                                                      
     2 '        ',                                                      
     3 '======= ',                                                      
     4 '        ',                                                      
     5 '======= ',                                                      
     6 '        ',                                                      
     7 '        '/
      DATA (LETTER(I,49),I=1,7) /                                       
     1 '  ''''    ',                                                    
     2 '   ''    ',                                                     
     3 '  ''     ',                                                     
     4 '        ',                                                      
     5 '        ',                                                      
     6 '        ',                                                      
     7 '        '/
C
      MAXCOL = MIN(NCOLS/9, MAXCHR)
C
C     ******************************************************************
C
C     DELIMIT NONBLANK STRING
C
      CALL STRIPB (LINEIN, ILEFT, IRIGHT)
      IF (ILEFT .LE. IRIGHT) THEN
      LINE = LINEIN (ILEFT:IRIGHT)
      LENIN = IRIGHT - ILEFT + 1
      IF (LENIN .GT. MAXCOL) THEN
      LENIN = MAXCOL
      CALL STRIPB (LINE(:LENIN), J, LENIN)
      END IF
       ELSE
      LINE = ' '
      LENIN = 0
      END IF
C
C     LENIN IS LAST PRINTABLE NONBLANK
C
C     CONVERT ALPHABET TO UPPER CASE
C
      DO 10 J=1,LENIN
      IF (LGE(LINE(J:J),'a') .AND. LLE(LINE(J:J),'z')) THEN
      ITEMP=ICHAR(LINE(J:J))
      LINE(J:J)=CHAR(ITEMP-(ICHAR('a')-ICHAR('A')))
      END IF
   10 CONTINUE
C
C     CALCULATE BLANK FILL.
C
      NBLANK = (NCOLS - LENIN * 9)/ 2
      NBLANK = MIN (NBLANK, 66)
C
C     LOAD UP CHARACTERS
C
      DO 40 ICOL = 1, LENIN
      IPT = INDEX(MATRIX,LINE(ICOL:ICOL))
      IF (IPT .EQ. 0) THEN
C
C     CHARACTER NOT FOUND - REPLACE WITH A BLANK
C
      DO 20 IROW = 1, 7
      SECT(IROW,ICOL)= ' '
   20 CONTINUE
C
       ELSE
C
C     CHARACTER FOUND - INSERT BANNER LETTER
C
      DO 30 IROW = 1, 7
      SECT(IROW,ICOL)= LETTER(IROW,IPT)
   30 CONTINUE
      END IF
   40 CONTINUE
      IF ((IRIGHT - ILEFT + 1) .NE. LENIN .AND. LENIN .NE. 0) THEN
C
C     STRING IS TRUNCATED.
C
      WRITE (IOUT, 70) LINEIN(ILEFT:IRIGHT)
C
       ELSE
C
C     STRING IS NOT TRUNCATED OR IS NULL.
C
      WRITE (IOUT, 60)
C
      END IF
      DO 50 IROW = 1, 7
      WRITE (IOUT, 80) BLANK(:NBLANK),(SECT(IROW,J), J = 1, LENIN)
   50 CONTINUE
      WRITE (IOUT, 60)
C
      RETURN
C
   60 FORMAT ()
   70 FORMAT (' WARNING, TRUNCATED BANNER STRING: ',A)
   80 FORMAT (25(:,1X,A))
      END
