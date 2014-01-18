C................................................................GRIDSZ
      SUBROUTINE GRIDSZ(IOCODE,II,JJ,KK,NX,NY,NZ, ALPHA, TOP)

      INCLUDE 'PARAMS.FOR'

C      READ GRID DESCRIPTION
      REAL KX,KY,KZ

      COMMON /TSTDAT/ IFATAL,IWARN

      COMMON /SPARM/ KX(LP1,LP2,LP3),KY(LP1,LP2,LP3),KZ(LP1,LP2,LP3),
     & EL(LP1,LP2,LP3),TX(LP4,LP2,LP3),TY(LP1,LP5,LP3),TZ(LP1,LP2,LP6),
     & PDAT(LP1,LP2,LP3),PDATUM,GRAD

      COMMON /VECTOR/ DX(LP1,LP2,LP3),DY(LP1,LP2,LP3),DZ(LP1,LP2,LP3),
     & DZNET(LP1,LP2,LP3),IQN1(LP11),IQN2(LP11),IQN3(LP11),IHEDIN(80)

      DIMENSION SUM(LP1,LP2),VAREL(LP1,LP2),RDXL(LP1),RDYL(LP2),
     & RDZL(LP3),RKEL(LP3),RDZNET(LP3)

C============== Arrays for new dipping reservior options ================
      DIMENSION X(LP1),DZZ(LP1),ZT(LP3)
C========================================================================
     
c............. Added 07/02/93 ............................
      DIMENSION TOP(LP1,LP2,LP3)
c.........................................................

      READ(20,69)
      WRITE(IOCODE,70)
   70 FORMAT(//T15,'***** INITIALIZATION DATA *****',//)
      READ(20,*) II,JJ,KK

C.... Grid dimension limit check
      IF(II.LE.NX) GO TO 1520
      IFATAL=IFATAL+1
      WRITE(IOCODE,1510) NX
 1510 FORMAT(/5X,5('-'),'No. of x-direction blocks cannot',
     & ' exceed ',I5)
 1520 IF(JJ.LE.NY) GO TO 1540
      IFATAL=IFATAL+1
      WRITE(IOCODE,1530) NY
 1530  FORMAT(/5X,5('-'),'No. of y-direction blocks cannot',
     & ' exceed ',I5)
 1540 IF(KK.LE.NZ) GO TO 1560
      IFATAL=IFATAL+1
      WRITE(IOCODE,1550) NZ
 1550 FORMAT(/5X,5('-'),'No. of z-direction blocks cannot',
     & ' exceed ',I5)
 1560 CONTINUE

      READ(20,69)
C*****READ INPUT CODES FOR DX,DY,DZ
      READ(20,*)KDX,KDY,KDZ,KDZNET
C*****ESTABLISH GRID BLOCK LENGTH (DX) DISTRIBUTION
      IF(KDX.GE.0)GO TO 180
      READ(20,*)DXC
      DO 175 K=1,KK
      DO 175 J=1,JJ
      DO 175 I=1,II
175   DX(I,J,K)=DXC
      WRITE(IOCODE,56)
      WRITE(IOCODE,29)DXC
      GO TO 195
  180 IF(KDX.GT.0)GO TO 185
      READ(20,*)(RDXL(I),I=1,II)
      DO 187 K=1,KK
      DO 187 J=1,JJ
      DO 187 I=1,II
  187 DX(I,J,K)=RDXL(I)
      DO 182 I=1,II
  182 WRITE(IOCODE,511)I,RDXL(I)
      GO TO 195
185   WRITE(IOCODE,43)
      K=1
      WRITE(IOCODE,38)K
      DO 190 J=1,JJ
      READ(20,*)(DX(I,J,K),I=1,II)
  190 WRITE(IOCODE,72)(DX(I,J,K),I=1,II)
      DO 194 K=2,KK
      WRITE(IOCODE,38) K
      DO 194 J=1,JJ
      DO 193 I=1,II
  193 DX(I,J,K)=DX(I,J,1)
  194 WRITE(IOCODE,72) (DX(I,J,K),I=1,II)
195   CONTINUE
      WRITE(IOCODE,56)

C*****ESTABLISH GRID BLOCK LENGTH (DY) DISTRIBUTION
      IF(KDY.GE.0)GO TO 200
      READ(20,*)DYC
      DO 202 K=1,KK
      DO 202 J=1,JJ
      DO 202 I=1,II
202   DY(I,J,K)=DYC
      WRITE(IOCODE,56)
      WRITE(IOCODE,33)DYC
      GO TO 220
  200 IF(KDY.GT.0)GO TO 207
      READ(20,*)(RDYL(J),J=1,JJ)
      DO 205 K=1,KK
      DO 205 J=1,JJ
      DO 205 I=1,II
  205 DY(I,J,K)=RDYL(J)
      DO 210 J=1,JJ
  210 WRITE(IOCODE,512)J,RDYL(J)
      GO TO 220
207   WRITE(IOCODE,47)
      K=1
      WRITE(IOCODE,38)K
      DO 215 J=1,JJ
      READ(20,*)(DY(I,J,K),I=1,II)
  215 WRITE(IOCODE,72)(DY(I,J,K),I=1,II)
      DO 214 K=2,KK
       WRITE(IOCODE,38) K
      DO 214 J=1,JJ
      DO 213 I=1,II
  213 DY(I,J,K)=DY(I,J,1)
  214 WRITE(IOCODE,72) (DY(I,J,K),I=1,II)
220   CONTINUE
      WRITE(IOCODE,56)




C*****ESTABLISH GRID BLOCK LENGTH (DZ) DISTRIBUTION
      IF(KDZ.GE.0)GO TO 225
      READ(20,*)DZC
      DO 230 K=1,KK
      DO 230 J=1,JJ
      DO 230 I=1,II
230   DZ(I,J,K)=DZC
      WRITE(IOCODE,56)
      WRITE(IOCODE,36)DZC
      GO TO 245
  225 IF(KDZ.GT.0)GO TO 232
      READ(20,*)(RDZL(K),K=1,KK)
      DO 235 K=1,KK
      DO 235 J=1,JJ
      DO 235 I=1,II
  235 DZ(I,J,K)=RDZL(K)
      DO 237 K=1,KK
  237 WRITE(IOCODE,513)K,RDZL(K)
      GO TO 245
232   WRITE(IOCODE,48)
      DO 240 K=1,KK
      WRITE(IOCODE,38)K
      DO 242 J=1,JJ
      READ(20,*)(DZ(I,J,K),I=1,II)
  242 WRITE(IOCODE,72)(DZ(I,J,K),I=1,II)
240   CONTINUE
245   CONTINUE
      WRITE(IOCODE,56)
      IF(KDZNET.GE.0) GO TO 255
      READ(20,*) DZNETC
      DO 250 K=1,KK
      DO 250 J=1,JJ
      DO 250 I=1,II
  250 DZNET(I,J,K)=DZNETC
      WRITE(IOCODE,56)
      WRITE(IOCODE,252) DZNETC
  252 FORMAT(T5,'GRID BLOCK NET THICKNESS (DZNET) IS INITIALLY',
     & ' SET AT',F10.4,' FOR ALL NODES',//)
      GO TO 274
  255 IF(KDZNET.GT.0) GO TO 268
      READ(20,*) (RDZNET(K),K=1,KK)
      DO 260 K=1,KK
      DO 260 J=1,JJ
      DO 260 I=1,II
  260 DZNET(I,J,K)=RDZNET(K)
      DO 266 K=1,KK
  266 WRITE(IOCODE,267) K,RDZNET(K)
  267 FORMAT(T5,'GRID SIZE (DZNET) IN LAYER ',I5,
     & ' IS INITIALLY SET AT',F8.2,' FOR ALL NODES',/)
      GO TO 274
  268 WRITE(IOCODE,269)
  269 FORMAT(//T5,8('*'),'GRID BLOCK NET THICKNESS ',
     & '(DZNET) DISTRIBUTION',8('*')/)
      DO 273 K=1,KK
       WRITE(IOCODE,38) K
      DO 270 J=1,JJ
      READ(20,*) (DZNET(I,J,K),I=1,II)
  270 WRITE(IOCODE,72) (DZNET(I,J,K),I=1,II)
  273 CONTINUE
  274 CONTINUE
      WRITE(IOCODE,56)
C********GRID BLOCK LENGTH MODIFICATIONS
      READ(20,69)
      READ(20,*) NUMDX,NUMDY,NUMDZ,NUMDZN,IDCODE
      IF(NUMDX.EQ.0) GO TO 8531
      WRITE(IOCODE,31)
      DO 275 L=1,NUMDX
      READ(20,*) I1,I2,J1,J2,K1,K2,REGVAL
      WRITE(IOCODE,32) I1,I2,J1,J2,K1,K2,REGVAL
      DO 275 K=K1,K2
      DO 275 J=J1,J2
      DO 275 I=I1,I2
      DX(I,J,K)=REGVAL
  275 CONTINUE
      IF(IDCODE.NE.1)GO TO 8531
      WRITE(IOCODE,43)
      DO 853 K=1,KK
       WRITE(IOCODE,38)K
      DO 854 J=1,JJ
  854 WRITE(IOCODE,72)(DX(I,J,K),I=1,II)
853   CONTINUE
8531  CONTINUE
      IF(NUMDY.EQ.0) GO TO 8551
      WRITE(IOCODE,34)
      DO 276 L=1,NUMDY
      READ(20,*) I1,I2,J1,J2,K1,K2,REGVAL
      WRITE(IOCODE,32) I1,I2,J1,J2,K1,K2,REGVAL
      DO 276 K=K1,K2
      DO 276 J=J1,J2
      DO 276 I=I1,I2
      DY(I,J,K)=REGVAL
  276 CONTINUE
      IF(IDCODE.NE.1) GO TO 8551
      WRITE(IOCODE,47)
      DO 855 K=1,KK
       WRITE(IOCODE,38)K
      DO 856 J=1,JJ
  856 WRITE(IOCODE,72)(DY(I,J,K),I=1,II)
855   CONTINUE
8551  CONTINUE
      IF(NUMDZ.EQ.0) GO TO 8571
      WRITE(IOCODE,37)
      DO 277 L=1,NUMDZ
      READ(20,*) I1,I2,J1,J2,K1,K2,REGVAL
      WRITE(IOCODE,32) I1,I2,J1,J2,K1,K2,REGVAL
      DO 277 K=K1,K2
      DO 277 J=J1,J2
      DO 277 I=I1,I2
      DZ(I,J,K)=REGVAL
  277 CONTINUE
      IF(IDCODE.NE.1) GO TO 8571
      WRITE(IOCODE,48)
      DO 857 K=1,KK
       WRITE(IOCODE,38)K
      DO 858 J=1,JJ
  858 WRITE(IOCODE,72)(DZ(I,J,K),I=1,II)
857   CONTINUE
8571  CONTINUE
      IF(NUMDZN.EQ.0) GO TO 890
      WRITE(IOCODE,860)
  860 FORMAT(//T5,8('*'),'GRID BLOCK NET THICKNESS',
     & ' (DZNET) NODE MODIFICATIONS',8('*'),//T5,
     & '   I1  I2  J1  J2  K1  K2  NEW DZNET VALUE')
      DO 865 L=1,NUMDZN
      READ(20,*) I1,I2,J1,J2,K1,K2,REGVAL
      WRITE(IOCODE,32) I1,I2,J1,J2,K1,K2,REGVAL
      DO 865 K=K1,K2
      DO 865 J=J1,J2
      DO 865 I=I1,I2
      DZNET(I,J,K)=REGVAL
  865 CONTINUE
      IF(IDCODE.NE.1) GO TO 890
      WRITE(IOCODE,269)
      DO 885 K=1,KK
       WRITE(IOCODE,38) K
      DO 880 J=1,JJ
  880 WRITE(IOCODE,72) (DZNET(I,J,K),I=1,II)
  885 CONTINUE
  890 CONTINUE
C      GRID BLOCK SIZE CHECK
      DO 900 K=1,KK
      DO 900 J=1,JJ
      DO 900 I=1,II
      IF(DX(I,J,K).GE.0.0) GO TO 892
      IFATAL=IFATAL+1
      WRITE(IOCODE,891) I,J,K
  891 FORMAT(/,5X,5('-'),'GRID BLOCK DX ERROR AT IJK =',3I5)
  892 IF(DY(I,J,K).GE.0.0) GO TO 894
      IFATAL=IFATAL+1
      WRITE(IOCODE,893) I,J,K
  893 FORMAT(/,5X,5('-'),'GRID BLOCK DY ERROR AT IJK =',3I5)
  894 IF(DZ(I,J,K).GE.0.0) GO TO 896
      IFATAL=IFATAL+1
      WRITE(IOCODE,895) I,J,K
  895 FORMAT(/,5X,5('-'),'GRID BLOCK DZ ERROR AT IJK =',3I5)
  896 IF(DZNET(I,J,K).GE.0.0) GO TO 900
      IFATAL=IFATAL+1
      WRITE(IOCODE,897) I,J,K
  897 FORMAT(/,5X,5('-'),'GRID BLOCK DZNET ERROR AT IJK =',3I5)
  900 CONTINUE



C.... Establish node mid-point elevations...............

C.... Read elevation code and dip angle ......................
C.... KEL < 0  ===> constant dip (alpha); otherwise alpha not used !!
C.... KEL = -1 ===> contigous layers
C.... KEL = -2 ===> non-contiguous layers

      READ(20,69)
      READ(20,*) KEL, ALPHA
      IF(KEL.GE.0) GO TO 902

c================== New dipping reservoir options =======================
C:::: NOTE: SIND, COSD, TAND and ATAND are NDP & VAX VMS Fortran extensions which
C::::       allows the argument to be in degrees. 
 
      ANG    = ABS(ALPHA)
ccc      SINALP = SIND(ANG)
ccc      COSALP = COSD(ANG)
ccc      TANALP = TAND(ANG)
      ANG = ANG * 3.14159/180.
      SINALP = SIN(ANG)
      COSALP = COS(ANG)
      TANALP = TAN(ANG)

      IF(KEL.EQ.-1) READ(20,*) ELEV
      IF(KEL.EQ.-2) READ(20,*) (ZT(K),K=1,KK)

      IF(KEL.EQ.-1) THEN

         ZT(1)  = ELEV
         DO 1000 K=2,KK
         ZT(K) = ZT(K-1) + DZ(1,1,K-1)*COSALP 
1000     CONTINUE
      ENDIF

C.... NOTE:  IF(KEL.EQ.-2) for non-contiguous layers, ZT's are read in ......

      WRITE(IOCODE,997) ALPHA
997   FORMAT(///1X,'Dipping reservoir option:  DIP = ',F7.3,'deg'/)

      DO 1010 K=1,KK
      WRITE(IOCODE,1001) K,ZT(K)
1010  CONTINUE
1001  FORMAT(/1X,'Layer =',I3,3X,'Distance to top left corner =',F10.3)

      X(1)  = 0.5*DX(1,1,1)
      DZZ(1) = 0.0
      DO 1012 I=2,II
      X(I)   = X(I-1) + 0.5*(DX(I-1,1,1)+DX(I,1,1))
      DZZ(I) = SINALP * (X(I) - X(1))
CCC      WRITE(IOCODE,1011) I,X(I),X(1),SINALP,DZZ(I)
1012  CONTINUE

CCC1011  FORMAT(1X,'I,X(I),X(1),SINALP,DZZ(I):',I3,4F8.3)

C---- Now must break into two cases:  alpha > 0  (dipping down in x-dir)
C                                     alpha < 0  (sloping up in x-dir)
      IF(ALPHA.LE.0.0) GO TO 1500

C========= alpha > 0   dipping down in positive x-direction ============
      ZS = X(1) * TANALP
CCC      WRITE(IOCODE,1201) X(1),ZS,TANALP
CCC1201  FORMAT(/1X,'X(1),ZS,TANALP:',I3,3G12.5)

      DO 1400 K=1,KK
      SLANT = ZS + 0.5*DZ(1,1,K)
      DELK =   SLANT * COSALP
      EL(1,1,K) = ZT(K) + DELK
      DO 1390 J=1,JJ
1390  EL(1,J,K) = EL(1,1,K)
CCC      WRITE(IOCODE,1401) K,SLANT,DELK,ZT(K),EL(1,1,K)
1400  CONTINUE

CCC1401  FORMAT(1X,'K,SLANT,DELK,ZT(K),EL(1,1,K):',I3,4G12.5)

CC      DO 1450 I=2,II    (ch'd like in MISC4 - 07/02/93)
      DO 1450 I=1,II
      DO 1450 J=1,JJ
      DO 1450 K=1,KK
      EL(I,J,K) = EL(1,J,K) + DZZ(I)
C.... NOTE:  Top is approximate in this case - it is precisely the top of
C....        the block if the block were rotated to be horizontal - which
C....        is the way colorgrid is going to draw it anyway !
      TOP(I,J,K) = EL(I,J,K) - 0.5 * DZ(I,J,K)
1450  CONTINUE


      GO TO 2000

1500  CONTINUE
C========= alpha < 0   "dipping" upward in positive x-direction ==========
      ALP = -ALPHA
      B   = X(1)
      BSQ = B*B 
      DO 1600 K=1,KK
      A    = 0.5*DZ(1,1,K)
      ARG  = A/B
ccc      BETA = ATAND(ARG)
      BETA = ATAN(ARG)
      beta  = beta * 180./3.14159
      GAMMA = ALP - BETA
      ASQ = A*A 
      C   = SQRT(ASQ+BSQ)
ccc      DELK = C * SIND(GAMMA)
      gamma = gamma * 3.14159/180.
      DELK  = C * SIN(GAMMA)
      EL(1,1,K) = ZT(K) - DELK
CCC      WRITE(IOCODE,1601) K,A,B,C,BETA,GAMMA,DELK,EL(1,1,K)
      DO 1590 J=1,JJ
1590  EL(1,J,K) = EL(1,1,K)
1600  CONTINUE

CCC1601  FORMAT(1X,'K,A,B,C,BETA,GAMMA,DELK,EL(1,1,K):',
CCC     &      /1X,I3,7F9.3)

CC      DO 1700 I=2,II  (ch'd like in MISC4 - 07/24/93-after sent to LSU & BDM!
      DO 1700 I=1,II
      DO 1700 J=1,JJ
      DO 1700 K=1,KK
      EL(I,J,K) = EL(1,J,K) - DZZ(I)
C.... NOTE:  Top is approximate in this case - it is precisely the top of
C....        the block if the block were rotated to be horizontal - which
C....        is the way colorgrid is going to draw it anyway !
      TOP(I,J,K) = EL(I,J,K) - 0.5 * DZ(I,J,K)
1700  CONTINUE


2000  CONTINUE
      WRITE(IOCODE,3901) ALPHA
3901  FORMAT(///T5,
     &'-- Depths to Block Mid-Points - Dipping Reservoir Option --'/,
     &T5,'                  DIP = ',F7.3,' degrees'/)
      IFMT=0
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,EL)

C.... Must also write depth to tops - added 07/02/93
      WRITE(IOCODE,390) 
390   FORMAT(//T5,'__________ Depths to Grid Block Tops __________'/)
      IFMT=0
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,TOP)
      RETURN
c========================================================================


902   CONTINUE
C.... KEL = 0, 1, 2, 3 options - not altered !
      IF(KEL.GT.1) GO TO 951
      IF(KEL.EQ.1) GO TO 920
      READ(20,*) ELEV
      DO 910 J=1,JJ
      DO 910 I=1,II
      VAREL(I,J) = ELEV
910   CONTINUE

  920 IF(KEL.NE.1)GO TO 930
      DO 922 J=1,JJ
      READ(20,*) (VAREL(I,J),I=1,II)
  922 CONTINUE
  930 CONTINUE

      DO 923 I=1,II
      DO 923 J=1,JJ
  923 SUM(I,J)=0.
      DO 926 K=1,KK
      DO 926 J=1,JJ
      DO 926 I=1,II
      IF(K.GE.2) GO TO 924
      EL(I,J,K) = VAREL(I,J)
      GO TO 926
  924 CONTINUE
      DEL = SUM(I,J)+DZ(I,J,K-1)
      EL(I,J,K) = VAREL(I,J)+DEL
      SUM(I,J)=DZ(I,J,K-1)+SUM(I,J)
  926 CONTINUE
      GO TO 601

  951 IF(KEL.EQ.3) GO TO 961
c.... KEL = 2 ....................................
      READ(20,*)(RKEL(K),K=1,KK)
      DO 953 K=1,KK
      DO 953 J=1,JJ
      DO 953 I=1,II
  953 EL(I,J,K)=RKEL(K)
      GO TO 971

  961 DO 963 K=1,KK
      DO 965 J=1,JJ
  965 READ(20,*)(EL(I,J,K),I=1,II)
  963 CONTINUE
  971 CONTINUE
601   CONTINUE

      WRITE(IOCODE,3903)
3903  FORMAT(//T5,'__________ Depths to Grid Block Tops __________'/)
      IFMT=0
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,EL)
     
      DO 973 K=1,KK
      DO 973 J=1,JJ
      DO 973 I=1,II
C.... Save top elevations for output to ____.MAP file  -  07/02/93
      TOP(I,J,K) = EL(I,J,K)
  973 EL(I,J,K) = EL(I,J,K)+DZ(I,J,K)*0.5


C- - - - - - - - - -   F  O  R  M  A  T  S   - - - - - - - - - - - - - - - 
69    FORMAT(40A2)
56    FORMAT(//)
   72 FORMAT(1X,15F8.0)
29    FORMAT(T5,'GRID BLOCK LENGTH (DX) IS INITIALLY',
     &' SET AT',F10.4,' FOR ALL NODES'//)
   31 FORMAT(//T5,'********GRID BLOCK LENGTH (DX) NODE MODIFICATIONS',
     & '**********',/T5,
     & '   I1  I2  J1  J2  K1  K2  NEW DX VALUE')
   32 FORMAT(15X,6I4,2X,E10.4)
33    FORMAT(T5,'GRID BLOCK WIDTH  (DY) IS INITIALLY',
     &' SET AT',F10.4,' FOR ALL NODES'//)
   34 FORMAT(//T5,'********GRID BLOCK WIDTH  (DY) NODE MODIFICATIONS',
     & '**********',//T5,
     & '   I1  I2  J1  J2  K1  K2  NEW DY VALUE')
36    FORMAT(T5,'GRID BLOCK GROSS THICKNESS (DZ) IS INITIALLY',
     &' SET AT',F10.4,' FOR ALL NODES'//)
   37 FORMAT(//T5,'********GRID BLOCK GROSS THICKNESS (DZ) NODE ',
     & 'MODIFICATIONS**********',//T5,
     & '   I1  I2  J1  J2  K1  K2  NEW DZ VALUE')
38    FORMAT(/1X,'K =',I2/)
43    FORMAT(//T5,'********GRID BLOCK LENGTH (DX) DISTRIBUTION********'
     &/)
47    FORMAT(//T5,'********GRID BLOCK WIDTH  (DY) DISTRIBUTION********'
     &/)
   48 FORMAT(//T5,8('*'),'GRID BLOCK GROSS THICKNESS (DZ) ',
     & 'DISTRIBUTION',8('*'),/)
  511 FORMAT(T5,'GRID SIZE (DX) IN COLUMN',I5,' IS INITIALLY SET AT'
     &,F8.2,' FOR ALL NODES',/)
  512 FORMAT(T5,'GRID SIZE (DY) IN ROW   ',I5,' IS INITIALLY SET AT'
     &,F8.2,' FOR ALL NODES',/)
  513 FORMAT(T5,'GRID SIZE (DZ) IN LAYER ',I5,' IS INITIALLY SET AT'
     &,F8.2,' FOR ALL NODES',/)
      RETURN
      END
