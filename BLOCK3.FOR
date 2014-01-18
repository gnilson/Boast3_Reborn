      
      SUBROUTINE OUTX(IOCODE,IFMT,II,JJ,KK,X)
      INCLUDE 'PARAMS.FOR'
      DIMENSION X(LP1,LP2,LP3)
      DO 2000 K=1,KK
      WRITE(IOCODE,1010) K
      IF(IFMT.EQ.0) WRITE(IOCODE,1011) (I,I=1,II)
      IF(IFMT.EQ.1) WRITE(IOCODE,1013) (I,I=1,II)
1040  DO 2000 J=1,JJ
      IF(IFMT.EQ.0) WRITE(IOCODE,2011) J,(X(I,J,K),I=1,II)
      IF(IFMT.EQ.1) WRITE(IOCODE,2013) J,(X(I,J,K),I=1,II)
2000  CONTINUE
1010  FORMAT(/1X,'K =',I3)
1011  FORMAT(1X,I10,9I13)
1013  FORMAT(1X,I11,9I13)
2011  FORMAT(1X,I3,10G13.5/(4X,10G13.5))
2013  FORMAT(1X,I3,F10.4,9F13.4/(4X,F10.4,9F13.4))
      RETURN
      END
 
      SUBROUTINE POSTP(NPLINE,IOCODE)
      INCLUDE 'PARAMS.FOR'
C===================== Only want table - no plots =================
C      POST-PLOT PACKAGE - deleted all plots - 9/30/91 !!!!!
C==================================================================
      CHARACTER*5 ATSNO(2),AOPR(5),AGPR(5),AWPR(5),AGOR(5),
     & AWOR(5),AGIR(5),AWIR(5),ARESP(5),AAIR(5),AAIC(5),ACOP(5),
     & ACGP(5),ACWP(5),ACGI(5),ACWI(5)

      COMMON /RUNSUM/ ITSNO(LP12),STIME(LP12),SOPROD(LP12),
     & SGPROD(LP12),
     & SWPROD(LP12),SGOR(LP12),SWOR(LP12),SGINJ(LP12),SWINJ(LP12)

      COMMON /RUN2/SPVWTP(LP12),SOCUMP(LP12),SWCUMP(LP12),SGCUMP(LP12),
     & SGCUMI(LP12),SWCUMI(LP12),SAQUIR(LP12),SAQUIC(LP12)

      COMMON /SPOST/ KOPR,KGPR,KWPR,KGOR,KWOR,KGIR,KWIR,KRESP,
     & KAIR,KAIC,KCOP,KCGP,KCWP,KCGI,KCWI,ITSMAX

      COMMON /VECTOR/ DX(LP1,LP2,LP3),DY(LP1,LP2,LP3),DZ(LP1,LP2,LP3),
     & DZNET(LP1,LP2,LP3),IQN1(LP11),IQN2(LP11),IQN3(LP11),IHEDIN(80)


      DATA ATSNO/'TIME:',' DAYS'/
      DATA AOPR/' OIL ','PROD ','RATE ','(STB/','D)   '/
      DATA AGPR/' GAS ','PROD ','RATE ','(MSCF','/D)  '/
      DATA AWPR/'WATER',' PROD',' RATE',' (STB','/D)  '/
      DATA AGOR/'PROD ','G/O R','ATIO ','(SCF/','STB) '/
      DATA AWOR/'PROD ','W/O R','ATIO ','(STB/','STB) '/
      DATA AGIR/' GAS ','INJ R','ATE (','MSCF/','D)   '/
      DATA AWIR/'WATER',' INJ ','RATE ','(STB/','D)   '/
      DATA ARESP/'PV WT',' AVG ','RES P',' (PSI','A)   '/
      DATA AAIR/'AQ IN','FLUX ','RATE ','(MSTB','/D)  '/
      DATA AAIC/'AQ IN','FLUX ','CUM  ','(MMST','B)   '/
      DATA ACOP/' CUM ','OIL P','ROD  ','(MSTB',')    '/
      DATA ACGP/' CUM ','GAS P','ROD  ','(MMSC','F)   '/
      DATA ACWP/' CUM ','WATER',' PROD',' (MST','B)   '/
      DATA ACGI/'  CUM',' GAS ','INJ  ','(MMSC','F)   '/
      DATA ACWI/' CUM ','WATER',' INJ ','(MSTB',')    '/

C===========================================================
C      TOTAL RUN SUMMARY
C===========================================================

      WRITE(IOCODE,2100)
      WRITE(9,2100)
      WRITE(10,2100)

 2100 FORMAT(/T51,29('*'),/,
     & T51,'*  TOTAL    RUN    SUMMARY  *',/,
     & T51,29('*'),//,
     & T5,' TIME STEP',14X,'PRODUCTION',19X,'INJECTION',
     & 5X,'PV WT AQUIFER INFLUX      CUM PRODUCTION',
     & 7X,'CUM INJECTION',/,
     & T5,10('-'),1X,37('-'),1X,16('-'),3X,'AVG',2X,
     & 14('-'),1X,24('-'),1X,15('-'))

      WRITE(IOCODE,2120)
      WRITE(9,2120)
      WRITE(10,2120)

 2120 FORMAT(T5,14X,'OIL     GAS     WATER    GOR   WATER',
     & 3X,'GAS   WATER    RES   RATE    CUM     OIL',
     & 5X,'GAS    WATER    GAS    WATER',/,
     & T5,37X,'SCF/  /OIL',20X,'PRES',/,
     & T6,'NO.  DAYS   STB/D  MSCF/D   STB/D',
     & 3X,'STB   RATIO  MSCF/D   STB/D   PSIA',
     & 2X,'MSTB/D  MMSTB    MSTB   MMSCF   MSTB',
     & 4X,'MMSCF   MSTB',/,
     & T5,4('-'),1X,5('-'),1X,8('-'),2(1X,7('-')),
     & 1X,6('-'),1X,5('-'),1X,8('-'),1X,7('-'),1X,2(6('-'),1X),
     & 6(7('-'),1X))

      DO 2200 N=1,ITSMAX
      WRITE(IOCODE,2140) ITSNO(N),STIME(N),SOPROD(N),
     & SGPROD(N),SWPROD(N),SGOR(N),SWOR(N),SGINJ(N),SWINJ(N),
     & SPVWTP(N),SAQUIR(N),SAQUIC(N),SOCUMP(N),SGCUMP(N),
     & SWCUMP(N),SGCUMI(N),SWCUMI(N)
C=================================================================
      WRITE(9,2140) ITSNO(N),STIME(N),SOPROD(N),
     & SGPROD(N),SWPROD(N),SGOR(N),SWOR(N),SGINJ(N),SWINJ(N),
     & SPVWTP(N),SAQUIR(N),SAQUIC(N),SOCUMP(N),SGCUMP(N),
     & SWCUMP(N),SGCUMI(N),SWCUMI(N)
      WRITE(10,2140) ITSNO(N),STIME(N),SOPROD(N),
     & SGPROD(N),SWPROD(N),SGOR(N),SWOR(N),SGINJ(N),SWINJ(N),
     & SPVWTP(N),SAQUIR(N),SAQUIC(N),SOCUMP(N),SGCUMP(N),
     & SWCUMP(N),SGCUMI(N),SWCUMI(N)
C=================================================================
 2140 FORMAT(T4,I4,1X,F6.0,1X,F8.0,1X,F7.0,F8.1,1X,F6.0,1X,
     & F5.1,1X,F8.0,1X,F7.0,1X,F6.0,F7.1,F8.0,5F8.0)
 2200 CONTINUE
      RETURN
      END
C.................................................................PORPRM
      SUBROUTINE PORPRM(IOCODE,II,JJ,KK)
C      MACHINE DEPENDENT INCLUDE STATEMENT
      INCLUDE 'PARAMS.FOR'

c......... Added 07/02/93 - to get new array PHI() ..................
c.........                  and new switches KPHIMP,KXMP,KYMP,KZMP
      INCLUDE 'COMMON.INC'


c....................................................................

C      READ POROSITY AND PERMEABILITY DISTRIBUTIONS
      DIMENSION RPHL(LP3),RKXL(LP1),RKYL(LP2),RKZL(LP3)
      REAL KX,KY,KZ,KXC,KYC,KZC
      COMMON /TSTDAT/ IFATAL,IWARN

      COMMON /SSOLN/ BO(LP1,LP2,LP3),BW(LP1,LP2,LP3),BG(LP1,LP2,LP3),
     & QO(LP1,LP2,LP3),QW(LP1,LP2,LP3),QG(LP1,LP2,LP3),
     & GOWT(LP1,LP2,LP3),GWWT(LP1,LP2,LP3),GGWT(LP1,LP2,LP3),
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC     & OW(LP4,LP2,LP3),OE(LP4,LP2,LP3),WW(LP4,LP2,LP3),WE(LP4,LP2,LP3),
CCC     & OS(LP1,LP5,LP3),ON(LP1,LP5,LP3),WS(LP1,LP5,LP3),WN(LP1,LP5,LP3),
CCC     & OT(LP1,LP2,LP6),OB(LP1,LP2,LP6),WT(LP1,LP2,LP6),WB(LP1,LP2,LP6),
     & O1(LP4,LP2,LP3),W1(LP4,LP2,LP3),
     & O2(LP1,LP5,LP3),W2(LP1,LP5,LP3),
     & O3(LP1,LP2,LP6),W3(LP1,LP2,LP6),
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     & QOWG(LP1,LP2,LP3),VP(LP1,LP2,LP3),CT(LP1,LP2,LP3)

      COMMON /SPARM/ KX(LP1,LP2,LP3),KY(LP1,LP2,LP3),KZ(LP1,LP2,LP3),
     & EL(LP1,LP2,LP3),TX(LP4,LP2,LP3),TY(LP1,LP5,LP3),TZ(LP1,LP2,LP6),
     & PDAT(LP1,LP2,LP3),PDATUM,GRAD
      READ(20,69)
C*****READ INPUT CODES FOR PHI,KX,KY,KZ
      READ(20,*)KPH,KKX,KKY,KKZ
C*****ESTABLISH POROSITY (PHI) DISTRIBUTION
      IF(KPH.GE.0)GO TO 135
      READ(20,*)PHIC
      DO 140 K=1,KK
      DO 140 J=1,JJ
      DO 140 I=1,II
140   VP(I,J,K)=PHIC
      WRITE(IOCODE,56)
      WRITE(IOCODE,26)PHIC
      GO TO 165
  135 IF(KPH.GT.0)GO TO 145
      READ(20,*)(RPHL(K),K=1,KK)
      DO 550 K=1,KK
      DO 550 J=1,JJ
      DO 550 I=1,II
  550 VP(I,J,K)=RPHL(K)
      DO 560 K=1,KK
  560 WRITE(IOCODE,510)K,RPHL(K)
      GO TO 165
145   WRITE(IOCODE,39)
      DO 160 K=1,KK
      WRITE(IOCODE,38)K
      DO 155 J=1,JJ
      READ(20,*)(VP(I,J,K),I=1,II)
  155 WRITE(IOCODE,73)(VP(I,J,K),I=1,II)
160   CONTINUE
165   CONTINUE
      WRITE(IOCODE,56)
C*****ESTABLISH PERMEABILITY (KX) DISTRIBUTION
      IF(KKX.GE.0)GO TO 180
      READ(20,*)KXC
      DO 175 K=1,KK
      DO 175 J=1,JJ
      DO 175 I=1,II
175   KX(I,J,K)=KXC
      WRITE(IOCODE,56)
      WRITE(IOCODE,29)KXC
      GO TO 195
  180 IF(KKX.GT.0)GO TO 185
      READ(20,*)(RKXL(K),K=1,KK)
      DO 187 K=1,KK
      DO 187 J=1,JJ
      DO 187 I=1,II
  187 KX(I,J,K)=RKXL(K)
      DO 182 K=1,KK
  182 WRITE(IOCODE,511)K,RKXL(K)
      GO TO 195
185   WRITE(IOCODE,43)
      DO 192 K=1,KK
      WRITE(IOCODE,38)K
      DO 190 J=1,JJ
      READ(20,*)(KX(I,J,K),I=1,II)
  190 WRITE(IOCODE,72)(KX(I,J,K),I=1,II)
192   CONTINUE
195   CONTINUE
      WRITE(IOCODE,56)
C*****ESTABLISH PERMEABILITY (KY) DISTRIBUTION
      IF(KKY.GE.0)GO TO 200
      READ(20,*)KYC
      DO 202 K=1,KK
      DO 202 J=1,JJ
      DO 202 I=1,II
202   KY(I,J,K)=KYC
      WRITE(IOCODE,56)
      WRITE(IOCODE,33)KYC
      GO TO 220
  200 IF(KKY.GT.0)GO TO 207
      READ(20,*)(RKYL(K),K=1,KK)
      DO 205 K=1,KK
      DO 205 J=1,JJ
      DO 205 I=1,II
  205 KY(I,J,K)=RKYL(K)
      DO 210 K=1,KK
  210 WRITE(IOCODE,512)K,RKYL(K)
      GO TO 220
207   WRITE(IOCODE,47)
      DO 212 K=1,KK
      WRITE(IOCODE,38)K
      DO 215 J=1,JJ
      READ(20,*)(KY(I,J,K),I=1,II)
  215 WRITE(IOCODE,72)(KY(I,J,K),I=1,II)
212   CONTINUE
220   CONTINUE
      WRITE(IOCODE,56)
C*****ESTABLISH PERMEABILITY (KZ) DISTRIBUTION
      IF(KKZ.GE.0)GO TO 225
      READ(20,*)KZC
      DO 230 K=1,KK
      DO 230 J=1,JJ
      DO 230 I=1,II
230   KZ(I,J,K)=KZC
      WRITE(IOCODE,56)
      WRITE(IOCODE,36)KZC
      GO TO 245
  225 IF(KKZ.GT.0)GO TO 232
      READ(20,*)(RKZL(K),K=1,KK)
      DO 235 K=1,KK
      DO 235 J=1,JJ
      DO 235 I=1,II
  235 KZ(I,J,K)=RKZL(K)
      DO 237 K=1,KK
  237 WRITE(IOCODE,513)K,RKZL(K)
      GO TO 245
232   WRITE(IOCODE,48)
      DO 240 K=1,KK
      WRITE(IOCODE,38)K
      DO 242 J=1,JJ
      READ(20,*)(KZ(I,J,K),I=1,II)
  242 WRITE(IOCODE,72)(KZ(I,J,K),I=1,II)
240   CONTINUE
245   CONTINUE
      WRITE(IOCODE,56)


C**********POROSITY AND PERMEABILITY MODIFICATIONS
      READ(20,69)
CCC      READ(20,*) NUMP,NUMKX,NUMKY,NUMKZ,IPCODE
      READ(20,*) NUMP,NUMKX,NUMKY,NUMKZ, KPHIMP,KXMP,KYMP,KZMP

      IF(NUMP.EQ.0) GO TO 8511

      WRITE(IOCODE,27)
      DO 274 L=1,NUMP
      READ(20,*) I1,I2,J1,J2,K1,K2,REGVAL
      WRITE(IOCODE,32) I1,I2,J1,J2,K1,K2,REGVAL
      DO 274 K=K1,K2
      DO 274 J=J1,J2
      DO 274 I=I1,I2
      VP(I,J,K)=REGVAL
  274 CONTINUE

8511  IF(KPHIMP.EQ.0) GO TO 8513
      WRITE(IOCODE,39)
      DO 851 K=1,KK
      WRITE(IOCODE,38)K
      DO 852 J=1,JJ
852   WRITE(IOCODE,73) (VP(I,J,K),I=1,II)
851   CONTINUE

8513  CONTINUE
      IF(NUMKX.EQ.0) GO TO 85312
      WRITE(IOCODE,31)
      DO 275 L=1,NUMKX
      READ(20,*) I1,I2,J1,J2,K1,K2,REGVAL
      WRITE(IOCODE,32) I1,I2,J1,J2,K1,K2,REGVAL
      DO 275 K=K1,K2
      DO 275 J=J1,J2
      DO 275 I=I1,I2
      KX(I,J,K)=REGVAL
  275 CONTINUE

85312 IF(KXMP.EQ.0) GO TO 8531
      WRITE(IOCODE,43)
      DO 853 K=1,KK
      WRITE(IOCODE,38)K
      DO 854 J=1,JJ
854   WRITE(IOCODE,72) (KX(I,J,K),I=1,II)
853   CONTINUE

8531  CONTINUE
      IF(NUMKY.EQ.0) GO TO 85512
      WRITE(IOCODE,34)
      DO 276 L=1,NUMKY
      READ(20,*) I1,I2,J1,J2,K1,K2,REGVAL
      WRITE(IOCODE,32) I1,I2,J1,J2,K1,K2,REGVAL
      DO 276 K=K1,K2
      DO 276 J=J1,J2
      DO 276 I=I1,I2
      KY(I,J,K)=REGVAL
  276 CONTINUE

85512 IF(KYMP.EQ.0) GO TO 8551
      WRITE(IOCODE,47)
      DO 855 K=1,KK
      WRITE(IOCODE,38)K
      DO 856 J=1,JJ
856   WRITE(IOCODE,72) (KY(I,J,K),I=1,II)
855   CONTINUE
8551  CONTINUE

      IF(NUMKZ.EQ.0) GO TO 85712
      WRITE(IOCODE,37)
      DO 277 L=1,NUMKZ
      READ(20,*) I1,I2,J1,J2,K1,K2,REGVAL
      WRITE(IOCODE,32) I1,I2,J1,J2,K1,K2,REGVAL
      DO 277 K=K1,K2
      DO 277 J=J1,J2
      DO 277 I=I1,I2
      KZ(I,J,K)=REGVAL
  277 CONTINUE

85712 IF(KZMP.EQ.0) GO TO 8571
      WRITE(IOCODE,48)
      DO 857 K=1,KK
      WRITE(IOCODE,38)K
      DO 858 J=1,JJ
858   WRITE(IOCODE,72) (KZ(I,J,K),I=1,II)
857   CONTINUE

8571  CONTINUE

C------- STORE VP() as PHI() for Time= 0.0 output to COLORGRID - 7/02/93
      DO 1652 K=1,KK
      DO 1652 J=1,JJ
      DO 1652 I=1,II
1652  PHI(I,J,K) = VP(I,J,K)
C------------------------------------------------------------------------

69    FORMAT(40A2)
56    FORMAT(//)
   72 FORMAT(1X,15F8.1)
   73 FORMAT(1X,15F8.4)
26    FORMAT(T5,'POROSITY (PHI) IS INITIALLY SET AT',F8.4,' FOR ALL',
     &' NODES'//)
   27 FORMAT(//T5,'**********POROSITY NODE MODIFICATIONS**********',
     &//T5,'   I1  I2  J1  J2  K1  K2  NEW PHI VALUE')
29    FORMAT(T5,'PERMEABILITY (KX) IS INITIALLY',
     &' SET AT',F10.4,' FOR ALL NODES'//)
   31 FORMAT(//T5,'**********PERMEABILITY (KX) NODE MODIFICATIONS',
     & '**********',//T5,
     & '   I1  I2  J1  J2  K1  K2  NEW KX VALUE')
32    FORMAT(5X,6I4,2X,E10.4)
33    FORMAT(T5,'PERMEABILITY (KY) IS INITIALLY',
     &' SET AT',F10.4,' FOR ALL NODES'//)
   34 FORMAT(//T5,'**********PERMEABILITY (KY) NODE MODIFICATIONS',
     & '**********',//T5,
     & '   I1  I2  J1  J2  K1  K2  NEW KY VALUE')
36    FORMAT(T5,'PERMEABILITY (KZ) IS INITIALLY',
     &' SET AT',F10.4,' FOR ALL NODES'//)
   37 FORMAT(//T5,'**********PERMEABILITY (KZ) NODE MODIFICATIONS',
     & '**********',//T5,
     & '   I1  I2  J1  J2  K1  K2  NEW KZ VALUE')
38    FORMAT(/1X,'K =',I2/)
39    FORMAT(//T5,'**********POROSITY DISTRIBUTION FOLLOWS**********'/)
43    FORMAT(//T5,'**********PERMEABILITY (KX) DISTRIBUTION**********'
     &/)
47    FORMAT(//T5,'**********PERMEABILITY (KY) DISTRIBUTION**********'
     &/)
48    FORMAT(//T5,'**********PERMEABILITY (KZ) DISTRIBUTION**********'
     &/)
  510 FORMAT(//T5,'POROSITY IN LAYER',I5,' IS INITIALLY SET AT',F8.5,
     &' FOR ALL NODES',/)
  511 FORMAT(T5,'PERMEABILITY (KX) IN LAYER',I5,' IS INITIALLY SET AT'
     &,F8.2,' FOR ALL NODES',/)
  512 FORMAT(T5,'PERMEABILITY (KY) IN LAYER',I5,' IS INITIALLY SET AT'
     &,F8.2,' FOR ALL NODES',/)
  513 FORMAT(T5,'PERMEABILITY (KZ) IN LAYER',I5,' IS INITIALLY SET AT'
     &,F8.2,' FOR ALL NODES',/)
      RETURN
      END
C.................................................................PRTPS
      SUBROUTINE PRTPS(NLOOP,II,JJ,KK,PAVG0,PAVG,CMBEO,CMBEW,CMBEG,
     &COP,CWP,CWI,CGP,CGI,MBEO,MBEW,MBEG,DELT0,
     &OPR,WPR,GPR,WIR,GIR,ETI, KOUT,
     &CWOR,CGOR,WOR,GOR,IPMAP,ISOMAP,ISWMAP,ISGMAP,IPBMAP,IAQMAP,
C========================================
     &KROMP,KRWMP,KRGMP, IRSOMP, IPCOW,IPCGO, KPHIMP,
     &RSOA,QGRSO,RKROMX,QGRSOT,CAPGO,CAPOW, RPO,RPW,RPG)
C========================================

      INCLUDE 'PARAMS.FOR'

      COMMON /REALS/ PHI(IMX,JMX,KMX), TOP(IMX,JMX,KMX)

C===========================================================
      DIMENSION RSOA(LP1,LP2,LP3), QGRSO(LP1,LP2,LP3),
     &          CAPGO(LP1,LP2,LP3), CAPOW(LP1,LP2,LP3),
     &  RPO(LP1,LP2,LP3), RPW(LP1,LP2,LP3), RPG(LP1,LP2,LP3)
C===========================================================

C      SUMMARY REPORT
      REAL MCFGI,MBEO,MBEW,MBEG,MCFG,MCFG1,MCFGT,KX,KY,KZ

      COMMON /BUBBLE/ PBO,VSLOPE(LP8),BSLOPE(LP8),RSLOPE(LP8),PMAXT,
     & IREPRS,MPGT(LP8),
     & RHOSCO(LP8),RHOSCW(LP8),RHOSCG(LP8),MSAT(LP7),MPOT(LP8),
     & MPWT(LP8),PBOT(LP1,LP2,LP3),PBOTN(LP1,LP2,LP3)

      COMMON /SAQUI/ IAQOPT,CPIAQ1(LP1,LP2,LP3),CPIAQ2(LP1,LP2,LP3),
     & CPI1(LP1,LP2,LP3),CPI2(LP1,LP2,LP3),EWAQ(LP1,LP2,LP3),
     & CUMAQW(LP1,LP2,LP3),
     & QWAQ(LP1,LP2,LP3),CUMEW(LP1,LP2,LP3),QWAQR(LP7),CUMAQR(LP7)
     & ,IAQREG(LP1,LP2,LP3),PAQ(LP1,LP2,LP3),PIAQ(LP1,LP2,LP3)

      COMMON /SARRAY/ PN(LP1,LP2,LP3),IOCODE,IDMAX,
     & SON(LP1,LP2,LP3),SWN(LP1,LP2,LP3),SGN(LP1,LP2,LP3),
     & A1(LP1,LP2,LP3),A2(LP1,LP2,LP3),A3(LP1,LP2,LP3),
     & SUM(LP1,LP2,LP3),GAM(LP1,LP2,LP3),QS(LP1,LP2,LP3)

      COMMON /SPARM/ KX(LP1,LP2,LP3),KY(LP1,LP2,LP3),KZ(LP1,LP2,LP3),
     & EL(LP1,LP2,LP3),TX(LP4,LP2,LP3),TY(LP1,LP5,LP3),TZ(LP1,LP2,LP6),
     & PDAT(LP1,LP2,LP3),PDATUM,GRAD

      COMMON /SPRTPS/ P(LP1,LP2,LP3),SO(LP1,LP2,LP3),SW(LP1,LP2,LP3),
     & SG(LP1,LP2,LP3)

      COMMON /SSOLN/ BO(LP1,LP2,LP3),BW(LP1,LP2,LP3),BG(LP1,LP2,LP3),
     & QO(LP1,LP2,LP3),QW(LP1,LP2,LP3),QG(LP1,LP2,LP3),
     & GOWT(LP1,LP2,LP3),GWWT(LP1,LP2,LP3),GGWT(LP1,LP2,LP3),
     & O1(LP4,LP2,LP3),W1(LP4,LP2,LP3),
     & O2(LP1,LP5,LP3),W2(LP1,LP5,LP3),
     & O3(LP1,LP2,LP6),W3(LP1,LP2,LP6),
     & QOWG(LP1,LP2,LP3),VP(LP1,LP2,LP3),CT(LP1,LP2,LP3)

      DIMENSION IHED(150)

      DO 15  I=1,100
   15 IHED(I)=I
      PPM=0.
      SOM=0.
      SWM=0.
      SGM=0.

      IF(NLOOP.EQ.1) GO TO 300

      DO 240 K=1,KK
      DO 240 J=1,JJ
      DO 240 I=1,II
      DPO=P(I,J,K)-PN(I,J,K)
      DSO=SO(I,J,K)-SON(I,J,K)
      DSW=SW(I,J,K)-SWN(I,J,K)
      DSG=SG(I,J,K)-SGN(I,J,K)
      IF(ABS(DPO).LE.ABS(PPM))GO TO 210
      PPM=DPO
      IPM=I
      JPM=J
      KPM=K
  210 IF(ABS(DSO).LE.ABS(SOM))GO TO 220
      SOM=DSO
      IOM=I
      JOM=J
      KOM=K
  220 IF(ABS(DSW).LE.ABS(SWM))GO TO 230
      SWM=DSW
      IWM=I
      JWM=J
      KWM=K
  230 IF(ABS(DSG).LE.ABS(SGM))GO TO 240
      SGM=DSG
      IGM=I
      JGM=J
      KGM=K
  240 CONTINUE

      NLM=NLOOP-1
      GORM=1000.*GOR

      WRITE(IOCODE,5)
5     FORMAT(//T5,69('*'),
     &        /T5,'*',T73,'*',
     &        /T5,'*',T73,'*')

      WRITE(IOCODE,105)
105   FORMAT(T5,'*',T15,'SUMMARY REPORT: BOAST 3 - July 03, 1993',
     &T73,'*')

      WRITE(IOCODE,6)
6     FORMAT(T5,'*',T73,'*',
     &      /T5,'*',T73,'*',
     &      /T5,69('*'),//)

      WRITE(IOCODE,110) ETI,NLM,DELT0,PAVG,PAVG0,IPM,JPM,KPM,PPM,
     & IOM,JOM,KOM,SOM,IGM,JGM,KGM,SGM,IWM,JWM,KWM,SWM,
     & MBEO,MBEG,MBEW 

      WRITE(IOCODE,1101) CMBEO,CMBEG,CMBEW, OPR,COP, GPR,CGP, WPR,
     & CWP,GIR,CGI,WIR,CWI,WOR,CWOR,GORM,CGOR
  
  110 FORMAT(/10X,
     & 'Elapsed time (days)          =',G14.6/10X,
     & 'Time-step number             =',I10  /10X,
     & 'Time-step size (days)        =',G14.6//10X,
     & 'Current avg res pressure     =',G14.6/10X,
     & 'Previous avg res pressure    =',G14.6//10X,
     & 'PRES DPMAX (',I3,',',I3,',',I3,')   =',F11.5/10X,
     & 'OIL  DSMAX (',I3,',',I3,',',I3,')   =',F11.5/10X,
     & 'GAS  DSMAX (',I3,',',I3,',',I3,')   =',F11.5/10X,
     & 'WATER DSMAX(',I3,',',I3,',',I3,')   =',F11.5//10X,
     & 'Oil material balance (%)      =',G16.7/10X,
     & 'Gas material balance (%)      =',G16.7/10X,
     & 'Water material balance (%)    =',G16.7) 
1101  FORMAT(//10X,
     & 'Cum. oil material balance(%)  =',G16.7/10X,
     & 'Cum. gas material balance(%)  =',G16.7/10X,
     & 'Cum. water material balance(%)=',G16.7//10X,
     & 'Oil production rate (stb/d)   =',G16.7/10X,
     & 'Cum. oil production (stb)     =',G16.7//10X,
     & 'Gas production rate (mscf/d)  =',G16.7/10X,
     & 'Cum. gas production (mscf)    =',G16.7//10X,
     & 'Water production rate(stb/d)  =',G16.7/10X,
     & 'Cum. water production (stb)   =',G16.7//10X,
     & 'Gas injection rate (mscf/d)   =',G16.7/10X,
     & 'Cum. gas injection (mscf)     =',G16.7/10X,
     & 'Water injection rate (stb/d)  =',G16.7/10X,
     & 'Cum. water injection (stb)    =',G16.7//10X,
     & 'Producing WOR (stb/stb)       =',G16.7/10X,
     & 'Cum. WOR (stb/stb)            =',G16.7//10X,
     & 'Producing GOR (scf/stb)       =',G16.7/10X,
     & 'Cum. GOR (SCF/STB)            =',G16.7/)

C=====================================
      IF(RKROMX.LE.1.E-06) THEN
         WRITE(IOCODE,11091) QGRSOT
11091    FORMAT(//5X,
     &  'Total gas rate dissolved from immobile oil (Mscf/d) =',G13.5//)
      ENDIF
C====================================
C      AQUIFER PRINT
      CALL AQPRNT

300   CONTINUE

      WRITE(IOCODE,302)
  302 FORMAT(///)

      IF(NLOOP.EQ.1) WRITE(IOCODE,304)
  304 FORMAT(/,T15,7('*'),' INITIAL ARRAYS ',7('*')//)

      IF(NLOOP.GT.1 .AND. KOUT.EQ.0) THEN
         RETURN 
c......... do not write maps to ___.OUT file..............
      ENDIF


      IF(KPHIMP.GT.0) THEN
C....... Write Porosity map...................
         WRITE(IOCODE,6133)
         IFMT=0
         CALL OUTX(IOCODE,IFMT,II,JJ,KK,PHI)
      ENDIF

      IF(IPMAP.GT.0) THEN 
C....... Write Po map.........................
         WRITE(IOCODE,61)
         IFMT=0
         CALL OUTX(IOCODE,IFMT,II,JJ,KK,P)
      ENDIF

      IF(IPCOW.GT.0) THEN
C....... Write Pcow map......................
         WRITE(IOCODE,613)
         IFMT=0
         CALL OUTX(IOCODE,IFMT,II,JJ,KK,CAPOW)
      ENDIF

      IF(IPCGO.GT.0) THEN
C....... Write Pcgo map......................
         WRITE(IOCODE,611)
         IFMT=0
         CALL OUTX(IOCODE,IFMT,II,JJ,KK,CAPGO)
      ENDIF


      IF(GRAD.LE.0.00001) GO TO 311
      DO 312 K=1,KK
      DO 312 J=1,JJ
      DO 312 I=1,II
      IF(VP(I,J,K).LE.0.0) GO TO 312
      PDAT(I,J,K)=P(I,J,K)+(PDATUM-EL(I,J,K))*GRAD
312   CONTINUE
      WRITE(IOCODE,62) PDATUM
      IFMT=0
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,PDAT)

311   CONTINUE 

C==================================================================
C     Deleted subroutine PLOT and all call's to same !!!!!!
C==================================================================
315   CONTINUE

      IF(ISOMAP.EQ.0) GO TO 422
C      OIL SAT ARRAY
      WRITE(IOCODE,71)
      IFMT=1
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,SO)


  422 IF(ISWMAP.EQ.0) GO TO 432

C      WATER SAT ARRAY
      WRITE(IOCODE,81)
      IFMT=1
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,SW)


  432 IF(ISGMAP.EQ.0) GO TO 442
C      GAS SAT ARRAY
      WRITE(IOCODE,91)
      IFMT=1
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,SG)

442   CONTINUE


C###################### ADDED - 03/18/93 ###################################
      IF(KROMP.NE.0 .AND. NLOOP.GT.1) THEN
         WRITE(IOCODE,9101)
9101     FORMAT(//10X,'...................  Kro ...................')
         IFMT=1
         CALL OUTX(IOCODE,IFMT,II,JJ,KK,RPO)
      ENDIF

      IF(KRWMP.NE.0 .AND. NLOOP.GT.1) THEN
         WRITE(IOCODE,9103)
9103     FORMAT(//10X,'...................  Krw ...................')
         IFMT=1
         CALL OUTX(IOCODE,IFMT,II,JJ,KK,RPW)
      ENDIF
         
      IF(KRGMP.NE.0 .AND. NLOOP.GT.1) THEN
         WRITE(IOCODE,9105)
9105     FORMAT(//10X,'...................  Krg ...................')
         IFMT=1
         CALL OUTX(IOCODE,IFMT,II,JJ,KK,RPG)
      ENDIF

C##########################################################################



      IF(IPBMAP.EQ.0) GO TO 452
C      BUBBLE POINT PRESSRE ARRAY
      WRITE(IOCODE,102)
  102 FORMAT(///,T15,'***** BUBBLE POINT PRESSURE DISTRIBUTION *****')
      IFMT=0
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,PBOT)


452   CONTINUE

      IF(IRSOMP.EQ.0) GO TO 4520
C.... Rso map...........................................................
      WRITE(IOCODE,1021)
1021  FORMAT(///,T15,'***** Solution gas-oil (Rso,scf/scf) *****')
      IFMT=0
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,RSOA)

      IF(RKROMX.GT.1.E-06) GO TO 4520
C.... Write evolved gas from immobile oil.....................
      WRITE(IOCODE,10211)
10211 FORMAT(///,T15,'***** Solution gas source term (QGRSO) *****')
      IFMT=0
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,QGRSO)

4520  CONTINUE

      IF(IAQOPT.LE.0) GO TO 462

      IF(IAQMAP.EQ.0) GO TO 462
C      CUM AQ INFLUX ARRAY
      WRITE(IOCODE,104)
  104 FORMAT(///,T15,'***** CUM. AQ INFLUX (STB) DISTRIBUTION *****',/)
      IFMT=0
      CALL OUTX(IOCODE,IFMT,II,JJ,KK,CUMAQW)

462   CONTINUE

      IF(NLOOP.NE.1) WRITE(IOCODE,7)
7     FORMAT(///T4,19('*'),'  END OF SUMMARY REPORT  ',19('*'),6(/))

      IF(NLOOP.EQ.1) WRITE(IOCODE,8)
8     FORMAT(///T4,19('*'),'  END OF INITIALIZATION  ',19('*'),6(/))

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC41    FORMAT(1X,I3,1X,15F8.0)
41    FORMAT(1X,I3,1X,15G11.4)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC51    FORMAT(/1X,'K =',I2/,5X,15(2X,I4,2X))
51    FORMAT(/1X,'K =',I2/,4X,15(4X,I4,3X))
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

52    FORMAT(/1X,'K =',I2/,5X,10(4X,I4,2X))

53     FORMAT(/)
61    FORMAT(///,T15,'***** RESERVOIR PRESSURE DISTRIBUTION *****'/)
62    FORMAT(///,T15,'****** RESERVOIR PRESSURE DISTRIBUTION ******'/,
     & T15,'(CORRECTED TO REFERENCE DEPTH OF ',F6.0,' FEET)'/)

611   FORMAT(///,T15,'_____ Gas-Oil Capillary Pressure _____')
613   FORMAT(///,T15,'_____ Oil-Water Capillary Pressure _____')
6133  FORMAT(///,T15,'__________ Porosity, fraction __________')

71    FORMAT(///,T15,'*********  OIL SATURATION  *********'/)
81    FORMAT(///,T15,'********* WATER SATURATION *********'/)
91    FORMAT(///,T15,'*********  GAS SATURATION  *********'/)

      RETURN
      END

      SUBROUTINE UNSAT(II,JJ,KK,SO,P,PN,PBOT,SGN,SW,SG)
ccc -----> Bug fix - 03/05/94 per Ted Bourgoyne (SO passed and dim'd twice!)

      INCLUDE 'PARAMS.FOR'

      DIMENSION SO(LP1,LP2,LP3),P(LP1,LP2,LP3),PN(LP1,LP2,LP3),
     &          PBOT(LP1,LP2,LP3),SG(LP1,LP2,LP3),
     &          SGN(LP1,LP2,LP3),SW(LP1,LP2,LP3)

C****UNDERSATURATED GRID BLOCK SATURATION CALCULATION.
C      SKIP CALC IF NO OIL IN BLOCK
      DO 410 I=1,II
      DO 410 J=1,JJ
      DO 410 K=1,KK
      IF(SO(I,J,K).LE.0.0) GO TO 410
      IF(P(I,J,K).GT.PN(I,J,K)) GO TO 410
      IF(P(I,J,K).LT.PBOT(I,J,K)) GO TO 410
      IP=I+1
      IM=I-1
      JP=J+1
      JM=J-1
      KP=K+1
      KM=K-1
      IF(IP.GT.II) GO TO 412
      IF(SGN(IP,J,K).GT.0.0001) GO TO 410
  412 IF(IM.LT.1) GO TO 414
      IF(SGN(IM,J,K).GT.0.0001) GO TO 410
  414 IF(JP.GT.JJ) GO TO 416
      IF(SGN(I,JP,K).GT.0.0001) GO TO 410
  416 IF(JM.LT.1) GO TO 418
      IF(SGN(I,JM,K).GT.0.0001) GO TO 410
  418 IF(KP.GT.KK) GO TO 420
      IF(SGN(I,J,KP).GT.0.0001) GO TO 410
  420 IF(KM.LT.1) GO TO 422
      IF(SGN(I,J,KM).GT.0.0001) GO TO 410
  422 SG(I,J,K)=0.0
      SO(I,J,K)=1.0-SW(I,J,K)
  410 CONTINUE

      RETURN
      END
