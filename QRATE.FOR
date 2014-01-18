C.................................................................QRATE
      SUBROUTINE QRATE(II,JJ,KK,NVQN,GORMAX,WORMAX,ETI
C==========================================
     &,NLOOP,DELT,RKROMX,DRSODP,QGRSO,QGRSOT,CAPGO,CAPOW)
C==========================================
      INCLUDE 'PARAMS.FOR'

C     WELL MODELS

C.... Added (passed) CAPGO & CAPOW 09/23/93 - to be able to consider 
C.... capillary pressure effects for KIP = -1 or -11
      DIMENSION CAPGO(LP1,LP2,LP3), CAPOW(LP1,LP2,LP3)

C=======================================================================
      DIMENSION DRSODP(LP1,LP2,LP3),COFRSO(LP1,LP2,LP3),
     &                               QGRSO(LP1,LP2,LP3)
C=======================================================================

      REAL KROT,KROGT,KRWT,KRGT,MUWT,MUOT,MUGT
     & ,MUO,MUW,MUG,KRO,KRW,KRG

      COMMON /BUBBLE/ PBO,VSLOPE(LP8),BSLOPE(LP8),RSLOPE(LP8),PMAXT,
     & IREPRS,MPGT(LP8),
     & RHOSCO(LP8),RHOSCW(LP8),RHOSCG(LP8),MSAT(LP7),MPOT(LP8),
     & MPWT(LP8),PBOT(LP1,LP2,LP3),PBOTN(LP1,LP2,LP3)

      COMMON /COEF/ AW(LP1,LP2,LP3),AE(LP1,LP2,LP3),AN(LP1,LP2,LP3),
     & AS(LP1,LP2,LP3),AB(LP1,LP2,LP3),AT(LP1,LP2,LP3),E(LP1,LP2,LP3),
     & B(LP1,LP2,LP3)

      COMMON /SARRAY/ PN(LP1,LP2,LP3),IOCODE,IDMAX,
     & SON(LP1,LP2,LP3),SWN(LP1,LP2,LP3),SGN(LP1,LP2,LP3),
     & A1(LP1,LP2,LP3),A2(LP1,LP2,LP3),A3(LP1,LP2,LP3),
     & SUM(LP1,LP2,LP3),GAM(LP1,LP2,LP3),QS(LP1,LP2,LP3)

      COMMON /SLIMIT/ GORT(LP11),WORT(LP11),ILIMOP(LP11),
     & GORL(LP11),WORL(LP11),QOC(LP11,LP3),QWC(LP11,LP3),QGC(LP11,LP3)

      COMMON /SPRTPS/ P(LP1,LP2,LP3),SO(LP1,LP2,LP3),SW(LP1,LP2,LP3),
     & SG(LP1,LP2,LP3)

      COMMON /SPVT/ SAT(LP7,LP9),KROT(LP7,LP9),KRWT(LP7,LP9),
     & BGT(LP7,LP9),
     & KRGT(LP7,LP9),ITHREE(LP7),RSOT(LP7,LP9),BWPT(LP7,LP9),
     & PCOWT(LP7,LP9),PCGOT(LP7,LP9),KROGT(LP7,LP9),SWR(LP7),
     & POT(LP7,LP9),MUOT(LP7,LP9),BOT(LP7,LP9),BOPT(LP7,LP9),
     & RSOPT(LP7,LP9),PWT(LP7,LP9),MUWT(LP7,LP9),BWT(LP7,LP9),
     & PGT(LP7,LP9),MUGT(LP7,LP9),
     & BGPT(LP7,LP9),CRT(LP7,LP9),IPVT(LP1,LP2,LP3),IROCK(LP1,LP2,LP3),
     & NROCK,NPVT,PSIT(LP7,LP9),PRT(LP7,LP9),WOROCK(LP7),GOROCK(LP7)

      COMMON /SRATE/ PID(LP11,LP3),PWF(LP11,LP3),PWFC(LP11,LP3),
     & KIP(LP11),LAYER(LP11),QVO(LP11),CUMG(LP11,LP3),
     & GMO(LP11,LP3),GMW(LP11,LP3),GMG(LP11,LP3),
     & QVW(LP11),QVG(LP11),QVT(LP11),CUMO(LP11,LP3),CUMW(LP11,LP3),
     &  IDWELL(LP11),ALIT(LP11),BLIT(LP11)


      COMMON /SSOLN/ BO(LP1,LP2,LP3),BW(LP1,LP2,LP3),BG(LP1,LP2,LP3),
     & QO(LP1,LP2,LP3),QW(LP1,LP2,LP3),QG(LP1,LP2,LP3),
     & GOWT(LP1,LP2,LP3),GWWT(LP1,LP2,LP3),GGWT(LP1,LP2,LP3),
     & O1(LP4,LP2,LP3),W1(LP4,LP2,LP3),
     & O2(LP1,LP5,LP3),W2(LP1,LP5,LP3),
     & O3(LP1,LP2,LP6),W3(LP1,LP2,LP6),
     & QOWG(LP1,LP2,LP3),VP(LP1,LP2,LP3),CT(LP1,LP2,LP3)

      COMMON /VECTOR/ DX(LP1,LP2,LP3),DY(LP1,LP2,LP3),DZ(LP1,LP2,LP3),
     & DZNET(LP1,LP2,LP3),IQN1(LP11),IQN2(LP11),IQN3(LP11),IHEDIN(80)

C** GORT IN SCF/STB; WORT IN STB/STB OR SCF/SCF.
      DO 1 J=1,NVQN
      GORT(J)=GORMAX
      WORT(J)=WORMAX
      GORL(J)=0.
      WORL(J)=0.
      ILIMOP(J)=1
C      WOR AND/OR GOR VARY WITH ROCK REGION
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IF(GORT(J).NE.0.0) GO TO 3
      LAY=IQ3+(LAYER(J)-1)
      IROCKR=IROCK(IQ1,IQ2,IQ3)
      GORT(J)=GOROCK(IROCKR)
      WRITE(IOCODE,*) 'STARTING LOOP 2'

      DO 2 K=IQ3,LAY
      IROCKR=IROCK(IQ1,IQ2,K)
      IF(GOROCK(IROCKR).GT.GORT(J)) GORT(J)=GOROCK(IROCKR)
    2 CONTINUE

    3 IF(WORT(J).NE.0.0) GO TO 1
      LAY=IQ3+(LAYER(J)-1)
      IROCKR=IROCK(IQ1,IQ2,IQ3)
      WORT(J)=WOROCK(IROCKR)

      DO 4 K=IQ3,LAY
      IROCKR=IROCK(IQ1,IQ2,K)
      IF(WOROCK(IROCKR).GT.WORT(J)) WORT(J)=WOROCK(IROCKR)
    4 CONTINUE

    1 CONTINUE

C** INITIALIZE RATES
      DO 5 K=1,KK
      DO 5 J=1,JJ
      DO 5 I=1,II
      QO(I,J,K)=0.0
      QW(I,J,K)=0.0
      QG(I,J,K)=0.0
      DO 5 M=1,NVQN
      IJ=IDWELL(M)
      QOC(IJ,K)=0.0
      QWC(IJ,K)=0.0
      QGC(IJ,K)=0.0
    5 CONTINUE
C======================================
      GMOMAX = 0.0
C=====================================
      DO 105 J=1,NVQN
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 105
      LAY=IQ3+(LAYER(J)-1)
      DO 1170 K=IQ3,LAY
      PWFC(J,K)=-1.0
      PP=P(IQ1,IQ2,K)
      BPT=PBOT(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      IROCKR=IROCK(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,VSLOPE(IPVTR),POT,MUOT,MPOT(IPVTR),PP,MUO)
      CALL INTERP(IPVTR,PWT,MUWT,MPWT(IPVTR),PP,MUW)
      CALL INTERP(IPVTR,PGT,MUGT,MPGT(IPVTR),PP,MUG)
      SSO=SO(IQ1,IQ2,K)
      SSW=SW(IQ1,IQ2,K)
      SSG=SG(IQ1,IQ2,K)
      CALL INTERP(IROCKR,SAT,KRWT,MSAT(IROCKR),SSW,KRW)
      IF(ITHREE(IROCKR).EQ.0) GO TO 1160
      CALL TRIKRO(IROCKR,SSO,SSW,KRO,KRW,KRG)
      GO TO 1165
 1160 CALL INTERP(IROCKR,SAT,KROT,MSAT(IROCKR),SSO,KRO)
 1165 CONTINUE
      CALL INTERP(IROCKR,SAT,KRGT,MSAT(IROCKR),SSG,KRG)
      GMW(J,K)=KRW/MUW
      GMO(J,K)=KRO/MUO
C=====================================================
      IF(GMO(J,K).GT.GMOMAX) GMOMAX = GMO(J,K)
C=====================================================
      GMG(J,K)=KRG/MUG
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC      WRITE(*,11701)      J,IQ1,IQ2,K,SSG,KRG,MUG,GMG(J,K)
CCC      WRITE(9,11701)      J,IQ1,IQ2,K,SSG,KRG,MUG,GMG(J,K)
CCC11701 FORMAT(/1X,'J,IQ1,IQ2,K, SSG, KRG, MUG, GMG(J,K):',
CCC     &       /1X,4I3, 3F8.4, G13.5)
CCC      WRITE(*,11703)  SSW, KRW, MUW, GMW(J,K)
CCC      WRITE(9,11703)  SSW, KRW, MUW, GMW(J,K)
CCC11703 FORMAT(1X,'SSW,KRW,MUW,GMW(J,K):',3F8.4,G13.5)
CCC     WRITE(*,11705)  SSO, KRO, MUO, GMO(J,K)
CCC      WRITE(9,11705)  SSO, KRO, MUO, GMO(J,K)
CCC11705 FORMAT(1X,'SSO,KRO,MUO,GMO(J,K):',3F8.4,G13.5)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 1170 CONTINUE

      IF(KIP(J).LT.0) GO TO 105

      IF(KIP(J).NE.1) GO TO 1190
C****** OIL INJECTION FOR SOLUBLE OIL PROCESS.
      IF(QVO(J).LE.-0.001) GO TO 1190
C****** OIL INJECTION CODE CONTINUES AT FORTRAN LINE 1194 BELOW.
      ITERQ=0
      QDENOM=0.0
      ALPHAO=0.0
      ALPHAW=0.0
      ALPHAG=0.0
      BBOSUM=0.0
      LAY=IQ3+(LAYER(J)-1)
 1172 ITERQ=ITERQ+1
      DO 1189 K=IQ3,LAY
      PP=P(IQ1,IQ2,K)
      BPT=PBOT(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PP,BBO)
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PP,BBW)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PP,BBG)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PP,RSO)
      IF(ITERQ.NE.1) GO TO 1174
cccc      QDENOM=QDENOM+PID(J,K)*GMO(J,K)/BBO
c----------------- added the following line - 02/11/94 -------------------
c               (trying to get QVW > 0 to work for KIP=1)
c                 !! Now works perfect !!
      IF(QVO(J).NE.0.0) QDENOM=QDENOM+PID(J,K)*GMO(J,K)/BBO
c-------------------------------------------------------------------------
      IF(QVW(J).NE.0.0) QDENOM=QDENOM+PID(J,K)*GMW(J,K)/BBW
      IF(QVG(J).NE.0.0) QDENOM=QDENOM+PID(J,K)*GMG(J,K)/BBG
      GMT=GMO(J,K)+GMW(J,K)+GMG(J,K)
      ALPHAO=GMO(J,K)/GMT+ALPHAO
      ALPHAW=GMW(J,K)/GMT+ALPHAW
      ALPHAG=GMG(J,K)/GMT+ALPHAG
CCC      BBOSUM=BBOSUM+BBO
      GO TO 1189
 1174 IF(QVT(J).EQ.0.0) GO TO 1176
C** CONVERT INPUT QVT(RB/D) TO QVT(STB/D)  <---- do not want to do this !!
CCC      BBOAVG=BBOSUM/LAYER(J)
C================== BBOAVG is "BUG" on following stm't ===================== 
c....... QVT is at res bbl/day !!!
CCC      TOTOR=(QVT(J)/BBOAVG)*ALPHAO/(ALPHAO+ALPHAW+ALPHAG)
C==================== fixed 3/10/93 - WKS ==================================
      TOTOR = QVT(J)*ALPHAO/(ALPHAO+ALPHAW+ALPHAG)
C
      GO TO 1178
 1176 TOTOR=QVO(J)
 1178 CONTINUE
      IF(QDENOM.EQ.0.0) GO TO 1189
      IF(QVO(J).LE.0.0.AND.QVT(J).LE.0.0) GO TO 1181
      IF(GMO(J,K).EQ.0.0) GO TO 1189
      QOC(IJ,K)=TOTOR*5.615*PID(J,K)*GMO(J,K) / (BBO*QDENOM)
      QWC(IJ,K)=QOC(IJ,K)*GMW(J,K)*BBO / (BBW*GMO(J,K))
      QGC(IJ,K)=QOC(IJ,K)*(GMG(J,K)*BBO/(BBG*GMO(J,K)) + RSO) 
CCCC!!!!!!!!!!!!!!!!!!!!
CCC      WRITE(*,11111) IJ,K,QOC(IJ,K),RSO, QGC(IJ,K)
CCC11111 FORMAT(1X,'IJ,K,QOC(),RSO,QGC():',2I4,3G14.6/)
CCC      PAUSE '.......... in QRATE............'
CCCC!!!!!!!!!!!!!!!!!!!!     
      GO TO 1189
C**WATER PROD RATE SPECIFIED
 1181 CONTINUE
      IF(QVW(J).LE.0.0.OR.GMW(J,K).EQ.0.0) GO TO 1183
      QWC(IJ,K)=QVW(J)*5.615*PID(J,K)*GMW(J,K)
     & /(BBW*QDENOM)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ccc      WRITE(IOCODE,11811)
ccc     & IJ,K,QVW(J),PID(J,K),GMW(J,K),BBW,QDENOM,QWC(IJ,K)
ccc11811 FORMAT(/1X,'IJ,K,QVW(J),PID(J,K),GMW(J,K),BBW,QDENOM,QWC(IJ,K):',
ccc     &       /1X,2I3,6G12.5)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      QOC(IJ,K)=QWC(IJ,K)*GMO(J,K)*BBW
     & /(BBO*GMW(J,K))
      QGC(IJ,K)=QWC(IJ,K)*(GMG(J,K)*BBW
     & /(BBG*GMW(J,K))) + RSO*QOC(IJ,K)
      GO TO 1189
C**GAS PRODUCTION RATE SPECIFIED
 1183 CONTINUE
      IF(QVG(J).LE.0.0.OR.GMG(J,K).EQ.0.0) GO TO 1189
      QGC(IJ,K)=QVG(J)*1000.*PID(J,K)*GMG(J,K)
     & /(BBG*QDENOM)
      QWC(IJ,K)=QGC(IJ,K)*GMW(J,K)*BBG
     & /(BBW*GMG(J,K))
      QOC(IJ,K)=QGC(IJ,K)*GMO(J,K)*BBG
     & /(BBO*GMG(J,K))
 1189 CONTINUE
      IF(ITERQ.EQ.1) GO TO 1172
      GO TO 105

 1190 CONTINUE
      LAY=IQ3+(LAYER(J)-1)
      ITERQ=0
      QDENOM=0

 1192 ITERQ=ITERQ+1
      DO 1200 K=IQ3,LAY
      IF(ITERQ.NE.1) GO TO 1194
      QDENOM=QDENOM+PID(J,K)*(GMO(J,K)+GMW(J,K)+GMG(J,K))
      GO TO 1200
C****** OIL INJECTION FOR SOLUBLE OIL PROCESS.
 1194 IF(QDENOM.EQ.0.0) GO TO 1200
      IF(QVO(J).GE.-0.001) GO TO 1195
      QOC(IJ,K)=QVO(J)*5.615*PID(J,K)*
     & (GMO(J,K)+GMW(J,K)+GMG(J,K))/QDENOM
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(IOCODE,1201)
C     &      J,IJ,K,QVO(J),GMO(J,K),GMW(J,K),GMG(J,K),QDENOM,QOC(IJ,K)
C      WRITE(*,1201)
C     &      J,IJ,K,QVO(J),GMO(J,K),GMW(J,K),GMG(J,K),QDENOM,QOC(IJ,K)
C      WRITE(9,1201)
C     &      J,IJ,K,QVO(J),GMO(J,K),GMW(J,K),GMG(J,K),QDENOM,QOC(IJ,K)
C
C1201  FORMAT(1X,'J,IJ,K,QVO(J), GMO,GMW,GMG, QDENOM, QOC(IJ,K):',
C     &/1X, 3I3, F8.2, 4F12.5, F8.2)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      GO TO 1200
C****** END OF OIL INJECTION.

 1195 IF(KIP(J).NE.2) GO TO 1196
C***** WATER INJECTION RATE SPECIFIED
      QWC(IJ,K)=QVW(J)*5.615*PID(J,K)
     & *(GMO(J,K)+GMW(J,K)+GMG(J,K))/QDENOM
      GO TO 1200
C***** GAS INJECTION RATE SPECIFIED
 1196 QGC(IJ,K)=QVG(J)*1000.*PID(J,K)
     & *(GMO(J,K)+GMW(J,K)+GMG(J,K))/QDENOM
 1200 CONTINUE
      IF(ITERQ.EQ.1) GO TO 1192

105   CONTINUE


C**** PRESSURE CONSTRAINT
      DO 1340 J=1,NVQN
      IF(KIP(J).GE.0) GO TO 1340
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 1340
      LAY=IQ3+(LAYER(J)-1)

      DO 9340 K=IQ3,LAY
      PPN=PN(IQ1,IQ2,K)
      BPT=PBOT(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PPN,BBO)
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PPN,BBW)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PPN,BBG)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PPN,RSO)

C**** OIL PRODUCER
      IF(KIP(J).NE.-1) GO TO 1310
      PPNO = PPN
      PPNW = PPNO - CAPOW(IQ1,IQ2,K)
      PPNG = PPNO + CAPGO(IQ1,IQ2,K) 
      IF(PPN.LE.PWF(J,K)) THEN
         IF(PPNO.LE.PWF(J,K)) QOC(IJ,K)=0.0
         IF(PPNW.LE.PWF(J,K)) QWC(IJ,K)=0.0
         IF(PPNG.LE.PWF(J,K)) QGC(IJ,K)=0.0
      ELSE
         QOC(IJ,K)=PID(J,K)*5.615*GMO(J,K) * (PPNO-PWF(J,K))/BBO
         QWC(IJ,K)=PID(J,K)*5.615*GMW(J,K) * (PPNW-PWF(J,K))/BBW
         QGC(IJ,K)=PID(J,K)*5.615*GMG(J,K) * (PPNG-PWF(J,K))/BBG + 
     &             QOC(IJ,K)*RSO

CCC         QGC(IJ,K)=QOC(IJ,K)*(GMG(J,K)*BBO  /(BBG*GMO(J,K))+RSO)
      ENDIF

CCC      IF(QOC(IJ,K).LE.0.0) GO TO 1305
CCC      QG1=QOC(IJ,K)*(GMG(J,K)*BBO  /(BBG*GMO(J,K))+RSO)
CCC      GO TO 1307
CCC    1305 QG1=0.0
CCC    1307 QGC(IJ,K)=QG1 

      GO TO 9340

C**** WATER INJECTOR
 1310 IF(KIP(J).NE.-2) GO TO 1320
      QWC(IJ,K)=PID(J,K)*5.615*(GMO(J,K)
     & +GMW(J,K)+GMG(J,K))*(PPN-PWF(J,K))/BBW
      IF(PPN.GE.PWF(J,K)) QWC(IJ,K)=0.0
      GO TO 9340
C**** GAS INJECTOR
 1320 IF(KIP(J).NE.-3) GO TO 9340
      QGC(IJ,K)=PID(J,K)*5.615*(GMO(J,K)
     & +GMW(J,K)+GMG(J,K))*(PPN-PWF(J,K))/BBG
      IF(PPN.GE.PWF(J,K)) QGC(IJ,K)=0.0
9340  CONTINUE

1340  CONTINUE

C**** GAS WELL
      DO 1390 J=1,NVQN
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 1390
      IF(KIP(J).NE.-4) GO TO 1390
      LAY=IQ3+(LAYER(J)-1)
      ITERQ=0
      QDENOM=0.0
 1345 ITERQ=ITERQ+1
      DO 1360 K=IQ3,LAY
      PP=P(IQ1,IQ2,K)
      BPT=PBOT(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PP,BBO)
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PP,BBW)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PP,BBG)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PP,RSO)
      IF(ITERQ.NE.1) GO TO 1350
      QDENOM=QDENOM+PID(J,K)*GMG(J,K)/BBG
      GO TO 1360
 1350 CONTINUE
      QOC(IJ,K)=PID(J,K)*5.615*GMO(J,K)
     & *(PP-PWF(J,K))/BBO
      IF(PP.LE.PWF(J,K)) QOC(IJ,K)=0.0
      QWC(IJ,K)=PID(J,K)*5.615*GMW(J,K)
     & *(PP-PWF(J,K))/BBW
      IF(PP.LE.PWF(J,K)) QWC(IJ,K)=0.0
      PWLFLO=PWF(J,K)
      CALL INTERP(IPVTR,PGT,PSIT,MPGT(IPVTR),PP,PSIR)
      CALL INTERP(IPVTR,PGT,PSIT,MPGT(IPVTR),PWLFLO,PSIWF)
      QLIT=0.
      QLITK=0.
      IF(PSIR.LT.PSIWF) GO TO 1355
      IF(QDENOM.EQ.0. ) GO TO 1355
C**  CONVERT MMSCF/D TO SCF/D
      QLIT= (1.0E+6)*(-ALIT(IJ)+SQRT(ALIT(IJ)*ALIT(IJ)
     & +4.*BLIT(IJ)*(PSIR-PSIWF)))/(2.*BLIT(IJ))
      QLITK=QLIT*PID(J,K)*GMG(J,K)/(QDENOM*BBG)
 1355 QGC(IJ,K)=QLITK+RSO*QOC(IJ,K) 
 1360 CONTINUE
      IF(ITERQ.EQ.1) GO TO 1345
 1390 CONTINUE

C**** MIN. OIL PROD. AND MAX. LIQUID WITHDRAWAL CONSTRAINTS.
      DO 1580 J=1,NVQN
      IF(KIP(J).NE.-1) GO TO 1580
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 1580
      LAY=IQ3+(LAYER(J)-1)
      QOT=0.
      QWT=0.
      PIDSUM=0.0
      DO 1510 K=IQ3,LAY
      QOT=QOT+QOC(IJ,K)
      QWT=QWT+QWC(IJ,K)
      PIDSUM=PIDSUM+PID(J,K)
 1510 CONTINUE
C      SKIP MESSAGE IF WELL HAS BEEN SHUT-IN
      IF(PIDSUM.LE.0.0) GO TO 1580
C** IS MIN. OIL PROD. RATE ACHIEVED?
C  5.615 CONVERTS STB TO SCF FOR COMPARISON WITH INTERNAL RATES.
      IF(QOT.GE.QVO(J)*5.615) GO TO 1520
      DO 1515 K=IQ3,LAY
      QOC(IJ,K)=0.
      QWC(IJ,K)=0.
      QGC(IJ,K)=0.
C** SHUT-IN WELL
      PID(J,K)=0.
 1515 CONTINUE
      WRITE(IOCODE,1518) J,IQ1,IQ2,ETI
 1518 FORMAT(/T10,110('-'),/T10,
     & 'MINIMUM OIL RATE NOT ACHIEVED BY WELL #',
     & I3,', AREAL LOCATION',I3,',',I3,' AFTER',F10.2,
     & ' DAYS OF ELAPSED TIME.',/T10,110('-'))
      GO TO 1580
 1520 CONTINUE
C      IS MAX OIL RATE EXCEEDED?
      FAC1=1.0
      IF(QVW(J).LE.0.0) GO TO 1521
      IF(QOT.LE.5.615*QVW(J)) GO TO 1521
      FAC1=5.615*QVW(J)/QOT
 1521 FAC2=1.0
      IF(QVT(J).LE.0.0) GO TO 1522
      QLIQT=(QOT+QWT)*FAC1
C      IS MAX LIQUID WITHDRAWAL RATE EXCEEDED?
      IF(QLIQT.LE.5.615*QVT(J)) GO TO 1522
      FAC2=5.615*QVT(J)/QLIQT
 1522 CONTINUE
      FAC=FAC1*FAC2
      IF(FAC.GE.1.0) GO TO 1540
      DO 1530 K=IQ3,LAY
      QOC(IJ,K)=QOC(IJ,K)*FAC
      QWC(IJ,K)=QWC(IJ,K)*FAC
      PPN=PN(IQ1,IQ2,K)
      BPT=PBOT(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PPN,BBO)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PPN,BBG)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PPN,RSO)
      IF(QOC(IJ,K).LE.0.0) GO TO 1523
      QG1=QOC(IJ,K)*(GMG(J,K)*BBO
     & /(BBG*GMO(J,K))+RSO)
      GO TO 1524
 1523 QG1=0.0
 1524 QGC(IJ,K)=QG1 
 1530 CONTINUE
 1540 CONTINUE
 1580 CONTINUE

C** RATE CONSTRAINTS ON PRESSURE CONTROLLED INJECTION WELLS
      DO 1680 J=1,NVQN
C===========================================================
      IF(KIP(J).NE.-2 .AND. KIP(J).NE.-3) GO TO 1680
C===========================================================
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      LAY=IQ3+(LAYER(J)-1)
      FACW=1.0
      FACG=1.0
C      WATER INJECTION WELL CONSTRAINT
      IF(KIP(J).NE.-2) GO TO 1640
      QWI=0.0
      DO 1600 K=IQ3,LAY
      QWI=QWI+QWC(IJ,K)
 1600 CONTINUE
      IF(QVW(J).GE.0.0) GO TO 1640
      IF(ABS(QWI).LE.ABS(QVW(J))*5.615) GO TO 1640
      FACW=QVW(J)*5.615/QWI
C      GAS INJECTION WELL CONSTRAINT
 1640 CONTINUE
      IF(KIP(J).NE.-3) GO TO 1660
      QGI=0.0
      DO 1650 K=IQ3,LAY
      QGI=QGI+QGC(IJ,K)
 1650 CONTINUE
      IF(QVG(J).GE.0.0) GO TO 1660
      IF(ABS(QGI).LE.ABS(QVG(J))*1000.) GO TO 1660
      FACG=QVG(J)*1000./QGI
 1660 CONTINUE
      IF(FACW.GE.1.0.AND.FACG.GE.1.0) GO TO 1680
      DO 1670 K=IQ3,LAY
      QWC(IJ,K)=QWC(IJ,K)*FACW
      QGC(IJ,K)=QGC(IJ,K)*FACG
 1670 CONTINUE
 1680 CONTINUE

C** GOR AND WOR CONSTRAINTS
C=================================================
      IF(GMOMAX.LE.1.E-06) GO TO 5002
C=================================================
      DO 5000 J=1,NVQN
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 5000
      LAY=IQ3+(LAYER(J)-1)
      IF(ILIMOP(J).EQ.0.OR.KIP(J).LT.-10) GO TO 5000
 4001 CONTINUE
      QOT=0.
      QWT=0.
      QGT=0.
      GOR=0.0
      WOR=0.0
      DO 4010 K=IQ3,LAY
      QOT=QOT+QOC(IJ,K)
      QWT=QWT+QWC(IJ,K)
      QGT=QGT+QGC(IJ,K)
 4010 CONTINUE
      IF(QOT.EQ.0.0) GO TO 4100
      GOR=QGT*5.615/QOT
      WOR=QWT/QOT
 4100 CONTINUE
C** GOR CONSTRAINTS
      IF(GOR.LE.GORT(J)) GO TO 4150
      DO 4110 K=IQ3,LAY
      IF(QOC(IJ,K).NE.0.0) GO TO 4105
      PID(J,K)=0.0
      QWC(IJ,K)=0.0
      QGC(IJ,K)=0.0
      GORL(K)=0.0
      GO TO 4110
 4105 GORL(K)=QGC(IJ,K)*5.615/QOC(IJ,K)
 4110 CONTINUE
C** FIND LAYER WITH MAX. GOR
      GORSI=GORL(IQ3)
      KMAX=IQ3
      DO 4120 K=IQ3,LAY
      IF(GORL(K).LE.GORSI) GO TO 4120
      GORSI=GORL(K)
      KMAX=K
 4120 CONTINUE
C** SHUT-IN LAYER WITH MAX. GOR
      PID(J,KMAX)=0.0
      QOC(IJ,KMAX)=0.0
      QWC(IJ,KMAX)=0.0
      QGC(IJ,KMAX)=0.0
      WRITE(IOCODE,4130) KMAX,J,IQ1,IQ2,ETI
 4130 FORMAT(/T10,110('-'),/T10,
     & 'GOR LIMIT EXCEEDED BY LAYER K =',I3,', WELL #',I3,
     & ', AREAL LOCATION'I3,',',I3,' AFTER',F10.2,
     & ' DAYS OF ELAPSED TIME.',/T10,110('-'))
C** REPEAT PROCEDURE
      GO TO 4001
 4150 CONTINUE
C** WOR CONSTRAINTS
      IF(WOR.LE.WORT(J)) GO TO 4250
      DO 4210 K=IQ3,LAY
      IF(QOC(IJ,K).NE.0.0) GO TO 4205
      PID(J,K)=0.0
      QWC(IJ,K)=0.0
      QGC(IJ,K)=0.0
      WORL(K)=0.0
      GO TO 4210
 4205 WORL(K)=QWC(IJ,K)/QOC(IJ,K)
 4210 CONTINUE
C** FIND LAYER WITH MAX. WOR
      WORSI=WORL(LAY)
      KMAX=LAY
      DO 4220 K=IQ3,LAY
      IF(WORL(K).LT.WORSI) GO TO 4220
      WORSI=WORL(K)
      KMAX=K
 4220 CONTINUE
C** SHUT-IN LAYER WITH MAX. WOR
      PID(J,KMAX)=0.0
      QOC(IJ,KMAX)=0.0
      QWC(IJ,KMAX)=0.0
      QGC(IJ,KMAX)=0.0
      WRITE(IOCODE,4230) KMAX,J,IQ1,IQ2,ETI
 4230 FORMAT(/T10,110('-'),/T10,
     & 'WOR LIMIT EXCEEDED BY LAYER K =',I3,', WELL #',I3,
     & ', AREAL LOCATION'I3,',',I3,' AFTER',F10.2,
     & ' DAYS OF ELAPSED TIME.',/T10,110('-'))
C** REPEAT PROCEDURE
      GO TO 4001
 4250 CONTINUE
 5000 CONTINUE
C========================
 5002 CONTINUE
C========================

C***** CALCULATE BOTTOM-HOLE FLOWING PRESSURE
      DO 5010 J=1,NVQN
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 5010
      IF(KIP(J).LT.-10) GO TO 5010
      LAY=IQ3+(LAYER(J)-1)
      DO 5005 K=IQ3,LAY
      PWFC(J,K)=0.0
      IF(PID(J,K).LE.0.0001) GO TO 5005
      PP=P(IQ1,IQ2,K)
      IF(PP.LE.0.0) GO TO 5005
      BPT=PBOT(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PP,BBO)
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PP,BBW)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PP,BBG)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PP,RSO)
      FAC=PID(J,K)*5.615
      GMTB=GMO(J,K)/BBO+GMW(J,K)/BBW+GMG(J,K)/BBG
      SOLN=RSO*QOC(IJ,K) 
      QT=QOC(IJ,K)+QWC(IJ,K)+QGC(IJ,K)
      PWFC(J,K)=PP-(QT-SOLN)/(FAC*GMTB)
 5005 CONTINUE
 5010 CONTINUE

C** TOTAL SOURCE/SINK TERMS BY GRID BLOCK (EXCEPT IMPLICIT RATES)
      DO 5200 J=1,NVQN
      IF(KIP(J).LT.-10) GO TO 5200
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 5200
      LAY=IQ3+(LAYER(J)-1)
      DO 5100 K=IQ3,LAY
      QO(IQ1,IQ2,K)=QO(IQ1,IQ2,K)+QOC(IJ,K)
      QW(IQ1,IQ2,K)=QW(IQ1,IQ2,K)+QWC(IJ,K)
      QG(IQ1,IQ2,K)=QG(IQ1,IQ2,K)+QGC(IJ,K)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC      WRITE(IOCODE,5101) J,IQ1,IQ2,K, QWC(IJ,K), QW(IQ1,IQ2,K)
CCC      WRITE(*,5101)      J,IQ1,IQ2,K, QWC(IJ,K), QW(IQ1,IQ2,K)
CCC      WRITE(9,5101)      J,IQ1,IQ2,K, QWC(IJ,K), QW(IQ1,IQ2,K)
CCC5101  FORMAT(1X,'QRATE:5200-LOOP: IQ1,IQ2,K,QWC(IJ,K),QW(IQ1,IQ2,K):',
CCC     &      /1X,4I3,2F10.2)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 5100 CONTINUE
 5200 CONTINUE
      RETURN
      END


      SUBROUTINE PRATEO(RKROMX,QGRSOT,QGRSO,DRSODP,CAPOW,CAPGO,NVQN)

      INCLUDE 'PARAMS.FOR'

C=======================================================================
      DIMENSION DRSODP(LP1,LP2,LP3),COFRSO(LP1,LP2,LP3),
     &                               QGRSO(LP1,LP2,LP3),
     &                               CAPOW(LP1,LP2,LP3),
     &                               CAPGO(LP1,LP2,LP3)

C=======================================================================


      COMMON /SSOLN/ BO(LP1,LP2,LP3),BW(LP1,LP2,LP3),BG(LP1,LP2,LP3),
     & QO(LP1,LP2,LP3),QW(LP1,LP2,LP3),QG(LP1,LP2,LP3),
     & GOWT(LP1,LP2,LP3),GWWT(LP1,LP2,LP3),GGWT(LP1,LP2,LP3),
     & O1(LP4,LP2,LP3),W1(LP4,LP2,LP3),
     & O2(LP1,LP5,LP3),W2(LP1,LP5,LP3),
     & O3(LP1,LP2,LP6),W3(LP1,LP2,LP6),
     & QOWG(LP1,LP2,LP3),VP(LP1,LP2,LP3),CT(LP1,LP2,LP3)


      COMMON /SLIMIT/ GORT(LP11),WORT(LP11),ILIMOP(LP11),
     & GORL(LP11),WORL(LP11),QOC(LP11,LP3),QWC(LP11,LP3),QGC(LP11,LP3)


      COMMON /SPVT/ SAT(LP7,LP9),KROT(LP7,LP9),KRWT(LP7,LP9),
     & BGT(LP7,LP9),
     & KRGT(LP7,LP9),ITHREE(LP7),RSOT(LP7,LP9),BWPT(LP7,LP9),
     & PCOWT(LP7,LP9),PCGOT(LP7,LP9),KROGT(LP7,LP9),SWR(LP7),
     & POT(LP7,LP9),MUOT(LP7,LP9),BOT(LP7,LP9),BOPT(LP7,LP9),
     & RSOPT(LP7,LP9),PWT(LP7,LP9),MUWT(LP7,LP9),BWT(LP7,LP9),
     & PGT(LP7,LP9),MUGT(LP7,LP9),
     & BGPT(LP7,LP9),CRT(LP7,LP9),IPVT(LP1,LP2,LP3),IROCK(LP1,LP2,LP3),
     & NROCK,NPVT,PSIT(LP7,LP9),PRT(LP7,LP9),WOROCK(LP7),GOROCK(LP7)


      COMMON /BUBBLE/ PBO,VSLOPE(LP8),BSLOPE(LP8),RSLOPE(LP8),PMAXT,
     & IREPRS,MPGT(LP8),
     & RHOSCO(LP8),RHOSCW(LP8),RHOSCG(LP8),MSAT(LP7),MPOT(LP8),
     & MPWT(LP8),PBOT(LP1,LP2,LP3),PBOTN(LP1,LP2,LP3)

      COMMON /SRATE/ PID(LP11,LP3),PWF(LP11,LP3),PWFC(LP11,LP3),
     & KIP(LP11),LAYER(LP11),QVO(LP11),CUMG(LP11,LP3),
     & GMO(LP11,LP3),GMW(LP11,LP3),GMG(LP11,LP3),
     & QVW(LP11),QVG(LP11),QVT(LP11),CUMO(LP11,LP3),CUMW(LP11,LP3),
     &  IDWELL(LP11),ALIT(LP11),BLIT(LP11)


      COMMON /VECTOR/ DX(LP1,LP2,LP3),DY(LP1,LP2,LP3),DZ(LP1,LP2,LP3),
     & DZNET(LP1,LP2,LP3),IQN1(LP11),IQN2(LP11),IQN3(LP11),IHEDIN(80)

      COMMON /SPRTPS/ P(LP1,LP2,LP3),SO(LP1,LP2,LP3),SW(LP1,LP2,LP3),
     & SG(LP1,LP2,LP3)

      COMMON /SARRAY/ PN(LP1,LP2,LP3),IOCODE,IDMAX,
     & SON(LP1,LP2,LP3),SWN(LP1,LP2,LP3),SGN(LP1,LP2,LP3),
     & A1(LP1,LP2,LP3),A2(LP1,LP2,LP3),A3(LP1,LP2,LP3),
     & SUM(LP1,LP2,LP3),GAM(LP1,LP2,LP3),QS(LP1,LP2,LP3)

C======================================================================
      IF(RKROMX.LE.1.E-06) THEN
C.... Calculate implicit gas source terms due to evolved gas from immobile oil
         QGRSOT = 0.0
         DO 20510 K=1,KK
         DO 20510 J=1,JJ
         DO 20510 I=1,II
         PPN = PN(I,J,K)
         PP  = P(I,J,K)
         QGRSO(I,J,K)  =  0.001 * COFRSO(I,J,K) * (PPN - PP)
         QGRSOT = QGRSOT + QGRSO(I,J,K)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC         IF(I.EQ.1 .AND. J.EQ.1) THEN
CCC           WRITE(IOCODE,20571) NLOOP,
CCC     &        I,J,K, DRSODP(I,J,K),COFRSO(I,J,K), PPN, PP, QGRSO(I,J,K)
CCC20571         FORMAT(/1X,4I3,5G12.5)
CCC         ENDIF
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
20510    CONTINUE
      ENDIF
C======================================================================

      DO 2059 J=1,NVQN
      IF(KIP(J).GE.-10) GO TO 2059
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 2059
      LAY=IQ3+(LAYER(J)-1)

      DO 2057 K=IQ3,LAY
      PP=P(IQ1,IQ2,K)
      PPN=PN(IQ1,IQ2,K)
      BPT=PBOT(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PPN,RSON)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PP,RSO)
      RSOAV=0.5*(RSO+RSON)
      FACTO = PID(J,K)*5.615*(PP - PWF(J,K))
      FACTW = PID(J,K)*5.615*(PP - CAPOW(IQ1,IQ2,K) - PWF(J,K))
      FACTG = PID(J,K)*5.615*(PP + CAPGO(IQ1,IQ2,K) - PWF(J,K))

CCC      FACTOR=PID(J,K)*5.615*(PP-PWF(J,K))

      IF(KIP(J).EQ.-13) GO TO 2053

C............ KIP = -11 ................................................
      QWC(J,K)=GMW(J,K)/BW(IQ1,IQ2,K)*FACTW

CCC      QWC(J,K)=GMW(J,K)/BW(IQ1,IQ2,K)*FACTOR
C.......................................................................
C====== Modified following stm't - 3/10/93 (removed GMO() & GMG()) =======
ccc -----> Bug fix - 03/05/95 per Ted Bourgoyne - (ch'd FACTOR to FACTW - this
ccc                                                is consistent with GPBOS)
ccc                                                
CCC      IF(KIP(J).EQ.-12) QWC(J,K) = GMW(J,K)/BW(IQ1,IQ2,K)*FACTOR
      IF(KIP(J).EQ.-12) QWC(J,K) = GMW(J,K)/BW(IQ1,IQ2,K)*FACTW
      IF(KIP(J).EQ.-12) GO TO 2057

C.............KIP = -11 ................................................
      QOC(J,K)=GMO(J,K)/BO(IQ1,IQ2,K)*FACTO
      QGC(J,K)=GMG(J,K)/BG(IQ1,IQ2,K)*FACTG 
     &         +RSOAV*QOC(J,K) 

CCC      QOC(J,K)=GMO(J,K)/BO(IQ1,IQ2,K)*FACTOR
CCC      QGC(J,K)=GMG(J,K)/BG(IQ1,IQ2,K)*FACTOR
CCC     &         +RSOAV*QOC(J,K) 
C.......................................................................

      GO TO 2057

 2053 QGC(J,K)=(GMO(J,K)+GMW(J,K)+GMG(J,K)) / BG(IQ1,IQ2,K)*FACTG

 2057 CONTINUE
 2059 CONTINUE
C** TOTAL SOURCE/SINK TERMS BY GRID BLOCK INCLUDING IMPLICIT RATES.
      DO 2200 J=1,NVQN
      IF(KIP(J).GE.-10) GO TO 2200
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJ=IDWELL(J)
      IF(IJ.EQ.0) GO TO 2200
      LAY=IQ3+(LAYER(J)-1)
      DO 2100 K=IQ3,LAY
      QO(IQ1,IQ2,K)=QO(IQ1,IQ2,K)+QOC(J,K)
      QW(IQ1,IQ2,K)=QW(IQ1,IQ2,K)+QWC(J,K)
      QG(IQ1,IQ2,K)=QG(IQ1,IQ2,K)+QGC(J,K)
 2100 CONTINUE
 2200 CONTINUE
      RETURN
      END





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc


C     IMPLICIT PRESSURE RATE

      SUBROUTINE PRATEI(II,JJ,KK,RKROMX,DELT,CAPGO,CAPOW,QGRSOT,DRSODP,
     & NVQN)

      INCLUDE 'PARAMS.FOR'

      DIMENSION CAPGO(LP1,LP2,LP3), CAPOW(LP1,LP2,LP3)

      COMMON /VECTOR/ DX(LP1,LP2,LP3),DY(LP1,LP2,LP3),DZ(LP1,LP2,LP3),
     & DZNET(LP1,LP2,LP3),IQN1(LP11),IQN2(LP11),IQN3(LP11),IHEDIN(80)

      COMMON /SPVT/ SAT(LP7,LP9),KROT(LP7,LP9),KRWT(LP7,LP9),
     & BGT(LP7,LP9),
     & KRGT(LP7,LP9),ITHREE(LP7),RSOT(LP7,LP9),BWPT(LP7,LP9),
     & PCOWT(LP7,LP9),PCGOT(LP7,LP9),KROGT(LP7,LP9),SWR(LP7),
     & POT(LP7,LP9),MUOT(LP7,LP9),BOT(LP7,LP9),BOPT(LP7,LP9),
     & RSOPT(LP7,LP9),PWT(LP7,LP9),MUWT(LP7,LP9),BWT(LP7,LP9),
     & PGT(LP7,LP9),MUGT(LP7,LP9),
     & BGPT(LP7,LP9),CRT(LP7,LP9),IPVT(LP1,LP2,LP3),IROCK(LP1,LP2,LP3),
     & NROCK,NPVT,PSIT(LP7,LP9),PRT(LP7,LP9),WOROCK(LP7),GOROCK(LP7)


      COMMON /BUBBLE/ PBO,VSLOPE(LP8),BSLOPE(LP8),RSLOPE(LP8),PMAXT,
     & IREPRS,MPGT(LP8),
     & RHOSCO(LP8),RHOSCW(LP8),RHOSCG(LP8),MSAT(LP7),MPOT(LP8),
     & MPWT(LP8),PBOT(LP1,LP2,LP3),PBOTN(LP1,LP2,LP3)

      COMMON /COEF/ AW(LP1,LP2,LP3),AE(LP1,LP2,LP3),AN(LP1,LP2,LP3),
     & AS(LP1,LP2,LP3),AB(LP1,LP2,LP3),AT(LP1,LP2,LP3),E(LP1,LP2,LP3),
     & B(LP1,LP2,LP3)

      COMMON /SARRAY/ PN(LP1,LP2,LP3),IOCODE,IDMAX,
     & SON(LP1,LP2,LP3),SWN(LP1,LP2,LP3),SGN(LP1,LP2,LP3),
     & A1(LP1,LP2,LP3),A2(LP1,LP2,LP3),A3(LP1,LP2,LP3),
     & SUM(LP1,LP2,LP3),GAM(LP1,LP2,LP3),QS(LP1,LP2,LP3)

      COMMON /SSOLN/ BO(LP1,LP2,LP3),BW(LP1,LP2,LP3),BG(LP1,LP2,LP3),
     & QO(LP1,LP2,LP3),QW(LP1,LP2,LP3),QG(LP1,LP2,LP3),
     & GOWT(LP1,LP2,LP3),GWWT(LP1,LP2,LP3),GGWT(LP1,LP2,LP3),
     & O1(LP4,LP2,LP3),W1(LP4,LP2,LP3),
     & O2(LP1,LP5,LP3),W2(LP1,LP5,LP3),
     & O3(LP1,LP2,LP6),W3(LP1,LP2,LP6),
     & QOWG(LP1,LP2,LP3),VP(LP1,LP2,LP3),CT(LP1,LP2,LP3)

      DIMENSION DRSODP(LP1,LP2,LP3),COFRSO(LP1,LP2,LP3),
     &                               QGRSO(LP1,LP2,LP3)

      COMMON /SPRTPS/ P(LP1,LP2,LP3),SO(LP1,LP2,LP3),SW(LP1,LP2,LP3),
     & SG(LP1,LP2,LP3)

      COMMON /SRATE/ PID(LP11,LP3),PWF(LP11,LP3),PWFC(LP11,LP3),
     & KIP(LP11),LAYER(LP11),QVO(LP11),CUMG(LP11,LP3),
     & GMO(LP11,LP3),GMW(LP11,LP3),GMG(LP11,LP3),
     & QVW(LP11),QVG(LP11),QVT(LP11),CUMO(LP11,LP3),CUMW(LP11,LP3),
     &  IDWELL(LP11),ALIT(LP11),BLIT(LP11)


C==================================================
      IF(RKROMX.LE.1.E-06) THEN
C.... Set up fully implicit gas source terms from immobile oil
         DO 52010 K=1,KK
         DO 52010 J=1,JJ
         DO 52010 I=1,II
         PPN = PN(I,J,K)
         BGN = BG(I,J,K)
         FACRSO        =  VP(I,J,K)*SO(I,J,K)/DELT 
         COFRSO(I,J,K) =  FACRSO * DRSODP(I,J,K)

CCC         WRITE(IOCODE,20311) I,J,K, FACRSO,DRSODP(I,J,K),COFRSO(I,J,K)
CCC20311    FORMAT(1X,'I,J,K,T,FACRSO,DRSODP(),COFRSO():',3I4,4G12.5)

C.....   Note:  Entire gas equation is multiplied by Bg to get pressure eq.
         B(I,J,K) = B(I,J,K) - BGN*COFRSO(I,J,K)*PPN
         E(I,J,K) = E(I,J,K) - BGN*COFRSO(I,J,K)
52010    CONTINUE
      ENDIF
C==================================================

      DO 205 J=1,NVQN
      IF(KIP(J).GE.-10) GO TO 205
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      LAY=IQ3+(LAYER(J)-1)

      DO 203 K=IQ3,LAY
      P56=PID(J,K)*5.615
      PPN=PN(IQ1,IQ2,K)
      BPT=PBOT(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PPN,BBO)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PPN,RSO)
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PPN,BBW)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PPN,BBG)

      CPIO=GMO(J,K)*P56 * (BBO-BBG*RSO)/BBO
      CPIW=GMW(J,K)*P56 
      CPIG=GMG(J,K)*P56

      IF(KIP(J).EQ.-11) THEN
         CPI = CPIO+CPIW+CPIG
         CPICON = CPIO*PWF(J,K) + CPIW*(PWF(J,K) + CAPOW(IQ1,IQ2,K))
     &                          + CPIG*(PWF(J,K) - CAPGO(IQ1,IQ2,K))
      ENDIF
      IF(KIP(J).EQ.-12) THEN
         CPI    = CPIW
         CPICON = CPIW*(PWF(J,K) + CAPOW(IQ1,IQ2,K))
      ENDIF

      IF(KIP(J).EQ.-13) THEN
         CPI    = CPIG
         CPICON = CPIG*(PWF(J,K) - CAPGO(IQ1,IQ2,K))
      ENDIF
C=========================================================================
C      BOLD = B(IQ1,IQ2,K)
C      EOLD = E(IQ1,IQ2,K) 

      B(IQ1,IQ2,K)=B(IQ1,IQ2,K) - CPICON 
      E(IQ1,IQ2,K)=E(IQ1,IQ2,K) - CPI

CC      WRITE(*,20009) IQ1,IQ2,K,BOLD,EOLD,PWF(J,K),CPI,B(IQ1,IQ2,K),
CC     &                                                E(IQ1,IQ2,K)
CC20009 FORMAT(/1X,'PRATEI: IQ1,IQ2,K,BOLD,EOLD,PWF(),CPI,B(),E():',
CC     &       /1X,3I3,6G13.5/)
CC      PAUSE 'BIII: QRATE.....'
  203 CONTINUE
  205 CONTINUE
      RETURN
      END
