C.... Subroutine to calculate solution matrix ..........
C.... RSW removed by WKS ------------------------------------- 12/08/91
C.... RSO changed to upstream using array RSOA by WKS -------- 12/08/91
C.... Arrays VISO,VISW,VISG,RHOO,RHOW,RHOG added by WKS ------ 12/08/91

C.... Working backwards to get "SOLMAT" from GPBOS to work in "BOAST"
C.... 
      SUBROUTINE SOLWKS(NLOOP,II,JJ,KK,DIV1,D288,KSM,KSM1,N,NN,KCOFF,
     & RPW,RPG, RPO,RSOA,VISO,VISW,VISG,RHOO,RHOW,RHOG,KR3,
     & CAPGO,CAPOW)

      INCLUDE 'PARAMS.FOR'

      REAL KROT,KROGT,KRWT,KRGT,MUOT,MUWT,MUGT,KX,KY,KZ
      REAL MUO4,MUW4,MUG4,MUO5,MUW5,MUG5,MUO6,MUW6,MUG6
     &,KRO,KRW,KRG
     &,KRO1,KRW1,KRG1,KRO2,KRW2,KRG2,KRO3,KRW3,KRG3
     &,MUO1,MUW1,MUG1,MUO2,MUW2,MUG2,MUO3,MUW3,MUG3
     &,KRO4,KRW4,KRG4,KRO5,KRW5,KRG5,KRO6,KRW6,KRG6
     &,MO1,MW1,MG1,MO2,MW2,MG2,MO3,MW3,MG3
     &,MO4,MW4,MG4,MO5,MW5,MG5,MO6,MW6,MG6
     &,MUO,MUW,MUG

      DIMENSION RSOA(LP1,LP2,LP3)

      DIMENSION VISO(LP1,LP2,LP3),VISW(LP1,LP2,LP3),VISG(LP1,LP2,LP3),
     &          RHOO(LP1,LP2,LP3),RHOW(LP1,LP2,LP3),RHOG(LP1,LP2,LP3)

      DIMENSION RPW(LP1,LP2,LP3),RPG(LP1,LP2,LP3),RPO(LP1,LP2,LP3),
     & KR3(LP1,LP2,LP3), CAPOW(LP1,LP2,LP3),CAPGO(LP1,LP2,LP3)

       COMMON /BUBBLE/ PBO,VSLOPE(LP8),BSLOPE(LP8),RSLOPE(LP8),
     & PMAXT,IREPRS,MPGT(LP8),
     & RHOSCO(LP8),RHOSCW(LP8),RHOSCG(LP8),MSAT(LP7),MPOT(LP8),
     & MPWT(LP8),PBOT(LP1,LP2,LP3),PBOTN(LP1,LP2,LP3)

      COMMON /COEF/ AW(LP1,LP2,LP3),AE(LP1,LP2,LP3),AN(LP1,LP2,LP3),
     & AS(LP1,LP2,LP3),AB(LP1,LP2,LP3),AT(LP1,LP2,LP3),E(LP1,LP2,LP3),
     & B(LP1,LP2,LP3)

      COMMON /SARRAY/ PN(LP1,LP2,LP3),IOCODE,IDMAX,
     & SON(LP1,LP2,LP3),SWN(LP1,LP2,LP3),SGN(LP1,LP2,LP3),
     & A1(LP1,LP2,LP3),A2(LP1,LP2,LP3),A3(LP1,LP2,LP3),
     & SUM(LP1,LP2,LP3),GAM(LP1,LP2,LP3),QS(LP1,LP2,LP3)

CCC      COMMON /SPARM/ KX(LP1,LP2,LP3),KY(LP1,LP2,LP3),KZ(LP1,LP2,LP3),
CCC     &   EL(LP1,LP2,LP3),TOP(LP1,LP2,LP3),PDAT(LP1,LP2,LP3),
CCC     &   TX(LP1M,LP2,LP3),TY(LP1,LP2M,LP3),TZ(LP1,LP2,LP3M)

      COMMON /SPARM/ KX(LP1,LP2,LP3),KY(LP1,LP2,LP3),KZ(LP1,LP2,LP3),
     & EL(LP1,LP2,LP3),TX(LP4,LP2,LP3),TY(LP1,LP5,LP3),TZ(LP1,LP2,LP6),
     & PDAT(LP1,LP2,LP3),PDATUM,GRAD

      COMMON /SPRTPS/ P(LP1,LP2,LP3),SO(LP1,LP2,LP3),SW(LP1,LP2,LP3),
     & SG(LP1,LP2,LP3)

      COMMON /SPVT/ SAT(LP7,LP9),KROT(LP7,LP9),KRWT(LP7,LP9),
     & BGT(LP7,LP9),
     & KRGT(LP7,LP9),ITHREE(LP7),RSOT(LP7,LP9),BWPT(LP7,LP9),
     & PCOWT(LP7,LP9),PCGOT(LP7,LP9),KROGT(LP7,LP9),SWR(LP7),
     & POT(LP7,LP9),MUOT(LP7,LP9),BOT(LP7,LP9),BOPT(LP7,LP9),
     & RSOPT(LP7,LP9),PWT(LP7,LP9),MUWT(LP7,LP9),BWT(LP7,LP9),
     &                              PGT(LP7,LP9),MUGT(LP7,LP9),
     & BGPT(LP7,LP9),CRT(LP7,LP9),IPVT(LP1,LP2,LP3),IROCK(LP1,LP2,LP3),
     & NROCK,NPVT,PSIT(LP7,LP9),PRT(LP7,LP9),WOROCK(LP7),GOROCK(LP7)

      COMMON /SSOLN/ BO(LP1,LP2,LP3),BW(LP1,LP2,LP3),BG(LP1,LP2,LP3),
     & QO(LP1,LP2,LP3),QW(LP1,LP2,LP3),QG(LP1,LP2,LP3),
     & GOWT(LP1,LP2,LP3),GWWT(LP1,LP2,LP3),GGWT(LP1,LP2,LP3),
     & O1(LP4,LP2,LP3),W1(LP4,LP2,LP3),
     & O2(LP1,LP5,LP3),W2(LP1,LP5,LP3),
     & O3(LP1,LP2,LP6),W3(LP1,LP2,LP6),
     & QOWG(LP1,LP2,LP3),VP(LP1,LP2,LP3),CT(LP1,LP2,LP3)

      COMMON /VECTOR/ DX(LP1,LP2,LP3),DY(LP1,LP2,LP3),DZ(LP1,LP2,LP3),
     & DZNET(LP1,LP2,LP3),IQN1(LP11),IQN2(LP11),IQN3(LP11),IHEDIN(80)

      DATA RSO1,RSO2,RSO3,RSO4,RSO5,RSO6/6*0.0/

      DO 2000 K=1,KK
      DO 2000 J=1,JJ
      DO 2000 I=1,II
      RPW(I,J,K)   = 0.0
      RPG(I,J,K)   = 0.0
      RPO(I,J,K)   = 0.0
      CAPOW(I,J,K) = 0.0
      CAPGO(I,J,K) = 0.0
      IF(VP(I,J,K).LE.0.0) GO TO 2000
      SSO = SO(I,J,K)
      SSW = SW(I,J,K)
      SSG = SG(I,J,K)

      IROCKB=IROCK(I,J,K)

      IF(KR3(I,J,K).NE.0) THEN
         CALL TRIKRO(IROCKB,SSO,SSW,KRO,KRW,KRG)
         RPO(I,J,K)=KRO
         RPW(I,J,K)=KRW
         RPG(I,J,K)=KRG
      ENDIF

      IF(KR3(I,J,K).EQ.0) THEN
         CALL INTERP(IROCKB,SAT,KROT,MSAT(IROCKB),SSO,KRO)
         RPO(I,J,K)=KRO
         CALL INTERP(IROCKB,SAT,KRWT,MSAT(IROCKB),SSW,KRW)
         RPW(I,J,K)=KRW
         CALL INTERP(IROCKB,SAT,KRGT,MSAT(IROCKB),SSG,KRG)
         RPG(I,J,K)=KRG
      ENDIF

      CALL INTERP(IROCKB,SAT,PCOWT,MSAT(IROCKB),SSW,PCOW)
      CAPOW(I,J,K)=PCOW

      CALL INTERP(IROCKB,SAT,PCGOT,MSAT(IROCKB),SSG,PCGO)
      CAPGO(I,J,K)=PCGO

 2000 CONTINUE

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC      WRITE(*,11111)
CCC11111 FORMAT(////)
CCC      PAUSE 'END OF 2000 LOOP IN SOLWKS'
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      DO 200 K=1,KK
      DO 200 J=1,JJ
      DO 200 I=1,II

      IF(VP(I,J,K).LE.1.E-06) GO TO 200

C.... Set values at reference cell .....................................
      PP  = P(I,J,K)
      SSO = SO(I,J,K)
      SSW = SW(I,J,K)
      SSG = SG(I,J,K)

      RSO = RSOA(I,J,K)

      MUO = VISO(I,J,K)
      MUW = VISW(I,J,K)
      MUG = VISG(I,J,K)

      RO  = RHOO(I,J,K)
      RW  = RHOW(I,J,K)
      RG  = RHOG(I,J,K)

      PCOW = CAPOW(I,J,K)
      PCGO = CAPGO(I,J,K)

      IF(I.EQ.1) GO TO 115

      IF(VP(I-1,J,K).LE.1.E-06) GO TO 115

C.... Set values at (i-1,j,k) cell .....................................
      P1   = P(I-1,J,K)

      RSO1 = RSOA(I-1,J,K)

      MUO1 = VISO(I-1,J,K)
      MUW1 = VISW(I-1,J,K)
      MUG1 = VISG(I-1,J,K)

      RO1  = RHOO(I-1,J,K) 
      RW1  = RHOW(I-1,J,K)
      RG1  = RHOG(I-1,J,K)

      PCOW1 = CAPOW(I-1,J,K)
      PCGO1 = CAPGO(I-1,J,K)


      FACT = -D288*(EL(I-1,J,K)-EL(I,J,K))

      GOW1=(RO1+RO)*FACT
      GWW1=(RW1+RW)*FACT + PCOW-PCOW1
      GGW1=(RG1+RG)*FACT + PCGO1-PCGO

      P11 = P1-PP

      HO1 = P11+GOW1
      HW1 = P11+GWW1
      HG1 = P11+GGW1

C.... Determine upstream rel perms & Rso ..........
      IF(HO1.LT.0.0) THEN
         KRO1=RPO(I,J,K)
         RSOW=RSOA(I,J,K)
      ELSE
         KRO1=RPO(I-1,J,K)
         RSOW=RSOA(I-1,J,K)
      ENDIF

      KRW1=RPW(I,J,K)
      IF(HW1.GE.0.) KRW1=RPW(I-1,J,K)

      KRG1=RPG(I,J,K)
      IF(HG1.GE.0.) KRG1=RPG(I-1,J,K)

      MO1 = 4.0*KRO1/((BO(I-1,J,K)+BO(I,J,K)) * (MUO1+MUO))
      MW1 = 4.0*KRW1/((BW(I-1,J,K)+BW(I,J,K)) * (MUW1+MUW))
      MG1 = 4.0*KRG1/((BG(I-1,J,K)+BG(I,J,K)) * (MUG1+MUG))

115   AOW = TX(I,J,K)*MO1
      AWW = TX(I,J,K)*MW1
      AGW = TX(I,J,K)*MG1

      IF(I.EQ.II) GO TO 125
      IF(VP(I+1,J,K).LE.1.E-06) GO TO 125

C.... Set values at (i+1,j,k) cell .....................................
      P2  = P(I+1,J,K)

      RSO2 = RSOA(I+1,J,K)

      MUO2 = VISO(I+1,J,K)
      MUW2 = VISW(I+1,J,K)
      MUG2 = VISG(I+1,J,K)

      RO2  = RHOO(I+1,J,K)
      RW2  = RHOW(I+1,J,K)
      RG2  = RHOG(I+1,J,K)

      PCOW2 = CAPOW(I+1,J,K)
      PCGO2 = CAPGO(I+1,J,K)


      FACT = -D288*(EL(I+1,J,K)-EL(I,J,K))

      GOW2 = (RO2+RO)*FACT
      GWW2 = (RW2+RW)*FACT + PCOW-PCOW2
      GGW2 = (RG2+RG)*FACT + PCGO2-PCGO

      P22 = P2-PP
      HO2 = P22+GOW2
      HW2 = P22+GWW2
      HG2 = P22+GGW2

      IF(HO2.LT.0.0) THEN
         KRO2=RPO(I,J,K)
         RSOE=RSOA(I,J,K)
      ELSE
         KRO2=RPO(I+1,J,K)
         RSOE=RSOA(I+1,J,K)
      ENDIF

      KRW2=RPW(I,J,K)
      IF(HW2.GE.0.) KRW2=RPW(I+1,J,K)

      KRG2=RPG(I,J,K)
      IF(HG2.GE.0.) KRG2=RPG(I+1,J,K)

      MO2 = 4.0*KRO2/((BO(I+1,J,K)+BO(I,J,K)) * (MUO2+MUO))
      MW2 = 4.0*KRW2/((BW(I+1,J,K)+BW(I,J,K)) * (MUW2+MUW))
      MG2 = 4.0*KRG2/((BG(I+1,J,K)+BG(I,J,K)) * (MUG2+MUG))

125   AOE = TX(I+1,J,K)*MO2
      AWE = TX(I+1,J,K)*MW2
      AGE = TX(I+1,J,K)*MG2

      IF(J.EQ.1) GO TO 135
      IF(VP(I,J-1,K).LE.1.E-06) GO TO 135

C.... Set values at (i,j-1,k) cell .....................................
      P3  = P(I,J-1,K)

      RSO3 = RSOA(I,J-1,K)

      MUO3 = VISO(I,J-1,K)
      MUW3 = VISW(I,J-1,K)
      MUG3 = VISG(I,J-1,K)

      RO3  = RHOO(I,J-1,K)
      RW3  = RHOW(I,J-1,K)
      RG3  = RHOG(I,J-1,K)

      PCOW3 = CAPOW(I,J-1,K)
      PCGO3 = CAPGO(I,J-1,K)

      FACT = -D288*(EL(I,J-1,K)-EL(I,J,K))

      GOW3 = (RO3+RO)*FACT
      GWW3 = (RW3+RW)*FACT + PCOW-PCOW3
      GGW3 = (RG3+RG)*FACT + PCGO3-PCGO

      P33 = P3-PP
      HO3 = P33+GOW3
      HW3 = P33+GWW3
      HG3 = P33+GGW3

      IF(HO3.LT.0.0) THEN 
         KRO3=RPO(I,J,K)
         RSOS=RSOA(I,J,K)
      ELSE
         KRO3=RPO(I,J-1,K)
         RSOS=RSOA(I,J-1,K)
      ENDIF

      KRW3=RPW(I,J,K)
      IF(HW3.GE.0.) KRW3=RPW(I,J-1,K)

      KRG3=RPG(I,J,K)
      IF(HG3.GE.0.) KRG3=RPG(I,J-1,K)

      MO3 = 4.0*KRO3/((BO(I,J-1,K)+BO(I,J,K)) * (MUO3+MUO))
      MW3 = 4.0*KRW3/((BW(I,J-1,K)+BW(I,J,K)) * (MUW3+MUW))
      MG3 = 4.0*KRG3/((BG(I,J-1,K)+BG(I,J,K)) * (MUG3+MUG))

135   AOS = TY(I,J,K)*MO3
      AWS = TY(I,J,K)*MW3
      AGS = TY(I,J,K)*MG3

      IF(J.EQ.JJ) GO TO 140

      IF(VP(I,J+1,K).LE.1.E-06) GO TO 140

C.... Set values at (i,j+1,k) block ....................................
      P4  = P(I,J+1,K)

      RSO4 = RSOA(I,J+1,K)

      MUO4 = VISO(I,J+1,K)
      MUW4 = VISW(I,J+1,K)
      MUG4 = VISG(I,J+1,K)

      RO4 = RHOO(I,J+1,K)
      RW4 = RHOW(I,J+1,K)
      RG4 = RHOG(I,J+1,K)

      PCOW4 = CAPOW(I,J+1,K)
      PCGO4 = CAPGO(I,J+1,K)

      FACT = -D288*(EL(I,J+1,K)-EL(I,J,K))

      GOW4 = (RO4+RO)*FACT
      GWW4 = (RW4+RW)*FACT + PCOW-PCOW4
      GGW4 = (RG4+RG)*FACT + PCGO4-PCGO

      P44 = P4-PP
      HO4 = P44+GOW4
      HW4 = P44+GWW4
      HG4 = P44+GGW4

      IF(HO4.LT.0.0) THEN
         KRO4=RPO(I,J,K)
         RSON=RSOA(I,J,K)
      ELSE
         KRO4=RPO(I,J+1,K)
         RSON=RSOA(I,J+1,K)
      ENDIF

      KRW4=RPW(I,J,K)
      IF(HW4.GE.0.) KRW4=RPW(I,J+1,K)

      KRG4=RPG(I,J,K)
      IF(HG4.GE.0.) KRG4=RPG(I,J+1,K)

      MO4 = 4.0*KRO4/((BO(I,J+1,K)+BO(I,J,K)) * (MUO4+MUO))
      MW4 = 4.0*KRW4/((BW(I,J+1,K)+BW(I,J,K)) * (MUW4+MUW))
      MG4 = 4.0*KRG4/((BG(I,J+1,K)+BG(I,J,K)) * (MUG4+MUG))

140   AON = TY(I,J+1,K)*MO4
      AWN = TY(I,J+1,K)*MW4
      AGN = TY(I,J+1,K)*MG4

      IF(K.EQ.1) GO TO 145

      IF(VP(I,J,K-1).LE.1.E-06) GO TO 145

C.... Set values at (i,j,k-1) cell .....................................
      P5  = P(I,J,K-1)

      RSO5 = RSOA(I,J,K-1)

      MUO5 = VISO(I,J,K-1)
      MUW5 = VISW(I,J,K-1)
      MUG5 = VISG(I,J,K-1)
 
      RO5  = RHOO(I,J,K-1)
      RW5  = RHOW(I,J,K-1)
      RG5  = RHOG(I,J,K-1)

      PCOW5 = CAPOW(I,J,K-1)
      PCGO5 = CAPGO(I,J,K-1)

      FACT = -D288*(EL(I,J,K-1)-EL(I,J,K))

      GOW5 = (RO5+RO)*FACT
      GWW5 = (RW5+RW)*FACT + PCOW-PCOW5
      GGW5 = (RG5+RG)*FACT + PCGO5-PCGO

      P55 = P5-PP
      HO5 = P55+GOW5
      HW5 = P55+GWW5
      HG5 = P55+GGW5

      IF(HO5.LT.0.0) THEN
         KRO5=RPO(I,J,K)
         RSOTT=RSOA(I,J,K)
      ELSE
         KRO5=RPO(I,J,K-1)
         RSOTT=RSOA(I,J,K-1)
      ENDIF

      KRW5=RPW(I,J,K)
      IF(HW5.GE.0.) KRW5=RPW(I,J,K-1)

      KRG5=RPG(I,J,K)
      IF(HG5.GE.0.) KRG5=RPG(I,J,K-1)

      MO5 = 4.0*KRO5/((BO(I,J,K-1)+BO(I,J,K)) * (MUO5+MUO))
      MW5 = 4.0*KRW5/((BW(I,J,K-1)+BW(I,J,K)) * (MUW5+MUW))
      MG5 = 4.0*KRG5/((BG(I,J,K-1)+BG(I,J,K)) * (MUG5+MUG))

145   AOT = TZ(I,J,K)*MO5
      AWT = TZ(I,J,K)*MW5
      AGT = TZ(I,J,K)*MG5

      IF(K.EQ.KK)GO TO 150
      IF(VP(I,J,K+1).LE.1.E-06) GO TO 150

C.... Set values at (i,j,k+1) cell .....................................
      P6  = P(I,J,K+1)

      RSO6 = RSOA(I,J,K+1)

      MUO6 = VISO(I,J,K+1)
      MUW6 = VISW(I,J,K+1)
      MUG6 = VISG(I,J,K+1)

      RO6  = RHOO(I,J,K+1)
      RW6  = RHOW(I,J,K+1)
      RG6  = RHOG(I,J,K+1)

      PCOW6 = CAPOW(I,J,K+1)
      PCGO6 = CAPGO(I,J,K+1)


      FACT = -D288*(EL(I,J,K+1)-EL(I,J,K))

      GOW6 = (RO6+RO)*FACT
      GWW6 = (RW6+RW)*FACT + PCOW-PCOW6
      GGW6 = (RG6+RG)*FACT + PCGO6-PCGO

      P66 = P6-PP
      HO6 = P66+GOW6
      HW6 = P66+GWW6
      HG6 = P66+GGW6

      IF(HO6.LT.0.0) THEN
         KRO6=RPO(I,J,K)
         RSOB=RSOA(I,J,K)
      ELSE
         KRO6=RPO(I,J,K+1)
         RSOB=RSOA(I,J,K+1)
      ENDIF

      KRW6=RPW(I,J,K)
      IF(HW6.GE.0.) KRW6=RPW(I,J,K+1)

      KRG6=RPG(I,J,K)
      IF(HG6.GE.0.) KRG6=RPG(I,J,K+1)

      MO6 = 4.0*KRO6/((BO(I,J,K+1)+BO(I,J,K)) * (MUO6+MUO))
      MW6 = 4.0*KRW6/((BW(I,J,K+1)+BW(I,J,K)) * (MUW6+MUW))
      MG6 = 4.0*KRG6/((BG(I,J,K+1)+BG(I,J,K)) * (MUG6+MUG))

150   AOB = TZ(I,J,K+1)*MO6
      AWB = TZ(I,J,K+1)*MW6
      AGB = TZ(I,J,K+1)*MG6



      AO1 = AOW*GOW1
      AO2 = AOE*GOW2
      AO3 = AOS*GOW3
      AO4 = AON*GOW4
      AO5 = AOT*GOW5
      AO6 = AOB*GOW6

      AW1 = AWW*GWW1
      AW2 = AWE*GWW2
      AW3 = AWS*GWW3
      AW4 = AWN*GWW4
      AW5 = AWT*GWW5
      AW6 = AWB*GWW6

      GOWT(I,J,K) = AO1 + AO2 + AO3 + AO4 + AO5 + AO6
      GWWT(I,J,K) = AW1 + AW2 + AW3 + AW4 + AW5 + AW6

C.... Note:  GGWT, AW, AE, AS, AN, AT, AB modified 12/08/91 to use
C....        "upstream" Rso's defined by RSOW, RSOE, RSOS, RSON, RSOTT & RSOB

      GGWT(I,J,K)=AGW*GGW1+AGE*GGW2+AGS*GGW3+AGN*GGW4+AGT*GGW5+AGB*GGW6
     &          +RSOW*AO1+RSOE*AO2+RSOS*AO3+RSON*AO4+RSOTT*AO5+RSOB*AO6

      QOWG(I,J,K)=(BO(I,J,K)-BG(I,J,K)*RSO)* (-GOWT(I,J,K)+QO(I,J,K)) +
     &             BW(I,J,K)               * (-GWWT(I,J,K)+QW(I,J,K)) +
     &             BG(I,J,K)               * (-GGWT(I,J,K)+QG(I,J,K))

CC      WRITE(*,10087) I,J,K, AOW,GOW1,
CC     &                      AOE,GOW2,
CC     &                      AOS,GOW3,
CC     &                      AON,GOW4,
CC     &                      AOT,GOW5,
CC     &                      AOB,GOW6
CC      WRITE(9,10087) I,J,K, AOW,GOW1,
CC     &                      AOE,GOW2,
CC     &                      AOS,GOW3,
CC     &                      AON,GOW4,
CC     &                      AOT,GOW5,
CC     &                      AOB,GOW6
CC10087 FORMAT(1X,'I,J,K,AOW,GOW1:',3I3,2G14.6,
CC     &     /10X, 'AOE,GOW2:',2G14.6,
CC     &     /10X, 'AOS,GOW3:',2G14.6,
CC     &     /10X, 'AON,GOW4:',2G14.6,
CC     &     /10X, 'AOT,GOW5:',2G14.6,
CC     &     /10X, 'AOB,GOW6:',2G14.6/)

CCC      WRITE(*,10089) I,J,K,AO1,AO2,AO3,AO4,AO5,AO6,GOWT(I,J,K)
CCC10089 FORMAT(1X,'I,J,K,AO1,AO2,AO3,AO4,AO5,AO6,GOWT(I,J,K):',
CCC     &     /1X,3I3,3G14.6/10X,4G14.6/)
CCC      PAUSE

CC      WRITE(*,10091) I,J,K, BO(I,J,K),BW(I,J,K),BG(I,J,K),RSO,
CC     &                      QO(I,J,K),  QW(I,J,K),  QG(I,J,K),
CC     &                    GOWT(I,J,K),GWWT(I,J,K),GGWT(I,J,K)
CC10091 FORMAT(1X,'I,J,K,BO,BW,BG,RSO:',3I3,2F10.4,F10.6,F8.2,
CC     &      /1X,'QO,QW,QG:      ',3G14.6,
CC     &      /1X,'GOWT,GWWT,GGWT:',3G14.6/)
CC      PAUSE

C!!!!!!!!!!!!!!!!!
C      WRITE(IOCODE,1009) I,J,K,GWW1,GWW2,GWW3,GWW4,GWW5,GWW6
C1009  FORMAT(1X,'SOLWKS:  IJK,GWW1-GWW6:',3I3,/1X,6G12.5)
C      WRITE(IOCODE,1007) I,J,K,AWW,AWE,AWS,AWN,AWT,AWB,
C     &               SSW,RPW(I,J,K),KRW1,KRW2,KRW3,KRW4,KRW5,KRW6,
C     &                      MUW,MUW1,MUW2,MUW3,MUW4,MUW5,MUW6
C1007  FORMAT(1X,'SOLWKS:  IJK,AWW,AWE,AWS,AWN,AWT,AWB:'3I3,/1X,6G12.5,
C     &           /1X,'SW,KRW,KRW1,KRW2,KRW3,KRW4,KRW5,KRW6',8F6.3,
C     &           /1X,'MUW,MUW1,MUW2,MUW3,MUW4,MUW5,MUW6',7F6.3)
C      WRITE(IOCODE,1111) I,J,K,QOWG(I,J,K),BW(I,J,K),BG(I,J,K),
C     &                   GWWT(I,J,K),GGWT(I,J,K),QW(I,J,K),QG(I,J,K)
C1111  FORMAT(1X,'IJK,QOWG,BW,BG,GWWT,GGWT,QW,QG:',3I3,3G12.5/10X,4G12.5)
CC!!!!!!!!!!!!!!!!

      AW(I,J,K)=(BO(I,J,K) + BG(I,J,K)*(RSOW-RSO))  * AOW +
     &           BW(I,J,K)                          * AWW +
     &           BG(I,J,K)                          * AGW

      AE(I,J,K)=(BO(I,J,K) + BG(I,J,K)*(RSOE-RSO))  * AOE +
     &           BW(I,J,K)                          * AWE +
     &           BG(I,J,K)                          * AGE

      AS(I,J,K)=(BO(I,J,K) + BG(I,J,K)*(RSOS-RSO))  * AOS +
     &           BW(I,J,K)                          * AWS +
     &           BG(I,J,K)                          * AGS

      AN(I,J,K)=(BO(I,J,K) + BG(I,J,K)*(RSON-RSO))  * AON +
     &           BW(I,J,K)                          * AWN +
     &           BG(I,J,K)                          * AGN

      AT(I,J,K)=(BO(I,J,K) + BG(I,J,K)*(RSOTT-RSO)) * AOT +
     &           BW(I,J,K)                          * AWT +
     &           BG(I,J,K)                          * AGT

      AB(I,J,K)=(BO(I,J,K) + BG(I,J,K)*(RSOB-RSO))  * AOB +
     &           BW(I,J,K)                          * AWB +
     &           BG(I,J,K)                          * AGB

      O1(I,J,K) = AOW
      O2(I,J,K) = AOS
      O3(I,J,K) = AOT
      W1(I,J,K) = AWW
      W2(I,J,K) = AWS
      W3(I,J,K) = AWT

C      IF(KCOFF.EQ.0) GO TO 200
C      WRITE(IOCODE,33)
C      WRITE(IOCODE,2) I,J,K,MUO1,MUO2,MUO3,MUO4,MUO5,MUO6
C      WRITE(IOCODE,2) I,J,K,MO1,MO2,MO3,MO4,MO5,MO6
C      WRITE(IOCODE,2) I,J,K,MW1,MW2,MW3,MW4,MW5,MW6
C      WRITE(IOCODE,2) I,J,K,MG1,MG2,MG3,MG4,MG5,MG6
C      WRITE(IOCODE,2) I,J,K,AOW,AOE,AOS,AON,AOT,AOB,BO(I,J,K),RSO
C      WRITE(IOCODE,2) I,J,K,AWW,AWE,AWS,AWN,AWT,AWB,BW(I,J,K)
C      WRITE(IOCODE,2) I,J,K,AGW,AGE,AGS,AGN,AGT,AGB,BG(I,J,K)
C      WRITE(IOCODE,2) I,J,K,GOWT(I,J,K),QO(I,J,K),GWWT(I,J,K),QW(I,J,K),
C     &                      GGWT(I,J,K),QG(I,J,K),QOWG(I,J,K)

200   CONTINUE

C**** CALCULATE MAIN DIAGONAL AND RHS VECTOR
      DO 300 K=1,KK
      DO 300 J=1,JJ
      DO 300 I=1,II
      SUM(I,J,K)=AW(I,J,K)+AE(I,J,K)+AS(I,J,K)+AN(I,J,K)+
     &    AT(I,J,K)+AB(I,J,K)
      GAM(I,J,K)=VP(I,J,K)*CT(I,J,K)*DIV1
      E(I,J,K)=-SUM(I,J,K) - GAM(I,J,K)
      B(I,J,K)= QOWG(I,J,K) - GAM(I,J,K)*PN(I,J,K)

      IF(KSM1.EQ.0) GO TO 300
      IF(N.NE.1.AND.N.NE.NN.AND.N.NE.KSM) GO TO 300 
      IF(I.EQ.1 .AND. J.EQ.1 .AND. K.EQ.1) THEN
         WRITE(IOCODE,295) NLOOP,DIV1
         WRITE(IOCODE,297) 
      ENDIF
      WRITE(IOCODE,2) I,J,K,VP(I,J,K),CT(I,J,K),GAM(I,J,K),QOWG(I,J,K),
     &                      PN(I,J,K),QO(I,J,K), QW(I,J,K),  QG(I,J,K)
300   CONTINUE

295   FORMAT(//1X,'BO3: SOLWKS: 300-LOOP: NLOOP=',I4,3X,'DIV1 =',G14.6/)
297   FORMAT(//1X,'NODE      VP(I,J,K)     CT(I,J,K)      GAM(I,J,K)',
     &'    QOWG(I,J,K)   PN(I,J,K)       QO(I,J,K)     QW(I,J,K)',
     &'     QG(I,J,K)'/)

C*** CALC. COEF. FOR 0 PV BLOCKS.
C*** ASSUMES NO PRESSURE CHANGE WITH TIME.
      DO 350 K=1,KK
      DO 350 J=1,JJ
      DO 350 I=1,II
      IF(VP(I,J,K).GT.0.0) GO TO 350
      E(I,J,K)=-1.0
      B(I,J,K)=-PN(I,J,K)
      AW(I,J,K)=0.0
      AE(I,J,K)=0.0
      AS(I,J,K)=0.0
      AN(I,J,K)=0.0
      AT(I,J,K)=0.0
      AB(I,J,K)=0.0
  350 CONTINUE
      IF(KSM1.EQ.0) RETURN
      IF(N.NE.1.AND.N.NE.NN.AND.N.NE.KSM) RETURN
      WRITE(IOCODE,4)
      DO 404 K=1,KK
      DO 404 J=1,JJ
      DO 404 I=1,II
      WRITE(IOCODE,2) I,J,K,AT(I,J,K),AS(I,J,K),AW(I,J,K),E(I,J,K),
     & AE(I,J,K)   ,AN(I,J,K),AB(I,J,K),B(I,J,K)
404   CONTINUE
4     FORMAT(//T3,'NODE      AT(I,J,K)     AS(I,J,K)       AW(I,J,K)',
     &'      E(I,J,K)     AE(I,J,K)      AN(I,J,K)     AB(I,J,K)',
     &'     B(I,J,K)'/)
2     FORMAT(1X,3I3,8G14.6)
33    FORMAT(//)
      RETURN
      END
