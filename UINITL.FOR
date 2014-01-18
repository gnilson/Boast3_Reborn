      SUBROUTINE UINITL(KPI,II,JJ,KK,ET0,CUMPO,MBEO,
     &CUMPW,MBEW,CUMPG,MBEG, WOC,GOC, CUMIW,CUMIG,CAPGO,CAPOW)

      INCLUDE 'PARAMS.FOR'

C======================================================================
      DIMENSION PWOC(LP7),PGOC(LP7),SOIRK(LP7),SWIRK(LP7),SGIRK(LP7)
      DIMENSION PIL(LP3), SOIL(LP3), SWIL(LP3), SGIL(LP3)
C======================================================================

      DIMENSION WOC(LP7),GOC(LP7), CAPGO(LP1,LP2,LP3),CAPOW(LP1,LP2,LP3)

      REAL MBEO,MBEW,MBEG,KX,KY,KZ,KROT,KRWT,KRGT,KROGT,MUOT,MUWT,MUGT

      COMMON /BUBBLE/ PBO,VSLOPE(LP8),BSLOPE(LP8),RSLOPE(LP8),PMAXT,
     & IREPRS,MPGT(LP8),
     & RHOSCO(LP8),RHOSCW(LP8),RHOSCG(LP8),MSAT(LP7),MPOT(LP8),
     & MPWT(LP8),PBOT(LP1,LP2,LP3),PBOTN(LP1,LP2,LP3)

      COMMON /SARRAY/ PN(LP1,LP2,LP3),IOCODE,IDMAX,
     & SON(LP1,LP2,LP3),SWN(LP1,LP2,LP3),SGN(LP1,LP2,LP3),
     & A1(LP1,LP2,LP3),A2(LP1,LP2,LP3),A3(LP1,LP2,LP3),
     & SUM(LP1,LP2,LP3),GAM(LP1,LP2,LP3),QS(LP1,LP2,LP3)

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
     & PGT(LP7,LP9),MUGT(LP7,LP9),
     & BGPT(LP7,LP9),CRT(LP7,LP9),IPVT(LP1,LP2,LP3),IROCK(LP1,LP2,LP3),
     & NROCK,NPVT,PSIT(LP7,LP9),PRT(LP7,LP9),WOROCK(LP7),GOROCK(LP7)


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

      COMMON /TSTDAT/ IFATAL,IWARN

      ET0=0.0
      CUMPO=0.0
      CUMPW=0.0
      CUMPG=0.0
      CUMIW=0.0
      CUMIG=0.0
      MBEO=0.0
      MBEW=0.0
      MBEG=0.0

C=======================================================================

C............... Read Initialization Data ............................
      READ(20,69)
      READ(20,*) KPI,KSI,PDATUM,GRAD
      WRITE(IOCODE,691) KPI,KSI,PDATUM,GRAD
691   FORMAT(///1X,
     &'Initialization data (KPI,KSI,PDATUM,GRAD):',2I3,F8.1,F10.4)

      READ(20,69)
      DO 25 M=1,NROCK
      READ(20,*)  NR, PWOC(NR),    WOC(NR),  PGOC(NR), GOC(NR),
     &                SOIRK(NR), SWIRK(NR), SGIRK(NR)
25    CONTINUE

      READ(20,69)
      DO 27 K=1,KK
      READ(20,*) L,PIL(L), SOIL(L), SWIL(L), SGIL(L)
27    CONTINUE




C.............. Execute Saturation Initialization Options...........
      IF(KSI.NE.0) GO TO 5600
C.... KSI = 0 -----> Use saturations by rock region - without any checks! 
      DO 40 NR=1,NROCK
      SOI = SOIRK(NR)
      SWI = SWIRK(NR)
      SGI = SGIRK(NR)
      DO 30 K=1,KK
      DO 30 J=1,JJ
      DO 30 I=1,II
      IF(IROCK(I,J,K).NE.NR) GO TO 30
      SON(I,J,K)=SOI
      SWN(I,J,K)=SWI
      SGN(I,J,K)=SGI
      SO(I,J,K)=SON(I,J,K)
      SW(I,J,K)=SWN(I,J,K)
      SG(I,J,K)=SGN(I,J,K)
   30 CONTINUE
   40 CONTINUE

5600  IF(KSI.NE.1) GO TO 4000

C.... KSI=1 ====> Read So and Sw by grid-block and calculate Sg  
      READ(20,69)
      DO 3011 K=1,KK
      DO 3006 J=1,JJ
3006  READ(20,*) (SO(I,J,K),I=1,II)
3011  CONTINUE
      READ(20,69)
      DO 3020 K=1,KK
      DO 3007 J=1,JJ
3007  READ(20,*) (SW(I,J,K),I=1,II)
3020  CONTINUE
      DO 3030 K=1,KK
      DO 3030 J=1,JJ
      DO 3030 I=1,II
      SG(I,J,K)=1.0-SO(I,J,K)-SW(I,J,K)
      IF(SG(I,J,K).LT.0.0) SG(I,J,K)=0.0
      SON(I,J,K)=SO(I,J,K)
      SWN(I,J,K)=SW(I,J,K)
      SGN(I,J,K)=SG(I,J,K)
 3030 CONTINUE
69    FORMAT(40A2)

      GO TO 5000

 4000 CONTINUE

      IF(KSI.EQ.2) THEN 	
C....    Use initial saturations by layer 
         DO 4402 K=1,KK
         DO 4202 J=1,JJ
         DO 4202 I=1,II
         SON(I,J,K) = SOIL(K)
         SWN(I,J,K) = SWIL(K)
         SGN(I,J,K) = SGIL(K)
         SO(I,J,K) = SON(I,J,K)
         SW(I,J,K) = SWN(I,J,K)
4202     SG(I,J,K) = SGN(I,J,K)
4402     CONTINUE
      ENDIF         

5000  CONTINUE
C................ Saturation Initialization complete .................


C................ Execute Pressure Initialization Options ..............
      IF(KPI.NE.0) GO TO 252
C.... KPI = 0  -----> Use pressure initialization for each region
      IFLG = 0
      DO 250 NR=1,NROCK
      IFLG = 1
      PGOC1 = PGOC(NR)
      PI =    PWOC(NR)
      
      DO 200 K=1,KK
      DO 200 J=1,JJ
      DO 200 I=1,II
      BPT=PBOT(I,J,K)
      IPVTR=IPVT(I,J,K)
      IROCKB=IROCK(I,J,K)
      IF(IROCKB.NE.NR) GO TO 200
      IF(EL(I,J,K).LT.GOC(NR)) GO TO 175
      IF(EL(I,J,K).GE.WOC(NR)) GO TO 150
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PI,BBO)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PI,RSO)
      RHOO=(RHOSCO(IPVTR)+RSO*RHOSCG(IPVTR))/BBO
      RHOGRD = RHOO/144.
      IF(IFLG.EQ.1) THEN
         WRITE(IOCODE,2001) NR,I,J,K,PI,PWOC(NR),RHOO,RHOGRD
ccc         WRITE(*,2001) NR,I,J,K,PI,PWOC(NR),RHOO,RHOGRD
ccc         WRITE(9,2001) NR,I,J,K,PI,PWOC(NR),RHOO,RHOGRD
2001     FORMAT(/1X,'Pressure Initialization between WOC and GOC',
     &          /1X,'     Region(NR) =',I3,3X,'Block =',3I3,
     &          /10X,'PI,PWOC(NR),RHOO,RHOGRD:',3F8.2,F8.4)
         IFLG = 0
      ENDIF
      PN(I,J,K)=PI+RHOGRD*(EL(I,J,K)-WOC(NR))
      GO TO 200

  150 CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PI,BBW)
      RHOW=RHOSCW(IPVTR)/BBW
      RHWGRD = RHOW/144.
      PN(I,J,K)=PI+RHWGRD*(EL(I,J,K)-WOC(NR))
      GO TO 200

  175 CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PGOC1,BBG)
      RHOG=RHOSCG(IPVTR)/BBG
      RHGGRD = RHOG/144.
      PN(I,J,K)=PGOC1+RHGGRD*(EL(I,J,K)-GOC(NR))
C:::: NOTE:  Since above GOC, we are initializing Pg; must convert to Po
C            i.e. Po = Pg - Pcgo
      SSG = SG(I,J,K)
      CALL INTERP(IROCKB,SAT,PCGOT,MSAT(IROCKB),SSG,PCGO)
      PN(I,J,K) = PN(I,J,K) - PCGO
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(IOCODE,2003) I,J,K,SSG,PCGO,PN(I,J,K)
C      WRITE(*,2003) I,J,K,SSG,PCGO,PN(I,J,K)
C      WRITE(9,2003) I,J,K,SSG,PCGO,PN(I,J,K)
C2003  FORMAT(1X,'UINITL: IJK,SSG,PCGO,PN(I,J,K):',3I3,3F10.4)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  200 CONTINUE

  250 CONTINUE



  252 CONTINUE

      IF(KPI.EQ.1) THEN
C....    Read header & initial pressures by grid-block      
         READ(20,69)
         DO 3010 K=1,KK
         DO 3005 J=1,JJ
3005     READ(20,*) (PN(I,J,K),I=1,II)
3010     CONTINUE
      ENDIF

      IF(KPI.EQ.2) THEN 	
C....    Use initial pressures by layer
         DO 30102 K=1,KK
         DO 30100 J=1,JJ
         DO 30100 I=1,II
30100    PN(I,J,K) = PIL(K) 
30102    CONTINUE
      ENDIF         

C**** Initialize n+1 pressure array.
      DO 3012 I=1,II
      DO 3012 J=1,JJ
      DO 3012 K=1,KK
 3012 P(I,J,K)=PN(I,J,K)

C................ Pressure Initialization complete .................


C========= Pressure and Saturation Initialization complete ===========

C.... Initialize capillary pressure arrays.................
      DO 2000 K=1,KK
      DO 2000 J=1,JJ
      DO 2000 I=1,II
      CAPOW(I,J,K) = 0.0
      CAPGO(I,J,K) = 0.0
      IF(VP(I,J,K).LE.0.0) GO TO 2000
      SSW = SW(I,J,K)
      SSG = SG(I,J,K)
      IROCKB=IROCK(I,J,K)
      CALL INTERP(IROCKB,SAT,PCOWT,MSAT(IROCKB),SSW,PCOW)
      CAPOW(I,J,K)=PCOW
      CALL INTERP(IROCKB,SAT,PCGOT,MSAT(IROCKB),SSG,PCGO)
      CAPGO(I,J,K)=PCGO
 2000 CONTINUE



C.... Now do array checks........................................    
      DO 4200 K=1,KK
      DO 4200 J=1,JJ
      DO 4200 I=1,II
C.... If zero pore volume block, bypass array checks ............
      IF(VP(I,J,K).LE.0.0) THEN 
         P(I,J,K)=0.0
         SO(I,J,K)=0.0
         SW(I,J,K)=0.0
         SG(I,J,K)=0.0
         PN(I,J,K)=0.0
         SON(I,J,K)=0.0
         SWN(I,J,K)=0.0
         SGN(I,J,K)=0.0
         GO TO 4200
      ENDIF

C.... Check initial arrays...........................................
      IF(P(I,J,K).GE.0.0) GO TO 4120
      IFATAL=IFATAL+1
      WRITE(IOCODE,4110) I,J,K
 4110 FORMAT(/5X,5('-'),'INIT P ERROR AT GRID BLOCK IJK = ',3I5)

 4120 IF(SO(I,J,K).GE.0.0.AND.SO(I,J,K).LE.1.0) GO TO 4140
      IFATAL=IFATAL+1
      WRITE(IOCODE,4130) I,J,K
 4130 FORMAT(/5X,5('-'),'INIT SO ERROR AT GRID BLOCK IJK = ',3I5)

 4140 IF(SW(I,J,K).GE.0.0.AND.SW(I,J,K).LE.1.0) GO TO 4160
      IFATAL=IFATAL+1
      WRITE(IOCODE,4150) I,J,K
 4150 FORMAT(/5X,5('-'),'INIT SW ERROR AT GRID BLOCK IJK = ',3I5)

 4160 IF(SG(I,J,K).GE.0.0.AND.SG(I,J,K).LE.1.0) GO TO 4180
      IFATAL=IFATAL+1
      WRITE(IOCODE,4170) I,J,K
 4170 FORMAT(/5X,5('-'),'INIT SG ERROR AT GRID BLOCK IJK = ',3I5)

 4180 SUMSAT=SO(I,J,K)+SW(I,J,K)+SG(I,J,K)
      ERR=(1.0-SUMSAT)
      IF(ABS(ERR).LE.0.0001) GO TO 4200
       WRITE(IOCODE,10) SO(I,J,K) ,SW(I,J,K),SG(I,J,K)
10     FORMAT(/,5X,' SO=',F10.6,' SW=',F10.6,' SG=',F10.6)
      IFATAL=IFATAL+1
      WRITE(IOCODE,4190) I,J,K, SUMSAT
 4190 FORMAT(/5X,5('-'),'INIT SAT SUM ERROR AT GRID BLOCK IJK = ',3I5,'
     & SUMSAT = ',F10.5)

 4200 CONTINUE
      RETURN
      END
