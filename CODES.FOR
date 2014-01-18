C.................................................................CODES
      SUBROUTINE CODES(IOCODE,KSM1,KSN1,KCO1,NN,
     & FACT1,FACT2,TMAX,KSOL,MITER,OMEGA,TOL,TOL1,
     & KSN,KSM,KCO,KCOFF,KSCRN, KOUT, DSMAX,DPMAX,
     & NUMDIS,IRK,THRUIN,
     &WORMAX,GORMAX,PAMIN,PAMAX)

C      MACHINE DEPENDENT INCLUDE STATEMENT
      INCLUDE 'PARAMS.FOR'

      REAL KROT,KRWT,KRGT,KROGT,MUOT,MUWT,MUGT

      COMMON /SPVT/ SAT(LP7,LP9),KROT(LP7,LP9),KRWT(LP7,LP9),
     & BGT(LP7,LP9),
     & KRGT(LP7,LP9),ITHREE(LP7),RSOT(LP7,LP9),BWPT(LP7,LP9),
     & PCOWT(LP7,LP9),PCGOT(LP7,LP9),KROGT(LP7,LP9),SWR(LP7),
     & POT(LP7,LP9),MUOT(LP7,LP9),BOT(LP7,LP9),BOPT(LP7,LP9),
     & RSOPT(LP7,LP9),PWT(LP7,LP9),MUWT(LP7,LP9),BWT(LP7,LP9),
     & PGT(LP7,LP9),MUGT(LP7,LP9),
     & BGPT(LP7,LP9),CRT(LP7,LP9),IPVT(LP1,LP2,LP3),IROCK(LP1,LP2,LP3),
     & NROCK,NPVT,PSIT(LP7,LP9),PRT(LP7,LP9),WOROCK(LP7),GOROCK(LP7)

C      READ DEBUG, MATRIX SOLUTION AND RUN CONTROL INFO

      COMMON /TSTDAT/ IFATAL,IWARN

      READ(20,69)
      READ(20,*) KSN1,KSM1,KCO1,KCOFF,KSCRN, KOUT
C Every KSM1 th step solution matrix will be written
C every KSN1 th step lsorx data will be written
C Every KCO1 th step compressibilities & volume factors will be written

      READ(20,69)
      READ(20,*)       NN,FACT1,FACT2,TMAX,WORMAX,GORMAX,PAMIN,PAMAX
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(IOCODE,11111)
C      WRITE(*,11111)
C      WRITE(9,11111)
11111 FORMAT(///)
C      PAUSE 'CODES - BEFORE WRITE(  ,59)'
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      WRITE(IOCODE,59) NN,FACT1,FACT2,TMAX,WORMAX,GORMAX,PAMIN,PAMAX
CC      WRITE(*,59)      NN,FACT1,FACT2,TMAX,WORMAX,GORMAX,PAMIN,PAMAX
CC      WRITE(9,59)      NN,FACT1,FACT2,TMAX,WORMAX,GORMAX,PAMIN,PAMAX

59    FORMAT(///T5,'RUN CONTROL PARAMETERS:',/,
     & T5,'Maximum number of time-steps    =',I5/,
     & T5,'Factor for increasing DELT      =',F7.3,3X,
     & 'when DSMAX & DPMAX not exceeded.',/,
     & T5,'Factor for decreasing DELT      =',F7.3,3X,
     & 'when DSMAX or DPMAX is exceeded.',/,
     & T5,'Maximum simulation time         =',F11.3/,
     & T5,'Maximum reservoir WOR/time-step =',G15.6,' stb/stb'/,
     & T5,'Maximum reservoir GOR/time-step =',G15.6,' scf/stb'/,
     & T5,'Minimum average reservoir pressure/time-step =',F8.0/,
     & T5,'Maximum average reservoir pressure/time-step =',F8.0/)
C!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(IOCODE,11111)
C      WRITE(*,11111)
C      WRITE(9,11111)
C      PAUSE 'CODES - AFTER WRITE(  ,59)'
C!!!!!!!!!!!!!!!!!!!!!!!!!!


      IF(WORMAX.NE.0.0) GO TO 20
      READ(20,*) (WOROCK(I),I=1,NROCK)
      WRITE(IOCODE,5)
    5 FORMAT(//T15,'ROCK REGION SPECIFIED WOR MAXIMA:')
      DO 10 I=1,NROCK
      WRITE(IOCODE,8) I,WOROCK(I)
    8 FORMAT(T20,'ROCK REGION ',I3,' WOR MAX (STB/STB) = ',F8.0)
   10 CONTINUE

   20 IF(GORMAX.NE.0.0) GO TO 40
      READ(20,*) (GOROCK(I),I=1,NROCK)
      WRITE(IOCODE,25)
   25 FORMAT(//T15,'ROCK REGION SPECIFIED GOR MAXIMA:')
      DO 30 I=1,NROCK
      WRITE(IOCODE,28) I,GOROCK(I)
   28 FORMAT(T20,'ROCK REGION ',I3,' GOR MAX (SCF/STB) = ',F8.0)
   30 CONTINUE

   40 CONTINUE

C NN--Maximum number of time-steps
C FACT1--Factor for increasing time-step size
C FACT2--Factor for decreasing time-step size
C TMAX--Maximum simulation time
      READ(20,69)
      READ(20,*) KSOL,MITER,OMEGA,TOL,TOL1,DSMAX,DPMAX
      IF(KSOL.EQ.1) WRITE(IOCODE,73)
      IF(KSOL.EQ.2) WRITE(IOCODE,75) MITER,OMEGA,TOL,TOL1
      IF(KSOL.EQ.3) WRITE(IOCODE,76) MITER,OMEGA,TOL,TOL1
      IF(KSOL.EQ.4) WRITE(IOCODE,77) MITER,OMEGA,TOL,TOL1
      IF(KSOL.GT.4) THEN
        WRITE(IOCODE,*) 'IMPROPER SOLUTION METHOD SPECIFIED,KSOL=',KSOL
        IFATAL=IFATAL + 1
      ENDIF

      WRITE(IOCODE,99) DSMAX,DPMAX

   73 FORMAT(//T15,'Solution method is band.')

   75 FORMAT(//T15,'Solution method is LSORX:',
     &/T10,'Maximum number of iterations      (miter) = ',5x,i5,
     &/T10,'Initial acceleration parameter    (omega) = ',f10.4,
     &/T10,'Maximum pressure residual           (tol) = ',f10.4,
     &/T10,'Parameter for changing omega       (tol1) = ',f10.4)

   76 FORMAT(//T15,'Solution method is LSORY:',
     &/T10,'Maximum number of iterations      (miter) = ',5x,i5,
     &/T10,'Initial acceleration parameter    (omega) = ',f10.4,
     &/T10,'Maximum pressure residual           (tol) = ',f10.4,
     &/T10,'Parameter for changing omega       (tol1) = ',f10.4)

   77 FORMAT(//T15,'Solution method is LSORZ:',
     &/T10,'Maximum number of iterations      (MITER) = ',5X,I5,
     &/T10,'Initial acceleration parameter    (OMEGA) = ',F10.4,
     &/T10,'Maximum pressure residual           (TOL) = ',F10.4,
     &/T10,'Parameter for changing omega       (TOL1) = ',F10.4)

   99 FORMAT(/T15,'AUTOMATIC TIME STEP CRITERIA:',
     &/T10,'Maximum allowed saturation change (DSMAX) = ',F10.4,
     &/T10,'Maximum allowed pressure change   (DPMAX) = ',F10.4/)

      KSN=KSN1
      KSM=KSM1
      KCO=KCO1
69    FORMAT(40A2)

C      NUM DIS AND IMPES/R-K SOLN CONTROLS
      READ(20,69)
      READ(20,*) NUMDIS,IRK,THRUIN 

      IF(IRK.EQ.0.AND.NUMDIS.EQ.0) THEN
        WRITE(IOCODE,200)
        WRITE(*,200)
        WRITE(9,200)
      ENDIF

  200 FORMAT(//T15,'IMPES FORMULATION SELECTED; ',
     & ' SINGLE-POINT UPSTREAM WEIGHTING.'/)

C!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(IOCODE,11111)
C      WRITE(*,11111)
C      WRITE(9,11111)
C      PAUSE 'CODES - AFTER WRITE(  ,200)'
C!!!!!!!!!!!!!!!!!!!!!!!!!!!

      
      IF(IRK.EQ.0.AND.NUMDIS.EQ.1) WRITE(IOCODE,220)
  220 FORMAT(//T15,'IMPES FORMULATION SELECTED; ',
     & ' TWO-POINT UPSTREAM WEIGHTING.'/)

      IF(IRK.GT.0.AND.NUMDIS.EQ.0) WRITE(IOCODE,240)  THRUIN
  240 FORMAT(//T15,'STABILISED IMPES FORMULATION:',
     & /T10,'SINGLE-POINT UPSTREAM WEIGHTING',
     & /T10,'USER SPEC THROUGHPUT, FRACTION  (THRUIN) =',5X,F10.4,/)

      IF(IRK.GT.0.AND.NUMDIS.EQ.1) WRITE(IOCODE,260)  THRUIN
  260 FORMAT(//T15,'STABILISED IMPES FORMULATION:',
     & /T10,'TWO-POINT UPSTREAM WEIGHTING',
     & /T10,'USER SPEC THROUGHPUT, FRACTION  (THRUIN) =',5X,F10.4,/)

C      CHECK CODE DATA
C      DEBUG
      IF(KSN1.EQ.0) GO TO 1020
      IWARN=IWARN+1
      WRITE(IOCODE,1010)
 1010 FORMAT(/5X,5('-'),'LSOR DEBUG OUTPUT ON')
 1020 IF(KSM1.EQ.0) GO TO 1040
      IWARN=IWARN+1
      WRITE(IOCODE,1030)
 1030 FORMAT(/5X,5('-'),'SOLN METHOD DEBUG OUTPUT ON')
 1040 IF(KCO1.EQ.0) GO TO 1060
      IWARN=IWARN+1
      WRITE(IOCODE,1050)
 1050 FORMAT(/5X,5('-'),'COMP AND FVF DEBUG OUTPUT ON')
 1060 IF(KCOFF.EQ.0) GO TO 1400
      IWARN=IWARN+1
      WRITE(IOCODE,1070)
 1070 FORMAT(/5X,5('-'),'DEN AND SAT DEBUG OUTPUT ON')
C      RUN CONTROL
 1400 CONTINUE

 3000 CONTINUE

C!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      WRITE(IOCODE,11111)
c      WRITE(*,11111)
c      WRITE(9,11111)
c      PAUSE 'CODES - BEFORE RETURN)'
C!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
      RETURN
      END
