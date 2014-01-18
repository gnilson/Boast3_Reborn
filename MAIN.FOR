
C=====================================================================
C                                                                     
C  BOAST3: BLACK OIL APPLIED SIMULATION TOOL  (from BOASTII 01/89)  
C  Modified for BDM by W.K. Sawyer - 05/06/92 (converted to NDP FORTRAN, etc.)
C  Modified for LSU by W.K. Sawyer - 07/02/93 (added B3PLOT & COLORGRID)
C  Modified for LSU by W.K. Sawyer - 05/31/95 (converted to MS PowerStation
C                                              Fortran and added Restart)
C=====================================================================


c----- PARAMS.FOR and COMMON.INC have been modified (from MISC4) for
c----- B3PLOT & COLORGRID 

      INCLUDE 'PARAMS.FOR'
      INCLUDE 'COMMON.INC'


c------- Added for ___.MAP file - 07/02/93 ------------------------------
c------- Note that NWX=LP11 and NODX=LP3  (see PARAMS.FOR)  
      CHARACTER*8  WELNAM(NWX)
      CHARACTER*30 PARNAM
      CHARACTER*75 HEADER, IDENT
      DIMENSION IDWEL(NWX),NPW(NWX)
      DIMENSION LWX(NWX,NODX), LWY(NWX,NODX), LWZ(NWX,NODX),
     &          IDIR(NWX,NODX)
C----- Arrays for outputting X & Y grid to ____.MAP file - added here for
C----- restart run - 06/03/95
      DIMENSION DXNEW(IMX),DYNEW(JMX),DY1(IMX)
c------------------------------------------------------------------------

C
      CHARACTER*75 DUMID(5)
      DIMENSION DRSODP(LP1,LP2,LP3), QGRSO(LP1,LP2,LP3),

     & RPW(LP1,LP2,LP3), RPG(LP1,LP2,LP3),  RPO(LP1,LP2,LP3),
     & RSOA(LP1,LP2,LP3),KR3(LP1,LP2,LP3),
     & VISO(LP1,LP2,LP3),VISW(LP1,LP2,LP3),VISG(LP1,LP2,LP3),
     & RHOO(LP1,LP2,LP3),RHOW(LP1,LP2,LP3),RHOG(LP1,LP2,LP3),
     & CAPGO(LP1,LP2,LP3),CAPOW(LP1,LP2,LP3)


C.... NOTE:  RSOA is passed to PRTPS
C....        DRSODP is passed to QRATE
C....        QGRSO  is passed to QRATE & PRTPS

C....        RKROMX is used to set oil compressibility to zero
C....               and is passed to TABLE, PRTPS & QRATE
C========================================================================

c;;;;;;;;;;;;;;;; Restart Files ;;;;;;;;;;;;;;;;;;;      
      CHARACTER*12 RESIN,RESOUT
c;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      CHARACTER*5 WELNM
      REAL KROT,KROGT,KRWT,KRGT,MUWT,MUOT,MUGT,KX,KY,KZ
     & ,MUO,MUW,MUG,MBEWI,MBEGI,MCFGI
     & ,MBEO,MBEW,MBEG,MCFG,MBEOI,MIN,MCFG1,MCFGT
C
       COMMON /BUBBLE/ PBO,VSLOPE(LP8),BSLOPE(LP8),RSLOPE(LP8),PMAXT,
     & IREPRS,MPGT(LP8),
     & RHOSCO(LP8),RHOSCW(LP8),RHOSCG(LP8),MSAT(LP7),MPOT(LP8),
     & MPWT(LP8),PBOT(LP1,LP2,LP3),PBOTN(LP1,LP2,LP3)

      COMMON /COEF/ AW(LP1,LP2,LP3),AE(LP1,LP2,LP3),AN(LP1,LP2,LP3),
     & AS(LP1,LP2,LP3),AB(LP1,LP2,LP3),AT(LP1,LP2,LP3),E(LP1,LP2,LP3),
     & B(LP1,LP2,LP3)

      COMMON /RUNSUM/ ITSNO(LP12),STIME(LP12),SOPROD(LP12),
     & SGPROD(LP12),
     & SWPROD(LP12),SGOR(LP12),SWOR(LP12),SGINJ(LP12),SWINJ(LP12)

      COMMON /RUN2/SPVWTP(LP12),SOCUMP(LP12),SWCUMP(LP12),SGCUMP(LP12),
     & SGCUMI(LP12),SWCUMI(LP12),SAQUIR(LP12),SAQUIC(LP12)

      COMMON /SAQUI/ IAQOPT,CPIAQ1(LP1,LP2,LP3),CPIAQ2(LP1,LP2,LP3),
     & CPI1(LP1,LP2,LP3),CPI2(LP1,LP2,LP3),EWAQ(LP1,LP2,LP3),
     & CUMAQW(LP1,LP2,LP3),
     & QWAQ(LP1,LP2,LP3),CUMEW(LP1,LP2,LP3),QWAQR(LP7),CUMAQR(LP7)
     & ,IAQREG(LP1,LP2,LP3),PAQ(LP1,LP2,LP3),PIAQ(LP1,LP2,LP3)

      COMMON /SARRAY/ PN(LP1,LP2,LP3),IOCODE,IDMAX,
     & SON(LP1,LP2,LP3),SWN(LP1,LP2,LP3),SGN(LP1,LP2,LP3),
     & A1(LP1,LP2,LP3),A2(LP1,LP2,LP3),A3(LP1,LP2,LP3),
     & SUM(LP1,LP2,LP3),GAM(LP1,LP2,LP3),QS(LP1,LP2,LP3)

      COMMON /SLIMIT/ GORT(LP11),WORT(LP11),ILIMOP(LP11),
     & GORL(LP11),WORL(LP11),QOC(LP11,LP3),QWC(LP11,LP3),QGC(LP11,LP3)

      COMMON /SPARM/ KX(LP1,LP2,LP3),KY(LP1,LP2,LP3),KZ(LP1,LP2,LP3),
     & EL(LP1,LP2,LP3),TX(LP4,LP2,LP3),TY(LP1,LP5,LP3),TZ(LP1,LP2,LP6),
     & PDAT(LP1,LP2,LP3),PDATUM,GRAD

      COMMON /SPOST/ KOPR,KGPR,KWPR,KGOR,KWOR,KGIR,KWIR,KRESP,
     & KAIR,KAIC,KCOP,KCGP,KCWP,KCGI,KCWI,ITSMAX

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
     & IDWELL(LP11),ALIT(LP11),BLIT(LP11)

      COMMON /SSOLN/ BO(LP1,LP2,LP3),BW(LP1,LP2,LP3),BG(LP1,LP2,LP3),
     & QO(LP1,LP2,LP3),QW(LP1,LP2,LP3),QG(LP1,LP2,LP3),
     & GOWT(LP1,LP2,LP3),GWWT(LP1,LP2,LP3),GGWT(LP1,LP2,LP3),
     & O1(LP4,LP2,LP3),W1(LP4,LP2,LP3),
     & O2(LP1,LP5,LP3),W2(LP1,LP5,LP3),
     & O3(LP1,LP2,LP6),W3(LP1,LP2,LP6),
     & QOWG(LP1,LP2,LP3),VP(LP1,LP2,LP3),CT(LP1,LP2,LP3)
 
      COMMON /TRIDI/ UM(LP15),AZL(LP15),BZL(LP15),CZL(LP15),DZL(LP15),
     & UZL(LP15),X(LP15),BETA(LP15),GAMMA(LP15),W(LP15)

      COMMON /TSTDAT/ IFATAL,IWARN

      COMMON /VECTOR/ DX(LP1,LP2,LP3),DY(LP1,LP2,LP3),DZ(LP1,LP2,LP3),
     & DZNET(LP1,LP2,LP3),IQN1(LP11),IQN2(LP11),IQN3(LP11)

      COMMON /CHAR/ WELNM(LP11)
C
      DIMENSION FTIO(50),OOIP(LP3),OWIP(LP3),ODGIP(LP3),OFGIP(LP3)
c;;;;;;;;;;;;;;;;;;;; Restart ;;;;;;;;;;;;;;;;;;;;;;;;;
      DIMENSION REDATE(LP17)
c;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      DIMENSION RKCOEF(7,7),THRUA(LP1,LP2,LP3)
      DIMENSION WOC(LP7),GOC(LP7)
C
      DATA KRUNGE/1/
      DATA RKCOEF
     & /1.0,6*0.0,
     & 0.13157894,1.0,5*0.0,
     & 0.03937504,0.15625736,1.0,4*0.0,
     & 0.01666967,0.053199139,0.16491829,1.0,3*0.0,
     & 0.0085487791,0.024394839,0.059605275,0.16893119,1.0,2*0.0,
     & .0049515953,.013195402,.028593749,.06308716,.17111215,1.0,0.0,
     & .0031198738,.0079382925,.015998138,.031126418,.06518731,
     & .17242757,1.0/

      DATA ETI,FT,FTMAX/0.0,0.0,0.0/
      DATA COP,CWP,CGP,CWI,CGI/0.0,0.0,0.0,0.0,0.0/
      DATA IOMETH,IFTCOD,IRSCNT/0,0,1/

      DATA NX,NY,NZ,NTER,NTEP,NW,
     & NTMAX,NRST,N1DIR,N23DIR,NITER,NTRI
     & /LP1,LP2,LP3,LP9,LP9,LP11,LP12,LP17,LP15,
     & LP14,LP22,LP15/
C................................................................


c........ Initialize write switches for output to ___.OUT and ___.MAP files
      DATA  KXMP,KYMP,KZMP,
     &      IPMAP,ISOMAP,ISWMAP,ISGMAP,IPBMAP,IAQMAP, 
     &      KROMP,KRWMP, KRGMP, IRSOMP, IPCOW,IPCGO, KPHIMP /16*0/

c........ Initialize new cum well & node production arrays 
      DATA  WCGP, WCWP, WCOP  /   NWX*0.,   NWX*0.,   NWX*0. /
      DATA CUMGN,CUMWN,CUMON  / NODWX*0., NODWX*0., NODWX*0. /


      IOCODE = 21

C.... New file structure for NDP Fortran...............................
      OPEN(20, FILE = 'B.SIM', STATUS = 'OLD')
      OPEN(21, FILE = 'B.OUT', STATUS = 'NEW')
      OPEN(07, FILE = 'B.WEL', STATUS = 'NEW')
      OPEN(09, FILE = 'B.SCR', STATUS = 'NEW')
      OPEN(10, FILE = 'B.TAB', STATUS = 'NEW')
      OPEN(15, FILE = 'B.GWN', STATUS = 'NEW')
      OPEN(17, FILE = 'B.CGD', STATUS = 'NEW')

      READ(20,69111)       HEADER 
      WRITE(IOCODE,69113)  HEADER 
      WRITE(*,69113)       HEADER 
      WRITE(9,69113)       HEADER 
      
      DO 10000 J=1,4
      READ(20,69111)  IDENT
      WRITE(IOCODE,69113) IDENT
      WRITE(*,69113) IDENT
      WRITE(9,69113) IDENT
10000 CONTINUE
69111 FORMAT(A75)
69113 FORMAT(1X,A75)

c.... Begin is in BLOCK3.FOR   (5/5/92)..............
      CALL BEGIN(IOCODE, NROCK,NTER,NPVT,NTEP,NW,
     &  NTMAX,NRST,N1DIR,N23DIR,NITER,NTRI,
     &  IREOPT,IPLOTP,IRNUM,IRSTRT,NN,TMAX, RESIN,RESOUT, REDATE)

      WRITE(17,69113) HEADER

      IDMAX=0.0
      CMBEO=0.0
      CMBEW=0.0
      CMBEG=0.0
693   FORMAT(A75)
69    FORMAT(40A2)
  33  FORMAT(//)



C====================== Deleted lots here 9/20/91 ====================
C                       for IPOSTP=1
C**** POST-PLOT PACKAGE CODES
      IF(IPLOTP.EQ.0) WRITE(IOCODE,3015)
 3015 FORMAT(//T10,'POST-RUN TABLE WILL BE WRITTEN.')
C====================================================================

c;;;;;;;;;;;;; Restart Option Added - May-June, 1995 ;;;;;;;;;;;;;;
      IF(IREOPT.GT.0) GO TO 20
c;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


C**** Establish reservoir and block dimensions
      CALL GRIDSZ(IOCODE,II,JJ,KK,NX,NY,NZ, ALPHA, TOP)

C**** Establish porosity and permeability at each zone..................
      CALL PORPRM(IOCODE,II,JJ,KK)

C**** Calculate interblock transmissibilities...........................
      CALL TRANS(II,JJ,KK)

C**** Empirical data
      CALL TABLE(IOCODE,II,JJ,KK,RKROMX,KR3)

C**** Establish initial conditions......................................
      CALL UINITL(KPI,II,JJ,KK,ET0,CUMPO,MBEO, CUMPW,MBEW,CUMPG,MBEG,
     &WOC,GOC,CUMIW,CUMIG,CAPGO,CAPOW)

C**** Solution method, debug print, and time-step control parameters....
      CALL CODES(IOCODE,KSM1,KSN1,KCO1,NN,FACT1,FACT2,TMAX,KSOL,MITER,
     & OMEGA,TOL,TOL1,KSN,KSM,KCO,KCOFF,KSCRN, KOUT, DSMAX,DPMAX,
     & NUMDIS,IRK,THRUIN,WORMAX,GORMAX,PAMIN,PAMAX)

C**** Aquifer data entry
      CALL AQUI(TMAX)

C==== READ NO. OF WELLS, NODES/WELL & CONTROLS & complete writes to _.CGD file 
      CALL WELLS(NWELLS,WELNAM,KSCRN,NODTOT,IDWEL,NPW,
     &           LWX,LWY,LWZ,IDIR, IOCODE)

C**** If a fatal input error occurs, do not begin run
      IF(IFATAL.GE.1) GO TO 1006
      IF(IWARN.GT.0) WRITE(IOCODE,19) IWARN
   19 FORMAT(
     &/5X,5('-'),'A TOTAL OF',I5,' WARNINGS HAVE BEEN NOTED',5('-'),/)


20    CONTINUE


C.... Read Recurrent Data Header
      READ(20,69)

      D5615=1.0/5.615
      D288=1.0/288.
      D144=1.0/144.

C...  Determine time step index range
      NINIT=1
      IF(IREOPT.NE.1) GO TO 24
      NINIT=IRSTRT
24    NMAX=NN+1


C============== THIS IS BEGINNING OF TIME-STEP LOOP ====================
C.... Recurrent calc loop....................
      DO 1000 N=NINIT,NMAX
      NLOOP=N

c;;;;;;;;;;;;;;;;;;;;; Restart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
C**** Restart algorithm
      IF(IREOPT.EQ.-1) GO TO 30

      IF(IREOPT.EQ.0) GO TO 25
      IF(N.LT.IRSTRT) GO TO 1000
      IF(N.GT.IRSTRT) GO TO 25
      IRS=0
   22 IRS=IRS+1
      READ (24) CPIAQ1,CPIAQ2,CPI1,CPI2,EWAQ,CUMAQW,
     & QWAQ,CUMEW,IAQREG,PAQ,PIAQ,
     & P,SO,SW,SG,PN,SON,SWN,SGN,
     & CUMO,CUMW,CUMG,TX,TY,TZ, DX,DY,DZ, 
     & QOC,QWC,QGC,PID,PWF,PWFC,GMO,GMW,GMG,
     & BO,BW,BG,VP,CT,EL,PBOT,PBOTN, RPO,RPW,RPG,RSOA,
     & NPW,LWX,LWY,LWZ, ALPHA,TOP, KR3, CAPGO,CAPGW,
     & RHOO,RHOW,RHOG, VISO,VISW,VISG, IDIR,IDWEL

C**    VECTORS
      READ (24) QWAQR,CUMAQR, KX,KY,KZ,PHI,
     & ITSNO,STIME,SOPROD,SGPROD,SWPROD,SGOR,SWOR,
     & SGINJ,SWINJ,SPVWTP,SOCUMP,SWCUMP,SGCUMP,
     & SGCUMI,SWCUMI,SAQUIR,SAQUIC, GORT,WORT,GORL,WORL,
     & ILIMOP,KIP,WELNAM,LAYER,IDWELL, QVO,QVG,QVW,QVT, IQN1,IQN2,IQN3,
     & MSAT,MPOT,MPWT,MPGT,RHOSCO,RHOSCW,RHOSCG, VSLOPE,BSLOPE,RSLOPE,
     & SAT,KROT,KRWT,KRGT,KROGT,PCOWT,PCGOT,POT,MUOT,BOT,BOPT,RSOT,RSOPT
     &,PWT,MUWT,BWT,BWPT, PGT,PSIT,MUGT,BGT,BGPT,PRT,CRT,
     & WQG,WQW,WQO,WCGP,WCWP,WCOP,WGOR,WWOR,WSGAV,WSWAV,WSOAV,WPOAV,
     & WPWFAV, QGN,QWN,QON,CUMGN,CUMWN,CUMON,NGOR,NWOR,SGG,SWW,SOO,  
     & POO,NPWF

C**    SCALARS
      READ (24) II,JJ,KK,KSM1,KSN1,KCO1,KSOL,MITER, NUMDIS,IRK,THRUIN,
     & FACT1,FACT2,OMEGA,TOL,TOL1,WORMAX,GORMAX,PAMIN,PAMAX,
     & NVQN,CWI,CGI,ETI,PBO,PMAXT, KSCRN,KOUT, KDX,KDY,KDZ,KDZNET,
     & STBO,STBOI,STBW,STBWI,MCFG,MCFGI,MBEO,MBEW,MBEG,CMBEO,CMBEW,
     & CMBEG,TOOIP,TOWIP,TOGIP,TODGIP,TOFGIP,DELT0,RESVOL,OP,WP,GP,WI,
     & GI,PAVG0,PAVG,OPR,WPR, GPR,WIR,GIR,COP, CWP,CGP,MCFG1,MCFGT,
     & CWOR,WOR,CGOR,GOR,DSMAX,DPMAX,KCOFF,KSN,KSM, KCO,SWR,PDATUM,
     & GRAD,DSMC,DPMC,IAQOPT,NTSTEP,IREPRS,ITHREE, NROCK,NPVT,IROCK,
     & IPVT,NWELLS,NODTOT,RKROMX,KXMP,KYMP,KZMP,IPMAP,ISOMAP,ISWMAP,
     & ISGMAP,IPBMAP,IAQMAP,KROMP,KRWMP,KRGMP,IRSOMP,IPCOW,IPCGO,KPHIMP

      WRITE(21,31) IRS,NTSTEP,ETI
      WRITE(9,31)  IRS,NTSTEP,ETI
      WRITE(*,31)  IRS,NTSTEP,ETI
   31 FORMAT(/T5,70('-'),/T5,
     & 'RESTART DATA READ - RESTART RECORD NO.=',I5,/T5,
     & 'NTSTEP =',I5,' AND ELAPSED TIME (ETI) =',F12.4,' DAYS.',
     & /T5,70('-'),/)

      IF(N.EQ.NTSTEP) GO TO 25
      IF(IRS.LT.3) GO TO 22

      WRITE(IOCODE,23) IRS,N,ETI,NTSTEP
   23 FORMAT(/1X,70('-'),/1X,
     &'ATTEMPT TO READ RESTART DATA FAILED AFTER',I3,' TRIES @ STEP',I4,
     &/1X,'TIME =',F10.2,' DAYS',3X,'LAST RESTART TIME STEP READ =',I5,
     &/1X,70('-')/)
      GO TO 1006

   25 CONTINUE

      IF(ETI.NE.REDATE(IRSCNT)) GO TO 30
      IF(IRSCNT.GT.3) GO TO 28
      NTSTEP=N
c--------------------------------
      INCLUDE 'RESTART.INC'
c--------------------------------
      WRITE(21,26) NTSTEP,ETI
      WRITE(9,26) NTSTEP,ETI
      WRITE(*,26) NTSTEP,ETI
   26 FORMAT(/T5,70('-'),
     &/T5,'RESTART RECORD WRITTEN AT BEGINNING OF STEP',I5,
     &/T5,'   TIME =',F12.4,' DAYS',
     &/T5,70('-'),//)

      IRSCNT=IRSCNT+1
      GO TO 30

   28 WRITE(IOCODE,29) IRSCNT,NTSTEP,ETI
   29 FORMAT(/T10,100('-'),/T10,
     & 'THE NUMBER OF RESTART WRITE (',I2,') EXCEEDS THE',
     & ' ALLOWED NUMBER AT TIME STEP',I5,
     & ' (TIME =',F10.2,' DAYS)',/T10,100('-'),/)
      GO TO 1006


30    CONTINUE
C;;;;;;;;;;;;;;;;;;;; End Restart Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF(N.GT.NINIT) GO TO 3000

c-------------------- Write grid and well data to .CGD file ------------------
      DO 9550 I=1,II
      DXNEW(I) = DX(I,1,1)
      IF(KDY.EQ.1) THEN
         DY1(I) = DY(I,1,1)
      ENDIF
9550  CONTINUE

      IF(KDY.LT.1) THEN
         DO 9560 J=1,JJ
         DYNEW(J) = DY(1,J,1)
9560     CONTINUE
      ENDIF

C------- NOTE: COLORGRD only recognizes KDY=0 or KDY=1; since 0 and -1
C-------       have the same meaning anyway, will change -1 to 0 for 
C-------       input to COLORGRD.
C---- NOTE: There is no KRAD=1 option in MISC4 OR BOAST3 -  so just define KRAD=0
      KDYY = KDY
      IF(KDYY.EQ.-1) KDYY = 0
      KRAD = 0     

      WRITE(17,959)
959   FORMAT(1X,'   NX   NY   NZ KRAD  KDYY')
      WRITE(17,9611) II,JJ,KK,KRAD,KDYY
9611  FORMAT(1X,5I5)
         WRITE(17,9631)
9631     FORMAT(1X,'.............. X-direction blocks...............')
         WRITE(17,9651) (DXNEW(I),I=1,II)
9651     FORMAT(1X,100G12.5)

         WRITE(17,9671)
9671     FORMAT(1X,'.............. Y-direction blocks...............')
         IF(KDYY.EQ.0) WRITE(17,9651) (DYNEW(J),J=1,JJ)
         IF(KDYY.EQ.1) WRITE(17,9651) (DY1(I),I=1,II)

c----------------- Now write Record Length for ___.CGD file  ----------------
      NBYTES = 4 + 30 + II*JJ*KK*4
c.... Round up to nearest multiple of 128 bytes (not really necessary).
      MAPREC = (NBYTES/128 + 1) * 128

      WRITE(*,77751) NBYTES,MAPREC
      WRITE(9,77751) NBYTES,MAPREC
      WRITE(IOCODE,77751) NBYTES,MAPREC
77751 FORMAT(//1X,
     &'NBYTES=',I5,'  Record Length of Map file: MAPREC=',I5/)

c.... Write record length to ____.CGD file............
      WRITE(17,7777)  
7777  FORMAT(1X,'Record Length for .MAP file')
      WRITE(17,7779) MAPREC 
7779  FORMAT(1X,2I5)

c----------------- Now write well and node data to .CGD file ----------------
      WRITE(17,1059)
1059  FORMAT(1X,'No. of Wells')
      WRITE(17,961) NWELLS
961   FORMAT(1X,I5)

      WRITE(17,963)
963   FORMAT(1X,' Well  Nodes  WellName')
965   FORMAT(1X,I5,I5,1X,A8)

      DO 411 NW = 1,NWELLS
      WRITE(17,965) IDWEL(NW), NPW(NW), WELNAM(NW)
411   CONTINUE

      WRITE(17,9591)
9591  FORMAT(1X,' Well   Node(i,j,k)   DIR')

      DO 4111 NW=1,NWELLS
      NODS = NPW(NW)
      DO 4111 M=1,NODS 
      I = LWX(NW,M)  
      J = LWY(NW,M)  
      K = LWZ(NW,M)  
      IDIRX = IDIR(NW,M) 
      WRITE(17,9611) IDWEL(NW),I,J,K,IDIRX
4111  CONTINUE
C*********************  ____.CGD FILE is now complete *********************



      OPEN(18, FILE = 'B.MAP', STATUS = 'UNKNOWN', FORM= 'UNFORMATTED',
     &         ACCESS= 'DIRECT', RECL=MAPREC)
C========================================================================

C========================================================================
C.... Write "time zero or initial" stuff to ____.MAP file.................
c===> NOTE :  If XMULT > 0.0, the array passed will be multiplied by XMULT.
 
         R4TIME = 0.0
         XMULT  = 0.0
         MREC = 0

         PARNAM  = 'Top Elev, ft                  '
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,TOP,XMULT,PARNAM)

         PARNAM  = 'Thickness, ft                 '
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,DZ,XMULT,PARNAM)

         PARNAM  = 'X-dir grid, ft                '
         MREC = MREC+1
         WRITE(18,REC=MREC)
     &   R4TIME,PARNAM,(((DX(I,J,K), I=1,II), J=1,JJ), K=1,KK)

         PARNAM  = 'Y-dir grid, ft                '
         MREC = MREC+1
         WRITE(18,REC=MREC)
     &   R4TIME,PARNAM,(((DY(I,J,K), I=1,II), J=1,JJ), K=1,KK)

         IF(KXMP.GT.0) THEN
            PARNAM  = 'X-perm, md                    '
            CALL OUTMAP(MREC,II,JJ,KK,R4TIME,KX,XMULT,PARNAM)
         ENDIF

         IF(KYMP.GT.0) THEN
            PARNAM  = 'Y-perm, md                    '
            CALL OUTMAP(MREC,II,JJ,KK,R4TIME,KY,XMULT,PARNAM)
         ENDIF

         IF(KZMP.GT.0) THEN
            PARNAM  = 'Z-perm, md                    '
            CALL OUTMAP(MREC,II,JJ,KK,R4TIME,KZ,XMULT,PARNAM)
         ENDIF

C------- ADD THESE LATER - TX, TY, TZ have different dimensions --------------
C-------                 - and therefore OUTMAP cannot be used ---------------
C        IF(ITCODE.GT.0) THEN
C           ITXW = 1
C           ITYW = 1
C           IYZW = 1
C        ENDIF
C         IF(ITXW.GT.0) THEN
C            PARNAM  = 'X-dir Trans.                  '
C            CALL OUTMAP(MREC,II,JJ,KK,R4TIME,TX,XMULT,PARNAM)
C         ENDIF
C
C         IF(ITYW.GT.0) THEN
C            PARNAM  = 'Y-dir Trans.                  '
C            CALL OUTMAP(MREC,II,JJ,KK,R4TIME,TY,XMULT,PARNAM)
C         ENDIF
C
C         IF(ITYZ.GT.0) THEN
C            PARNAM  = 'Z-dir Trans.                  '
C            CALL OUTMAP(MREC,II,JJ,KK,R4TIME,TZ,XMULT,PARNAM)
C         ENDIF
C=============================================================================


C.... Calculate record length for binary file (to contain data for B3PLOT)
c.... Open file and write first (header) record..........................
c--------------- Description of record length of ___.BPD file ------------
c....         time    welmam    field inj stuff   field-well-node stuff
c.... NRECL =  1*4  + 8*NWELLS  +     7*4      +  13(1+NWELLS+NODTOT)*4
c------------------------------------------------------------------------
      NBYTES = 32 +   8*NWELLS  + 52*(1+NWELLS+NODTOT)
c.... Round up to nearest multiple of 128 bytes (not really necessary).
      NRECL = (NBYTES/128 + 1) * 128
      WRITE(*,77753) NBYTES,NRECL
      WRITE(9,77753) NBYTES,NRECL
      WRITE(IOCODE,77753) NBYTES,NRECL
77753 FORMAT(//1X,
     &'NBYTES=',I5,'  Record Length of BPD file = ',I5,' bytes'/)
   
C.... Open binary production data file & write run ID header..........
C.... Note that record length (NRECL) is not known until this point!
      OPEN(14, FILE = 'B.BPD', STATUS = 'UNKNOWN', FORM= 'UNFORMATTED',
     &         ACCESS= 'DIRECT', RECL=NRECL)
      IREC=1
      WRITE(14,REC=IREC) HEADER,NWELLS,(NPW(L),L=1,NWELLS)

C.... Write to ___.GWN file (basic information: grid, well, nodes & NRECL)
C.... Note:  NRECL must be known by POSTSIM in order to open ___.BPD file....
      WRITE(15,27713) HEADER
27713 FORMAT(1X,A75)
      WRITE(15,151) II,JJ,KK, NRECL
151   FORMAT(1X,3I5,I10)    

      WRITE(15,153) NWELLS,NODTOT 
      DO 152 L=1,NWELLS
      DO 152 M=1,NPW(L)
C.... Retrieve (i,j,k) location ...................
      I = LWX(L,M)
      J = LWY(L,M)
      K = LWZ(L,M)
      WRITE(15,153) L,IDWEL(L),WELNAM(L),NPW(L), I,J,K
152   CONTINUE
153   FORMAT(1X,    I5,I5,     3X,A8,     I5,  3X,I2,'-',I2,'-',I2)
C=======================================================================

3000  CONTINUE


C**** RECURRENT DATA....................................................
      IF(N.EQ.1) GO TO 40
      IF(FT.LT.FTMAX) GO TO 46
      IF(IOMETH.EQ.0) GO TO 40

      IF(IFTCOD.EQ.IOMETH) GO TO 40
      IFTCOD=IFTCOD+1
      FTMAX=FTIO(IFTCOD)
      IF(FTMAX.GT.TMAX) FTMAX=TMAX
      GO TO 46

C.... Read Header (e.g. "Data Set 1") ....................
40    READ(20,69,END=1006)

C==================================================================
C.....NOTE THAT IOMETH & IWLCNG REVERSED - MORE LOGICAL
C==================================================================
C      READ(20,*) ICHANG
C      READ(20,*) IOMETH
C      READ(20,*) IWLCNG
      READ(20,*) ICHANG, IOMETH, IWLCNG

      IF(IOMETH.GT.0) GO TO 42
      READ(20,*) IWLREP,ISUMRY
      READ(20,*) IPMAP,ISOMAP,ISWMAP,ISGMAP,IPBMAP,IAQMAP 
      READ(20,*) KROMP,KRWMP, KRGMP, IRSOMP, IPCOW,IPCGO, KPHIMP
      READ(20,*) DAY,DTMIN,DTMAX
      DELT=DAY
      FTMAX=ETI+ICHANG*DELT
      IF(FTMAX.GT.TMAX) FTMAX=TMAX
      IFREQW=0
      IFREQS=0
      GO TO 43

42    READ(20,*) (FTIO(I),I=1,IOMETH)
      READ(20,*) IPMAP,ISOMAP,ISWMAP,ISGMAP,IPBMAP,IAQMAP
      READ(20,*) KROMP,KRWMP,KRGMP,IRSOMP, IPCOW,IPCGO, KPHIMP
      READ(20,*) DAY,DTMIN,DTMAX

      DELT=DAY
      IFTCOD=1
      FTMAX=FTIO(IFTCOD)
      IF(FTMAX.GT.TMAX) FTMAX=TMAX
   43 CONTINUE
      IF(IWLCNG.EQ.0) GO TO 45
      CALL NODES(NVQN)

C.... Time step summary heading activated
      ITSS=0
45    CONTINUE

46    CONTINUE

      IF(N.EQ.1) THEN
         DELT0=DELT
         GO TO 1050
      ENDIF

C.... Auto time-step adjustment.........................................
      IF(DSMC.LT.DSMAX.AND.DPMC.LT.DPMAX) DELT=DELT*FACT1
      IF(DSMC.GT.DSMAX.OR.DPMC.GT.DPMAX)  DELT=DELT*FACT2
      IF(DELT.GT.DTMAX) DELT=DTMAX

      IF(IRK.EQ.0) GO TO 1045
C.... R-K Soln loop max.
      DO 47 I=1,II
      DO 47 J=1,JJ
      DO 47 K=1,KK
   47 THRUA(I,J,K)=0.0

      DO 48 J=1,NVQN
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJLOC=IDWELL(J)

      LAY=IQ3+(LAYER(J)-1)
      DO 48 K=IQ3,LAY
      BPT=PBOT(IQ1,IQ2,K)
      PP=P(IQ1,IQ2,K)
      IPVTR=IPVT(IQ1,IQ2,K)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PP,BBO)
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PP,BBW)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PP,BBG)
      THRUA(IQ1,IQ2,K)= THRUA(IQ1,IQ2,K) + DELT*
     & (QOC(IJLOC,K)*BBO + QWC(IJLOC,K)*BBW + QGC(IJLOC,K)*BBG)

48    CONTINUE


      THRU=0.
      DO 49 J=1,NVQN
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      LAY=IQ3+(LAYER(J)-1)
      DO 49 K=IQ3,LAY
      IF(VP(IQ1,IQ2,K).LE.0.0) GO TO 49
      THRUPV=ABS(THRUA(IQ1,IQ2,K))/VP(IQ1,IQ2,K)
      IF(THRU.LT.THRUPV) THRU=THRUPV
   49 CONTINUE

      KRUNGE=1.+SQRT(THRU/THRUIN)
      IF(KRUNGE.LE.7) GO TO 1045
      DELT=DELT*7./KRUNGE
      KRUNGE=7

 1045 CONTINUE

      IF(DELT.LT.DTMIN)     DELT=DTMIN
      IF(ETI+DELT.GT.FTMAX) DELT=FTMAX-ETI
1050  FT=ETI+DELT
      IF(ETI+DELT*0.5.GE.TMAX) GO TO 1006

C**** Itflag counts the number of time step repetitions.
      ITFLAG=0

C**** Reentry point for repeated time step..........................

1060  CONTINUE
      DIV1=1.0/DELT
      IF(N.GT.1.OR.ITFLAG.GT.0) GO TO 105

C**** Initial fluid calculations........................................
      WRITE(IOCODE,80)
   80 FORMAT(///T15,'******* INITIAL FLUID VOLUMES *******'/)
      RESVOL=0.0
      SCFO=0.0
c......... added SCFW - 10/02/93 .........
      SCFW=0.0
      SCFG=0.0
      SCFG1=0.0

      DO 102 K=1,KK
      OOIP(K)=0.
      OWIP(K)=0.
      ODGIP(K)=0.
      OFGIP(K)=0.

      DO 100 J=1,JJ
      DO 100 I=1,II
      PP=P(I,J,K)
c........ must use correct phase pressures - fixed 10/02/93 ...........
c........... (was ok in 450-loop below) 
      PPG = PP + CAPGO(I,J,K)
      PPW = PP - CAPOW(I,J,K)
c........ note that PPG & PPW are used below to get gas & water properties !!

      BPT=PBOT(I,J,K)
      RESVOL=RESVOL+VP(I,J,K)
      IPVTR=IPVT(I,J,K)
C**** NOTE: We are assuming initial phi is at initial reservoir pressure
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PP,RSO)
C==============================
      RSOA(I,J,K) = RSO
C==============================
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),
     & PP,BO(I,J,K))
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PPW,BW(I,J,K))
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PPG,BG(I,J,K))
C
C--------- Also get viscosities & calculate densities here !!!!!

      CALL INTPVT(IPVTR,BPT,VSLOPE(IPVTR),POT,MUOT,MPOT(IPVTR),PP,MUO)
      CALL INTERP(IPVTR,PWT,MUWT,MPWT(IPVTR),PPW,MUW)
      CALL INTERP(IPVTR,PGT,MUGT,MPGT(IPVTR),PPG,MUG)
      VISO(I,J,K) = MUO
      VISW(I,J,K) = MUW
      VISG(I,J,K) = MUG

      RHOO(I,J,K) =(RHOSCO(IPVTR) + RSO*RHOSCG(IPVTR))/BO(I,J,K)
      RHOW(I,J,K) = RHOSCW(IPVTR)/BW(I,J,K)
      RHOG(I,J,K) = RHOSCG(IPVTR)/BG(I,J,K)
C-----------------------------------------------------------------------


      FF1  = SO(I,J,K)/BO(I,J,K)
      FF2  = SW(I,J,K)/BW(I,J,K)
      SCFO = SCFO+VP(I,J,K)*FF1
      SCFW = SCFW+VP(I,J,K)*FF2
      SCFG = SCFG+VP(I,J,K)*SG(I,J,K)/BG(I,J,K)
      SCFG1= SCFG1+VP(I,J,K)*(RSO*FF1)
C**** Calculate compressibilitiies......................................
      CALL INTCOM(IPVTR,POT,BOPT,MPOT(IPVTR),PP,BODER)
      CALL INTCOM(IPVTR,POT,RSOPT,MPOT(IPVTR),PP,RSODER)
C=================================
      DRSODP(I,J,K) = RSODER
C=================================
      CALL INTCOM(IPVTR,PWT,BWPT,MPWT(IPVTR),PP,BWDER)
      CALL INTCOM(IPVTR,PGT,BGPT,MPGT(IPVTR),PP,BGDER)
c.......................................................................
CCC      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PP,BBG)
CCC      PP1 = PP + 5.0
CCC      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PP1,BBG1)
CCC      BGDER = (BBG1 - BBG)/5.0
c.......................................................................
      IF(PP.GT.PBOT(I,J,K)) BODER=BSLOPE(IPVTR)
      IF(PP.GT.PBOT(I,J,K)) RSODER=RSLOPE(IPVTR)
      CO = -(BODER-BG(I,J,K)*RSODER)/BO(I,J,K)
      CW = -BWDER/BW(I,J,K)
      CG = -BGDER/BG(I,J,K)
      CALL INTERP(IPVTR,PRT,CRT,MPGT(IPVTR),PP,CR)
      CT(I,J,K)=CR + CO*SO(I,J,K) +CW*SW(I,J,K) + CG*SG(I,J,K)

      IF(KCO1.NE.0)
     & WRITE(IOCODE,33771) I,J,K,PP,BWDER,BW(I,J,K),CW,BGDER,BG(I,J,K),CG
33771 FORMAT(1X,'IJK,P,BWDER,BW(),CW,BGDER,BG(),CG:',
     &      /1X,3I3,F8.1,G12.5,F8.4,4G12.5)

C**** Calculate initial fluids in place.................................
c.... ch'd SON, SWN and SGN to SO, SW and SG on the following four stmt's 
c.... 10/02/93 ......
      OOIP(K)=OOIP(K)+D5615*1.E-06*SO(I,J,K)*VP(I,J,K)/BO(I,J,K)
      OWIP(K)=OWIP(K)+D5615*1.E-06*SW(I,J,K)*VP(I,J,K)/BW(I,J,K)
      ODGIP(K)=ODGIP(K)+0.001*1.E-06*(RSO*SO(I,J,K)*
     & VP(I,J,K)/BO(I,J,K)  )
      OFGIP(K)=OFGIP(K)+1.E-09*SG(I,J,K)*VP(I,J,K)/BG(I,J,K)

      IF(KCO1.EQ.0) GO TO 100
      WRITE(IOCODE,21) I,J,K,VP(I,J,K),CT(I,J,K),BO(I,J,K),SO(I,J,K),
     & BW(I,J,K),SW(I,J,K),BG(I,J,K),SG(I,J,K)

  100 CONTINUE

      WRITE(IOCODE,110)K,OOIP(K),OWIP(K),ODGIP(K),OFGIP(K)

  110 FORMAT(/,T15,'LAYER',I3,' INITIAL FLUID VOLUMES:',
     & /,T20,'OIL IN PLACE (MILLION STB)',T65,F16.10,
     & /,T20,'WATER IN PLACE (MILLION STB)',T65,F16.10,
     & /,T20,'SOLUTION GAS IN PLACE (BILLION SCF)',T65,F16.10,
     & /,T20,'FREE GAS IN PLACE (BILLION SCF)',T65,F16.10/)
  102 CONTINUE

      TOOIP=0.
      TOWIP=0.
      TODGIP=0.
      TOFGIP=0.
      DO 103 K=1,KK
      TOOIP=TOOIP+OOIP(K)
      TOWIP=TOWIP+OWIP(K)
      TODGIP=TODGIP+ODGIP(K)
      TOFGIP=TOFGIP+OFGIP(K)
  103 CONTINUE

      TOGIP = TODGIP + TOFGIP
      WRITE(IOCODE,115) TOOIP,TOWIP,TODGIP,TOFGIP
  115 FORMAT(/,T15,'TOTAL INITIAL FLUID VOLUMES IN RESERVOIR:',
     & /,T20,'OIL IN PLACE (MILLION STB)',T65,F16.10,
     & /,T20,'WATER IN PLACE (MILLION STB)',T65,F16.10,
     & /,T20,'SOLUTION GAS IN PLACE (BILLION SCF)',T65,F16.10,
     & /,T20,'FREE GAS IN PLACE (BILLION SCF)',T65,F16.10/)

      STBO=SCFO*D5615
      STBW=SCFW*D5615
      MCFG=SCFG*0.001
      MCFG1=SCFG1*0.001
cccC!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ccc      WRITE(*,10011) MCFG,MCFG1,TOFGIP
ccc10011 FORMAT(/1X,'MCFG,MCFG1,TOFGIP:',3G15.6/)
ccc      PAUSE
cccC!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      STBOI=STBO
      STBWI=STBW
      MCFGT=MCFG+MCFG1
      NLOOP=N

  105 IF(N.EQ.NINIT.AND.ITFLAG.LE.0)
     & CALL PRTPS(NLOOP,II,JJ,KK,PAVG0,PAVG,CMBEO,CMBEW,CMBEG,
     & COP,CWP,CWI,CGP,CGI,MBEO,MBEW,MBEG,DELT0,
     & OPR,WPR,GPR,WIR,GIR,ETI, KOUT, 
     &CWOR,CGOR,WOR,GOR,IPMAP,ISOMAP,ISWMAP,ISGMAP,IPBMAP,IAQMAP,
     &KROMP,KRWMP,KRGMP, IRSOMP, IPCOW,IPCGO, KPHIMP,
     &RSOA,QGRSO,RKROMX,QGRSOT,CAPGO,CAPOW, RPO,RPW,RPG)

      IF(N.EQ.NMAX) GO TO 1006


C**** BEGIN OUTER ITERATION LOOP FOR STABILIZED IMPES ******************
C     -----------------------------------------------
      ITNCNT=0
 4000 ITNCNT=ITNCNT+1

C**** Establish rates & calculate BHFP(if PID is nonzero)...............
      IF(NVQN.EQ.0) GO TO 1160
      CALL QRATE(II,JJ,KK,NVQN,GORMAX,WORMAX,ETI
     &,NLOOP,DELT,RKROMX,DRSODP,QGRSO,QGRSOT,CAPGO,CAPOW)
1160  CONTINUE

C**** CALCULATE SEVEN DIAGONAL MATRIX FOR FLOW EQ SOLUTION *************
C     ----------------------------------------------------

      IF(NUMDIS.EQ.0) THEN
         CALL SOLWKS(NLOOP,II,JJ,KK,DIV1,D288,KSM,KSM1,N,NN,
     &   KCOFF, RPW,RPG, RPO,RSOA,VISO,VISW,VISG,RHOO,RHOW,RHOG,KR3,
     &   CAPGO,CAPOW)
      ENDIF

      IF(NUMDIS.EQ.1) CALL SOLTWO(II,JJ,KK,DIV1,D288,
     &KSM,KSM1,N,NN,KCOFF, RPO,RPW,RPG,CAPOW,CAPGO)

C!!!! NOTE:  CAPOW & CAPGO are defined in SOLWKS or SOLTWO !!!


C**** Modify matrix elements for wells under implicit control...........
      IF(NVQN.EQ.0) GO TO 1170
      CALL PRATEI(II,JJ,KK,RKROMX,DELT,CAPGO,CAPOW,QGRSOT,DRSODP,NVQN)
 1170 CONTINUE

C***  AQUIFER MODEL: P EQ COEF. MODIFICATIONS **************************
      IF(IAQOPT.NE.0) CALL AQIN(II,JJ,KK,DELT,ETI)

C**** CALCULATE NEW PRESSURE DISTRIBUTION ******************************
C     -----------------------------------
C     Skip P calc when outer iteration exceeds 1
      IF(ITNCNT.GT.1) GO TO 1175

      IF(KSOL.EQ.1) CALL GAUS1D(IOCODE,IFATAL,II,JJ,KK)

      IF(IFATAL.GE.1) GO TO 1006

      IF(KSOL.EQ.2) CALL LSORX(II,JJ,KK,OMEGA,TOL,TOL1,MITER,
     & DELT,DELT0,KSN,NLOOP,NLITER)

      IF(KSOL.EQ.3) CALL LSORY(II,JJ,KK,OMEGA,TOL,TOL1,MITER,
     & DELT,DELT0,KSN,NLOOP,NLITER)

      IF(KSOL.EQ.4) CALL LSORZ(II,JJ,KK,OMEGA,TOL,TOL1,MITER,
     & DELT,DELT0,KSN,NLOOP,NLITER)

 1175 CONTINUE


C**** Calculate implicit rates..........................................
      IF(NVQN.EQ.0) GO TO 1180
      CALL PRATEO(RKROMX,QGRSOT,QGRSO,DRSODP,CAPOW,CAPGO,NVQN)
 1180 CONTINUE

C**** AQUIFER MODEL RATES **********************************************
      IF(IAQOPT.NE.0) CALL AQOUT(II,JJ,KK,DELT)

C**** CALCULATE NEW FLUID SATURATIONS **********************************
C     -------------------------------
      SCFO=0.0
      SCFW=0.0
      SCFG=0.0
      SCFG1=0.0
      RESVOL=0.0

c......... Also need average saturations for ____.BPD file
      SOAV = 0.0
      SWAV = 0.0
      SGAV = 0.0

      DO 400 K=1,KK
      DO 400 J=1,JJ
      DO 400 I=1,II
C.... Calculate new reservoir pore volume...............................
      PPN  = PN(I,J,K)
      PP   = P(I,J,K)
      PPG   = PP + CAPGO(I,J,K)
      PPW   = PP - CAPOW(I,J,K)

      BPT  = PBOT(I,J,K)
      IPVTR= IPVT(I,J,K)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PP,RSO)
      RSOA(I,J,K) = RSO
      CALL INTERP(IPVTR,PRT,CRT,MPGT(IPVTR),PPN,CR)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PP,BBO)
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PPW,BBW)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PPG,BBG)
      VPP=VP(I,J,K) * (1.0+CR*(P(I,J,K)-PPN))
      RESVOL=RESVOL+VPP
      DP1=0.0
      DP2=0.0
      DP3=0.0
      DP4=0.0
      DP5=0.0
      DP6=0.0
      IF((I-1).GT.0)  DP1=P(I-1,J,K)-PP
      IF((I+1).LE.II) DP2=P(I+1,J,K)-PP
      IF((J-1).GT.0)  DP3=P(I,J-1,K)-PP
      IF((J+1).LE.JJ) DP4=P(I,J+1,K)-PP
      IF((K-1).GT.0)  DP5=P(I,J,K-1)-PP
      IF((K+1).LE.KK) DP6=P(I,J,K+1)-PP
 
      DAODP = O1(I,J,K)*DP1 + O1(I+1,J,K)*DP2 + O2(I,J,K)*DP3
     &       +O2(I,J+1,K)*DP4 + O3(I,J,K)*DP5 + O3(I,J,K+1)*DP6

      DAWDP = W1(I,J,K)*DP1 + W1(I+1,J,K)*DP2 + W2(I,J,K)*DP3
     &       +W2(I,J+1,K)*DP4 + W3(I,J,K)*DP5 + W3(I,J,K+1)*DP6

C***  Skip saturation calculations for zero pore volume blocks..........
      IF(VPP.EQ.0.0) GO TO 405

      IRKK = IROCK(I,J,K)

      SW(I,J,K)=((DAWDP+GWWT(I,J,K)-(QW(I,J,K)+EWAQ(I,J,K)))*DELT
     &            +VP(I,J,K)*SWN(I,J,K)/BW(I,J,K)) * BBW/VPP

      SO(I,J,K)=((DAODP+GOWT(I,J,K)-QO(I,J,K))*DELT
     &           +VP(I,J,K)*SON(I,J,K)/BO(I,J,K)) * BBO/VPP

      IF(SO(I,J,K).LE.0.0) SO(I,J,K) = 0.0
      IF(SO(I,J,K).GT.(1.E0-SWR(IRKK)))  SO(I,J,K) = 1.0 - SWR(IRKK)

      SG(I,J,K)=1.0-SO(I,J,K)-SW(I,J,K)

      DIFSW = SW(I,J,K) - SWR(IRKK)
      IF(SW(I,J,K).LT.SWR(IRKK) .OR. ABS(DIFSW).LE.0.001) THEN
         SW(I,J,K) = SWR(IRKK)
         SG(I,J,K) = 1.0-SO(I,J,K)-SW(I,J,K)
         GO TO 405
      ENDIF

      IF(SW(I,J,K).GT.1.0) THEN
         SW(I,J,K) = 1.0
         SG(I,J,K) = 0.0
         SO(I,J,K) = 0.0 
         GO TO 405
      ENDIF

      IF(SG(I,J,K).LT.0.0) THEN
         SG(I,J,K) = 0.0
           IF(RKROMX.LE.1.E-06) SW(I,J,K) = 1.0 - SO(I,J,K)
           IF(RKROMX.GT.1.E-06) SO(I,J,K) = 1.0 - SW(I,J,K)
      ENDIF

405   CONTINUE

      SOAV = SOAV + SO(I,J,K) * VPP
      SWAV = SWAV + SW(I,J,K) * VPP
      SGAV = SGAV + SG(I,J,K) * VPP


      IF(KCOFF.EQ.0) GO TO 397

      RHO1=VPP*SO(I,J,K)/BBO
      RHO2=VP(I,J,K)*SON(I,J,K)/BO(I,J,K)
      DIFFO=RHO1-RHO2
      RHW1=VPP*SW(I,J,K)/BBW
      RHW2=VP(I,J,K)*SWN(I,J,K)/BW(I,J,K)
      DIFFW=RHW1-RHW2
      RHG1=VPP*SG(I,J,K)/BBG
      RHG2=VP(I,J,K)*SGN(I,J,K)/BG(I,J,K)
      DIFFG=RHG1-RHG2
      WRITE(IOCODE,3301)
 3301 FORMAT(1X,' IJK  P  SO  SON  SW  SWN  SG  SGN  VPP:') 
      WRITE(IOCODE,21)I,J,K,P(I,J,K),SO(I,J,K),SON(I,J,K),SW(I,J,K),
     &                   SWN(I,J,K),SG(I,J,K),SGN(I,J,K),VPP
      WRITE(IOCODE,3303)
3303  FORMAT(1X,'  IJK  GOWT  QO  GWWT  QW:')
      WRITE(IOCODE,21)I,J,K,GOWT(I,J,K),QO(I,J,K),GWWT(I,J,K),
     &                      QW(I,J,K)

CCC      WRITE(IOCODE,21)I,J,K,DAODP,DAWDP,DELT
CCC      WRITE(IOCODE,21)I,J,K,RHO1,RHO2,DIFFO
CCC      WRITE(IOCODE,21)I,J,K,RHW1,RHW2,DIFFW
CCC      WRITE(IOCODE,21)I,J,K,RHG1,RHG2,DIFFG,GGWT(I,J,K),QG(I,J,K)

21    FORMAT(1X,3I3,8G13.5)
397   CONTINUE
400   CONTINUE

      DIVR = 1./RESVOL
      SOAV = SOAV * DIVR
      SWAV = SWAV * DIVR
      SGAV = SGAV * DIVR
    



C      NEW SET OF INTERPOLATED VALUES FOR NEXT R-K OUTER ITER.
      IF(IRK.EQ.0) GO TO 4300

      DO 4200 I=1,II
      DO 4200 J=1,JJ
      DO 4200 K=1,KK
      SO(I,J,K)=SON(I,J,K)+RKCOEF(ITNCNT,KRUNGE)*(SO(I,J,K)-SON(I,J,K))
      SW(I,J,K)=SWN(I,J,K)+RKCOEF(ITNCNT,KRUNGE)*(SW(I,J,K)-SWN(I,J,K))
      SG(I,J,K)=SGN(I,J,K)+RKCOEF(ITNCNT,KRUNGE)*(SG(I,J,K)-SGN(I,J,K))
C      INTERPOLATE BUBBLE POINT PRESSURE FOR NEXT R-K ITERATION
      IF(IREPRS.EQ.1) GO TO 4200
      IBP=I
      JBP=J
      KBP=K
      IF(IREPRS.EQ.0) CALL REPRS1(IBP,JBP,KBP)

 4200 CONTINUE
      IF(ITNCNT.EQ.KRUNGE) GO TO 4300
      GO TO 4000

 4300 CONTINUE

C.... Update FVF's and compressibilities................................
      SCFO  = 0.0 
      SCFW  = 0.0 
      SCFG  = 0.0 
      SCFG1 = 0.0  
      DO 450 I=1,II
      DO 450 J=1,JJ
      DO 450 K=1,KK
      PPN=PN(I,J,K)
      PP=P(I,J,K)
      PPG   = PP + CAPGO(I,J,K)
      PPW   = PP - CAPOW(I,J,K)
      BPT=PBOT(I,J,K)
      IPVTR=IPVT(I,J,K)
      CALL INTPVT(IPVTR,BPT,RSLOPE(IPVTR),POT,RSOT,MPOT(IPVTR),PP,RSO)
      RSOA(I,J,K) = RSO
      CALL INTERP(IPVTR,PRT,CRT,MPGT(IPVTR),PPN,CR)
      CALL INTPVT(IPVTR,BPT,BSLOPE(IPVTR),POT,BOT,MPOT(IPVTR),PP,BBO)
      CALL INTERP(IPVTR,PWT,BWT,MPWT(IPVTR),PPW,BBW)
      CALL INTERP(IPVTR,PGT,BGT,MPGT(IPVTR),PPG,BBG)
      VPP=VP(I,J,K) * (1.0+CR*(P(I,J,K)-PPN))
      VP(I,J,K)=VPP
      BO(I,J,K)=BBO
      BW(I,J,K)=BBW
      BG(I,J,K)=BBG

C----------------- Also get viscosities here & calculate densities !!!!!!
      CALL INTPVT(IPVTR,BPT,VSLOPE(IPVTR),POT,MUOT,MPOT(IPVTR),PP,MUO)
      CALL INTERP(IPVTR,PWT,MUWT,MPWT(IPVTR),PPW,MUW)
      CALL INTERP(IPVTR,PGT,MUGT,MPGT(IPVTR),PPG,MUG)
      VISO(I,J,K) = MUO
      VISW(I,J,K) = MUW
      VISG(I,J,K) = MUG

      RHOO(I,J,K) =(RHOSCO(IPVTR) + RSO*RHOSCG(IPVTR))/BO(I,J,K)
      RHOW(I,J,K) = RHOSCW(IPVTR)/BW(I,J,K)
      RHOG(I,J,K) = RHOSCG(IPVTR)/BG(I,J,K)
C-----------------------------------------------------------------------

      FF1=SO(I,J,K)/BO(I,J,K)
      FF2=SW(I,J,K)/BW(I,J,K)
      SCFO=SCFO+VP(I,J,K)*FF1
      SCFW=SCFW+VP(I,J,K)*FF2
      SCFG=SCFG+VP(I,J,K)*SG(I,J,K)/BG(I,J,K)
      SCFG1=SCFG1+VP(I,J,K)*(RSO*FF1)
      CALL INTCOM(IPVTR,POT,BOPT,MPOT(IPVTR),PP,BODER)
      CALL INTCOM(IPVTR,POT,RSOPT,MPOT(IPVTR),PP,RSODER)
      DRSODP(I,J,K) = RSODER
      CALL INTCOM(IPVTR,PWT,BWPT,MPWT(IPVTR),PPW,BWDER)
      CALL INTCOM(IPVTR,PGT,BGPT,MPGT(IPVTR),PPG,BGDER)
      IF(PP.GT.PBOT(I,J,K)) BODER=BSLOPE(IPVTR)
      IF(PP.GT.PBOT(I,J,K)) RSODER=RSLOPE(IPVTR)
      CO=-(BODER-BG(I,J,K)*RSODER)/BO(I,J,K)
C=======================================================================
C.... If oil is immobile, set oil compressibility to zero
      IF(RKROMX.LE.1.E-06) CO = 0.0
C=======================================================================
      CW = -BWDER/BW(I,J,K)
      CG = -BGDER/BG(I,J,K)
      CALL INTERP(IPVTR,PRT,CRT,MPGT(IPVTR),PP,CR)
      CT(I,J,K)=CR + CO*SO(I,J,K) +CW*SW(I,J,K) + CG*SG(I,J,K)
 
      IF(N.EQ.KCO) WRITE(IOCODE,21) I,J,K,CR,CO,RSO,CW,CG

450   CONTINUE

C      AUTO. TIME STEP CONTROL CALC. OF PRESSURE AND SAT. MAXIMA.
      PPM=0.
      SOM=0.
      SWM=0.
      SGM=0.
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
      DPMC=ABS(PPM)
      DSMC=ABS(SOM)
C
      ISM=IOM
      JSM=JOM
      KSMM=KOM
      IF(DSMC.GT.ABS(SGM)) GO TO 245
      DSMC=ABS(SGM)
      ISM=IGM
      JSM=JGM
      KSMM=KGM
  245 IF(DSMC.GT.ABS(SWM)) GO TO 248
      DSMC=ABS(SWM)
      ISM=IWM
      JSM=JWM
      KSMM=KWM
  248 CONTINUE

C****REPEAT TIME STEP?
      IF(DSMC.LT.DSMAX.AND.DPMC.LT.DPMAX) GO TO 402
      IF(DELT.LE.DTMIN.OR.FACT2.GE.1.0)   GO TO 402
      ITFLAG=ITFLAG+1
      DELT=DELT*FACT2
      IF(DELT.LT.DTMIN) DELT=DTMIN
C=======================================================================
      WRITE(IOCODE,2601) NLOOP,DELT, PPM,SOM,SWM,SGM
      WRITE(*,2601)       NLOOP,DELT, PPM,SOM,SWM,SGM
2601  FORMAT(1X,'===>Repeating time-step',I4,3X,'Reduced DELT=',F8.3,
     &/1X,'PPM,SOM,SWM,SGM:',4G15.6)
C=======================================================================
      FT=ETI+DELT
      IF(FT.GT.FTMAX) DELT=FTMAX-ETI
C****RESET VARIABLES.
      DO 260 I=1,II
      DO 260 J=1,JJ
      DO 260 K=1,KK
      P(I,J,K)=PN(I,J,K)
      SO(I,J,K)=SON(I,J,K)
      SW(I,J,K)=SWN(I,J,K)
      SG(I,J,K)=SGN(I,J,K)
      PBOT(I,J,K)=PBOTN(I,J,K)
  260 CONTINUE
      GO TO 1060
  402 CONTINUE

      CALL UNSAT(II,JJ,KK,SO,P,PN,PBOT,SGN,SW,SG)

C**** REPRESSURIZATION ALGORITHM.
      IF(IREPRS.EQ.1) GO TO 5011
      DO 50 I=1,II
      DO 50 J=1,JJ
      DO 50 K=1,KK
      IBP=I
      JBP=J
      KBP=K
      IF(IREPRS.EQ.0) CALL REPRS1(IBP,JBP,KBP)
   50 CONTINUE
5011  CONTINUE

C**** Update old fluid volumes for material balance.....................
      STBOI=STBO
      STBWI=STBW
      MCFGI=MCFGT

C**** Update  new fluid volumes.........................................
      STBO=SCFO*D5615
      STBW=SCFW*D5615
      MCFG=SCFG*0.001
      MCFG1=SCFG1*0.001
      MCFGT=MCFG+MCFG1
CC**** Debug print of present and future p,so,sw,sg values.
C      IF(KCOFF.EQ.0)GO TO 2901
C      DO 290 K=1,KK
C      DO 290 J=1,JJ
C      DO 290 I=1,II
C      WRITE(IOCODE,21)I,J,K,PN(I,J,K),SON(I,J,K),SWN(I,J,K),SGN(I,J,K)
C      WRITE(IOCODE,21)I,J,K,P(I,J,K),SO(I,J,K),SW(I,J,K),SG(I,J,K)
C290   CONTINUE
C2901  CONTINUE

C     AQUIFER MODEL: CUMULATIVES
      IF(IAQOPT.NE.0) CALL AQCUM(II,JJ,KK,DELT,ETI,NLOOP)

C.... Write headers for _______.WEL file ....................................
      IF(NLOOP.EQ.1) THEN
      WRITE(7,681) HEADER 
681   FORMAT(T5,A75)
      WRITE(7,6811) 
6811  FORMAT(/6X,
     &'TIME   NODE  NAME   LOCATION   PWFC     PWFS     QO       QG   ',
     &'     QW    CUMO   CUMG    CUMW      So      Sg      Sw'/6X,        
     &'days                 I  J  K   psia     psia    stb/d    Mcf/d',
     &'   stb/d    Mstb   MMcf    Mstb'/1X,124('-')/)
      ENDIF
C.............................................................................

C**** WELL REPORT (ALL RATE & PRESSURE DATA APPLICABLE THIS STEP)

      IJ=0
      TOR=0.
      TGR=0.
      TWR=0.
      TOC=0.
      TGC=0.
      TWC=0.
      IFREQW=IFREQW+1

      DO 2040 J=1,NVQN
      IQ1=IQN1(J)
      IQ2=IQN2(J)
      IQ3=IQN3(J)
      IJLOC=IDWELL(J)
      IJ=IJ+1

      LAY=IQ3+(LAYER(J)-1)

      DO 2040 K=IQ3,LAY
      GOR=0.
      WOR=0.
      QOO=QOC(IJLOC,K)*D5615
      QWW=QWC(IJLOC,K)*D5615
      QGG=QGC(IJLOC,K)*0.001
      CUMO(J,K)=CUMO(J,K)+QOO*DELT*0.001
      CUMW(J,K)=CUMW(J,K)+QWW*DELT*0.001
      CUMG(J,K)=CUMG(J,K)+QGG*DELT*0.001

      IF(QOO.LE.1.E-06) THEN
         GOR = 0.0
         WOR = 0.0
      ELSE 
         GOR=QGG*1000./QOO
         WOR=QWW/QOO
      ENDIF

C.... Write to _____.WEL file ............................................
      WRITE(7,59333) FT,IDWELL(J),WELNM(J),IQN1(J),IQN2(J),K,PWFC(J,K),
     & PWF(J,K),QOO,QGG,QWW,CUMO(J,K),CUMG(J,K),CUMW(J,K),
     & SO(IQN1(J),IQN2(J),K),SG(IQN1(J),IQN2(J),K),SW(IQN1(J),IQN2(J),K)
59333 FORMAT(1X,F11.4,I5,2X,A5,1X,3I3,2F8.2, F9.1,F9.0,F9.1, F8.1
     &      ,F8.0,F8.1,3F8.4)
C.........................................................................

      IF(IOMETH.GT.0) GO TO 992

      IF(IWLREP.GT.IFREQW.AND.FT.LT.FTMAX) THEN
         GO TO 2040
      ENDIF 

      GO TO 994

C.............. Added ABS - 12/12/91 .................................
  992 IF(ABS(FT-FTMAX).GT..0001) THEN
         GO TO 2040
      ENDIF

  994 CONTINUE
      IF(J.EQ.1 .AND. K.EQ.IQ3) THEN
         WRITE(IOCODE,591) N,FT
         WRITE(IOCODE,592)
      ENDIF

  591 FORMAT(///,
     & T5,10('.'),' WELL REPORT:  Time-Step = ',I4,
     &3X,'Time =',F11.4,' days',3X,10('.'))

  592 FORMAT(/,T54,'------- RATE --------',21X,'----- CUMULATIVE -----',
     & /1X,'  WELL        LOCATION',4X,'CALC    SPEC    SPEC',6X,
     & 'OIL     GAS    WATER    GOR     WOR',5X,
     & 'OIL     GAS    WATER',/1X,
     & '    #    ID',4X,'I  J  K    BHFP    BHFP     PI',
     & 6X,'STB/D    MCF/D   STB/D',19X,'MSTB    MMCF    MSTB'/)

      WRITE(IOCODE,593) IDWELL(J),WELNM(J),IQN1(J),IQN2(J),K,PWFC(J,K),
     & PWF(J,K),PID(J,K),QOO,QGG,QWW,GOR,WOR,CUMO(J,K),CUMG(J,K),
     & CUMW(J,K)

593   FORMAT(1X,I5,2X,A5,1X,3I3,2F8.0,F9.4,F9.1,F9.0,F9.1,F7.1,F7.3,F8.1
     &      ,F8.0,F8.1)

  999 CONTINUE

C.... Time-step summary heading activated...............................
      ITSS=0
      TOR=TOR+QOO
      TGR=TGR+QGG
      TWR=TWR+QWW
      TOC=TOC+CUMO(J,K)
      TGC=TGC+CUMG(J,K)
      TWC=TWC+CUMW(J,K)

2040  CONTINUE


      IF(IOMETH.GT.0) GO TO 2542

      IF(IWLREP.GT.IFREQW.AND.FT.LT.FTMAX) GO TO 2552
      IFREQW=0
      GO TO 2544

 2542 IF(ABS(FT-FTMAX).GT..0001) GO TO 2552

 2544 CONTINUE

      WRITE(IOCODE,594) TOR,TGR,TWR,TOC,TGC,TWC
594   FORMAT(6X,108('-'),/12X,'TOTALS',31X,3F9.1,14X,3F8.1,/)

 2552 CONTINUE

C**** Calculate material balance errors & average reservoir pressure....
      DELT0=DELT
      ETI=ETI+DELT
      CALL MATBAL(II,JJ,KK,STBO,STBOI,STBW,STBWI,TOWIP,TOOIP,TOGIP,
     &MCFGI,MBEO,MBEW,MBEG,CMBEO,CMBEW,CMBEG,DELT0,RESVOL,
     &OP,WP,GP,WI,GI,PAVG0,PAVG,OPR,WPR,GPR,WIR,GIR,D5615,
     &COP,CWP,CGP,CWI,CGI,MCFGT,CWOR,WOR,CGOR,GOR)
C=======================================================================
C.... Fixing WORMAX & GORMAX so they are not used if left zero..........
C.... Will also check QRATE........
      IF(WOR.GT.WORMAX        .AND. WORMAX.GT..0001) GO TO 1002
      IF(1000.0*GOR.GT.GORMAX .AND. GORMAX.GT..0001) GO TO 1003
C=======================================================================
      IF(PAVG.LT.PAMIN) GO TO 1004
      IF(PAVG.GT.PAMAX) GO TO 1005


C------------- COPIED & ADAPTED FROM MISC4 - 07/02/93 ----------------------------
C.... Write to ___.BPD file ............................................
      IREC = IREC+1
      CALL WRTBPD(IREC,NWELLS,NODTOT,WELNAM, FT, D5615, DELT,
     &     GIR,   WIR, CGI, CWI, PAVG, 
     &     GPR,   WPR, OPR, 
     &     CGP,   CWP, COP,  
     &     GOR,   WOR, 
     &     SGAV, SWAV, SOAV,  
     & NPW,LWX,LWY,LWZ,
     & QO,QW,QG,SO,SW,SG,P,PWF,PWFC)


      IF(ABS(FT-FTMAX).LT..0001) THEN
c....... Write to ___.MAP file...............
c.......  (modified  WRTMAP for BOAST3 07/03/93) ..........................
         R4TIME = ETI
         CALL WRTMAP(MREC,II,JJ,KK,R4TIME,XMULT,
     &   P,SO,SW,SG,RPO,RPW,RPG,CAPOW,CAPGO,PBOT,RSOA,QWAQ)
      ENDIF


C.... Summary report....................................................
      IFREQS=IFREQS+1
      IF(IOMETH.GT.0) GO TO 2554
      IF(ISUMRY.GT.IFREQS.AND.FT.LT.FTMAX) GO TO 2557
      IFREQS=0
      GO TO 2556
2554  CONTINUE
      IF(FT.LT.FTMAX) GO TO 2557
2556  NLP=N+1

C.... Time-step summary heading activated...............................
      ITSS=0

      CALL PRTPS(NLP,II,JJ,KK,PAVG0,PAVG,CMBEO,CMBEW,CMBEG,
     &COP,CWP,CWI,CGP,CGI,MBEO,MBEW,MBEG,DELT0,
     &OPR,WPR,GPR,WIR,GIR,ETI, KOUT, 
     &CWOR,CGOR,WOR,GOR,IPMAP,ISOMAP,ISWMAP,ISGMAP,IPBMAP,IAQMAP,
     &KROMP,KRWMP,KRGMP, IRSOMP, IPCOW,IPCGO, KPHIMP,
     &RSOA,QGRSO,RKROMX,QGRSOT,CAPGO,CAPOW, RPO,RPW,RPG)

 2557 IF(N.NE.KCO.OR.KCO1.EQ.0) GO TO 500
      DO 300 K=1,KK
      DO 300 J=1,JJ
      DO 300 I=1,II
      WRITE(IOCODE,21)I,J,K,VP(I,J,K),CT(I,J,K),BO(I,J,K),SO(I,J,K),
     &BW(I,J,K),SW(I,J,K),BG(I,J,K),SG(I,J,K)
300   CONTINUE
  500 CONTINUE
      IF(N.EQ.KSN)KSN=KSN+KSN1
      IF(N.EQ.KSM)KSM=KSM+KSM1
      IF(N.EQ.KCO)KCO=KCO+KCO1

C**** UPDATE ARRAYS ****************************************************
      DO 1150 K=1,KK
      DO 1150 J=1,JJ
      DO 1150 I=1,II
      QO(I,J,K)=0.0
      QW(I,J,K)=0.0
      QG(I,J,K)=0.0
       PN(I,J,K)=P(I,J,K)
       SON(I,J,K)=SO(I,J,K)
       SWN(I,J,K)=SW(I,J,K)
       SGN(I,J,K)=SG(I,J,K)
       PBOTN(I,J,K)=PBOT(I,J,K)
1150  CONTINUE

C.... Required one line time step summary...............................
      IF(ITSS.GT.0) GO TO 1275
      ITSS=1
      WRITE(IOCODE,1200)
 1200 FORMAT(///T44,29('*'),/,
     & T44,'*   TIME   STEP   SUMMARY   *',/,
     & T44,29('*'),//,
     & T2,'TIME STEP',15X,'PRODUCTION',21X,'INJECTION',
     & 5X,'PV WT   MATERIAL  BALANCES',2X,
     & 'MAX SATN CHANGE  MAX PRES CH  ITER ',/,
     & T2,9('-'),1X,40('-'),1X,16('-'),2X,' AVG',3X,
     & 20('-'),1X,15('-'),1X,14('-'),1X,3('-'))
      WRITE(IOCODE,1250)
 1250 FORMAT(T3,13X,'OIL     GAS     WATER    GOR   WATER',
     & 3X,' GAS     WATER   RES     OIL    GAS',
     & 3X,'WATER  I  J  K  DSMAX  I  J  K  DPMX  ',/,
     & T3,38X,'SCF/   /OIL',20X,'PRES',5X,'%',2(6X,'%'),/,
     & T1,2X,'NO. DAYS    STB/D  MSCF/D    STB/D',
     & 3X,' STB   RATIO  MSCF/D    STB/D   PSIA',/,
     & T2,4('-'),1X,4('-'),1X,8('-'),2(2X,7('-')),
     & 1X,6('-'),2X,5('-'),1X,8('-'),1X,7('-'),1X,6('-'),2X,3(6('-'),1X),
     & 3(2('-'),1X),6('-'),1X,3(2('-'),1X),5('-'),
     & 1X,3('-'),/)
 1275 GORS=0.
      WORS=0.
C===========================================
      IF(OPR.GT.1.E-06) GORS=GPR*1000./OPR
      IF(OPR.GT.1.E-06) WORS=WPR/OPR
C===========================================
C.... Total run summary saved information............................
      ITSMAX=N
      ITSNO(N)=N
      STIME(N)=ETI
      SOPROD(N)=OPR
      SGPROD(N)=GPR
      SWPROD(N)=WPR
      SGOR(N)=GORS
      SWOR(N)=WORS
      SGINJ(N)=GIR
      SWINJ(N)=WIR
      SPVWTP(N)=PAVG
      NM=N-1
      IF(NM.EQ.0) NM=1
      SOCUMP(N)=SOPROD(N)*DELT/1000. + SOCUMP(NM)
      SWCUMP(N)=SWPROD(N)*DELT/1000. + SWCUMP(NM)
      SGCUMP(N)=SGPROD(N)*DELT/1000. + SGCUMP(NM)
      SWCUMI(N)=SWINJ(N)*DELT/1000. + SWCUMI(NM)
      SGCUMI(N)=SGINJ(N)*DELT/1000. + SGCUMI(NM)
      WRITE(IOCODE,1280) N,ETI,OPR,GPR,WPR,GORS,WORS,GIR,WIR,PAVG,
     & MBEO,MBEG,MBEW,ISM,JSM,KSMM,DSMC,IPM,JPM,KPM,DPMC, NLITER

 1280 FORMAT(       I4,F6.0,1X,F8.1,1X,F8.1,1X,F8.1,F7.0,           
     & 1X,F5.1,1X,F9.1,1X,F7.0,1X,F6.0,3(1X,F6.3),3(1X,I2),1X,F6.3,1X,
     & 3(1X,I2),1X,F6.1,1X,I3)

      IF(KSCRN.GE.1) THEN
         WRITE(*,2773) N,ETI,DELT, NLITER, OPR,GPR,WPR
         WRITE(9,2773) N,ETI,DELT, NLITER, OPR,GPR,WPR
         IF(ABS(WIR).GT..01 .OR. ABS(GIR).GT..01) THEN
            WRITE(*,27731) GIR,WIR
            WRITE(9,27731) GIR,WIR
         ENDIF
      ENDIF

2773  FORMAT(/1X,
     &'BOAST3:  Step=',I4,'  Time=',G12.5,'  Delt=',G12.5,2X,
     &'ITER=',I5/2X,
     &'        Field Prd Rates (Oil, Gas, Wat):', 3F10.1)
27731 FORMAT(2X,
     &'        Field Injection Rates (Gas,Wat):', 10X,2F10.1)  

     
C      END OF TIME?
      IF(FT.GE.TMAX) GO TO 10066
1000  CONTINUE


1002  WRITE(IOCODE,2902)
      GO TO 1006
1003  WRITE(IOCODE,2903)
      GO TO 1006
1004  WRITE(IOCODE,2904)
      GO TO 1006
1005  WRITE(IOCODE,2905)

10066 CONTINUE
C------------ SEE IF RESTART IS TO BE WRITTEN AT TMAX? 
      NTSTEP=N+1
      IF(IREOPT.EQ.-1 .OR. IRNUM.EQ.0  
     &                .OR. ETI.NE.REDATE(IRSCNT)) GO TO 1006
C**    ARRAYS
c--------------------------------
      INCLUDE 'RESTART.INC'
c--------------------------------
      WRITE(21,261) ETI,NTSTEP
      WRITE(9,261)  ETI,NTSTEP
      WRITE(*,261)  ETI,NTSTEP
261   FORMAT(/T5,70('-'),
     & /T5,'RESTART RECORD WRITTEN AT TMAX =',F12.4,' DAYS',
     & /T5,'   NEXT TIME-STEP =',I5,  
     & /T5,70('-'),//)


C******  EXIT POINT

1006  CONTINUE

      IF(IFATAL.LT.1) GO TO 2020
      WRITE(IOCODE,1010) IFATAL
 1010 FORMAT(//5X,5('-'),'FATAL INPUT ERRORS HAVE BEEN',
     & ' DETECTED (# =',I9,'). RUN TERMINATED.',5('-'),/)
      GO TO 2250
 2020 CONTINUE

C=============================================================
C     POST-     PACKAGE CALL - Now just to get summary table
      IF(IPLOTP.EQ.0) CALL POSTP(NPLINE,IOCODE)
C=============================================================
 2250 CONTINUE

2902  FORMAT(/T15,'MAXIMUM WOR HAS BEEN EXCEEDED --- SIMULATION',
     &' IS BEING TERMINATED'//)
2903  FORMAT(/T15,'MAXIMUM GOR HAS BEEN EXCEEDED --- SIMULATION',
     &' IS BEING TERMINATED'//)
2904  FORMAT(/T15,'MINIMUM AVERAGE RESERVOIR PRESSURE WAS NOT',
     &' ACHIEVED --- SIMULATION IS BEING TERMINATED'//)
 2905 FORMAT(/T15,'MAXIMUM AVERAGE RESERVOIR PRESSURE
     1 HAS BEEN EXCEEDED --- SIMULATION IS BEING TERMINATED'//)
C      CLOSE I/O FILES FOR DEC 20/60.
C      MACHINE DEPENDENT CLOSE STATEMENTS
      CLOSE(UNIT=20)
      CLOSE(UNIT=21)
C.... Binary restart files added back - 05/31/95
      CLOSE(UNIT=24)
      CLOSE(UNIT=25)
      STOP ' '
      END
C..................................................BLOCK DATA
      BLOCK DATA 
C=======================================================================
      INCLUDE 'PARAMS.FOR'
C=======================================================================

      REAL KROT,KROGT,KRWT,KRGT,MUWT,MUOT,MUGT,KX,KY,KZ
     & ,MUO,MUW,MUG,MBEWI,MBEGI,MCFGI
     & ,MBEO,MBEW,MBEG,MCFG,MBEOI,MIN,MCFG1,MCFGT

      COMMON /COEF/ AW(LP1,LP2,LP3),AE(LP1,LP2,LP3),AN(LP1,LP2,LP3),
     & AS(LP1,LP2,LP3),AB(LP1,LP2,LP3),AT(LP1,LP2,LP3),E(LP1,LP2,LP3),
     & B(LP1,LP2,LP3)

      COMMON /SAQUI/ IAQOPT,CPIAQ1(LP1,LP2,LP3),CPIAQ2(LP1,LP2,LP3),
     & CPI1(LP1,LP2,LP3),CPI2(LP1,LP2,LP3),EWAQ(LP1,LP2,LP3),
     & CUMAQW(LP1,LP2,LP3),
     & QWAQ(LP1,LP2,LP3),CUMEW(LP1,LP2,LP3),QWAQR(LP7),CUMAQR(LP7)
     & ,IAQREG(LP1,LP2,LP3),PAQ(LP1,LP2,LP3),PIAQ(LP1,LP2,LP3)   

        COMMON /RUNSUM/ ITSNO(LP12),STIME(LP12),SOPROD(LP12),
     & SGPROD(LP12),
     & SWPROD(LP12),SGOR(LP12),SWOR(LP12),SGINJ(LP12),SWINJ(LP12)

      COMMON /RUN2/SPVWTP(LP12),SOCUMP(LP12),SWCUMP(LP12),SGCUMP(LP12),
     & SGCUMI(LP12),SWCUMI(LP12),SAQUIR(LP12),SAQUIC(LP12)
      COMMON /SPARM/ KX(LP1,LP2,LP3),KY(LP1,LP2,LP3),KZ(LP1,LP2,LP3),
     & EL(LP1,LP2,LP3),TX(LP4,LP2,LP3),TY(LP1,LP5,LP3),TZ(LP1,LP2,LP6),
     & PDAT(LP1,LP2,LP3),PDATUM,GRAD

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
     & IDWELL(LP11),ALIT(LP11),BLIT(LP11)

      COMMON /SSOLN/ BO(LP1,LP2,LP3),BW(LP1,LP2,LP3),BG(LP1,LP2,LP3),
     & QO(LP1,LP2,LP3),QW(LP1,LP2,LP3),QG(LP1,LP2,LP3),
     & GOWT(LP1,LP2,LP3),GWWT(LP1,LP2,LP3),GGWT(LP1,LP2,LP3),
     & O1(LP4,LP2,LP3),W1(LP4,LP2,LP3),
     & O2(LP1,LP5,LP3),W2(LP1,LP5,LP3),
     & O3(LP1,LP2,LP6),W3(LP1,LP2,LP6),
     & QOWG(LP1,LP2,LP3),VP(LP1,LP2,LP3),CT(LP1,LP2,LP3)

      COMMON /TSTDAT/ IFATAL,IWARN

      DATA IFATAL,IWARN/0,0/

      DATA NROCK,NPVT /LP7,LP8/
C................ Added 12/09/91 .......................................
      DATA ALIT,BLIT/LP11*0.0,LP11*0.0/
C.......................................................................

      DATA TX,TY,TZ/LP19*0.0,LP20*0.0,LP21*0.0/

      DATA    CUMO,CUMW,CUMG/LP23*0.0,LP23*0.0,LP23*0.0/
      DATA            AW,AS,AT/LP22*0.0,LP22*0.0,LP22*0.0/
      DATA            AE,AN,AB/LP22*0.0,LP22*0.0,LP22*0.0/
      DATA  O1,O2,O3 /LP19*0.0, LP20*0.0, LP21*0.0/
      DATA  W1,W2,W3 /LP19*0.0, LP20*0.0, LP21*0.0/
      DATA  IROCK,IPVT/LP22*1,LP22*1/
      DATA  CPIAQ1,CPIAQ2/LP22*0.,LP22*0./

      END
