C**********************************************************************
C:::: PROGRAM POSTHIS:  READS _____.BPD output file(s) from BOAST3  and
C::::                   prepares input file for Program HISPLT

C     Remarks:         Currently dimensioned for the following:
C                      --- 11/17/96 - PowerStation Fortran ----
C                         (1) Maximum simulator time-steps = 5000 !!
C                         (2) Maximum no of points per graph = 1800 !!  
C**********************************************************************


C===================== NEW STUFF FOR POWERSTATION FORTRAN ===================
      INCLUDE 'FGRAPH.FI'

C.... Simpler to just use a PARAMETER STATEMENT here
C.... NOTE: Parameters must be sure same here and in HISSUBS.FOR !!!
      PARAMETER(NWX=60,NODX=10) 

C===================== New stuff for PowerStation Fortran ===================
      INTEGER*2     int2


      CHARACTER*1  ANS,ANS2,MFILES
      CHARACTER*75 IHEDIN 
      CHARACTER*8  FNAME 
      CHARACTER*12 FNAMTST,DUMMY,DUMY1 

      CHARACTER*1  LOCTYP
      CHARACTER*5  LOCWEL(NWX) 
      CHARACTER*8  WELNAM, LOCNOD(NWX,NODX), LOCID
      CHARACTER*7           ALNOD(NWX,NODX), ALNODE(12)
      CHARACTER*10 PARM(9) 
      CHARACTER*20 PNAME(12) 

c------------ Stuff for SLPLOT32 ------------------
      CHARACTER*60 GTITLE
      CHARACTER*11 XLABL
      CHARACTER*42 YLABL
      CHARACTER*20 LEGND(5)
c---------------------------------------------------
      INTEGER*2    IPARM, IPAR(9)  
C--------------------------------------------------------------------------

      REAL NGOR,NWOR,NSOR,NPWF

      COMMON /BPDDATA/  NWELLS, FTT,
     &  FGIR, FWIR, FCGI, FCWI, FPAV,
     &        FGPR, FWPR, FOPR,  
     &        FCGP, FCWP, FCOP,  
     &        FGOR, FWOR, 
     &        FSGAV, FSWAV, FSOAV, FPWFAV

      COMMON /BPDARYS/  WELNAM(NWX), NPW(NWX),       
c....... 13 "well production" quantities for plotting ................
     &        WQG(NWX),    WQW(NWX),   WQO(NWX),      
     &       WCGP(NWX),   WCWP(NWX),  WCOP(NWX),        
     &       WGOR(NWX),   WWOR(NWX),  
     &      WSGAV(NWX),  WSWAV(NWX), WSOAV(NWX),    
     &                               WPOAV(NWX),   WPWFAV(NWX),
           
c....... 13 "node production" quantities for plotting ................
     &  QGN(NWX,NODX),  QWN(NWX,NODX),  QON(NWX,NODX),  
     &CUMGN(NWX,NODX),CUMWN(NWX,NODX),CUMON(NWX,NODX), 
     & NGOR(NWX,NODX), NWOR(NWX,NODX), 
     &  SGG(NWX,NODX),  SWW(NWX,NODX),  SOO(NWX,NODX),  
     &                                  POO(NWX,NODX), NPWF(NWX,NODX)

C--------------------------------------------------------------------------


      DIMENSION
     & IN(NWX,NODX), JN(NWX,NODX), KN(NWX,NODX), IDWEL(NWX) 

C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      CHARACTER*1   PCHAR(5)
      INTEGER*2     IPLOPT(5)
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^      


C######  DO NOT FORGET TO CHANGE IM = ___ WHEN CHANGING DIMENSIONS ######
C######  DO NOT FORGET TO DATA /   /      WHEN CHANGING DIMENSIONS ######

c------------- Arrays for HISPLT & HPLOT-------------------
c........ changing from 600 to 1800 - 8/17/96 .............
c........ Added ISTEP - 8/17/96
      DIMENSION  ISTEP(1800)
      DIMENSION  XH(1800),   YH(1800)
      DIMENSION  XP(1800,2), YP(1800,2)

      DIMENSION       T(5000)  
      DIMENSION   PLOT1(5000), PLOT2(5000)
      DIMENSION   PLOT3(5000), PLOT4(5000)
      DIMENSION   PLOT5(5000), PLOT6(5000) 
      DIMENSION   PLOT7(5000), PLOT8(5000) 
      DIMENSION   PLOT9(5000), YTMP(5000)
      DIMENSION    TIME(1800), JMAX(9)
CCCC      DIMENSION   YY(1800,9) 
C########################################################################

      EQUIVALENCE (XH(1), XP(1,2)),
     &            (YH(1), YP(1,2))

CCCCC      DATA   YY/16200*0.0/

      DATA   PLOT1 /5000*0.0/
      DATA   PLOT2 /5000*0.0/
      DATA   PLOT3 /5000*0.0/
      DATA   PLOT4 /5000*0.0/
      DATA   PLOT5 /5000*0.0/
      DATA   PLOT6 /5000*0.0/
      DATA   PLOT7 /5000*0.0/
      DATA   PLOT8 /5000*0.0/
      DATA   PLOT9 /5000*0.0/      

      DATA PARM /'Gprd Mscfd',
     &           'Wprd stb/d',
     &           'Oprd stb/d',
     &           'GORmcf/stb',
     &           'WORstb/stb',
     &           'CGprd MMcf',
     &           'CWprd Mstb',
     &           'COprd Mstb',
     &           'Press psia'/

      DATA IPAR /9*0/

      DATA   XMARF,YMARF,XOFSET,YOFSET /4*0.0/

C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      DATA  PCHAR  /' ', 'o', ' ', ' ', ' '/
      DATA IPLOPT  /0, 1, 0, 0, 0/
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      OPEN(24, FILE = 'HISPLT.ASC', STATUS = 'UNKNOWN')
      CLOSE(24, STATUS = 'DELETE')
      OPEN(24, FILE = 'HISPLT.ASC', STATUS = 'NEW')


c---- Fixed parameters for HPLOT ---------
      IM   = 1800
      KMAX = 2
c-----------------------------------------


c     PLOT1(J) =  plot item 1 @ time J  (used for initial storage)
c     PLOT2(J) =  plot item 2 @ time J  (used for initial storage)
c     PLOT3(J) =  plot item 3 @ time J  (used for initial storage)
c     PLOT4(J) =  plot item 4 @ time J  (used for initial storage)
c     PLOT5(J) =  plot item 5 @ time J  (used for initial storage)
c     PLOT6(J) =  plot item 6 @ time J  (used for initial storage)
c     PLOT7(J) =  plot item 7 @ time J  (used for initial storage)
c     PLOT8(J) =  plot item 8 @ time J  (used for initial storage)
c     PLOT9(J) =  plot item 9 @ time J  (used for initial storage)

21    FORMAT(A1)


C.... Read filename from file "INP.DAT" created by batch file B3PLOT or B3PRST)
      OPEN(9, FILE = 'INP.DAT', STATUS = 'OLD')
      READ(9,131) FNAME   
131   FORMAT(A8)
      WRITE(*,111) FNAME
111   FORMAT(
     &///1X,'********** Welcome to Post Processor POSTHIS **********',
     & //8X,'.....Filename = [',A8,'] has been specified..... ')


C.... Determine prod/inj multiplier and time increment.................
      PIMULT = -1.0
      WRITE(*,133) 
133   FORMAT(
     &//5X,'Default production/injection multipler is 1.00 ',
     & /5X,'Do you wish to change it [N]? ',$)
      READ(*,21) ANS
      IF(ANS.EQ.'Y' .or. ANS.eq.'y') THEN
         WRITE(*,135) 
135      FORMAT(/5X,'Enter multiplier: ',$)
         READ(*,*) PIMULT
      ENDIF


c........ Define default values for KSTART & KSKIP - used in all cases!
          KSTART = 1
          KSKIP  = 1

      TINC = -1.0
      WRITE(*,137) 
137   FORMAT(
     &//5X,'Default time basis is by simulator time-step.',
     & /5X,'Do you wish to put data on uniform time interval [N]? ',$)
      READ(*,21) ANS
      IF(ANS.EQ.'Y' .or. ANS.eq.'y') THEN
         WRITE(*,139) 
139      FORMAT(/5X,'Enter time interval (days): ',$)
         READ(*,*) TINC
      ENDIF
      
C********* WILL TRY (AGAIN) TO USE KSTART AND KSKIP WHEN TINC IS USED!!!
C....... Read starting time-step number (KSTART)...........
         WRITE(*,24331) 
24331    FORMAT(
     &   //5X,'Default starting time-step number is 1 ',
     &    /5X,'Do you wish to change it [N]? ',$)
         READ(*,21) ANS
         IF(ANS.EQ.'Y' .or. ANS.eq.'y') THEN
            WRITE(*,24333) 
24333       FORMAT(/5X,'Enter starting time-step number: ',$)
            READ(*,*) KSTART 
         ENDIF

C....... Read time-step skip parameter (KSTART)...........
         write(*,24335) 
24335    Format(/5x,'Default time-step skip parameter is 1 ',
     &   /5X,'Do you wish to change it [N]? ',$)
         READ(*,21) ANS
         IF(ANS.EQ.'Y' .or. ANS.eq.'y') THEN
            WRITE(*,24337) 
24337       FORMAT(/5X,'Enter time-step skip parameter: ',$)
            READ(*,*) KSKIP 
         ENDIF


c---------------------- specify plot options-----------------------------
      ITCODE = 0
      write(*,1391)
1391  format(/
     &//5x,'Default x-axis graph scale is days.',
     & /5x,'Do you wish to change to years [N]? ',$)
      read(*,21) ANS
      if(ANS.EQ.'Y' .or. ANS.EQ.'y') THEN
         ITCODE=1
      endif

      ILOG=0
      write(*,1393)
1393  format(/
     &//5x,'Default y-axis graph scale is linear.',
     & /5x,'Do you wish to change to logarithmic [N]? ',$)
      read(*,21) ANS
      if(ANS.EQ.'Y' .or. ANS.EQ.'y') THEN
         ILOG=1
      endif
c------------------------------------------------------------------

      OPEN(15, FILE = FNAME//'.GWN', STATUS = 'OLD', ERR=8888)
      READ(15,*) 
      READ(15,*) II,JJ,KK, NRECL

c.... Open file ____.BPD and read first record..........................
      OPEN(21, FILE = FNAME//'.BPD', STATUS = 'OLD', 
     &         FORM= 'UNFORMATTED', ACCESS='DIRECT',RECL=NRECL,ERR=9999)
      IREC=1
      READ(21,REC=IREC) IHEDIN,NWELLS,(NPW(L),L=1,NWELLS)


C.... Now read rest of file fname.GWN................................... 
      READ(15,*) NWELLS,NODTOT
      DO 96 L=1,NWELLS
      DO 96 M=1,NPW(L)
      READ(15,153) NW,IDWEL(L),WELNAM(L),NPW(L), IN(L,M),JN(L,M),KN(L,M)

C.... Read well no & node loc again as character data......
      BACKSPACE(15)
      READ(15,155) NW, LOCWEL(L), LOCNOD(L,M)

C-----------------------------------------------------------------------
c.... Construct 7-character historical node file name (ALNOD) from
c....  IN(), JN() and KN().....
      OPEN(31,FILE='TMP2.DAT',STATUS='UNKNOWN')
      CLOSE(31,STATUS='DELETE')
      OPEN(31,FILE='TMP2.DAT',STATUS='NEW')

      WRITE(31,33) IN(L,M),JN(L,M),KN(L,M)
33    FORMAT(1X,'N',3I2)
      REWIND(31)
      READ(31,35) ALNOD(L,M)
35    FORMAT(1X,A7) 

c.... Check for blank characters in positions 2 to 6; fill in with '0'
c.... e.g. if node = 3 8 1, must end up with ALNOD() = 'N030801'
      DO 98 I=2,6
      IF(ALNOD(L,M)(I:I).EQ.' ') ALNOD(L,M)(I:I) = '0'
98    CONTINUE
cc      WRITE(*,981) L,M,ALNOD(L,M)
cc981   FORMAT(1X,'L,M,ALNOD(L,M):',2I3,A7)
cc      PAUSE 'Checking node file names'

C------------------------------------------------------------------------
96    CONTINUE

153   FORMAT(1X,I5,I5, 3X,A8,I5,3X, I2,1X,I2,1X,I2)
155   FORMAT(1X,I5,A5,     19X,          A8)



C========= BRANCH BACK TO HERE FOR ANOTHER PLOT SELECTION =============
3000  CONTINUE

      WRITE(*,2130) 
2130  FORMAT(
     &/7X,' Select One Observed Data Item For History Matching',
     &/7X,' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
     &/11X,'        1 - Field')
      ILOC = 1
      DO 2200 L=1,NWELLS                                     
      ILOC = L+1
      IF(MOD(ILOC,15).EQ.0) THEN
         WRITE(*,2210)
2210     FORMAT(5X,'Enter for more....',$)
         READ(*,21) ANS2
         IF(ANS2.NE. ' ') GOTO 2360
      ENDIF
      WRITE(*,2151) ILOC,LOCWEL(L),WELNAM(L)
2200  CONTINUE
2151  FORMAT(17X,I3,' - Well:',A5,2X,A8)        

      DO 2300 L=1,NWELLS
      DO 2300 M=1,NPW(L)
      ILOC = ILOC+1          
      IF(MOD(ILOC,15).EQ.0) THEN
          WRITE(*,2210)
          READ(*,21) ANS2
          IF(ANS2.NE. ' ') GOTO 2360
      ENDIF
      WRITE(*,2170) ILOC,LOCWEL(L), LOCNOD(L,M) 
2300  CONTINUE
2170  FORMAT(17X,I3,' - Well:',A5,2X, A8) 
2360  CONTINUE

2308  WRITE(*,2310)
2310  FORMAT(
     &/5X,'Enter item number or 0 to review selection: ',$)
      READ(*,*) LOC
      IF(LOC.EQ.0) GO TO 3000
      IF(LOC.LT.1 .OR. LOC.GT.ILOC) GOTO 2308


C---- Now select simulated parameters for history matching corresponding to
c---- the FIELD, Well, or Node Observed Data selection above 
2400  WRITE(*,2411)
2411  FORMAT(
     & /1X,'    Select Simulated Data for History Matching',
     & /1X,'    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
     & /1X,'      1 - Gas Rate----------------- (Gprod)',
     & /1X,'      2 - Water Rate--------------- (Wprod)',
     & /1X,'      3 - Oil Rate----------------- (Oprod)',
     & /1X,'      4 - Cumulative Gas----------- (CGprd)',
     & /1X,'      5 - Cumulative Water--------- (CWprd)',
     & /1X,'      6 - Cumulative Oil----------- (COprd)',
     & /1X,'      7 - Producing GOR------------ (PGOR) ',
     & /1X,'      8 - Producing WOR------------ (PWOR) ',
     & /1X,'      9 - Pressure----------------- (Press)')

  
2600  WRITE(*,2431)
2431  FORMAT(/5X,'Enter number of selected parameter(s): ',$) 
      READ(*,*) NPARMS
      IF(NPARMS.LT.1 .OR. NPARMS.GT.9) GOTO 2600

      IF(NPARMS.EQ.9) THEN
         DO 2432 L=1,9
2432     IPAR(L) = L
         GO TO 2440
      ENDIF

      WRITE(*,2433) NPARMS
2433  FORMAT(//5X,
     & 'Enter',I3,' parameter numbers from above list: ',$)
      READ(*,*) (IPAR(J), J=1,NPARMS)

c.... NOTE: MAXPTS should always be set to IM, max no. of pts that can 
c....       actually be plotted!
      MAXPTS = IM



C==== B E G I N   L O O P   T O   R E A D   S I M U L A T E D   D A T A =====
C============================================================================
C.... NOTE:  5000 IS UPPER LIMIT ON NUMBER OF TIME-STEPS! 
C.... NOTE:  1800 = IM IS UPPER LIMIT ON NUMBER OF POINTS THAT CAN BE PLOTTED
C....               PER GRAPH         

C.... Read ____.BPD file and store selected data for history match plot(s)

2440  DO 500 J=1,5000  
c--------------------------------------------------------------------------
c.... NOTE: IREC = KSTART+1, because IREC=1 contains only header information! 
c....                   i.e. first time-step data is in second record!
         IF(J.EQ.1) THEN
            IREC = KSTART + 1
         ELSE
            IREC = IREC + KSKIP
         ENDIF
c....... Subtract 1 from IREC to get actual time-step number.........
         ISTEP(J) = IREC-1
c----------------------------------------------------------------------------

c.... Note:  J is the total number of records read - but not the number of
c....        time-steps if KSKIP > 0 !!
      IF(J.GT.MAXPTS) THEN
         WRITE(*,6001)  MAXPTS 
         WRITE(24,6001) MAXPTS 
6001     FORMAT(
     & //5X,'! WARNING:  Max number of points (',I4,' ) reached',
     &  /5X,'!           Increase KSKIP to solve this problem',
     &  /5X,'!           press Enter to continue processing...'/)
         PAUSE ' '
         GO TO 510 
      ENDIF

      IEOF=0
      CALL BPD(21,IREC,IEOF,PIMULT)
C----------------------------------------------------------------------------

      IF(IEOF.EQ.1) GO TO 600

      T(J) = FTT

      IF(LOC.EQ.1) THEN
         IF(J.EQ.1) THEN
            LOCTYP = 'T'
            LOCID  = 'Field   '
         ENDIF

C************* HERE IS THE NPARMS LOOP - LET'S SEE IF IT WORKS !!!!!!
C              FOR LOC=1
         DO 502 I=1,NPARMS
         IF(IPAR(I).EQ.1) THEN
            PLOT1(J) = FGPR
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.2) THEN
             PLOT2(J) = FWPR
             GOTO 501
         ENDIF
         IF(IPAR(I).EQ.3) THEN
            PLOT3(J) = FOPR
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.4) THEN
            PLOT4(J) = FCGP
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.5) THEN
            PLOT5(J) = FCWP
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.6) THEN
            PLOT6(J) = FCOP
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.7) THEN
            PLOT7(J) = FGOR
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.8) THEN
            PLOT8(J) = FWOR
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.9) THEN
            PLOT9(J) = FPAV
            GOTO 501
         ENDIF
         GO TO 502
501      IF(J.EQ.1) PNAME(I) = PARM(IPAR(I))//LOCTYP//' '//LOCID
502      CONTINUE
      ENDIF

C********* HERE IS THE NPARMS LOOP FOR "WELLS" !!!!!!!!!!!!!!!1
      DO 516 L=1,NWELLS
      K = L+1
      IF(LOC.EQ.K) THEN
         IF(J.EQ.1) THEN
            LOCTYP = 'W'
            LOCID  = WELNAM(L)
         ENDIF
         DO 512 I=1,NPARMS 
         IF(IPAR(I).EQ.1) THEN
            PLOT1(J) = WQG(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.2) THEN
            PLOT2(J) = WQW(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.3) THEN
            PLOT3(J) = WQO(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.4) THEN
            PLOT4(J) = WCGP(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.5) THEN
            PLOT5(J) = WCWP(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.6) THEN
            PLOT6(J) = WCOP(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.7) THEN
            PLOT7(J) = WGOR(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.8) THEN
            PLOT8(J) = WWOR(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.9) THEN
            PLOT9(J) = WPWFAV(L)    
            GOTO 511
         ENDIF
         GOTO 512
511      IF(J.EQ.1) PNAME(I) = PARM(IPAR(I))//LOCTYP//' '//LOCID
512      CONTINUE      
      ENDIF
516   CONTINUE

C******** HERE IS THE NPARMS LOOP FOR "NODES" !!!!!!!!!!
      DO 548 L=1,NWELLS
      DO 548 M=1,NPW(L)
      K = K+1
      IF(LOC.EQ.K) THEN
         IF(J.EQ.1) THEN
            LOCTYP = 'N'
            LOCID  = LOCNOD(L,M)
         ENDIF

         DO 538 I=1,NPARMS
         IF(IPAR(I).EQ.1) THEN
            PLOT1(J) = QGN(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.2) THEN
            PLOT2(J) = QWN(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.3) THEN
            PLOT3(J) = QON(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.4) THEN
            PLOT4(J) = CUMGN(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.5) THEN
            PLOT5(J) = CUMWN(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.6) THEN
            PLOT6(J) = CUMON(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.7) THEN
            PLOT7(J) = NGOR(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.8) THEN
            PLOT8(J) = NWOR(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.9) THEN
            PLOT9(J) = NPWF(L,M)
            GOTO 537
         ENDIF
         GOTO 538
537      IF(J.EQ.1) THEN
            PNAME(I)  = PARM(IPAR(I))//LOCTYP//' '//LOCID
            ALNODE(I) = ALNOD(L,M)
ccc            WRITE(*,5371) I,ALNODE(I)
ccc5371        FORMAT(1X,'I=',I5,5X,'ALNODE(I)=',A7)
         ENDIF
538      CONTINUE
      ENDIF

548   CONTINUE

500   CONTINUE      
510   CONTINUE
C==== E N D   O F   R E A D   S I M U L A T I O N   D A T A  L O O P ======
C==========================================================================


600   NSTEPS = J-1 
c.... NOTE:  NSTEPS is the number of steps which have been selected for
c....        plotting.  If KSTART>1, the steps selected are not sequential.

C======== PREPARE HISTORY MATCH PLOTS FOR DATA SELECTED ABOVE==============
C         [ based on NPARM selected plots [IPAR(j), j=1,NPARMS]
C-------> Also write to HISPLT.ASC file to be able to access simulated data
C-------> selected for plotting using other graphics software


C===========================================================================
C********* HERE IS THE LOOP TO PLOT ALL (NPARMS) PLOTS SELECTED ***********
C===========================================================================

      DO 4000 I=1,NPARMS
C=======================================================================
C            DEFINE AXES LABELS & LEGENDS 
C            THEN CALL HPLOT TO PLOT SIMULATED & HISTORICAL DATA
C=======================================================================

c.... Label for x-axis.........................................
         IF(ITCODE.EQ.0) XLABL = 'Time, days '
         IF(ITCODE.EQ.1) XLABL = 'Time, years '

c.... Label for y-axis.........................................
      IF(IPAR(I).EQ.1) THEN
CCC               123456789012345678901234567890123456789012
         YLABL = '   Gas Production Rate, Mscf/day          '
      ELSEIF(IPAR(I).EQ.2) THEN       
         YLABL = ' Water Production Rate, stb/day           '
      ELSEIF(IPAR(I).EQ.3) THEN       
         YLABL = '   Oil Production Rate, stb/day           '
      ELSEIF(IPAR(I).EQ.4) THEN       
         YLABL = 'Cumulative Gas Production, MMscf          '
      ELSEIF(IPAR(I).EQ.5) THEN       
         YLABL = 'Cumulative Water Production, Mstb         '
      ELSEIF(IPAR(I).EQ.6) THEN       
         YLABL = 'Cumulative Oil Production, Mstb           '
      ELSEIF(IPAR(I).EQ.7) THEN       
         YLABL = 'Producing Gas-Oil Ratio, Mscf/stb         '
      ELSEIF(IPAR(I).EQ.8) THEN       
         YLABL = 'Producing Water-Oil Ratio, stb/stb        '
      ELSEIF(IPAR(I).EQ.9) THEN       
         YLABL = '              Pressure, psia              '
      ENDIF

c.... Legends..................................................
      LEGND(1) = 'Simulated '//PNAME(I)(11:20)
      LEGND(2) = 'Historical          '

c+++++++++++++ check y-label & legends ++++++++++++++++++++
cc      WRITE(*,2223) YLABL, LEGND(1),LEGND(2)
cc2223  FORMAT(1X,A42/1X,A20/1X,A20)
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      WRITE(24,221) IHEDIN
221   FORMAT(1X,A75) 
      WRITE(24,1219) YLABL
1219  FORMAT(1X,
     &   'Time-Step  Point No.    Time   ', A42)  
                                            
      IF(TINC.GT.0.0) GO TO 1210

      DO 1200 J=1,NSTEPS 
      XP(J,1)  = T(J)
      IF(IPAR(I) .EQ. 1) YP(J,1)  = PLOT1(J) 
      IF(IPAR(I) .EQ. 2) YP(J,1)  = PLOT2(J) 
      IF(IPAR(I) .EQ. 3) YP(J,1)  = PLOT3(J) 
      IF(IPAR(I) .EQ. 4) YP(J,1)  = PLOT4(J) 
      IF(IPAR(I) .EQ. 5) YP(J,1)  = PLOT5(J) 
      IF(IPAR(I) .EQ. 6) YP(J,1)  = PLOT6(J) 
      IF(IPAR(I) .EQ. 7) YP(J,1)  = PLOT7(J) 
      IF(IPAR(I) .EQ. 8) YP(J,1)  = PLOT8(J) 
      IF(IPAR(I) .EQ. 9) YP(J,1)  = PLOT9(J) 

      WRITE(24,1221) ISTEP(J), J, XP(J,1), YP(J,1)
ccc      WRITE(*,12211) J,XP(J,1),YP(J,1)
ccc      PAUSE '.....checking output to hisplt.asc.....'
ccc12211 FORMAT(/1X,'ISTEP(J),J,XP(J,1),YP(J,1):',2I4,2G13.5)
 1200 CONTINUE
c.... There are NSTEPS points in simulated data.........
      JMAX(1) = NSTEPS

      GO TO 2220

C.... Put data on time interval TINC ................................
1210  CONTINUE
      DTINC = 1.0/TINC
      TOTTIM = T(NSTEPS) + .00001
      ITINC  = INT(TOTTIM/TINC)

      DO 1212 J=1,NSTEPS
      IF(IPAR(I).EQ.1) YTMP(J)  = PLOT1(J)
      IF(IPAR(I).EQ.2) YTMP(J)  = PLOT2(J)
      IF(IPAR(I).EQ.3) YTMP(J)  = PLOT3(J)
      IF(IPAR(I).EQ.4) YTMP(J)  = PLOT4(J)
      IF(IPAR(I).EQ.5) YTMP(J)  = PLOT5(J)
      IF(IPAR(I).EQ.6) YTMP(J)  = PLOT6(J)
      IF(IPAR(I).EQ.7) YTMP(J)  = PLOT7(J)
      IF(IPAR(I).EQ.8) YTMP(J)  = PLOT8(J)
      IF(IPAR(I).EQ.9) YTMP(J)  = PLOT9(J)
ccc      WRITE(*,12121) NSTEPS,J,ISTEP(J),T(J),YTMP(J)
ccc12121 FORMAT(1X,'NSTEPS,J,ISTEP(J),T(J),YTMP(J):',3I4,2G12.5)
ccc      PAUSE '....... debugging.......'
1212  CONTINUE


      DO 1220 J=1,ITINC
      TIME(J) = FLOAT(J)*TINC
      XP(J,1) = TIME(J)
c.... If KSTART=1 and KSKIP=1, IPTNO=time-step number; otherwise 
c....                          means nothing!
      IDUM = 1  
      CALL INTRP3(T,YTMP,NSTEPS,IPTNO,TIME(J),YYINTP) 
      YP(J,1) = YYINTP
      IF(KSTART.EQ.1 .AND. KSKIP.EQ.1) THEN
         WRITE(24,1221) IPTNO, J, XP(J,1),YP(J,1)
      ELSE
         WRITE(24,12211) J,XP(J,1),YP(J,1)
      ENDIF
1221     FORMAT(1X, I6, I12, 2X, 2G15.6) 
12211    FORMAT(1X, 6X, I12, 2X, 2G15.6) 
1220  CONTINUE

C.... Determine how many points in simulated data.............
      LAST  = ITINC
      DTEND = T(NSTEPS) - TIME(ITINC)
      IF(DTEND.GT..001) THEN
         LAST = ITINC+1
      ENDIF
      JMAX(1) = LAST
   
      IF(DTEND.GT..001) THEN
         J = NSTEPS
         XP(LAST,1) = T(J)
         IF(IPAR(I).EQ.1) YP(LAST,1) = PLOT1(J)
         IF(IPAR(I).EQ.2) YP(LAST,1) = PLOT2(J)
         IF(IPAR(I).EQ.3) YP(LAST,1) = PLOT3(J)
         IF(IPAR(I).EQ.4) YP(LAST,1) = PLOT4(J)
         IF(IPAR(I).EQ.5) YP(LAST,1) = PLOT5(J)
         IF(IPAR(I).EQ.6) YP(LAST,1) = PLOT6(J)
         IF(IPAR(I).EQ.7) YP(LAST,1) = PLOT7(J)
         IF(IPAR(I).EQ.8) YP(LAST,1) = PLOT8(J)
         IF(IPAR(I).EQ.9) YP(LAST,1) = PLOT9(J)
         IF(KSTART.EQ.1 .AND. KSKIP.EQ.1) THEN
            WRITE(24,1221) J,LAST,XP(LAST,1),YP(LAST,1)
         ELSE
            WRITE(24,12211) LAST,XP(LAST,1),YP(LAST,1)
         ENDIF

      ENDIF

2220  CONTINUE

C============= ALL SIMULATOR DATA NOW DEFINED FOR SET I =================


C=====================================================================
C            CALL HISPLT:  DEFINE GTITLE & READ HISTORICAL DATA
C=====================================================================

      CALL HISPLT (I,IPAR,LOCTYP,LOCID,ALNODE, NHPTS,XH,YH,GTITLE)

      JMAX(2) = NHPTS

cc      WRITE(*,2231) NHPTS
cc      DO 2230 J=1,NHPTS
c.... Not necessary - see EQUIVALENCE  
cc      XP(J,2) = XH(J)
cc      YP(J,2) = YH(J)
cc      WRITE(*,2233) J,XP(J,2),YP(J,2)
cc2230  CONTINUE     
cc2231  FORMAT(/1X,I4,' historical data pts from HISPLT follows:')
cc2233  FORMAT(1X,'J,XP(J,2),YP(J,2):',I3,2F12.2)
cc      PAUSE 'All historical data defined'      



C######## SWITCH "KLOG" IN SLPLOT32 DETERMINES LINEAR OR LOG SCALE ON Y-AXIS
C######## PLOT ARRAYS IN POSTHIS ARE  XP & YP  (instead of X & Y)
      KLOG = ILOG
      CALL HPLOT (IM,KMAX,JMAX,ITCODE,XP,YP, GTITLE,XLABL,YLABL,LEGND,
     &             XMARF,YMARF,XOFSET,YOFSET,
     &             PCHAR,IPLOPT,KLOG)
 
      WRITE(*,3999)
3999  FORMAT(/5X,'  Enter to continue.....',
     &       /5X,'  1 - Reselect',
     &       /5X,'  2 - Exit to DOS',
     &       /5X,'  Choice: ',$)
      READ(*,21)  ANS
      IF(ANS.EQ.'1') THEN
         REWIND(41)
         GO TO 3000
      ELSEIF(ANS.EQ.'2') THEN
         GO TO 6666
      ELSE
         REWIND(41)
c....... continue 4000 loop for next IPAR(I)........
      ENDIF
4000  CONTINUE
C======================= E N D   O F   4000-LOOP ==========================


4002  WRITE(*,4001)
4001  FORMAT(/5X,'  1 - Continue history matching',
     &       /5X,'  2 - Exit to DOS',
     &       /5X,'  Choice: ',$)
      READ(*,*) IANS
      IF(IANS.NE.1 .AND. IANS.NE.2) GO TO 4002
      IF(IANS.EQ.2) GO TO 6666

      REWIND(41)
      GO TO 3000

6666  CLOSE(15)
      CLOSE(16)
      CLOSE(21)
      CLOSE(22)
      STOP ' '

8888  CONTINUE
CCC      FNAMTST = FNAME(1:KNT)//'.GWN'
      FNAMTST = FNAME//'.GWN'
      WRITE(*,8887) FNAMTST 
8887  FORMAT(///5X,'File ',A12,'  does not exist!'//)
      STOP ' '

9999  CONTINUE
CCC      FNAMTST = FNAME(1:KNT)//'.BPD'
      FNAMTST = FNAME//'.BPD'
      WRITE(*,8887) FNAMTST 
      STOP ' '
      END



      SUBROUTINE HISPLT (I,IPAR,LOCTYP,LOCID,ALNODE, NHPTS,XH,YH,GTITLE)

      CHARACTER*1  LOCTYP
      CHARACTER*7  ALNODE(1)
      CHARACTER*8  LOCID    
      CHARACTER*60 GTITLE

      CHARACTER*12 FNAMTST

      INTEGER*2  IPAR(1)

      DIMENSION XH(1), YH(1)

      KNT = 0
      DO 90 J=8,1,-1
      IF(LOCID(J:J) .NE. ' ') KNT = KNT+1 
90    CONTINUE


c.... Open proper historical file based on LOCTYP & LOCID & ALNODE......
      IF(LOCTYP.EQ.'T') THEN
         OPEN(41, FILE = 'FIELD.HIS', STATUS = 'OLD', ERR=7777)
      ELSEIF(LOCTYP.EQ.'W') THEN
         OPEN(41, FILE = LOCID//'.HIS', STATUS = 'OLD', ERR=8888)
      ELSEIF(LOCTYP.EQ.'N') THEN
         OPEN(41, FILE = ALNODE(I)//'.HIS', STATUS = 'OLD', ERR=9999)
      ENDIF


c.... Read title & headers..............................................
      READ(41,11) GTITLE
11    FORMAT(1X,A60)       
      DO 10 L=1,3
      READ(41,*)
10    CONTINUE

c.... Select proper historical data.....................................

      NHPTS = 0
      DO 100 J=1,9999
CC      READ(41,*,END=10000) XH(J),Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9
      READ(41,*,ERR=10000) XH(J),Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9
      NHPTS = NHPTS+1

cc      WRITE(*,101) J,I,IPAR(I),XH(J), 
cc     &  Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9
cc101   FORMAT(1X,'J,I,IPAR(I),XH(J), 
cc     &  Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9:',
cc     &     (/1X,3I5,13F10.2))
cc      PAUSE '.....Reading FIELD.HIS'
      
      IF(IPAR(I).EQ.1) THEN
         YH(J) = Y1
      ELSEIF(IPAR(I).EQ.2) THEN
         YH(J) = Y2
      ELSEIF(IPAR(I).EQ.3) THEN
         YH(J) = Y3
      ELSEIF(IPAR(I).EQ.4) THEN
         YH(J) = Y4
      ELSEIF(IPAR(I).EQ.5) THEN
         YH(J) = Y5
      ELSEIF(IPAR(I).EQ.6) THEN
         YH(J) = Y6
      ELSEIF(IPAR(I).EQ.7) THEN
         YH(J) = Y7
      ELSEIF(IPAR(I).EQ.8) THEN
         YH(J) = Y8
      ELSEIF(IPAR(I).EQ.9) THEN
         YH(J) = Y9
      ENDIF
100   CONTINUE

cc      PAUSE 'Checking data in HISPLT'

7777  WRITE(*,1107)
1107  FORMAT(/5X,'File FIELD.HIS does not exist!'/)
      STOP ' '

8888  FNAMTST = LOCID(1:KNT)//'.HIS'
      WRITE(*,1111) FNAMTST 
1111  FORMAT(/5X,'File ',A12,'  does not exist!'//)
      STOP ' '

9999  FNAMTST = ALNODE(I)//'.HIS'
      WRITE(*,1111) FNAMTST 
      STOP ' '

10000 RETURN
      END


C======== FOR NOW WILL ADD HISSUBS.FOR HERE - THEN HPLOT & ALL SUBS
C======== FROM SLPLOT32.FOR TO HAVE ALL FORTRAN TOGETHER IN ONE MODULE

C=========================================================================
C     ********************  H I S S U B S . F O R ************************
C                           =====================
C     ******** POST PROCESSOR (SUB-MODULE) FOR BOAST3 SIMULATOR ***********
C                           USED BY MODULES POSTHIS & POSTSIM 
C           Now compiled & linked using PowerStation Fortran!
C           NOTE:  Both POSTSIM/POSTSUBS and POSTHIS/HISSUBS require 
C                  that parameters NWX & NODX not exceed those in the
C                  simulator (NWX=max no of wells; NODX=max nodes/well) 
C=========================================================================

      SUBROUTINE BPD (IOUNIT,IREC,IEOF,PIMULT)
      PARAMETER(NWX=60,NODX=10) 

      CHARACTER*8 WELNAM
C--------------------------------------------------------------------------

      REAL NGOR,NWOR,NSOR,NPWF

      COMMON /BPDDATA/  NWELLS, FTT,
     &  FGIR, FWIR, FCGI, FCWI, FPAV,
c....... 13 "field quantities (only 12 shown because FPAV on previous line)...
     &        FGPR, FWPR, FOPR,  
     &        FCGP, FCWP, FCOP,  
     &        FGOR, FWOR, 
     &        FSGAV, FSWAV, FSOAV, FPWFAV

      COMMON /BPDARYS/  WELNAM(NWX), NPW(NWX),       
c....... 13 "well production" quantities for plotting ................
     &        WQG(NWX),    WQW(NWX),   WQO(NWX),      
     &       WCGP(NWX),   WCWP(NWX),  WCOP(NWX),        
     &       WGOR(NWX),   WWOR(NWX),  
     &      WSGAV(NWX),  WSWAV(NWX), WSOAV(NWX),    
     &                               WPOAV(NWX),   WPWFAV(NWX),
           
c....... 13 "node production" quantities for plotting ................
     &  QGN(NWX,NODX),  QWN(NWX,NODX),  QON(NWX,NODX),  
     &CUMGN(NWX,NODX),CUMWN(NWX,NODX),CUMON(NWX,NODX), 
     & NGOR(NWX,NODX), NWOR(NWX,NODX), 
     &  SGG(NWX,NODX),  SWW(NWX,NODX),  SOO(NWX,NODX),  
     &                                  POO(NWX,NODX), NPWF(NWX,NODX)

C--------------------------------------------------------------------------

      READ(IOUNIT,REC=IREC,END=999) FTT,
c....... Field only menu (7 items)..................
     &  FGIR, FWIR, FCGI, FCWI, FPAV,
c....... Field items for general menu (13 items)........ 
     &        FGPR, FWPR, FOPR,  
     &        FCGP, FCWP, FCOP,  
     &        FGOR, FWOR, 
     &        FSGAV, FSWAV, FSOAV, FPAV, FPWFAV,
c....... Well items for general menu (13 items)........ 
     &(WELNAM(L),
     &   WQG(L),   WQW(L),   WQO(L),     
     &  WCGP(L),  WCWP(L),  WCOP(L),   
     &  WGOR(L),  WWOR(L),  
     & WSGAV(L), WSWAV(L), WSOAV(L),   
     &           WPOAV(L), WPWFAV(L), L=1,NWELLS),
c....... Node items for general menu (13 items)........ 
     &(  (QGN(L,M),   QWN(L,M),   QON(L,M),  
     &  CUMGN(L,M), CUMWN(L,M), CUMON(L,M), 
     &   NGOR(L,M),  NWOR(L,M),  
     &    SGG(L,M),   SWW(L,M),   SOO(L,M),   
     &                POO(L,M),  NPWF(L,M),  M=1,NPW(L)), L=1,NWELLS )


C.... Convert field cums to MMcf & Mstb  and saturations to percent.......
      FCGI = FCGI * .001
      FCWI = FCWI * .001

      FCGP = FCGP  * .001
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC      WRITE(*,1111) FTT,FCGP
CCC1111  FORMAT(1X,'HISSUBS: FTT,FCGP:',2G14.6)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      FCWP = FCWP  * .001
      FCOP = FCOP  * .001

      FSGAV = FSGAV * 100.
      FSWAV = FSWAV * 100.
      FSOAV = FSOAV * 100.

      DO 8 L=1,NWELLS
C.... Convert well cums to MMcf & Mstb  and saturations to percent.......
      WCGP(L)  = WCGP(L) * .001
      WCWP(L)  = WCWP(L) * .001
      WCOP(L)  = WCOP(L) * .001

      WSGAV(L) = WSGAV(L) * 100. 
      WSWAV(L) = WSWAV(L) * 100.
      WSOAV(L) = WSOAV(L) * 100.

      DO 8 M=1,NPW(L)
C.... Convert node gas cums to MMcf & Mstb  and saturations to percent....
      CUMGN(L,M) = CUMGN(L,M) * .001
      CUMWN(L,M) = CUMWN(L,M) * .001
      CUMON(L,M) = CUMON(L,M) * .001

      SGG(L,M)   = SGG(L,M) * 100.
      SWW(L,M)   = SWW(L,M) * 100.
      SOO(L,M)   = SOO(L,M) * 100.

8     CONTINUE

      IF(PIMULT.GT.0.0) THEN
         FGIR = FGIR * PIMULT
         FWIR = FWIR * PIMULT

         FCGI = FCGI * PIMULT
         FCWI = FCWI * PIMULT

         FGPR = FGPR * PIMULT
         FWPR = FWPR * PIMULT
         FOPR = FOPR * PIMULT

         FCGP = FCGP * PIMULT
         FCWP = FCWP * PIMULT
         FCOP = FCOP * PIMULT

         DO 10 L=1,NWELLS
         WQG(L) = WQG(L) * PIMULT
         WQW(L) = WQW(L) * PIMULT
         WQO(L) = WQO(L) * PIMULT

         WCGP(L)= WCGP(L)* PIMULT
         WCWP(L)= WCWP(L)* PIMULT
         WCOP(L)= WCOP(L)* PIMULT

         DO 10 M=1,NPW(L)
         QGN(L,M) = QGN(L,M) * PIMULT
         QWN(L,M) = QWN(L,M) * PIMULT
         QON(L,M) = QON(L,M) * PIMULT

         CUMGN(L,M)= CUMGN(L,M) * PIMULT
         CUMWN(L,M)= CUMWN(L,M) * PIMULT
         CUMON(L,M)= CUMON(L,M) * PIMULT

10       CONTINUE
      ENDIF

      RETURN
999   IEOF=1
      RETURN
      END

      SUBROUTINE INTRP3(X,Y,N,I,XO,YO)
      DIMENSION X(1),Y(1) 

      IF(XO.GE.X(N)) THEN
         YO=Y(N)
         RETURN
      ENDIF
      IF(XO.LT.X(1)) THEN
         YO = Y(1)
         RETURN
      ENDIF
      DO 10 I=2,N
      IF(XO.GE.X(I)) GO TO 10
      YO = Y(I-1) +(XO - X(I-1)) * (Y(I) - Y(I-1))/(X(I) - X(I-1))
      RETURN
  10  CONTINUE
      END

c.................. NOW SUBROUTINE HPLOT & ALL SUBS FROM BETAPLOT.FOR......


      SUBROUTINE HPLOT (IM, KMAX, JMAX, ITCODE, X, Y, 
     &                  GTITLE, XLABL, YLABL, LEGND,
     &                  XMARF, YMARF, XOFSET,YOFSET,
     &                  PCHAR,IPLOPT,KLOG)


C***********************************************************************
C     PROGRAM:     SLPLOT.FOR 
C     INITIATED:   07/11/92
C     COMPLETED:   08/30/92
c     RE-WORKED:   Converted to Microsoft FORTRAN PowerStation run-time
c................. graphics library - beginning 06/28/96 
c.................                    completed __/__/__

C
C     SUBROUTINES:  LEGEND.FOR, HSUBS.FOR, LGSUBS.FOR (all included below)
C
C     REMARKS:  (a) LEGND is passed, so dimension could be (1) below
C               (b) This program will handle any positive values, however
C                   large or small; however, it uses the log function to
C                   determine magnitudes and hence WILL NOT WORK FOR 
C                   NEGATIVE NUMBERS!!!
C               (c) Note that LEGND is divided on 2 lines (see LEGEND.FOR);
C                        1st 10-characters on 1st line of plot legend
C                        2nd 10-characters on 2nd line of plot legend
C
C***********************************************************************

C====================== PowerStation Run-Time Library set up==================
      INCLUDE 'FGRAPH.FD'
C============================= END OF SETUP ==================================

      CHARACTER*1   PCHAR(5) 
      INTEGER*2     IPLOPT(5), FORCOL(5)
      INTEGER*2     maxx, maxy     ! Returned from graphicsmode
      INTEGER*2     IVX0,IVY0,IVXM,IVYM
      INTEGER*4     status4

      LOGICAL*2     finvert



c........ font stuff......
      CHARACTER*10  option /"t'tms rmn'"/
      CHARACTER*30  list

      DIMENSION     JMAX(1), X(IM,1), Y(IM,1)
      REAL          LBY, YINC(10)

      CHARACTER*60  GTITLE
      CHARACTER*11  XLABL
      CHARACTER*42  YLABL
      CHARACTER*20  LEGND(5)

      CHARACTER*1   ANS  

c........ New 2-byte integers for PowerStation Graphics..........
      INTEGER*2  status2, int2 
      INTEGER*2 line1, line2, line3, line4, line5

C........ Arguments to SETWINDOW must be double precision !!
      REAL*8  WXMIN, WXMAX, WYMIN, WYMAX
      REAL*8  VX0, VY0, VXM, VYM
C........ Arguments to LINETO_W must be double precision !!
      REAL*8  GX0, GY0, GXM, GYM, XS,YS, XS1,YS1 


      RECORD /wxycoord/  wxy
      RECORD /xycoord/   xy
      RECORD /rccoord/   curpos


C-------------------------------------
      DATA  FORCOL /10,12,1,15,13/
C-------------------------------------

c.... Define linestyles......................
      DATA line1, line2, line3, line4, line5 /
     & 2#1111111111111111,
     & 2#1111111100000000,
     & 2#1100110011001100,
     & 2#1111000011110000,
     & 2#1111001100110011/
 
     

      finvert = .TRUE.

c.... Margins for determining world coordinates & graph translation for
c.... centering are as follows:
c....   XMARF = fraction of graph upper bound (UBX);  XMAR = XMARF * UBX
c....           is total xmargin (both sides of graph) if graph is centered
c....   YMARF = fraction of graph upper bound (UBY);  XMAR = YMARF * UBY
c....           is total xmargin (top and bottom) if graph is centered
c....   XOFSET= fraction of XMAR to shift graph to right
c....   YOFSET= fraction of YMAR to shift graph up
c....   (e.g., if XOFSET=YOFSET=0.0, then graph is not offset and origin
c....          is left lower corner of screen (0.0,0.0).
c....          if XOFSET=YOFSET=1.0, then graph is in upper right of screen
c....          with upper right corner of screen (GXM,GYM) 

c.... Set default values, if value passed is 0.0.
      IF(XMARF.LE.1.E-06)  XMARF  = 0.25 
      IF(YMARF.LE.1.E-06)  YMARF  = 0.40 
      IF(XOFSET.LE.1.E-06) XOFSET = 0.75
      IF(YOFSET.LE.1.E-06) YOFSET = 0.65

C.... Determine XMAX & YMAX from data...................................
      XMAX  = 0.0
      YMAX  = 0.0
      DO 1000 K=1,KMAX 
      DO 1000 J=1,JMAX(K)
c................. IF ITCODE=1, divide all x-values by 365. .................
         IF(ITCODE.EQ.1) THEN
            X(J,K) = X(J,K)/365.
         ENDIF

cc      WRITE(*,1001) K,JMAX(K),J,X(J,K),Y(J,K)
cc1001  FORMAT(1X,   'K,JMAX(K),J,X(J,K),Y(J,K):',3I5,2F10.2)

      IF(X(J,K).GT.XMAX) XMAX = X(J,K)
      IF(Y(J,K).GT.YMAX) YMAX = Y(J,K)

 1000 CONTINUE


      IF(KLOG.NE.0) THEN
C....... Also determine YMIN from data for log scale on y-axis ......
         YMIN = YMAX
         DO 1010 K=1,KMAX
         DO 1010 J=1,JMAX(K)
         IF(Y(J,K).LT.YMIN) YMIN = Y(J,K)
1010     CONTINUE
      ENDIF

cc      write(*,10101) xmax,ymax,ymin
cc10101 format(1x,'xmax,ymax,ymin:',3g14.6)
cc      PAUSE 'SLPLOT - checking data'

c.... Determine upper bounds for x- and y-scales........................
c.... NOTE:  F cartesian plots (KLOG=0) min_x and min_y values are assumed
c....        to be 0.0 
      CALL UBGRAF(XMAX,UBX)

      IF(KLOG.EQ.0) THEN
         CALL UBGRAF(YMAX,UBY)
      ELSE
c....... Use special routine for log scale on y-axis 
c....... LBY is log of ymin; UBY is log of ymax for y-scale
         CALL UBLOG(YMIN,YMAX,LBY,UBY)
      ENDIF

C===========================================================

      IF(KLOG.EQ.1) THEN
c....... Now take log of y-values.........................
         DO 1200 K=1,KMAX 
         DO 1200 J=1,JMAX(K)
         Y(J,K) = ALOG10(Y(J,K))
ccc      write(*,12001) k,j,y(j,k), lby,uby
ccc12001 format(1x,'K,J,LOGY(J,K), LBY,UBY:',2I4,3F10.3)
1200     CONTINUE
      ENDIF

ccc      PAUSE 'SLPLOT - checking data again after taking log of y-values'
C===========================================================

c.... Used in LOTICY ...........................................
      YINC(1) = ALOG10(1) 
      YINC(2) = ALOG10(2) 
      YINC(3) = ALOG10(3)
      YINC(4) = ALOG10(4)
      YINC(5) = ALOG10(5)
      YINC(6) = ALOG10(6)
      YINC(7) = ALOG10(7)
      YINC(8) = ALOG10(8)
      YINC(9) = ALOG10(9)  
c.................................................................
C===========================================================


C.... Determine x-axis & y-axis divisions...............................
      XINC  = UBX  * 0.20   
      XINC2 = XINC * 0.50  
      IF(KLOG.EQ.0) THEN
         YINC0  = UBY  * 0.20 
         YINC2 = YINC0 * 0.50  
      ENDIF

C============= E N T E R   G R A P H I C S   M O D E =================
C.... Initialize graphics mode for PowerStation Graphics........
      CALL graphicsmode(maxx,maxy)

c......... Set background color here ................................ 
      status4 = SETBKCOLOR($BLACK)


c.... Register fonts..............................................
c.... Be sure font files are in current directory !!!!!!
      int2 = REGISTERFONTS( '*.FON' )

c.... Select font & set height & width of characters...........
ccc      list = option // 'h20w12b'  ! too large !
      list = option // 'h15w9b'
      if ( SETFONT( list ) .LE.0 ) THEN
         status2 = SETVIDEOMODE( $DEFAULTMODE )
         STOP 'Error:  cannot set font!'
      endif


c....... By default, CURSOR is on for text and off when in graphics mode - 
c....... exactly what we want!
cc      int2 = DISPLAYCURSOR ( $GCURSORON )

C==================================================================
500   CONTINUE 
C==================================================================

C.... Calculate margins and define world coordinates ....................
c.... x-margin is a fraction of graph upper bound UBX
c.... y-margin is a fraction of graph upper bound UBY

      XMAR  = XMARF * UBX 

      IF(KLOG.EQ.0) THEN
         YMAR  = YMARF * UBY 
      ELSE
         YMAR  = YMARF * (UBY - LBY) 
      ENDIF

      WXMIN = 0.0
      WYMIN = 0.0
      WXMAX = UBX + XMAR 

      IF(KLOG.EQ.0) THEN
         WYMAX = UBY + YMAR 
      ELSE
         WYMAX = WYMIN + (UBY-LBY) + YMAR
      ENDIF

      status2 = SETWINDOW ( finvert, WXMIN, WYMIN, WXMAX, WYMAX )

C.... Translate graph to "center" of screen .................. 
c.... Offsets (XOFSET & YOFSET) are fractions of XMAR & YMAR, respectively.
      GX0 = XOFSET * XMAR
      GY0 = YOFSET * YMAR 
      GXM = GX0 + UBX
      GYM = GY0 + (UBY - LBY)
cc      WRITE(*,601) XMAR,YMAR,WXMAX,WYMAX,GX0,GY0,GXM,GYM
cc601   FORMAT(1X,   'XMAR,YMAR,WXMAX,WYMAX,GX0,GY0,GXM,GYM:',/(4G12.5))
cc      WRITE(*,603) LBY, UBY
cc603   FORMAT(1X,'LBY, UBY=',2F10.3)

c.... Graph border (bottom, right, top, left)...........................
      status2 = SETCOLOR (11)

      CALL MOVETO_W (GX0, GY0, wxy)
      status2 = LINETO_W (GXM, GY0)
c      PAUSE 'Bottom drawn'

      status2 = LINETO_W (GXM, GYM)
c      PAUSE 'Right drawn'

      status2 = LINETO_W (GX0, GYM)
c      PAUSE 'Top drawn'

      status2 = LINETO_W (GX0, GY0)
c      PAUSE 'Left drawn'


c.... Tic marks & light lines...........................................
      CALL TICX (XINC2,GX0,GY0,GYM)
c     PAUSE 'x-tics done'

      IF(KLOG.EQ.0) THEN
         CALL TICY (YINC2,GX0,GY0,GXM)
c        PAUSE 'y-tics done'
      ELSE
c....... Use special routine forlog scale..... 
         CALL LOTICY (YINC, GX0,GY0, GXM,GYM, UBY,LBY)
c        PAUSE 'y-tics done'
      ENDIF


c.... Axis scale numbers................................................
      CALL SCALEX (XINC,GX0,GY0,UBX,UBY)  
cc      PAUSE 'x-scale numbers done'

      IF(KLOG.EQ.0) THEN
         CALL SCALEY (YINC0,GX0,GY0,UBX,UBY)  
c        PAUSE 'y-scale numbers done'
      ELSE
         CALL SCYLOG (YINC,GX0,GY0,UBX,UBY, LBY)  
c        PAUSE 'y-scale numbers done'
      ENDIF


c.... Reset character size...................................
      list = option // 'h15w9b'
      if ( SETFONT( list ) .LE.0 ) THEN
         status2 = SETVIDEOMODE( $DEFAULTMODE )
         STOP 'Error:  cannot set font!'
      endif


C.... Graph title.......................................................
cc      status2 = SETCOLOR (1)
      status2 = SETCOLOR (6)
 
      GXLEN = GXM - GX0
      GYLEN = GYM - GY0

c....... Title for screen plot.........
         XS = GX0 + .040*GXLEN  
         YS = GYM + 0.40*GY0 
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT (GTITLE) 

c.... x-axis label.....................................................

         XS = GX0 + 0.425*GXLEN 
         YS = GY0 - 0.350*GY0  
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT (XLABL) 

c.... y-axis label......................................................
         XS = GX0 - 0.600*GX0
         YS = GY0 + 0.150*GYLEN

         CALL MOVETO_W (XS,YS, wxy)
c....... Increase character size slightly for y-axis label .............
         list = option // 'h16w13b'
         if ( SETFONT( list ) .LE.0 ) THEN
            status2 = SETVIDEOMODE( $DEFAULTMODE )
            STOP 'Error:  cannot set font!'
         endif

c....... Rotate 90 degrees counterclockwise for YLABL
         CALL SETGTEXTVECTOR (0,1)
         CALL OUTGTEXT (YLABL) 
         CALL SETGTEXTVECTOR (1,0)  ! back to normal orientation

c....... Reset character size ..........................................
         list = option // 'h15w9b'
         if ( SETFONT( list ) .LE.0 ) THEN
            status2 = SETVIDEOMODE( $DEFAULTMODE )
            STOP 'Error:  cannot set font!'
         endif


CC      CALL SETLINESTYLE (#FFFF)
c.... Linestyles are now defined above in DATA STM'T..................


c............ P L O T   T H E   D A T A ................................... 
      DO 800 K = 1,KMAX
      NPTS = JMAX(K)
      IF(K.EQ.1) THEN
           CALL SETLINESTYLE (line1)
           status2 = SETCOLOR( FORCOL(1) )   ! 10 = light green
      ENDIF 
      IF(K.EQ.2) THEN
           CALL SETLINESTYLE (line2)
           status2 = SETCOLOR( FORCOL(2) )   ! 12 = light red
      ENDIF
      IF(K.EQ.3) THEN
           CALL SETLINESTYLE (line3) 
           status2 = SETCOLOR( FORCOL(3) )   !  1 = blue
      ENDIF 
      IF(K.EQ.4) THEN
           CALL SETLINESTYLE (line4)
           status2 = SETCOLOR( FORCOL(4) )   ! 15 = bright white
      ENDIF
      IF(K.EQ.5) THEN
           CALL SETLINESTYLE (line5)
           status2 = SETCOLOR( FORCOL(5) )   ! 13 = light magenta
      ENDIF

      DO 700 J=1,NPTS
      XVAL = X(J,K)
      YVAL = Y(J,K)
      XS = GX0+XVAL

      IF(KLOG.EQ.0) THEN
         YS = GY0+YVAL
      ELSE
         YS = GY0 + (YVAL - LBY)
      ENDIF

      IF(J.EQ.1) THEN
c....... Position cursor at first data point.........
         CALL MOVETO_W (XS,YS, wxy)
         IF(IPLOPT(K).EQ.0) GO TO 700
      ENDIF

      IF(IPLOPT(K).EQ.0 .OR. IPLOPT(K).EQ.2) THEN
c....... Draw line from previous cursor position to (XS,YS)..........
         status2 = LINETO_W (XS,YS)
      ENDIF

      IF(IPLOPT(K).GT.0) THEN
c....... Plot character PCHAR(K) at (XS,YS) if IPLOPT=1 or 2............
         IF(K.EQ.5) THEN
            XS1 = XS - 0.040*GX0
            YS1 = YS + 0.100*GY0
         ELSEIF (K.EQ.4) THEN
            XS1 = XS - 0.030*GX0
            YS1 = YS + 0.100*GY0
         ELSEIF (K.EQ.3) THEN
            XS1 = XS - 0.040*GX0
            YS1 = YS + 0.100*GY0
         ELSEIF (K.EQ.2) THEN
            XS1 = XS - 0.040*GX0
            YS1 = YS + 0.100*GY0
         ELSE  ! (K=1)
            XS1 = XS - 0.050*GX0
            YS1 = YS + 0.100*GY0
         ENDIF

         CALL MOVETO_W (XS1,YS1, wxy)
         CALL OUTGTEXT (PCHAR(K))         
c....... Then move back to avoid  "offset" 
         CALL MOVETO_W (XS,YS, wxy)
         
      ENDIF

700   CONTINUE
800   CONTINUE


C.... Place KMAX legends below x-axis label using graph coordinates...
      CALL LEGEND (KMAX,GX0,GY0,GXM, PCHAR, IPLOPT,FORCOL, LEGND,
     &             line1,line2,line3,line4,line5)


CCCCCCCCCCCCC DON'T NEED VIEWPORT 
C.... Set VIEWPORT  coordinates ................................
CC      IVX0 = 200
CC      IVY0 = 5
CC      IVXM = 500
CC      IVYM = 100
CC      list = option // 'h15w9b'
CC      if ( SETFONT( list ) .LE.0 ) THEN
CC         status2 = SETVIDEOMODE( $DEFAULTMODE )
CC         STOP 'Error:  cannot set font!'
CC      endif
CC      CALL SETVIEWPORT (IVX0, IVY0, IVXM, IVYM)
CC      CALL CLEARSCREEN ($GVIEWPORT)
CC      status2 = SETCOLOR (9)
CC      status4 = SETBKCOLOR (8)
CC      CALL MOVETO (110,35, xy)
CC      CALL OUTGTEXT ('Hi from viewport')
CC      PAUSE ' '

2000  CALL endprogram()

3000  CLOSE(17)
      RETURN 
      END
 


C============ BEGIN SUBROUTINES & FUNCTIONS FOR  PowerStation Graphics

      SUBROUTINE graphicsmode( maxx, maxy )

      INCLUDE 'FGRAPH.FD'

      INTEGER*2            modestatus2, maxx, maxy
      RECORD /videoconfig/ myscreen

c     Set highest resolution graphics mode
c
      modestatus2 = SETVIDEOMODE( $MAXRESMODE )
      IF( modestatus2.eq.0) STOP 'Error: cannot set graphics mode'
C
C     Determine the minimum and maximum dimensions.
c
      CALL GETVIDEOCONFIG( myscreen )
      maxx =  myscreen.numxpixels - 1
      maxy =  myscreen.numypixels - 1
      END


CC    ENDPROGRAM - This routine waits for the ENTER key to be pressed,
CC                 then resets the screen before returning.

      SUBROUTINE endprogram()

      INCLUDE 'FGRAPH.FD'
      INTEGER*2 status2

      READ(*,*)        ! Wait for ENTER to be pressed
      status2 = SETVIDEOMODE( $DEFAULTMODE )
      END
 


C************ FUNCTIONS *******************
C!!!!!!!!!!!!!!!!!!!! MAYBE DON'T NEED NEWX OR NEWY !!!!!!!!!!!!!!!!!!!!
CC    NEWX - This function finds new x-coordinates.

      INTEGER*2 FUNCTION newx( xcoord )

      INTEGER*2 xcoord, maxx, maxy
      REAL*4    tempx
      COMMON    maxx, maxy

      tempx = maxx / 1000.0
      tempx = xcoord * tempx + 0.5
      newx  = tempx
      END

CC    NEWY - This function finds new y-coordinates
cc
      INTEGER*2 FUNCTION newy( ycoord )

      INTEGER*2 ycoord, maxx, maxy
      REAL*4    tempy
      COMMON    maxx, maxy

      tempy = maxy / 1000.0
      tempy = ycoord * tempy + 0.5
      newy  = tempy
      END
      
    
      
c.... Subroutines for HPLOT..........................................
 
      SUBROUTINE WAIT(ITIME)
C:::: WAIT:  Produces a delay before continuing the program;
C            delay is proportional to TIME
      INTEGER*4  I, J, JMAX, ITIME
      IMAX = 1000 * ITIME
      JMAX = 1000 * ITIME
      DO 5 I=1,IMAX
      DO 5 J=1,JMAX
5     CONTINUE
      RETURN
      END

      SUBROUTINE UBGRAF(Z,UZ)
c.... Z  = Xmax or Ymax based on data......
c.... UZ = Calculated value for upper bound of x- or y-scale....
      ZLOG = ALOG10(Z)
      IEXP = INT(ZLOG)
      IF(Z.LE.1.0) IEXP = IEXP - 1     
      X1 = 10.**IEXP
      IF(ABS(X1-Z) .LE. 1.E-06) THEN
         UZ = X1
         RETURN
      ENDIF
      DO 100 I=1,10
      IF(I.EQ.1) THEN
         X = X1*1.5
      ELSE
        X = X1*I
      ENDIF
      IF(X.GE.Z) THEN
         UZ = X
         RETURN
      ENDIF
100   CONTINUE
c.... Error has occurred if here; UZ will default to Z .................
      WRITE(*,101)
101   FORMAT(1X,'UZ cannot be determined in UBGRAF: will default to Z')
      UZ = Z     
      RETURN
      END



      SUBROUTINE SCALEX (XINC,GX0,GY0,UBX,UBY)  

c----------------------------- new stuff - 07/08/96-----------------
      INCLUDE 'FGRAPH.FD'
      INTEGER*2  status2, forecolor 
      INTEGER*4  backcolor
      REAL*8     GX0,GY0, XS,YS 

      RECORD /wxycoord/  wxy
      RECORD /rccoord/   curpos

c........ font stuff......
      CHARACTER*10  option /"t'tms rmn'"/
      CHARACTER*30  list

c-------------------------------------------------------------------


      DIMENSION     X(5) 
      INTEGER*4     IX(5), NONBL(5)
      CHARACTER*10  CHX(5) 
      CHARACTER*12  HCHX(5) 
      CHARACTER*4   STR0
      CHARACTER*7   FM1,FM2,FM3,FM4,FM5,FM6 
      DATA   FM1,      FM2,      FM3,      FM4,      FM5,      FM6
     &   /'(1X,I1)','(1X,I2)','(1X,I3)','(1X,I4)','(1X,I5)','(1X,I6)'/

    
c----------------------------------------------
       forecolor = SETCOLOR (15)  
c----------------------------------------------

      STR0 = '0   '
      XS = GX0 - 0.025*GX0
      YS = GY0 - 0.10*GY0

cc      WRITE(*,1001) XS,YS
cc1001  FORMAT(1X,2G14.6)


      CALL MOVETO_W (XS,YS, wxy)
      list = option // 'h15w9b'
      if ( SETFONT( list ) .LE.0 ) THEN
         status2 = SETVIDEOMODE( $DEFAULTMODE )
         STOP 'Error:  cannot set font!'
      endif


      CALL OUTGTEXT(STR0)


      ZLOG = ALOG10(UBX)
      ILOGX = INT(ZLOG)
      ICHX  = ILOGX + 1

      OPEN(22,FILE='TMP1.DAT',STATUS='UNKNOWN')
      CLOSE(22,STATUS='DELETE')
      OPEN(22,FILE='TMP1.DAT',STATUS='NEW')

      DO 20 I=1,5
      IF(UBX.GE.10 .AND. UBX.LE.999999) THEN
         IX(I) = XINC*I
         IF(IX(I).GE.1 .AND. IX(I).LE.9) THEN
            WRITE(22,FM1) IX(I)   
            NONBL(I) = 1
         ELSEIF(IX(I).GE.10 .AND. IX(I).LE.99) THEN
            WRITE(22,FM2) IX(I)   
            NONBL(I) = 2
         ELSEIF(IX(I).GE.100 .AND. IX(I).LE.999) THEN
            WRITE(22,FM3) IX(I)   
            NONBL(I) = 3
         ELSEIF(IX(I).GE.1000 .AND. IX(I).LE.9999) THEN
            WRITE(22,FM4) IX(I)   
            NONBL(I) = 4
         ELSEIF(IX(I).GE.10000 .AND. IX(I).LE.99999) THEN
            WRITE(22,FM5) IX(I)   
            NONBL(I) = 5
         ELSEIF(IX(I).GE.100000 .AND. IX(I).LE.999999) THEN
            WRITE(22,FM6) IX(I)   
            NONBL(I) = 6
         ENDIF
      ELSE
         X(I) = XINC*I
         WRITE(22,3091)  X(I)
ccc         WRITE(*, 30911) XINC,I,X(I)
      ENDIF
20    CONTINUE

3091  FORMAT(1X,E8.2)
30911 FORMAT(1X,F10.2,I5,F10.2)

      REWIND(22)
      READ(22,311) (CHX(I),I=1,5)
311   FORMAT(1X,A10)


C---------------- DEBUG STUFF ----------------------------------------
CC      IF(UBX.GE.10 .AND. UBX.LE.999999) THEN
CC         DO 32 I=1,5
CC         WRITE(*,3111)  UBX,XINC,I,IX(I),CHX(I)
CC3111     FORMAT(1X,    'UBX,XINC,I,IX(I)=',2F10.2,2I5,
CC     &         /1X,'CHX(I)=',A10)
CC         PAUSE ' '
CC32       CONTINUE
CC      ELSE
CC         DO 34 I=1,5
CC         WRITE(*,3113) UBX,XINC,I,X(I),CHX(I)
CC3113     FORMAT(1X,'UBX,XINC,I,X(I)=',2F15.6,I5,F15.6,
CC     &         /1X,'CHX(I)=',A10)
CC34       CONTINUE  
CC         PAUSE ' '
CC      ENDIF
C----------------------------------------------------------------------

      YS = GY0 - 0.10*GY0

      DO 40 I=1,5
      XS = GX0 + XINC*I
      IF(UBX.GE.10 .AND. UBX.LE.999999) THEN
            IF(NONBL(I).EQ.1) XS = XS - .050*GX0
            IF(NONBL(I).EQ.2) XS = XS - .090*GX0
            IF(NONBL(I).EQ.3) XS = XS - .120*GX0
            IF(NONBL(I).EQ.4) XS = XS - .170*GX0
            IF(NONBL(I).EQ.5) XS = XS - .220*GX0
            IF(NONBL(I).EQ.6) XS = XS - .270*GX0
      ELSE
           XS = XS - .350*XINC
      ENDIF

  
      CALL MOVETO_W (XS,YS, wxy)
      CALL OUTGTEXT ( CHX(I) )

40    CONTINUE
      CLOSE(22,STATUS='DELETE')
      RETURN
      END


      SUBROUTINE SCALEY (YINC0,GX0,GY0,UBX,UBY)  
c----------------------------- new stuff - 07/08/96-----------------
      INCLUDE 'FGRAPH.FD'
      INTEGER*2  status2, forecolor 
      INTEGER*4  backcolor
      REAL*8     GX0,GY0, XS,YS, GY00

c........ font stuff......
      CHARACTER*10  option /"t'tms rmn'"/
      CHARACTER*30  list

      RECORD /wxycoord/  wxy
      RECORD /rccoord/   curpos
c-------------------------------------------------------------------

      DIMENSION     Y(5) 
      INTEGER*4     IY(5), NONBL(5)
      CHARACTER*10  CHY(5) 
      CHARACTER*12  HCHY(5) 
      CHARACTER*4   STR0
      CHARACTER*7   FM1,FM2,FM3,FM4,FM5,FM6 
      DATA   FM1,      FM2,      FM3,      FM4,      FM5,      FM6
     &   /'(1X,I1)','(1X,I2)','(1X,I3)','(1X,I4)','(1X,I5)','(1X,I6)'/


      STR0 = '0   '
      XS = GX0 - 0.20*GX0
      YS = GY0 - 0.025*GY0 
  
      CALL MOVETO_W (XS,YS, wxy)
c.... Rotate 90 degrees counterclockwise for y-axis scale
      CALL SETGTEXTVECTOR (0,1)
c.... Set desired character size for y-axis scale...........
      list = option // 'h14w8b'
      if ( SETFONT( list ) .LE.0 ) THEN
         status2 = SETVIDEOMODE( $DEFAULTMODE )
         STOP 'Error:  cannot set font!'
      endif

      CALL OUTGTEXT( STR0 )

      ZLOG = ALOG10(UBY)
      ILOGY = INT(ZLOG)
      ICHY  = ILOGY + 1

      OPEN(22,FILE='TMP1.DAT',STATUS='UNKNOWN')
      CLOSE(22,STATUS='DELETE')
      OPEN(22,FILE='TMP1.DAT',STATUS='NEW')

      DO 20 I=1,5
      IF(UBY.GE.10 .AND. UBY.LE.999999) THEN
         IY(I) = YINC0*I
c....... Idea here is to get blanks at end; MS-Fortran 4.01 puts blanks at beginning!

         IF(IY(I).GE.1 .AND. IY(I).LE.9) THEN
            WRITE(22,FM1) IY(I)   
            NONBL(I) = 1
         ELSEIF(IY(I).GE.10 .AND. IY(I).LE.99) THEN
            WRITE(22,FM2) IY(I)   
            NONBL(I) = 2
         ELSEIF(IY(I).GE.100 .AND. IY(I).LE.999) THEN
            WRITE(22,FM3) IY(I)   
            NONBL(I) = 3
         ELSEIF(IY(I).GE.1000 .AND. IY(I).LE.9999) THEN
            WRITE(22,FM4) IY(I)   
            NONBL(I) = 4
         ELSEIF(IY(I).GE.10000 .AND. IY(I).LE.99999) THEN
            WRITE(22,FM5) IY(I)   
            NONBL(I) = 5
         ELSEIF(IY(I).GE.100000 .AND. IY(I).LE.999999) THEN
            WRITE(22,FM6) IY(I)   
            NONBL(I) = 6
         ENDIF
      ELSE
         Y(I) = YINC0*I
         WRITE(22,3091) Y(I)
      ENDIF
20    CONTINUE

3091  FORMAT(1X,E8.2)


      REWIND(22)
      READ(22,311) (CHY(I),I=1,5)
311   FORMAT(1X,A10)    

      XS = GX0 - 0.20*GX0

      DO 80 I=1,5
      YS = GY0 + YINC0*I
      IF(UBY.GE.10 .AND. UBY.LE.999999) THEN
            IF(NONBL(I).EQ.1) YS = YS - .020*GY0
            IF(NONBL(I).EQ.2) YS = YS - .075*GY0
            IF(NONBL(I).EQ.3) YS = YS - .125*GY0
            IF(NONBL(I).EQ.4) YS = YS - .175*GY0
            IF(NONBL(I).EQ.5) YS = YS - .225*GY0
            IF(NONBL(I).EQ.6) YS = YS - .275*GY0
      ELSE
         YS = YS - 0.500*YINC0
      ENDIF

      CALL MOVETO_W (XS,YS, wxy)
      CALL OUTGTEXT( CHY(I) )

80    CONTINUE

      CALL SETGTEXTVECTOR (1,0)  ! back to normal orientation

      CLOSE(22,STATUS='DELETE')
      RETURN
      END


      SUBROUTINE TICX (XINC2,GX0,GY0,GYM)
      INCLUDE 'FGRAPH.FD'

      REAL*8 GX0,GY0, GXM, GYM,X,Y
      INTEGER*2 status2
      RECORD /wxycoord/  wxy

c.... Routine to place 9 tic marks along x-axis; 4 major tics and
c.... 5 minor tics - dividing axis into 10 equal parts; i.e.
c.... GX0 + 10*INCX2 = GXM (end of axis).  All 9 tics are painted white;
c.... at major tics, a light grey line is drawn to the opposite side of 
c.... the graph.  

      DO 100 I=1,9 
      X = GX0 + I*XINC2
      IF(MOD(I,2) .EQ. 0) THEN
c....... Draw light gray line...................
         status2 = SETCOLOR (8)

         CALL MOVETO_W (X,GY0, wxy)
         status2 = LINETO_W (X,GYM)


c....... overlay with white tic marks ........
         status2 = SETCOLOR (15)
         Y = GYM - 0.02*GYM

         status2 = LINETO_W (X,Y)
         CALL MOVETO_W (X,GY0, wxy)

         Y = GY0 + 0.02*GYM 
         status2 = LINETO_W (X,Y)
c...............................................
      ELSE
        CALL MOVETO_W (X,GY0, wxy)

        status2 = SETCOLOR (15)
        Y = GY0 + 0.01*GYM 
        status2 = LINETO_W (X,Y)

        CALL MOVETO_W (X,GYM, wxy)
        Y = GYM - 0.01*GYM
        status2 = LINETO_W (X,Y)
      ENDIF
100   CONTINUE
      RETURN
      END 

      SUBROUTINE TICY (YINC2,GX0,GY0,GXM)
      INCLUDE 'FGRAPH.FD'

      REAL*8 GX0,GY0, GXM, GYM,X,Y
      INTEGER*2 status2
      RECORD /wxycoord/  wxy

c.... Routine to place 9 tic marks along y-axis 
      DO 100 I=1,9 
      Y = GY0 + I*YINC2
cc      WRITE(*,101)     YINC2,GY0,GYM,Y
cc101   FORMAT(1X,'TICY: YINC2,GY0,GYM,Y:',4F10.2)
cc      PAUSE ' '
      IF(MOD(I,2) .EQ. 0) THEN
c....... Draw light gray line...................
         status2 = SETCOLOR (8)
         CALL MOVETO_W (GX0,Y, wxy)
         status2 = LINETO_W (GXM,Y)
c....... overlay with white tic marks ........
         status2 = SETCOLOR (15)
         X = GXM - 0.0150*GXM
         status2 = LINETO_W (X,Y)
         CALL MOVETO_W (GX0,Y, wxy)
         X = GX0 + 0.0150*GXM 
         status2 = LINETO_W (X,Y)
c...............................................
      ELSE
        CALL MOVETO_W (GX0,Y, wxy)
        status2 = SETCOLOR (15)
        X = GX0 + 0.0075*GXM 
        status2 = LINETO_W (X,Y)
        CALL MOVETO_W (GXM,Y, wxy)
        X = GXM - 0.0075*GXM
        status2 = LINETO_W (X,Y)
      ENDIF
100   CONTINUE
      RETURN
      END 



      SUBROUTINE LEGEND (KMAX,GX0,GY0,GXM, PCHAR,IPLOPT,FORCOL, LEGND,
     &                   line1,line2,line3,line4,line5)
 
c.... Converted from world coordinates to graph coordinates - 08/13/92
c.... because graph may not always be exactly in center; e.g. will 
c.... usually be offset to right because don't need space for y-axis
c.... label on right. 

c---- Note that legend is divided on 2 lines; 1st 10-characters on 1st line
c----                                         2nd 10-characters on 2nd line

C=====> NOTE:  Array FORCOL from main (HPLOT)is used to assign colors!


C***************************************************************
c.... New stuff for PowerStation Graphics - 07/28/96 ...........

      INCLUDE 'FGRAPH.FD'

c....... font stuff..........
      CHARACTER*10  option /"t'tms rmn'"/
      CHARACTER*30  list

      INTEGER*2  status2 
C........ Arguments to LINETO_W must be double precision !!
      REAL*8  GX0, GY0, GXM, GYM, XS,YS, XS1,YS1 

      INTEGER*2 line1, line2, line3, line4, line5


      RECORD /wxycoord/  wxy
      RECORD /rccoord/   curpos
C***************************************************************


      CHARACTER*1  PCHAR(5) 
      CHARACTER*4  PCH(5)

      CHARACTER*20 LEGND(5)
      CHARACTER*10 LEG1(2), LEG2(2), LEG3(2), LEG4(2), LEG5(2)
      
      INTEGER*2     IPLOPT(5), FORCOL(5) 
C-----------------------------------------------
      LEG1(1) =      LEGND(1)(1:10)
      LEG1(2) =      LEGND(1)(11:20)
      LEG2(1) =      LEGND(2)(1:10)
      LEG2(2) =      LEGND(2)(11:20)
      LEG3(1) =      LEGND(3)(1:10)
      LEG3(2) =      LEGND(3)(11:20)
      LEG4(1) =      LEGND(4)(1:10) 
      LEG4(2) =      LEGND(4)(11:20)
      LEG5(1) =      LEGND(5)(1:10) 
      LEG5(2) =      LEGND(5)(11:20)
C------------------------------------------------------


C************ Added for PowerStation Graphics - 07/28/96 *************
      list = option // 'h13w7b'
      if ( SETFONT( list ) .LE.0 ) THEN
         status2 = SETVIDEOMODE( $DEFAULTMODE )
         STOP 'Error:  cannot set font!'
      endif
C*********************************************************************


      TW  = GX0      ! just a guess - but made it work (with minor changes)!

      GXLEN  = GXM - GX0
      GXCTR  = GX0 + 0.5*GXLEN  

c.... We aregoing to print legend on two lines; e.g. LEG1(1) is first 10 char.
c     & LEG1(2) is 2nd 10 characters of LEGND(1)
c.... Distance between legends = 0.044*GXLEN  (just a guess!!)
      XLINE  = 0.0250 * GXLEN
      HALFLN = 0.0125 * GXLEN  

c.... Define y-values for bar & two lines of legend
         YBAR = 0.50*GY0
         YLN1 = 0.35*GY0
         YLN2 = 0.20*GY0

C.... Set color for first legend .......      
      status2 = SETCOLOR (FORCOL(1))

      IF(KMAX.EQ.1) THEN
c....... Write legend 1 - without color bar......................
         CALL SETLINESTYLE (line1)
         YS = YLN1 
         XS = GXCTR - 0.30*TW  
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(2))
         RETURN


      ELSEIF(KMAX.EQ.2) THEN
c....... Write legend 1 and short color bar or plot character...........
         CALL SETLINESTYLE (line1)
         YS =  YLN1 
         XS = GXCTR - TW - .022*GXLEN 
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(2))
         IF(IPLOPT(1).EQ.0) THEN
c.......... Draw short colored bar........................
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
c.......... Print colored plot character PCH..............
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(1) )
         ENDIF

c....... Write legend 2 and short color bar or plot character............
         CALL SETLINESTYLE (line2)
         status2 = SETCOLOR (FORCOL(2))
         YS =  YLN1 
         XS = GXCTR + 0.022*GXLEN
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG2(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG2(2))
         IF(IPLOPT(2).EQ.0) THEN
c.......... Draw short colored bar......................
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
c.......... Print colored plot character PCH..............
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(2) )
         ENDIF
         RETURN


      ELSEIF(KMAX.EQ.3) THEN
c....... Write legend 1 and short color bar or plot character...........
         CALL SETLINESTYLE (line1)
         YS =  YLN1 
         XS = GXCTR - 1.50*TW - .044*GXLEN 
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(2))
         IF(IPLOPT(1).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
c.......... Print colored plot character PCH..............
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(1) )
         ENDIF

c....... Write legend 2 and short color bar or plot character...........
         CALL SETLINESTYLE (line2)
         status2 = SETCOLOR (FORCOL(2))
         YS =  YLN1 
         XS = GXCTR - 0.50*TW
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG2(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG2(2))
         IF(IPLOPT(2).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(2) )
         ENDIF

c....... Write legend 3 and short color bar or plot character...........
         CALL SETLINESTYLE (line3)
         status2 = SETCOLOR (FORCOL(3))
         YS =  YLN1 
         XS = GXCTR + 0.50*TW + .044*GXLEN 
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG3(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG3(2))
         IF(IPLOPT(3).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(3) )
         ENDIF     
         RETURN


      ELSEIF(KMAX.EQ.4) THEN
c....... Write legend 1 and short color bar or plot character...........
         CALL SETLINESTYLE (line1)
         YS =  YLN1 
         XS = GXCTR - 2.0*TW - .066*GXLEN 
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(2))
         IF(IPLOPT(1).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(1) )
         ENDIF

c....... Write legend 2 and short color bar or plot character...........
         CALL SETLINESTYLE (line2)
         status2 = SETCOLOR (FORCOL(2))
         YS =  YLN1 
         XS = GXCTR - TW - .022*GXLEN
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG2(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG2(2))

         IF(IPLOPT(2).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
           CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(2) )
         ENDIF

c....... Write legend 3 and short color bar or plot character...........
         CALL SETLINESTYLE (line3)
         status2 = SETCOLOR (FORCOL(3))
         YS =  YLN1 
         XS = GXCTR + 0.022*GXLEN
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG3(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG3(2))
         IF(IPLOPT(3).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(3) )
         ENDIF

c....... Write legend 4 and short color bar or plot character...........
         CALL SETLINESTYLE (line4)
         status2 = SETCOLOR (FORCOL(4))
         YS =  YLN1 
         XS = GXCTR + TW + .066*GXLEN 
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG4(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG4(2))

         IF(IPLOPT(4).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(4) )
         ENDIF
         RETURN


      ELSEIF(KMAX.EQ.5) THEN
c....... Write legend 1 and short color bar or plot character...........
         CALL SETLINESTYLE (line1)
         YS =  YLN1 
         XS = GXCTR - 2.50*TW - .088*GXLEN 
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG1(2))

         IF(IPLOPT(1).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(1) )
         ENDIF

c....... Write legend 2 and short color bar or plot character...........
         CALL SETLINESTYLE (line2)
         status2 = SETCOLOR (FORCOL(2))
         YS =  YLN1 
         XS = GXCTR - 1.50*TW - .044*GXLEN
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG2(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG2(2))
         IF(IPLOPT(2).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(2) )
         ENDIF

c....... Write legend 3 and short color bar or plot character...........
         CALL SETLINESTYLE (line3)
         status2 = SETCOLOR (FORCOL(3))
         YS =  YLN1 
         XS = GXCTR - 0.5*TW
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG3(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG3(2))
         IF(IPLOPT(3).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(3) )
         ENDIF

c....... Write legend 4 and short color bar or plot character...........
         CALL SETLINESTYLE (line4)
         status2 = SETCOLOR (FORCOL(4))
         YS =  YLN1 
         XS = GXCTR + 0.50*TW + .044*GXLEN 
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG4(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG4(2))

         IF(IPLOPT(4).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE 
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(4) )
         ENDIF

c....... Write legend 5 and short color bar or plot character...........
         CALL SETLINESTYLE (line5)
         status2 = SETCOLOR (FORCOL(5))
         YS = YLN1 
         XS = GXCTR + 1.50*TW + .088*GXLEN  
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG5(1))
         YS = YLN2
         CALL MOVETO_W (XS,YS, wxy)
         CALL OUTGTEXT(LEG5(2))

         IF(IPLOPT(5).EQ.0) THEN
            XS = XS + 0.23*TW 
            YS = YBAR - 0.10*GY0
            CALL MOVETO_W (XS,YS, wxy)
            XS = XS + XLINE  
            status2 = LINETO_W (XS,YS)
         ELSE
            XS = XS + 0.25*TW 
            YS = YBAR - 0.020*GY0  
            CALL MOVETO_W (XS,YS, wxy)
            CALL OUTGTEXT ( PCHAR(5) )
         ENDIF

      ENDIF

      RETURN
      END


C============= L G S U B S  B E G I N S  H E R E ====================
C===========   THREE ROUTINES FOR LOG SCALE ON Y-AXIS =============
c....... LGSUBS.FOR (modified for PowerStation Grapics....
C......... THINK ALL THIS WILL NOW COMPILE IN POWERSTATION FORTRAN.......

      SUBROUTINE UBLOG(YMIN,YMAX,LBY,UBY)
      REAL LBY
c.... YMIN = Ymin based on data......
c.... YMAX = Ymax based on data......
c.... LBY  = Calculated LOG value for lower bound of y-scale....
c.... UBY  = Calculated LOG value for upper bound of y-scale....

      ZLOG1 = ALOG10(YMIN)
      ZLOG2 = ALOG10(YMAX)

      IF(ZLOG1.GT.0) THEN
         IEXP1 = INT(ZLOG1)
      ELSE
         IEXP1 = INT(ZLOG1-1.)
      ENDIF

      IF(ZLOG2.GT.0) THEN
         IEXP2 = INT(ZLOG2) + 1
      ELSE
         IEXP2 = INT(ZLOG2)
      ENDIF

      LBY = IEXP1
      UBY = IEXP2

      RETURN
      END


      SUBROUTINE SCYLOG (YINC,GX0,GY0,UBX,UBY, LBY )  

      INCLUDE 'FGRAPH.FD'
      INTEGER*2  status2, forecolor 
      INTEGER*4  backcolor
      REAL*8     GX0,GY0, XS,YS, GY00

      CHARACTER*4  STR0

c........ font stuff......
      CHARACTER*10  option /"t'tms rmn'"/
      CHARACTER*30  list

      RECORD /wxycoord/  wxy
      RECORD /rccoord/   curpos
c-------------------------------------------------------------------

c.... Increased no of y tic-marks from 5 to 10 for log scale.......
c.... Also added LBY.........
c.... Also added array IY1 below (03/30/95).......
      DIMENSION     Y(10), YINC(1)  
      REAL          LBY
      INTEGER*4     IY(10), IY1(10), NONBL(10)
      CHARACTER*10  CHY(10) 

      CHARACTER*7   FM1,FM2,FM3,FM4,FM5,FM6,FM7 
      DATA   FM1,      FM2,      FM3,      FM4,      FM5,      FM6,FM7
     &   /'(1X,I1)','(1X,I2)','(1X,I3)','(1X,I4)','(1X,I5)','(1X,I6)',
     &                                                      '(1X,I7)'/

      XS = GX0 - 0.20*GX0
      YS = GY0 - 0.025*GY0 

      CALL MOVETO_W (XS,YS, wxy)
c.... Rotate 90 degrees counterclockwise for y-axis scale
      CALL SETGTEXTVECTOR (0,1)
c.... Set desired character size for y-axis scale...........
      list = option // 'h14w8b'
      if ( SETFONT( list ) .LE.0 ) THEN
         status2 = SETVIDEOMODE( $DEFAULTMODE )
         STOP 'Error:  cannot set font!'
      endif

C========================================================================

      OPEN(22,FILE='TMP1.DAT',STATUS='UNKNOWN')
      CLOSE(22,STATUS='DELETE')
      OPEN(22,FILE='TMP1.DAT',STATUS='NEW')

      NCYCLE = INT(UBY - LBY) 
      IF(NCYCLE.EQ.0) STOP
      NY = NCYCLE+1

      DO 20 I=1,NY
      IY(I) = INT(LBY) + (I-1)
      IF(LBY.GE.0.0 .AND. UBY.LT.7.0) THEN
C------- Raise 10 to power IY and store in IY1()......        
         IY1(I) = INT(10.**IY(I) + 1.E-06)
         IF(IY1(I).GE.1 .AND. IY1(I).LE.9) THEN
            WRITE(22,FM1) IY1(I)   
            NONBL(I) = 1
         ELSEIF(IY1(I).GE.10 .AND. IY1(I).LE.99) THEN
            WRITE(22,FM2) IY1(I)   
            NONBL(I) = 2
         ELSEIF(IY1(I).GE.100 .AND. IY1(I).LE.999) THEN
            WRITE(22,FM3) IY1(I)   
            NONBL(I) = 3
         ELSEIF(IY1(I).GE.1000 .AND. IY1(I).LE.9999) THEN
            WRITE(22,FM4) IY1(I)   
            NONBL(I) = 4
         ELSEIF(IY1(I).GE.10000 .AND. IY1(I).LE.99999) THEN
            WRITE(22,FM5) IY1(I)   
            NONBL(I) = 5
         ELSEIF(IY1(I).GE.100000 .AND. IY1(I).LE.999999) THEN
            WRITE(22,FM6) IY1(I)   
            NONBL(I) = 6
         ELSEIF(IY1(I).GE.1000000 .AND. IY1(I).LE.9999999) THEN
            WRITE(22,FM7) IY1(I)   
            NONBL(I) = 7
         ENDIF


      ELSE
         Y(I) = 10.**IY(I)
         WRITE(22,3091) Y(I)
         NONBL(I) = 8
      ENDIF
20    CONTINUE
3091  FORMAT(1X,E8.2)

      REWIND(22)
      READ(22,311) (CHY(I),I=1,NY)
311   FORMAT(1X,A10)    

      XS = GX0 - 0.15*GX0
      DO 80 I=1,NY
      YS = GY0 + (I-1)
            IF(NONBL(I).EQ.1) YS = YS - .020*GY0
            IF(NONBL(I).EQ.2) YS = YS - .075*GY0
            IF(NONBL(I).EQ.3) YS = YS - .125*GY0
            IF(NONBL(I).EQ.4) YS = YS - .175*GY0
            IF(NONBL(I).EQ.5) YS = YS - .225*GY0
            IF(NONBL(I).EQ.6) YS = YS - .275*GY0
            IF(NONBL(I).EQ.7) YS = YS - .325*GY0
            IF(NONBL(I).EQ.8) YS = YS - .375*GY0

      CALL MOVETO_W (XS,YS, wxy)
      CALL OUTGTEXT( CHY(I) )       
80    CONTINUE

      CALL SETGTEXTVECTOR (1,0)  ! back to normal orientation

      CLOSE(22,STATUS='DELETE')

      RETURN
      END


      SUBROUTINE LOTICY (YINC, GX0,GY0, GXM,GYM, UBY,LBY)
      INCLUDE 'FGRAPH.FD'

      REAL*8 GX0,GY0, GXM, GYM,X,Y
      INTEGER*2 status2
      RECORD /wxycoord/  wxy


      REAL LBY, YINC(1)
c.... Routine to place tic marks along y-axis @ each log cycle
c.... First, see how many log cycles we have.
      NCYCLE = INT(UBY - LBY) 
      IF(NCYCLE.EQ.1) GO TO 60
      DO 50 I=2,NCYCLE
c.... Draw light gray horizontal line for each log cycle (min of one cycle).... 
      Y = GY0 + (I-1) 
c....... Draw light gray line...................
         status2 = SETCOLOR (8)
         CALL MOVETO_W (GX0,Y, wxy)
         status2 = LINETO_W (GXM,Y)
c....... overlay with white tic marks ........
         status2 = SETCOLOR (15)
         X = GXM - 0.0150*GXM
         status2 = LINETO_W (X,Y)
         CALL MOVETO_W (GX0,Y, wxy)
         X = GX0 + 0.0150*GXM 
         status2 = LINETO_W (X,Y)
c...............................................
50    CONTINUE
60    CONTINUE

c.... Now must go back over each cycle and add tic marks at each log increment
c.... i.e.  2, 3, 4, 5, 6, 7, 8 and 9  for each cycle
      DO 100 I=1,NCYCLE
      DO 70  J=2,9
        Y = GY0 + (I-1) + YINC(J)        
        status2 = SETCOLOR (8)
        CALL MOVETO_W (GX0,Y, wxy)
        status2 = LINETO_W (GXM,Y)
70    CONTINUE
100   CONTINUE

      RETURN
      END 
