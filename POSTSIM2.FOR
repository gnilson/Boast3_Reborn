C**********************************************************************
C:::: PROGRAM POSTSIM2:  READS _____.BPF output file from BOAST3  and
C::::                    prepares input file for Program SIMPLT2
C=======> NOTE: RENAMED FROM POSTSIM.FOR - 8/24/96 - JUST TO DISTINGUISH
C=======>       FROM OLD POSTSIM.FOR WHICH USED SIMPLT COMPILED WITH MS
C=======>       FORTRAN AND OLD HALO GRAPHICS!
C
C     SUBROUTINES:   SIMSUBS.FOR (BPD & INTRP3)
C**********************************************************************

C:::: NOTE:  No double precision in this program ................

C!!!! PARAMETER STM'T BELOW MUST ALSO BE SAME IN SIMSUBS.FOR - 09/25/93 
      PARAMETER(NWX=60, NODX=10)

      CHARACTER*1  ANS,ANS2,OPT,MFILES
      CHARACTER*8  WELNAM
      CHARACTER*75 IHEDIN,  IHED2,  IHED3,  IHED4,  IHED5
      CHARACTER*8  FNAME, FNAME2, FNAME3, FNAME4, FNAME5, DUMY
      CHARACTER*12 FNAMTST

      CHARACTER*1  LOCTYP
      CHARACTER*5  LOCWEL(NWX) 
      CHARACTER*8  LOCNOD(NWX,NODX), LOCID
      CHARACTER*10 PARM(13), PARAM
      CHARACTER*20 PNAME(5) 

      INTEGER*2    IPARM, ISEL(5), IPAR(5) 

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
 
c----------------> Note:  1800 is not really a limit here; just using 
c---------------->        1800 to be consistent with POSTHIS2. 
c---------------->        The 5000 time-step limit should be sufficient 
c---------------->        for all simulators !!

c........ Added ISTEP - 8/24/96 ===> WILL TRY TO USE LIKE IN POSTHIS2.FOR
      DIMENSION  ISTEP(1800)

      DIMENSION     T(5000), TIME(1800) 
      DIMENSION  PLT1(5000),   Y1(1800)
      DIMENSION  PLT2(5000),   Y2(1800)
      DIMENSION  PLT3(5000),   Y3(1800)
      DIMENSION  PLT4(5000),   Y4(1800)
      DIMENSION  PLT5(5000),   Y5(1800)

      DIMENSION PLOT(5000,5)
 


      EQUIVALENCE (PLT1(1), PLOT(1,1)),
     &            (PLT2(1), PLOT(1,2)),
     &            (PLT3(1), PLOT(1,3)),
     &            (PLT4(1), PLOT(1,4)),
     &            (PLT5(1), PLOT(1,5))

      DATA PLOT /25000*0.0/
      
      DATA ISEL,LOOP, NITEMS /5*0, 0,0/
      DATA IFILES,IOUNIT/1,21/

      DATA PARM /'Gprd Mscfd',
     &           'Wprd stb/d',
     &           'Oprd stb/d',
     &           'CGprd MMcf',
     &           'CWprd Mstb',
     &           'COprd Mstb',
     &           'GORmcf/stb',
     &           'WORstb/stb',
     &           'Gsatn pct ',
     &           'Wsatn pct ',
     &           'Osatn pct ',
     &           'Pres psia ',
     &           'Pwf  psia '/

c      PLOT(J,I) =  plot item I @ time J  (used for initial storage)
c                                         (see equivalence statement above)
c      PLT1(J)   =  1st item @ time J 
c      PLT2(J)   =  2nd item @ time J 
c      PLT3(J)   =  3rd item @ time J                
c      PLT4(J)   =  4th item @ time J                 
c      PLR5(J)   =  5th item @ time J                

21    FORMAT(A1)
157   FORMAT(//)
159   FORMAT(7(/))

C******************* NOTE: MAX NO. OF POINTS MUST BE CONSISTENT WITH
      MAXPTS = 1800
C*******************       DIMENSION STATEMENTS ABOVE !

c.... Read filename from file "inp.dat" created by batch file (B3PLOT or B3PRST)
      OPEN(9, FILE = 'INP.DAT', STATUS = 'OLD')
      READ(9,131) FNAME
131   FORMAT(A8)
      WRITE(*,111) FNAME
111   FORMAT(
     &///1X,'********** Welcome to Post Processor POSTSIM **********',
     & //8X,'.....Filename = [',A8,'] has been specified......')  
C***************************************************************************

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

C================= CODE FROM POSTHIS2.FOR ===============================
c........ Define default values for KSTART & KSKIP - used in all cases!
          KSTART = 1
          KSKIP  = 1
C========================================================================

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

C================== CODE FROM POSTHIS2.FOR ==============================
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
C======================================================================


c---------------------- specify plot options-----------------------
      IYRS = 0
      write(*,1391)
1391  format(/
     &//5x,'Default x-axis graph scale is days.',
     & /5x,'Do you wish to change to years [N]? ',$)
      read(*,21) ANS
      if(ANS.EQ.'Y' .or. ANS.EQ.'y') THEN
         IYRS = 1
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
      OPEN(IOUNIT, FILE = FNAME//'.BPD', STATUS = 'OLD', 
     &         FORM= 'UNFORMATTED', ACCESS='DIRECT',RECL=NRECL,ERR=9999)

      IREC=1
      READ(IOUNIT,REC=IREC) IHEDIN,NWELLS,(NPW(L),L=1,NWELLS)


C.... Now read rest of file fname.GWN................................... 
      READ(15,*) NWELLS,NODTOT
      DO 96 L=1,NWELLS
      DO 96 M=1,NPW(L)
      READ(15,153) NW,IDWEL(L),WELNAM(L),NPW(L), IN(L,M),JN(L,M),KN(L,M)
C.... Read well no & node loc again as character data......
      BACKSPACE(15)
      READ(15,155) NW, LOCWEL(L), LOCNOD(L,M)
ccc      WRITE(*,155) NW, LOCWEL(L), LOCNOD(L,M)
96    CONTINUE
153   FORMAT(1X,I5,I5, 3X,A8,I5,3X, I2,1X,I2,1X,I2)
155   FORMAT(1X,I5,A5,     19X,          A8)

C.... Open ASCII file to write selected data for input to SIMPLT2.........
      OPEN(16, FILE = 'SIMPLT.ASC', STATUS = 'UNKNOWN')
      CLOSE(16,STATUS='DELETE')
      OPEN(16, FILE = 'SIMPLT.ASC', STATUS = 'NEW')


C====================== Select only field values? ========================
150   WRITE(*,11)
11    FORMAT(
     &/5X,'A - PLOT VALUES BY FIELD, TOTAL WELL, AND WELL-BLOCK (NODE)',
     &/5X,'B - PLOT ONLY TOTAL FIELD VALUES',
     &//5X,'Enter Option: ',$)
      READ(*,21) OPT
      IF((OPT.NE.'A' .AND. OPT.NE.'a') .AND.
     &   (OPT.NE.'B' .AND. OPT.NE.'b')) GOTO 150

      IF(OPT.EQ.'A' .OR. OPT.EQ.'a') GOTO 1600


C==================== Select one field parameter ========================
152   WRITE(*,13)
13    FORMAT(
     & /1X,'        Select one of the following field values',
     & /1X,'        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
     & /1X,'        1 - Gas injection rate------------ (GIrat)',
     & /1X,'        2 - Water injection rate---------- (WIrat)',
     & /1X,'        3 - Cumulative gas injection------ (CGinj)',
     & /1X,'        4 - Cumulative water injection---- (CWinj)',
     & /1X,'        5 - PV average oil pressure------- (AOprs)',
     &//1X,'        Choice: ',$)
         
      READ(*,*) IPARM
      IF(IPARM.EQ.1)  PARAM = 'GIrat-Mcfd'
      IF(IPARM.EQ.2)  PARAM = 'WIrat-stbd'
      IF(IPARM.EQ.3)  PARAM = 'CGinj-MMcf'
      IF(IPARM.EQ.4)  PARAM = 'CWinj-Mstb'
      IF(IPARM.EQ.5)  PARAM = 'AOprs-psia'

C*****************************************************************************
C.... Read data from BPD file; store selected item in array PLOT
C$$$$$$$$$$$$ BRANCH BACK TO HERE FOR PROCESSING MULTIPLE FILES $$$$$$$$$$$$$
C                             (OPTION B)
98    LOOP = LOOP+1    
      IF(LOOP.GT.1) IOUNIT = IOUNIT+1
      DO 100 J=1,9999
      IREC = J+1
      IEOF=0
      CALL BPD(IOUNIT,IREC,IEOF,PIMULT)
      IF(IEOF.EQ.1) GOTO 110
      T(J) = FTT
C*****************************************************************************
C---------- Reverse sign on injections - so HPLOT can handle !
      IF(IPARM.EQ.1) PLOT(J,1) = -FGIR 
      IF(IPARM.EQ.2) PLOT(J,1) = -FWIR
      IF(IPARM.EQ.3) PLOT(J,1) = -FCGI 
      IF(IPARM.EQ.4) PLOT(J,1) = -FCWI 
      IF(IPARM.EQ.5) PLOT(J,1) =  FPAV
100   CONTINUE
110   NSTEPS = J-1 

      IF(LOOP.EQ.1) FNAMTST = FNAME//'.BPD'
      IF(LOOP.EQ.2) FNAMTST = FNAME2//'.BPD'
      IF(LOOP.EQ.3) FNAMTST = FNAME3//'.BPD'
      IF(LOOP.EQ.4) FNAMTST = FNAME4//'.BPD'
      IF(LOOP.EQ.5) FNAMTST = FNAME5//'.BPD'
      WRITE(*,1001) OPT,LOOP,NSTEPS,FNAMTST
1001  FORMAT(/5X,
     &'Option= ',A1,'   LOOP=',I2,'   Total data records=',I4,
     &'   File= ',A12)

C==============================
      NITEMS=1
      PNAME(1) = PARAM//'T Field   '
      GO TO 800
c     End of Option = 'B'
C==============================


C======================================================================
C     BEGIN OPTION = 'A' Logic  - (up to 5 locations by field, well & node)
C======================================================================
1600  CONTINUE

162   WRITE(*,2131) 
2131  FORMAT(
     & /5X,' Do you wish to select:',
     & /5X,'   0 - A single parameter at multiple locations',
     & /5X,'       [e.g. oil rate by field, well or node]',
     &//5X,'   1 - Multiple parameters @ a single location',
     & /5X,'       [e.g. oil rate, gas rate & BHP @ node (i,j,k)]',
     &//5X,'   Choice: ',$)
      READ(*,*) IANS
      IF(IANS.NE.0 .AND. IANS.NE.1) GO TO 162

      IF(IANS.EQ.1) GO TO 2150        


C==== BEGIN OPTION= A-0 LOGIC - Select single parameter @ up to 5 locations 
200   WRITE(*,211)
211   FORMAT(
     & /1X,'       Parameters Available for Plotting',
     & /1X,'      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
     & /1X,'      1 - Gas Rate----------------- (Gprod)',
     & /1X,'      2 - Water Rate--------------- (Wprod)',
     & /1X,'      3 - Oil Rate----------------- (Oprod)',
     & /1X,'      4 - Cumulative Gas----------- (CGprd)',
     & /1X,'      5 - Cumulative Water--------- (CWprd)',
     & /1X,'      6 - Cumulative Oil----------- (COprd)',
     & /1X,'      7 - Producing GOR------------ (PGOR) ',
     & /1X,'      8 - Producing WOR------------ (PWOR) ',
     & /1X,'      9 - Gas Saturation----------- (Gsatn)',
     & /1X,'     10 - Water Saturation--------- (Wsatn)',
     & /1X,'     11 - Oil Saturation----------- (Osatn)',
     & /1X,'     12 - Oil pressure------------- (Pres )',
     & /1X,'     13 - Bottomhole Pressure------ (Pwf)  ')

      
270   WRITE(*,161)
161   FORMAT(/1X,'   Specify parameter number: ',$)
      READ(*,*) IPARM
      IF(IPARM.LT.1 .OR. IPARM.GT.13) GOTO 270
c.... Store code based on selection......................
      DO 2702 I=1,13
      IF(IPARM.EQ.I)  PARAM = PARM(I)
2702  CONTINUE 

300   WRITE(*,213) PARAM
213   FORMAT(
     &/7X,'Select up to 5 locations from the following list',
     &/7X,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
     &/1X,'        1 - Field',2X,A10)
      ITEM = 1
      DO 220 L=1,NWELLS                                     
      ITEM = L+1
      IF(MOD(ITEM,15).EQ.0) THEN
         WRITE(*,221)
221      FORMAT(5X,'Enter for more....',$)
         READ(*,21) ANS2
         IF(ANS2.NE. ' ') GOTO 236
      ENDIF
      WRITE(*,215) ITEM,LOCWEL(L),WELNAM(L),PARAM
220   CONTINUE
215   FORMAT(7X,I3,' - Well:',A5,2X,A8,2X,A10)        

      DO 230 L=1,NWELLS
      DO 230 M=1,NPW(L)
      ITEM = ITEM+1          
      IF(MOD(ITEM,15).EQ.0) THEN
          WRITE(*,221)
          READ(*,21) ANS2
          IF(ANS2.NE. ' ') GOTO 236
      ENDIF
      WRITE(*,217) ITEM,LOCWEL(L), LOCNOD(L,M), PARAM
230   CONTINUE
217   FORMAT(7X,I3,' - Well:',A5,2X,  A8,       2X,A10)
236   CONTINUE

      WRITE(*,231)
231   FORMAT(/5X,'Enter number of locations (1 to 5)', 
     &       /5X,'or 0 to review selection: ',$)
      READ(*,*) NITEMS
      IF(NITEMS.LT.1 .OR. NITEMS.GT.5) GOTO 300

      WRITE(*,233)
233   FORMAT(//5X,'Enter specific locations: ',$)
      READ(*,*) (ISEL(J), J=1,NITEMS)

      WRITE(*,2333) 
2333  FORMAT(/5X,'Enter to continue or R to reselect: ',$)
      READ(*,21) ANS2
      IF(ANS2.EQ.' ') THEN
         GO TO 240
      ELSE
         GO TO 300
      ENDIF

240   CONTINUE
      WRITE(*,219)
219   FORMAT(/5X,
     &'Enter to continue or 0 to select a different parameter: ',$)
      READ(*,21) ANS2
      IF(ANS2.EQ.' ') THEN
         GO TO 400
      ELSE
         GO TO 200
      ENDIF
C===================================
C    End of Option = 'A'  Case = 0
C===================================



C==== BEGIN OPTION= A-1 LOGIC - Plot up to 5 parameters @ a single location  
2150  CONTINUE

3000  WRITE(*,2130) 
2130  FORMAT(
     &/7X,'Select one location from the following list',
     &/7X,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
     &/1X,'        1 - Field')
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
2151  FORMAT(7X,I3,' - Well:',A5,2X,A8)        

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
2170  FORMAT(7X,I3,' - Well:',A5,2X, A8) 
2360  CONTINUE

2308  WRITE(*,2310)
2310  FORMAT(
     &/5X,'Enter location number or 0 to review selection: ',$)
      READ(*,*) LOC
      IF(LOC.EQ.0) GO TO 3000
      IF(LOC.LT.1 .OR. LOC.GT.ILOC) GOTO 2308


C--------------- Selection of location completed ------------------
C--------------- Now select 1 to 5 parameters @ this location
2400  WRITE(*,211)

2600  WRITE(*,2431)
2431   FORMAT(/5X,'Enter number of selected parameters (1 to 5): ',$) 
      READ(*,*) NPARMS
      IF(NPARMS.LT.1 .OR. NPARMS.GT.5) GOTO 2600

      WRITE(*,2433) NPARMS
2433  FORMAT(//5X,
     & 'Enter',I3,' parameter numbers from above list: ',$)
      READ(*,*) (IPAR(J), J=1,NPARMS)


C$$$$$$$$$$$$ BRANCH BACK TO HERE FOR PROCESSING MULTIPLE FILES $$$$$$$$$$$$$
C                             (OPTION = 'A')
C============================================================================

C.... NOTE:  5000 IS UPPER LIMIT ON NUMBER OF TIME-STEPS! 
C.... NOTE:  1800 IS UPPER LIMIT ON NUMBER OF POINTS THAT CAN BE PLOTTED
C....             PER GRAPH         


C.... Read BPD file;  select & store items selected for plotting.....
400   LOOP = LOOP+1
      IF(LOOP.GT.1) IOUNIT = IOUNIT+1

      DO 500 J=1,5000

C============ CODE FROM POSTHIS2.FOR =======================================
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
C===========================================================================

      IEOF=0
      CALL BPD(IOUNIT,IREC,IEOF,PIMULT)
      IF(IEOF.EQ.1) GO TO 600
      T(J) = FTT

      IF(IANS.EQ.1) GO TO 460 

C==== CHOOSE CORRECT DATA FOR CASE 0  BASED ON "IPARM" & "ISEL(J)" =======
      DO 452 I=1,NITEMS

      IF(ISEL(I).EQ.1) THEN
         IF(J.EQ.1) THEN
            LOCTYP = 'T'
            LOCID  = 'Field   '
         ENDIF
         IF(IPARM.EQ.1)  PLOT(J,I) = FGPR 
         IF(IPARM.EQ.2)  PLOT(J,I) = FWPR 
         IF(IPARM.EQ.3)  PLOT(J,I) = FOPR 
         IF(IPARM.EQ.4)  PLOT(J,I) = FCGP
         IF(IPARM.EQ.5)  PLOT(J,I) = FCWP
         IF(IPARM.EQ.6)  PLOT(J,I) = FCOP
         IF(IPARM.EQ.7)  PLOT(J,I) = FGOR 
         IF(IPARM.EQ.8)  PLOT(J,I) = FWOR
         IF(IPARM.EQ.9)  PLOT(J,I) = FSGAV
         IF(IPARM.EQ.10) PLOT(J,I) = FSWAV
         IF(IPARM.EQ.11) PLOT(J,I) = FSOAV
         IF(IPARM.EQ.12) PLOT(J,I) = FPAV
         IF(IPARM.EQ.13) PLOT(J,I) = FPWFAV
         GO TO 450
      ENDIF

      DO 446 L=1,NWELLS
      K = L+1
      IF(ISEL(I).EQ.K) THEN
         IF(J.EQ.1) THEN
            LOCTYP = 'W'
            LOCID  = WELNAM(L)
         ENDIF
         IF(IPARM.EQ.1)  PLOT(J,I) = WQG(L)
         IF(IPARM.EQ.2)  PLOT(J,I) = WQW(L)
         IF(IPARM.EQ.3)  PLOT(J,I) = WQO(L)
         IF(IPARM.EQ.4)  PLOT(J,I) = WCGP(L)
         IF(IPARM.EQ.5)  PLOT(J,I) = WCWP(L)
         IF(IPARM.EQ.6)  PLOT(J,I) = WCOP(L)
         IF(IPARM.EQ.7)  PLOT(J,I) = WGOR(L)
         IF(IPARM.EQ.8)  PLOT(J,I) = WWOR(L)
         IF(IPARM.EQ.9)  PLOT(J,I) = WSGAV(L)
         IF(IPARM.EQ.10) PLOT(J,I) = WSWAV(L)
         IF(IPARM.EQ.11) PLOT(J,I) = WSOAV(L)
         IF(IPARM.EQ.12) PLOT(J,I) = WPOAV(L)
         IF(IPARM.EQ.13) PLOT(J,I) = WPWFAV(L)
         GO TO 450
      ENDIF
446   CONTINUE

      DO 448 L=1,NWELLS
      DO 448 M=1,NPW(L)
      K = K+1
      IF(ISEL(I).EQ.K) THEN
         IF(J.EQ.1) THEN
            LOCTYP = 'N'
            LOCID  = LOCNOD(L,M)
         ENDIF
         IF(IPARM.EQ.1)  PLOT(J,I) = QGN(L,M)
         IF(IPARM.EQ.2)  PLOT(J,I) = QWN(L,M)
         IF(IPARM.EQ.3)  PLOT(J,I) = QON(L,M)
         IF(IPARM.EQ.4)  PLOT(J,I) = CUMGN(L,M) 
         IF(IPARM.EQ.5)  PLOT(J,I) = CUMWN(L,M) 
         IF(IPARM.EQ.6)  PLOT(J,I) = CUMON(L,M) 
         IF(IPARM.EQ.7)  PLOT(J,I) = NGOR(L,M)
         IF(IPARM.EQ.8)  PLOT(J,I) = NWOR(L,M)
         IF(IPARM.EQ.9)  PLOT(J,I) = SGG(L,M)
         IF(IPARM.EQ.10) PLOT(J,I) = SWW(L,M)
         IF(IPARM.EQ.11) PLOT(J,I) = SOO(L,M)
         IF(IPARM.EQ.12) PLOT(J,I) = POO(L,M)
         IF(IPARM.EQ.13) PLOT(J,I) = NPWF(L,M)
         GO TO 450
      ENDIF
448   CONTINUE

450   IF(J.EQ.1) PNAME(I) = PARAM//LOCTYP//' '//LOCID

452   CONTINUE
C--------------------
      GO TO 500
C-------------------

460   CONTINUE

C==== CHOOSE CORRECT DATA FOR CASE 1  BASED ON "NLOC" & "IPAR(J)" =======

      IF(LOC.EQ.1) THEN
         IF(J.EQ.1) THEN
            LOCTYP = 'T'
            LOCID  = 'Field   '
         ENDIF
         DO 502 I=1,NPARMS
         IF(IPAR(I).EQ.1) THEN
            PLOT(J,I) = FGPR
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.2) THEN
             PLOT(J,I) = FWPR
             GOTO 501
         ENDIF
         IF(IPAR(I).EQ.3) THEN
            PLOT(J,I) = FOPR
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.4) THEN
            PLOT(J,I) = FCGP 
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.5) THEN
            PLOT(J,I) = FCWP  
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.6) THEN
            PLOT(J,I) = FCOP 
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.7) THEN
            PLOT(J,I) = FGOR  
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.8) THEN
            PLOT(J,I) = FWOR  
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.9) THEN
            PLOT(J,I) = FSGAV
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.10) THEN
            PLOT(J,I) = FSWAV
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.11) THEN
            PLOT(J,I) = FSOAV
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.12) THEN
            PLOT(J,I) = FPAV
            GOTO 501
         ENDIF
         IF(IPAR(I).EQ.13) THEN
            PLOT(J,I) = FPWFAV
            GOTO 501
         ENDIF
         GO TO 502
501      IF(J.EQ.1) PNAME(I) = PARM(IPAR(I))//LOCTYP//' '//LOCID
502      CONTINUE
      ENDIF

      DO 516 L=1,NWELLS
      K = L+1
      IF(LOC.EQ.K) THEN
         IF(J.EQ.1) THEN
            LOCTYP = 'W'
            LOCID  = WELNAM(L)
         ENDIF
         DO 512 I=1,NPARMS 
         IF(IPAR(I).EQ.1) THEN
            PLOT(J,I) = WQG(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.2) THEN
            PLOT(J,I) = WQW(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.3) THEN
            PLOT(J,I) = WQO(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.4) THEN
            PLOT(J,I) = WCGP(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.5) THEN
            PLOT(J,I) = WCWP(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.6) THEN
            PLOT(J,I) = WCOP(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.7) THEN
            PLOT(J,I) = WGOR(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.8) THEN
            PLOT(J,I) = WWOR(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.9) THEN
            PLOT(J,I) = WSGAV(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.10) THEN
            PLOT(J,I) = WSWAV(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.11) THEN
            PLOT(J,I) = WSOAV(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.12) THEN
            PLOT(J,I) = WPOAV(L)    
            GOTO 511
         ENDIF
         IF(IPAR(I).EQ.13) THEN
            PLOT(J,I) = WPWFAV(L)    
            GOTO 511
         ENDIF
         GOTO 512
511      IF(J.EQ.1) PNAME(I) = PARM(IPAR(I))//LOCTYP//' '//LOCID
512      CONTINUE      
      ENDIF
516   CONTINUE

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
            PLOT(J,I) = QGN(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.2) THEN
            PLOT(J,I) = QWN(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.3) THEN
            PLOT(J,I) = QON(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.4) THEN
            PLOT(J,I) = CUMGN(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.5) THEN
            PLOT(J,I) = CUMWN(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.6) THEN
            PLOT(J,I) = CUMON(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.7) THEN
            PLOT(J,I) = NGOR(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.8) THEN
            PLOT(J,I) = NWOR(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.9) THEN
            PLOT(J,I) = SGG(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.10) THEN
            PLOT(J,I) = SWW(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.11) THEN
            PLOT(J,I) = SOO(L,M)
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.12) THEN
            PLOT(J,I) = POO(L,M) 
            GOTO 537
         ENDIF
         IF(IPAR(I).EQ.13) THEN
            PLOT(J,I) = NPWF(L,M)
            GOTO 537
         ENDIF
         GOTO 538
537      IF(J.EQ.1) PNAME(I) = PARM(IPAR(I))//LOCTYP//' '//LOCID
538      CONTINUE
      ENDIF
548   CONTINUE

500   CONTINUE      
510   CONTINUE
C==== E N D   O F   R E A D   S I M U L A T I O N   D A T A  L O O P ======
C==========================================================================

600   NSTEPS = J-1 

C======================== CODE FROM POSTHIS2.FOR ==========================
c.... NOTE:  NSTEPS is the number of steps which have been selected for
c....        plotting.  If KSTART>1, the steps selected are not sequential.
C==========================================================================


      IF(LOOP.EQ.1) FNAMTST = FNAME//'.BPD'
      IF(LOOP.EQ.2) FNAMTST = FNAME2//'.BPD'
      IF(LOOP.EQ.3) FNAMTST = FNAME3//'.BPD'
      IF(LOOP.EQ.4) FNAMTST = FNAME4//'.BPD'
      IF(LOOP.EQ.5) FNAMTST = FNAME5//'.BPD'
      WRITE(*,1001) OPT,LOOP,NSTEPS,FNAMTST

C==== Write to ASCII file for Program SIMPLT ========================
800   CONTINUE
      IF(LOOP.GT.1) THEN
         IF(LOOP.EQ.2) IHEDIN = IHED2
         IF(LOOP.EQ.3) IHEDIN = IHED3
         IF(LOOP.EQ.4) IHEDIN = IHED4
         IF(LOOP.EQ.5) IHEDIN = IHED5
      ENDIF  
      WRITE(16,897) IHEDIN
897   FORMAT(1X,A75)

C.... Adjust NITEMS for Case A-1 ............................
      IF((OPT.EQ.'A'.OR.OPT.EQ.'a') .AND. NITEMS.EQ.0) NITEMS = NPARMS

      IF(TINC.GT.0.0) GO TO 1210

      WRITE(16,1197) NSTEPS,NITEMS,IANS, ILOG,IYRS
      WRITE(16,1199) (PNAME(I),I+1, I=1,NITEMS)
1197  FORMAT(1X,3I5,2X,2I4/1X,'Time (days)',T30,'(col 1)')
1199  FORMAT(1X,A20,T30,'(col',I2,')')
      DO 1200 J=1,NSTEPS
      WRITE(16,1201) T(J), PLT1(J), PLT2(J), PLT3(J), PLT4(J),PLT5(J)
1200  CONTINUE
1201  FORMAT(1X,6F13.3)
      GO TO 2220


C.... Put data on time interval TINC ................................
1210  CONTINUE
      DTINC = 1.0/TINC
c-------added T(1) here  11/19/94 for plotting Restart File; T(1) >> 0 ! 
c------ removed T(1) ---- 8/25/96 (may have to put back later and do more
c------                            work here - but now all I see is that
c------                            for a normal run, T(1) is a problem!
CC      TOTTIM = (T(NSTEPS) - T(1)) + .00001
      TOTTIM = T(NSTEPS)  + .00001
c----------------------------------------------------------------------
      ITINC  = INT(TOTTIM/TINC)
      DO 1220 I=1,ITINC
c-------added T(1) here 11/19/94 for plotting Restart File; T(1) >> 0 ! 
CCC      TIME(I) = T(1) + FLOAT(I)*TINC
      TIME(I) =  FLOAT(I)*TINC
c----------------------------------------------------------------------
      CALL INTRP3(T,PLT1,NSTEPS,TIME(I),Y1(I)) 
      CALL INTRP3(T,PLT2,NSTEPS,TIME(I),Y2(I)) 
      CALL INTRP3(T,PLT3,NSTEPS,TIME(I),Y3(I)) 
      CALL INTRP3(T,PLT4,NSTEPS,TIME(I),Y4(I)) 
      CALL INTRP3(T,PLT5,NSTEPS,TIME(I),Y5(I)) 
1220  CONTINUE

C.... Determine how many points.............
      LAST  = ITINC
      DTEND = T(NSTEPS) - TIME(ITINC)
      IF(DTEND.GT..001) THEN
         LAST = ITINC+1
      ENDIF


C---- Write data to ASCII file on uniform time basis-----------------------
      WRITE(16,1197) LAST,NITEMS,IANS, ILOG,IYRS
      WRITE(16,1199) (PNAME(I),I+1, I=1,NITEMS)

      DO 1230 I=1,ITINC
      WRITE(16,1201) TIME(I), Y1(I), Y2(I), Y3(I), Y4(I), Y5(I)
1230  CONTINUE

      IF(DTEND.GT..001) THEN
         J = NSTEPS
         WRITE(16,1201) T(J), PLT1(J), PLT2(J), PLT3(J), PLT4(J),PLT5(J)
      ENDIF
C--------------------------------------------------------------------------
2220  CONTINUE

      IF(LOOP.EQ.1) GO TO 2222

      IF(IFILES.GT.1) THEN
         IF(LOOP.LT.IFILES) THEN
             IF(OPT.EQ.'A' .OR. OPT.EQ.'a') GO TO 400
             IF(OPT.EQ.'B' .OR. OPT.EQ.'b') GO TO 98
         ELSE
             GO TO 6666
         ENDIF
      ENDIF


2222  CONTINUE
C.... IF LOOP=1, SEE IF WANT TO PROCESS MORE FILES......................
C.... IF SO, BRANCH BACK TO 400 IF OPT = A ........
C....    OR  BRANCH BACK TO  98 IF OPT = B ........
      WRITE(*,1203) 
1203  FORMAT(/
     &/5X,'Process additional files (with identical grid & well',         
     &    ' locations)?',
     &/1X,'    Y - Yes',
     &/1X,'    N - No',
     &/1X,'    Choice: ',$)
      READ(*,21) MFILES
      IF(MFILES.EQ.' '.OR.(MFILES.EQ.'N'.OR.MFILES.EQ.'n')) GOTO 6666

C-----------------------------------------------------------------------
C.... Option to process 1 to 4 additional files ......
900   CONTINUE
      WRITE(*,1009)
1009  FORMAT(/5X,'How many additional files do you wish to process? ',
     &       /5X,'     Please specify (1, 2, 3 or 4): ',$)
      READ(*,*) NFILES
      IFILES = NFILES + 1

      WRITE(*,901)
901   FORMAT(/)
      DO 910 K=1,NFILES
      KFILE = K+1
      WRITE(*,1115) KFILE
1115  FORMAT(5X,'Enter filename',I2,' = ',$)
c=============== an experiment =========================
cc      READ(*,*) DUMY
      READ(*,131) DUMY
c=======================================================
      IF(KFILE.EQ.2) FNAME2 = DUMY
      IF(KFILE.EQ.3) FNAME3 = DUMY
      IF(KFILE.EQ.4) FNAME4 = DUMY
      IF(KFILE.EQ.5) FNAME5 = DUMY
910   CONTINUE

      WRITE(*,1315) NFILES
1315  FORMAT(
     &/5X,'You have specified',I3,' additional file(s) as follows:')
      IF(NFILES.EQ.1) WRITE(*,1317) FNAME2
      IF(NFILES.EQ.2) WRITE(*,1319) FNAME2,FNAME3
      IF(NFILES.EQ.3) WRITE(*,1321) FNAME2,FNAME3,FNAME4
      IF(NFILES.EQ.4) WRITE(*,1323) FNAME2,FNAME3,FNAME4,FNAME5
1317  FORMAT(10X,'filename2 = ',A8)
1319  FORMAT(10X,'filename2 = ',A8/
     &       10X,'filename3 = ',A8)
1321  FORMAT(10X,'filename2 = ',A8/
     &       10X,'filename3 = ',A8/
     &       10X,'filename4 = ',A8)
1323  FORMAT(10X,'filename2 = ',A8/
     &       10X,'filename3 = ',A8/
     &       10X,'filename4 = ',A8/
     &       10X,'filename5 = ',A8)
      WRITE(*,1325)
1325  FORMAT(/5X,'Enter to continue or R to respecify filenames: ',$)
      READ(*,21) ANS
      IF(ANS.NE.' ') GO TO 900
C-------------------------------------------------------------------------


      OPEN(22, FILE = FNAME2//'.BPD', STATUS = 'OLD', 
     &     FORM= 'UNFORMATTED', ACCESS='DIRECT',RECL=NRECL,ERR=10002)

      IREC2=1
      READ(22,REC=IREC2) IHED2,NWELLS,(NPW(L),L=1,NWELLS)

      IF(IFILES.GE.3) THEN


         OPEN(23, FILE = FNAME3//'.BPD', STATUS = 'OLD', 
     &        FORM= 'UNFORMATTED', ACCESS='DIRECT',RECL=NRECL,ERR=10003)

         IREC3=1
         READ(23,REC=IREC3) IHED3,NWELLS,(NPW(L),L=1,NWELLS)
      ENDIF  

      IF(IFILES.GE.4) THEN

         OPEN(24, FILE = FNAME4//'.BPD', STATUS = 'OLD', 
     &        FORM= 'UNFORMATTED', ACCESS='DIRECT',RECL=NRECL,ERR=10004)

         IREC4=1
         READ(24,REC=IREC4) IHED4,NWELLS,(NPW(L),L=1,NWELLS)
      ENDIF  

      IF(IFILES.EQ.5) THEN

         OPEN(25, FILE = FNAME5//'.BPD', STATUS = 'OLD', 
     &        FORM= 'UNFORMATTED', ACCESS='DIRECT',RECL=NRECL,ERR=10005)

         IREC5=1
         READ(25,REC=IREC5) IHED5,NWELLS,(NPW(L),L=1,NWELLS)
      ENDIF  
C==============================================================================

      IF(OPT.EQ.'A' .OR. OPT.EQ.'a') GO TO 400
      IF(OPT.EQ.'B' .OR. OPT.EQ.'b') GO TO  98


6666  CONTINUE 
      WRITE(*,2221) 
2221  FORMAT(//5X,'ASCII Plot File (SIMPLT.ASC) has been created for',
     &            ' Program SIMPLT2.'/)

      CLOSE(21)
      CLOSE(15)
      CLOSE(16)

      CLOSE(22)
      CLOSE(23)
      CLOSE(24)
      CLOSE(25)
      STOP ' '

8888  CONTINUE
      FNAMTST = FNAME//'.GWN'
      WRITE(*,8887) FNAMTST 
8887  FORMAT(///5X,'File ',A12,'  does not exist!'//)
      STOP ' '

9999  CONTINUE
      FNAMTST = FNAME//'.BPD'
      WRITE(*,8887) FNAMTST 
      STOP ' '

10002 FNAMTST = FNAME2//'.BPD'
      WRITE(*,8887) FNAMTST 
      STOP
10003 FNAMTST = FNAME3//'.BPD'
      WRITE(*,8887) FNAMTST 
      STOP ' '
10004 FNAMTST = FNAME4//'.BPD'
      WRITE(*,8887) FNAMTST 
      STOP ' '
10005 FNAMTST = FNAME5//'.BPD'
      WRITE(*,8887) FNAMTST 
      STOP ' '
      END


C=========================================================================
C     ********************  S I M S U B S . F O R ************************
C                           =====================
C     ******** POST PROCESSOR (SUB-MODULE) FOR BOAST3 SIMULATOR ***********
C                          USED BY MODULE POSTSIM & POSTHIS
C       POSTSIM/SIMSUBS are compiled using MS Powerstation Fortran 
C       (NOTE:  Requires NWX & NODX at least as great as used in
C               simulator:  NWX=max no. of wells; NODX=max nodes/well)
C=========================================================================

      SUBROUTINE BPD (IOUNIT,IREC,IEOF,PIMULT)

      PARAMETER(NWX=60,NODX=10)

      CHARACTER*8 WELNAM
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

      READ(IOUNIT,REC=IREC,END=999) FTT,
c....... Field only menu (7 items)..................
     &  FGIR, FWIR, FCGI, FCWI, FPAV,
c....... Field items for general menu (13 items)........ 
     &        FGPR, FWPR, FOPR,  
     &        FCGP, FCWP, FCOP,  
     &        FGOR, FWOR, 
     &        FSGAV, FSWAV, FSOAV, FPAV, FPWFAV,
c....... Well items for general menu (13items)........ 
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

      SUBROUTINE INTRP3(X,Y,N,XO,YO)
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

