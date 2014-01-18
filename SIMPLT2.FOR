
C***********************************************************************
C     PROGRAM:         SIMPLT2.FOR  (from SIMPLT.FOR - 8/25/96)
C     Date initiated:  
C     Function:        Set up data for HPLOT USING FILE (SIMPLT.asc FROM
C                      POSTSIM.FOR
C     Remarks:         Dimensioned for 1800 points using PowerStation
C                      Fortran - 08/25/96 
C***********************************************************************

C===================== NEW STUFF FOR POWERSTATION FORTRAN ===================
      INCLUDE 'FGRAPH.FI'

      CHARACTER*5   LSUB(5)
      CHARACTER*11  XLABL
      CHARACTER*42  YLABL
      CHARACTER*20  LEGND(5)
      CHARACTER*60  GTITLE

C------------------------------------------------------------------------
C.... These arrays store data from POSTSIM output (_____.ASC).......
      CHARACTER*20  PNAME(5)
      INTEGER       PCODE(5)
      CHARACTER*60  TITLE(5)
      DIMENSION     NDATA(5), NPARMS(5), DAYS(1800,5)
      DIMENSION     PLOT(1800,5,5) 
C------------------------------------------------------------------------

c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      CHARACTER*1  PCHAR(5)
      INTEGER*2    IPLOPT(5), WIDPCH,HGTPCH
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


      DIMENSION   NOPSEL(5), NORSEL(5), JMAX(5)
      DIMENSION   X(1800,5)
      DIMENSION   Y(1800,5)

      DATA  IM /1800/

      DATA  PLOT  /45000*0.0/ 
      DATA  DAYS  /9000*0.0/
      DATA  X, Y  /9000*0.0, 9000*0.0/
      DATA  NOPSEL, NORSEL  /5*0, 5*0/

      DATA LSUB /5*' '/ 

c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      DATA  IPLOPT /5*0/,  PCHAR /5*' '/
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

C===> Read SIMPLT.ASC file, store data, and select plotting option(s)
C     ===============================================================
      OPEN(20, FILE = 'SIMPLT.ASC', STATUS = 'OLD')


      NSETS = 0
      DO 112 I=1,5
      READ(20,110,END=120) TITLE(I)
      NSETS = NSETS+1
c..............................................................
      READ(20,*) NDATA(I), NPARMS(I), PCODE(I), ILOG, IYRS
c.... If ILOG = 0, do linear y-scale.
c.... If ILOG = 1, do logarithmic y-scale.
c.... If IYRS = 0, use days for x-scale.
c.... If IYRS = 1, use years for x-scale.
c.... NOTE:  ILOG and IYRS  MUST be read on each set of data; i.e.
c....        user may select any combination of runs to plot!

ccc      WRITE(*,3101) NSETS,TITLE(I), NDATA(I),NPARMS(I), ILOG,IYRS
ccc3101  FORMAT(1X,I3,1X,A60,
ccc     & /5X,'NDATA=',I4,3X,'NPARMS=',I4,3X,'PCODE=',I3,3X,2I4)

      IF(NDATA(I).GT.1800) THEN
         WRITE(*,2103) NSETS
2103     FORMAT(//1X,'!!! FATAL ERROR:  No of data records in set',I4,
     &               ' exceeds 1800')
         STOP ' '
      ENDIF

      READ(20,*) 
      
      READ(20,1101) (PNAME(K),K=1,NPARMS(I))
ccc     WRITE(*,1101) (PNAME(K),K=1,NPARMS(I))

      DO 112 J=1,NDATA(I)
      READ(20,1103) DAYS(J,I), (PLOT(J,K,I),K=1,NPARMS(I))
ccc      WRITE(*,1103) DAYS(J,I), (PLOT(J,K,I),K=1,NPARMS(I))

112   CONTINUE

120   CONTINUE  

110   FORMAT(1X,A60)
1101  FORMAT(1X,A20)
1103  FORMAT(1X,6F13.3)

C--------------- ALL DATA HAS BEEN READ & STORED FOR PLOTTING ----------
c     NSETS       = No of datasets (max of 5)
c     NDATA(I)    = No of data records in set I
c     NPARMS(I)   = No of parameters included in set I (SAME FOR EACH SET)
c     PLOT(J,K,I) = Parameter K @ time J for set I


C.... Display simulation data available for plotting ...................
100   WRITE(*,127) 
      DO 130 I=1,NSETS
      WRITE(*,129) I,TITLE(I)
130   CONTINUE
C.... Note:  Nparms & parameters are exactly the same for each set - 
C....        so just print for first set.
      WRITE(*,131) NPARMS(1), (K,PNAME(K),K=1,NPARMS(1))

127   FORMAT(
     &/5X,'Simulation runs available for plotting are as follows:'/)
129   FORMAT(5X,I3,2X,A60)
131   FORMAT(10X,'Number of parameters =',I3,/,
     &      (13X,'Parameter',I3,' - ',A20))


C.....Specify options...................................................
      IF(NSETS.EQ.1) THEN
         IRUN = 1
         IOPT = 1
1310     WRITE(*,1311)
1311     FORMAT(/10X,'   1 - To select parameter(s)',
     &          /10X,'   2 - Exit to DOS',
     &          /10X,'   Enter option: ',$)
         READ(*,*) IANS1
         IF(IANS1.NE.1 .AND. IANS1.NE.2) GO TO 1310
         IF(IANS1.EQ.1) GO TO 1350
         IF(IANS1.EQ.2) STOP ' '
      ENDIF

      WRITE(*,133)
133   FORMAT(
     & //5X,'*************** SIMULATION PLOT MAIN MENU ***************',
     & /10X,'Choose one of the following options:',
     & /10X,'   1 - Select Run Number and Parameters',
     & /10X,'   2 - Select Parameter and Run Numbers', 
     & /10X,'   3 - Exit to DOS',
     & /10X,'   Enter option: ',$)
      READ(*,*) IOPT 
      IF(IOPT.EQ.3) STOP ' '

1350  IF(IOPT.EQ.1) THEN
C....... Select run no. & parameters to plot......
         if(nsets.gt.1) then
            write(*,135)
135         format(/5x,'Please enter run number: ',$)
            read(*,*) IRUN
         endif

         if(nparms(1).eq.1) then
            noparm = 1
            nopsel(1) = 1
            goto 1398
         endif

         write(*,137)
137      format(/5x,'Enter number of parameters to plot: ',$)
         read(*,*) NOPARM
         IF(NOPARM.LT.NPARMS(1)) THEN
            write(*,139) NOPARM
139         format(5x,'Please enter',I3,' parameter number(s): ',$)
            read(*,*) (NOPSEL(K),K=1,NOPARM)
         ELSE
C.......... NOPARM = NPARMS(I) - same for all I ........
            DO 1370 K=1,NPARMS(1)
1370        NOPSEL(K) = K
         ENDIF        
C....... Store data to plot for option 1.............................
1398     NOPTS = NDATA(IRUN)
         DO 1400 J=1,NOPTS
         DO 1400 K=1,NOPARM
         KPARM  = NOPSEL(K)
         X(J,K) = DAYS(J,IRUN)
c----------------------------------------------
c....... Note:  IYRS is passed to HPLOT, which does time
c.......        conversion, if necessary.
c----------------------------------------------
         Y(J,K) = PLOT(J,KPARM,IRUN)
ccc         WRITE(*,13981) J,K,KPARM,IRUN,X(J,K),Y(J,K)
13981    FORMAT('J,K,KPARM,IRUN,X(J,K),Y(J,K):',4I5,2F10.3)
1400     CONTINUE
ccc         PAUSE 'SIMPLT; after 1400-loop'

C....... Define plot title & legends....................................
         GTITLE = TITLE(IRUN) 
1401     FORMAT(/1X,A60)
         DO 1410 K=1,NOPARM
         KPARM = NOPSEL(K)
         LEGND(K) = PNAME(KPARM)
c....    May need LSUB() below............
         LSUB(K)  = PNAME(KPARM)(1:5)
1410     CONTINUE

C....... Define x-axis and y-axis labels................................
         IF(IYRS.EQ.0) XLABL = 'Time, days '
         IF(IYRS.EQ.1) XLABL = 'Time, years '
ccc         XLABL = 'Time, days'
c....... Note that all pcode's same in input file....
c....... PCODE = IANS from GWPOST (POSTSIM);
c.......         PCODE=0 ===> plot single parameter @ mult loc's
c.......         PCODE=1 ===> plot multiple parameters @ single loc
         IF(PCODE(1).EQ.0) THEN
            INDX=1 
            CALL YLABEL (PNAME, INDX, YLABL)
         ELSE
            IF(NOPARM.EQ.1) THEN
               INDX=NOPSEL(1)
               CALL YLABEL (PNAME, INDX, YLABL)
            ELSEIF(NOPARM.EQ.2) THEN
               YLABL = '             '//LSUB(1)//' or '//LSUB(2)//
     &                 '             '  
            ELSEIF(NOPARM.EQ.3) THEN           
               YLABL = '        '//LSUB(1)//' or '//LSUB(2)//' or '//
     &                 LSUB(3)//'         '
            ELSEIF(NOPARM.EQ.4) THEN           
               YLABL = '    '//LSUB(1)//' or '//LSUB(2)//' or '//
     &                 LSUB(3)//' or '//LSUB(4)//'    '
            ELSE
               YLABL = LSUB(1)//' or '//LSUB(2)//' or '//LSUB(3)//
     &                 ' or '//LSUB(4)//' or '//LSUB(5)//' '
            ENDIF
         ENDIF
      ENDIF
         

      IF(IOPT.EQ.2) THEN
C....... Select parameter & run numbers to plot......
         write(*,235)
235      format(/5x,'Enter parameter number: ',$)
         read(*,*) NPAR
         IF(NSETS.EQ.1) THEN
            NORSEL(1) = 1
            NORUNS = 1
            GOTO 2498
         ELSEIF(NSETS.GT.1) THEN
            write(*,237)
237         format(/5x,'Enter number of runs: ',$)
            read(*,*) NORUNS
            if(NORUNS.EQ.NSETS) THEN
               do 2372 k=1,nsets
2372           norsel(k) = k
               goto 2498
            endif 
            write(*,239) NORUNS
239         format(5x,'Enter',I3,' run numbers: ',$)
            read(*,*) (NORSEL(L),L=1,NORUNS)
         ENDIF

C....... Store data to plot for option 2.............................
2498     DO 2500 I=1,NSETS
         DO 2400 L=1,NORUNS
         IF(NORSEL(L).NE.I) GO TO 2400
         NS    = NORSEL(L)
         NOPTS = NDATA(NS)
C....... Retrieve data for parameter NPAR for run NORSEL(I)..........
         DO 2300 J=1,NOPTS
         X(J,L) = DAYS(J,NS)
c----------------------------------------------
c....... Note:  IYRS is passed to HPLOT, which does time
c.......        conversion, if necessary.
c----------------------------------------------
2300     Y(J,L) = PLOT(J,NPAR,NS)
2400     CONTINUE   
2500     CONTINUE

C....... Define x-axis and y-axis labels................................
c---------------------------------------------
         IF(IYRS.EQ.0) XLABL = 'Time, days '
         IF(IYRS.EQ.1) XLABL = 'Time, years '
c---------------------------------------------
         INDX = NPAR
         CALL YLABEL (PNAME, INDX, YLABL)

C....... Define plot title & legends......................................
c....... This case is primarily for sensitivity runs; hence will construct
c....... graph title & legends as follows (REQUIRES SPECIAL CARE IN SETTING
c....... UP SIMULATION TITLES-BEFORE RUNS ARE MADE!)
         WRITE(*,2501)
2501     FORMAT(
     &  //5X,'You may wish to edit the SIMPLT.ASC file and modify the',
     &   /5X,'simulation headers (first line of each run) as follows:',
     &  //5X,'     Graph title will be the last part of the first',
     &   /5X,'     simulation header (beginning in col 22).',
     &  //5X,'     The legend for each run will be the first 20',
     &   /5X,'     characters of the simulation header (columns 2',
     &   /5X,'     to 21), split up on two lines; i.e. the first',
     &   /5X,'     line of the legend will be columns 2 to 11 and',
     &   /5X,'     the second line will be columns 12 to 21.',
     &  //5X,'After editing SIMPLT.ASC, type SIMPLT to generate plots.',
     &  //)

         PAUSE 'Enter to continue or ctl-c to exit'

         GTITLE = '          '//TITLE(1)(21:60)//'          '
         DO 2410 K=1,NORUNS
         NS = NORSEL(K)
         LEGND(K) = TITLE(NS)(1:20)
2410     CONTINUE

      ENDIF


C.... Define limits on subscripts for X & Y plot arrays.................
C.... Note:  Will use notation X(J,K) & Y(J,K) from here on, where
C....        subscript J represents time and subscript K represents
C....        either a parameter (IOPT=1) or a run number (IOPT=2). 
      IF(IOPT.EQ.1) THEN
         KMAX = NOPARM
         DO 2510 K=1,KMAX
2510     JMAX(K) = NDATA(IRUN)
      ENDIF

      IF(IOPT.EQ.2) THEN
         KMAX = NORUNS
         DO 2520 K=1,KMAX
2520     JMAX(K) = NDATA(NORSEL(K))
      ENDIF

C^^^^^^^^^^^^^^^^^^^^^^^^^ TEMPORARY CODE ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ccC.... Verify data retrieval correct per user selections..............
cc      OPEN(55, FILE='TMP.DAT',STATUS='UNKNOWN')
cc      CLOSE(55,STATUS='DELETE')
cc      OPEN(55, FILE = 'TMP.DAT', STATUS = 'NEW')
cc
cc      WRITE(55,25201) GTITLE
cc25201 FORMAT(//1X,A60)
cc
cc      WRITE(55,24999) XLABL
cc24999 FORMAT(1X,A10)
cc
cc      WRITE(55,24997) YLABL
cc24997 FORMAT(1X,A42)
cc
cc      DO 25202 K=1,KMAX
cc      WRITE(55,25203) LEGND(K)
cc25202 CONTINUE
cc25203 FORMAT(1X,A19)
cc
cc      WRITE(55,2521) KMAX, (JMAX(K), K=1,KMAX)
cc2521  FORMAT(/1X,'KMAX=',I3,3X,'JMAXs=',5I3)
cc
cc      DO 2530 K=1,KMAX
cc      WRITE(55,2533) K
cc      DO 2530 J=1,JMAX(K)
cc      WRITE(55,2531)  J,K,  X(J,K), Y(J,K)
cc2530  CONTINUE
cc
cc2533  FORMAT(/1X,'Begin (X,Y) set for K =',I3)
cc2531  FORMAT(1X,'J,K, X(J,K), Y(J,K):',2I5,2F12.3)         
cc
ccC^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c.... Margins for determining world coordinates & graph translation for
c.... centering (will use defaults; see HPLOT for details)
      XMARF  = 0.0
      YMARF  = 0.0
      XOFSET = 0.0 
      YOFSET = 0.0

1050  CONTINUE


C######## SWITCH "KLOG" IN SLPLOT32 DETERMINES LINEAR OR LOG SCALE ON Y-AXIS
      KLOG = ILOG
      CALL HPLOT (IM,KMAX,JMAX,IYRS,X,Y,GTITLE, XLABL, YLABL, LEGND,
     &             XMARF,YMARF, XOFSET,YOFSET,
     &             PCHAR,IPLOPT,KLOG)

      GO TO 100
      STOP ' '
      END


      SUBROUTINE YLABEL (PNAME, INDX,YLABL)
      CHARACTER*20 PNAME(5)
      CHARACTER*42 YLABL 

      IF(PNAME(INDX)(1:5).EQ.'Gprd ') 
     &        YLABL = '      Gas Production Rate, Mscf/day       '
      IF(PNAME(INDX)(1:5).EQ.'Wprd ') 
     &        YLABL = '     Water Production Rate, stb/day       '
      IF(PNAME(INDX)(1:5).EQ.'Oprd ') 
     &        YLABL = '       Oil Production Rate, stb/day       '
      IF(PNAME(INDX)(1:5).EQ.'CGprd') 
     &        YLABL = '     Cumulative Gas Production, MMscf     '
      IF(PNAME(INDX)(1:5).EQ.'CWprd') 
     &        YLABL = '    Cumulative Water Production, Mstb     '
      IF(PNAME(INDX)(1:5).EQ.'COprd') 
     &        YLABL = '    Cumulative  Oil  Production, Mstb     '
      IF(PNAME(INDX)(1:5).EQ.'GORmc') 
     &        YLABL = '     Producing Gas-Oil Ratio, Mscf/stb    '
      IF(PNAME(INDX)(1:5).EQ.'WORst') 
     &        YLABL = '     Producing Water-Oil Ratio, stb/stb   '
      IF(PNAME(INDX)(1:5).EQ.'Gsatn') 
     &        YLABL = '         Gas Saturation, percent          '
      IF(PNAME(INDX)(1:5).EQ.'Wsatn') 
     &        YLABL =   '      Water Saturation, percent         '
      IF(PNAME(INDX)(1:5).EQ.'Osatn') 
     &        YLABL =   '       Oil Saturation, percent          '
      IF(PNAME(INDX)(1:5).EQ.'Pres ') 
     &        YLABL = '           Oil Pressure, psia             '
      IF(PNAME(INDX)(1:5).EQ.'Pwf  ') 
     &        YLABL = '          Bottomhole pressure, psia       '


      IF(PNAME(INDX)(1:5).EQ.'GIrat') 
     &        YLABL = '    Field Gas Injection Rate, Mscf/day    '
      IF(PNAME(INDX)(1:5).EQ.'WIrat') 
     &        YLABL = '   Field Water Injection Rate, stb/day    '
      IF(PNAME(INDX)(1:5).EQ.'CGinj') 
     &        YLABL = '     Cum Field Gas Injection, MMscf       '
      IF(PNAME(INDX)(1:5).EQ.'CWinj') 
     &        YLABL = '     Cum Field Water Injection, Mstb      '
      IF(PNAME(INDX)(1:5).EQ.'AOprs') 
     &        YLABL = '       Field Avg Oil Pressure, psia       '
      RETURN
      END


cc=== WILL PUT NEW "HPLOT.FOR & SUBS" FOR PowerStation Fortran here 

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