
C--------- Adapting WRTBPD from MISC4 for BOAST3 - 07/02/93 -----------

      SUBROUTINE WRTBPD(IREC,NWELLS,NODTOT,WELNAM, FT, D5615, DELT,
     &     GIR,   WIR, CGI, CWI, PAVG, 
     &     GPR,   WPR, OPR, 
     &     CGP,   CWP, COP,  
     &     GOR,   WOR, 
     &     SGAV, SWAV, SOAV, 
     & NPW,LWX,LWY,LWZ,
     & QO,QW,QG,SO,SW,SG,P,PWF,PWFC)

c.... NOTE:  New well and node production arrays added to COMMON.INC
c....        all new cum arrays are initialized in main.

      INCLUDE 'PARAMS.FOR'
      INCLUDE 'COMMON.INC'

      CHARACTER*8  WELNAM(NWX)
      DIMENSION NPW(NWX),
     &          LWX(NWX,NODX), LWY(NWX,NODX), LWZ(NWX,NODX)

      DIMENSION QO(IMX,JMX,KMX),QW(IMX,JMX,KMX),QG(IMX,JMX,KMX),
     &          SO(IMX,JMX,KMX),SW(IMX,JMX,KMX),SG(IMX,JMX,KMX),
     &          P(IMX,JMX,KMX), PWF(NWX,NODX),PWFC(NWX,NODX)


c.... We will write a binary file ___.BPD to include all information
c.... for B3PLOT.  This file file consists of one initial record (written
c.... in main) including HEADER followed by exactly one record per time-step
c.... with the following (real*4) data:

c.. 13 "field production" quantities for plotting ................
c              FGPR,   FWPR,  FOPR,  
c              FCGP,   FCWP,  FCOP, 
c              FGOR,   FWOR,  
c              FSGAV, FSWAV, FSOAV, FPAV, FPWFAV

c.. 13 "well production" quantities for plotting - see COMMON.INC /RATES/
c             WQG(NWX),    WQW(NWX),   WQO(NWX),      
c            WCGP(NWX),   WCWP(NWX),  WCOP(NWX),        
c            WGOR(NWX),   WWOR(NWX),  
c           WSGAV(NWX),  WSWAV(NWX), WSOAV(NWX),    
c                                    WPOAV(NWX),   WPWFAV(NWX)
           
c.. 13 "node production" quantities for plotting - see COMMON.INC /RATES/
c       QGN(NWX,NODX),  QWN(NWX,NODX),  QON(NWX,NODX),  
c     CUMGN(NWX,NODX),CUMWN(NWX,NODX),CUMON(NWX,NODX), 
c      NGOR(NWX,NODX), NWOR(NWX,NODX), 
c       SGG(NWX,NODX),  SWW(NWX,NODX),  SOO(NWX,NODX),  
c                                       POO(NWX,NODX), NPWF(NWX,NODX)

c.... Field data for plotting..........................

c---- Items for menu 2 - DO NOT reverse sign on injection values ----------
c----                    for BOAST3 !!
      FGIR  =  GIR
      FWIR  =  WIR
      FCGI  =  CGI
      FCWI  =  CWI
      FPAV  = PAVG
c--------------------------------------------
      FGPR = GPR
      FWPR = WPR
      FOPR = OPR 
      FCGP = CGP 
      FCWP = CWP 
      FCOP = COP 
      FGOR = GOR
      FWOR = WOR
      FSGAV= SGAV   
      FSWAV= SWAV   
      FSOAV= SOAV 

      FPWFAV = 0.0

      DO 100 NW=1,NWELLS

      WQG(NW)  = 0.0 
      WQW(NW)  = 0.0  
      WQO(NW)  = 0.0  

      WSGAV(NW)  = 0.0
      WSWAV(NW)  = 0.0
      WSOAV(NW)  = 0.0

      WPOAV(NW)  = 0.0
      WPWFAV(NW) = 0.0

      DO 90 M=1,NPW(NW)
C.... Retrieve (i,j,k) location ...................
      I = LWX(NW,M)
      J = LWY(NW,M)
      K = LWZ(NW,M)
     
      QGN(NW,M)   = QG(I,J,K)  * .001 
      QWN(NW,M)   = QW(I,J,K)  * D5615
      QON(NW,M)   = QO(I,J,K)  * D5615 
      CUMGN(NW,M) = CUMGN(NW,M) + QGN(NW,M)*DELT 
      CUMWN(NW,M) = CUMWN(NW,M) + QWN(NW,M)*DELT
      CUMON(NW,M) = CUMON(NW,M) + QON(NW,M)*DELT

      WQG(NW) = WQG(NW) + QGN(NW,M)
      WQW(NW) = WQW(NW) + QWN(NW,M)
      WQO(NW) = WQO(NW) + QON(NW,M)

      IF(QON(NW,M).NE.0.0) THEN
         DIV = 1./QON(NW,M)
         NGOR(NW,M) = QGN(NW,M) * DIV  
         NWOR(NW,M) = QWN(NW,M) * DIV 
      ELSE
         NGOR(NW,M) = 0.0
         NWOR(NW,M) = 0.0
      ENDIF

      SGG(NW,M)  = SG(I,J,K)
      SWW(NW,M)  = SW(I,J,K)
      SOO(NW,M)  = SO(I,J,K)  

      POO(NW,M)  = P(I,J,K) 

C!!!!!!!!!!!! CAUTION:  This may not work for horizontal wells!!!
C!!!!!!!!!!!! The difference in the "NVQN well logic" and the new "(NW,M)"
C!!!!!!!!!!!! well logic requires more work for hozizontal wells !!!!!
      NPWF(NW,M) = PWF(NW,K) 
c-----------------------------------------------------------------------
C:::: NOTE:  In BOAST3 Recurrent data, PWF must be left 0.0 for rate
C::::        specified nodes in order for NPWF() to be the "calculated"
C::::        bottomhole pressure
      IF(ABS(NPWF(NW,M)).LE.0.001) NPWF(NW,M) = PWFC(NW,K)
c------------------------------------------------------------------------

C.... NOTE:  Well avg pressures and saturations are not very useful;
C....        they are included only for consistency of having
C....        FIELD & WELL & NODE values for each parameter.
C....        Well avg sat's are simple arithmetic avg saturations at all
C....        well blocks; they are not weighted by anything!
C....        Also, WPWFAV is average of all PWF's for well - not very useful!
      WSGAV(NW)  = WSGAV(NW)  +  SGG(NW,M)
      WSWAV(NW)  = WSWAV(NW)  +  SWW(NW,M)
      WSOAV(NW)  = WSOAV(NW)  +  SOO(NW,M)
      WPOAV(NW)  = WPOAV(NW)  +  POO(NW,M)
      WPWFAV(NW) = WPWFAV(NW) + NPWF(NW,M)

      FPWFAV = FPWFAV + NPWF(NW,M)

 90   CONTINUE

C.... Well cumulatives...............................
      WCGP(NW) = WCGP(NW) + WQG(NW)*DELT
      WCWP(NW) = WCWP(NW) + WQW(NW)*DELT
      WCOP(NW) = WCOP(NW) + WQO(NW)*DELT

C.... Well ratios....................................
      IF(WQO(NW).NE.0.0) THEN
         DIV = 1./WQO(NW)
         WGOR(NW) = WQG(NW) * DIV
         WWOR(NW) = WQW(NW) * DIV
      ELSE
         WGOR(NW) = 0.0
         WWOR(NW) = 0.0
      ENDIF

C.... Well averages..................................
      DIVNPW     = 1/FLOAT(NPW(NW))
      WSGAV(NW)  = WSGAV(NW) * DIVNPW 
      WSWAV(NW)  = WSWAV(NW) * DIVNPW 
      WSOAV(NW)  = WSOAV(NW) * DIVNPW 
      WPOAV(NW)  = WPOAV(NW) * DIVNPW
      WPWFAV(NW) = WPWFAV(NW)* DIVNPW 

100   CONTINUE

      FPWFAV = FPWFAV / FLOAT(NODTOT)
CCC!!!!!!!!!!!!!!!!!!!!
CC      WRITE(8,1111) PWF(1,1),PWFC(1,1), NPWF(1,1),FPWFAV,WPWFAV(1)
CC1111  FORMAT(1X,'WRTBPD:PWF(1,1),PWFC(1,1), NPWF(1,1),FPWFAV,WPWFAV(1)',
CC     &      /1X,5G14.6/)
CC      WRITE(8,1113) PWF(2,1),PWFC(2,1), NPWF(2,1),FPWFAV,WPWFAV(2)
CC1113  FORMAT(1X,'WRTBPD:PWF(2,1),PWFC(2,1), NPWF(2,1),FPWFAV,WPWFAV(2)',
CC     &      /1X,5G14.6/)
CCC!!!!!!!!!!!!!!!!!!!!

      WRITE(14,REC=IREC) FT,
     &  FGIR, FWIR, FCGI, FCWI, FPAV,
     &        FGPR, FWPR, FOPR,  
     &        FCGP, FCWP, FCOP,  
     &        FGOR, FWOR, 
     &        FSGAV, FSWAV, FSOAV, FPAV, FPWFAV,

     &(WELNAM(L),
     &   WQG(L),   WQW(L),   WQO(L),    
     &  WCGP(L),  WCWP(L),  WCOP(L),   
     &  WGOR(L),  WWOR(L),  
     & WSGAV(L), WSWAV(L), WSOAV(L),   
     &           WPOAV(L), WPWFAV(L), L=1,NWELLS),

     &( (QGN(L,M),  QWN(L,M),  QON(L,M),  
     &  CUMGN(L,M), CUMWN(L,M), CUMON(L,M), 
     &   NGOR(L,M),  NWOR(L,M),  
     &    SGG(L,M),   SWW(L,M),   SOO(L,M),   
     &                POO(L,M),  NPWF(L,M),  M=1,NPW(L)), L=1,NWELLS )

       RETURN
       END
