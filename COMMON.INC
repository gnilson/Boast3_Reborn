

C**************** COMMON.INC  from G:\MISC4\ ************************
C:::: 2/04/93 - Added arrays PHI() and TOP() to MISC4 
C:::: 7/02/93 - Added PHI() & TOP() to BOAST3 (modified PORPRM & GRIDSZ) 
C---------------------------------------------------------------------

      REAL NGOR,NWOR,NPWF

      COMMON /REALS/ PHI(IMX,JMX,KMX), TOP(IMX,JMX,KMX)

C---------------------------------------------------------------------
ccc----- NOTE:  In order to use all existing BOAST3 well logic, cannot
ccc-----        use this /SRATE/; must use the existing /SRATE/ in MAIN 
ccc-----        which includes all these arrays, but uses different dimensions.
ccc-----        NWX has been changed from 30 to LP11(=100) and KMX=LP3(=5)
ccc      COMMON /SRATE/ PID(NWX,KMX),PWF(NWX,KMX),PWFC(NWX,KMX),KIP(NWX),
ccc     &       GMO(NWX,KMX),GMW(NWX,KMX),GMG(NWX,KMX),LAYER(NWX),QVO(NWX),
ccc     &       QVW(NWX),QVG(NWX),QVT(NWX),CUMO(NWX,KMX),CUMW(NWX,KMX),
ccc     &       CUMG(NWX,KMX)
C------------------------------------------------------------------------

C................ Added /SWITCH/  02/04/93 ..............................
      COMMON /SWITCH/  KXMP,KYMP,KZMP,
     &       IPMAP,ISOMAP,ISWMAP,ISGMAP,IPBMAP,IAQMAP, 
     &       KROMP,KRWMP, KRGMP, IRSOMP, IPCOW,IPCGO, KPHIMP

C................ Added /RATES/  07/02/93 for WRTBPD ....................
C.... NOTE:  There is some redundancy with arrays in /SRATE/.  However,
C....        this is intentional since the arrays in SRATE assume
C....        vertical wells (from BOASTII) and here we want to allow
C....        for horizontal wells.  
C....        Also, there is no true "well/node logic" in BOASTII; here we
C....        are adding it via arrays NPW(), LWX(), LWY(), LWZ() - see main
C....        and CGDB3.FOR !!
      COMMON /RATES/
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
