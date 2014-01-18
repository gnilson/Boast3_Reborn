
C============================================================================
C!!! THIS IS "WELLS" and "WRTMAP" from MIS4 - being modified for BOAST3 !!!
C============================================================================
      SUBROUTINE WELLS(NWELLS,WELNAM,KSCRN,NODTOT, IDWEL,NPW,
     &                 LWX,LWY,LWZ,IDIR, IOCODE)
      INCLUDE 'PARAMS.FOR'

      CHARACTER*75 HEADIN

C.... NOTE:  NWX  = max no of wells in PARAMS.FOR .............
C....        NODX = max nodes per well in PARAMS.FOR..........
      CHARACTER*8  WELNAM(NWX)
      DIMENSION  IDWEL(NWX), NPW(NWX)
      DIMENSION  LWX(NWX,NODX), LWY(NWX,NODX), LWZ(NWX,NODX),
     &          IDIR(NWX,NODX)

69    FORMAT(A75)
691   FORMAT(1X,A75/)

      READ(20,69)  HEADIN
      READ(20,69)  HEADIN
ccc      WRITE(*,691) HEADIN
ccc      PAUSE 'Wells:  After 2nd header........'
      READ(20,*)   NWELLS
C===============================================
C.... NOTE:  All writes to "17" now done in main - in time-step loop -
C....        after restart stuff - for N=NINIT
C==============================================  
      
      WRITE(IOCODE,71) NWELLS 
      IF(KSCRN.GE.1) THEN
         WRITE(*,71) NWELLS 
         WRITE(9,71) NWELLS 
      ENDIF
71    FORMAT(///5X,'Number of wells this simulation = ',I5)

      READ(20,69)  HEADIN
      WRITE(IOCODE,731)
      IF(KSCRN.GE.1) THEN
         WRITE(*,731)
         WRITE(9,731)
      ENDIF
731   FORMAT(///1X,
     &'Well   Nodes    Name  ',/1X,
     &'=====  ===== ======== ')

      NODTOT = 0
      DO 411 NW = 1,NWELLS
      READ(20,*) I,NPW(NW),WELNAM(NW)  
      NODTOT = NODTOT + NPW(NW)
      WRITE(IOCODE,73) I,NPW(NW),WELNAM(NW)   
      IDWEL(NW) = I
      IF(KSCRN.GE.1) THEN
         WRITE(*,73) IDWEL(NW),NPW(NW),WELNAM(NW) 
         WRITE(9,73) IDWEL(NW),NPW(NW),WELNAM(NW) 
      ENDIF
411   CONTINUE
73    FORMAT(1X,I4,I7,2X,A8,F7.1,F9.1)

9591  FORMAT(1X,' Well   Node(i,j,k)   DIR')
      IF(KSCRN.GE.1) WRITE(*,9591)

      READ(20,69) HEADIN

      DO 4111 NW=1,NWELLS
      NODS = NPW(NW)
      DO 4111 M=1,NODS 
      READ(20,*)     IDWELX,I,J,K,IDIRX
      IF(KSCRN.GE.1) THEN
         WRITE(*,9611) IDWELX,I,J,K,IDIRX
         WRITE(9,9611) IDWELX,I,J,K,IDIRX
      ENDIF
      LWX(NW,M)  = I
      LWY(NW,M)  = J
      LWZ(NW,M)  = K
      IDIR(NW,M) = IDIRX

C.... Check to ensure that IDWELX = IDWEL(NW) ................
      IF(IDWELX.NE.IDWEL(NW)) THEN
         WRITE(IOCODE,21) NW,M,IDWEL(NW),IDWELX
         WRITE(*,21)      NW,M,IDWEL(NW),IDWELX
         WRITE(9,21)      NW,M,IDWEL(NW),IDWELX
         STOP
      ENDIF
21    FORMAT(
     &//1X,'!!! WELL/NODE DATA ERROR:   NW =',I4,'   M =',I4,
     & /1X,' Well numbers must be in same order as previously read!',
     &//1X,'    IDWEL(NW) =',I4, ' read in Well data',
     & /1X,'    IDWELX    =',I4, ' read in Node data'//)

4111  CONTINUE
9611  FORMAT(1X,I5,2X,3I4,2X,I5)

      RETURN
      END

      SUBROUTINE OUTMAP(MREC,II,JJ,KK,TIME,X,XMULT,XNAME)
      INCLUDE 'PARAMS.FOR'
      CHARACTER*30 XNAME

      REAL*4     X(IMX,JMX,KMX)
      DIMENSION XM(IMX,JMX,KMX)
      INT = 0
      IF(XMULT.GT.1.E-06) INT = 1
      DO 35000 K=1,KK
      DO 35000 J=1,JJ
      DO 35000 I=1,II
      IF(INT.EQ.0) THEN
         XM(I,J,K) = X(I,J,K)
      ELSE
         XM(I,J,K) = X(I,J,K) * XMULT
      ENDIF
35000 CONTINUE
      MREC = MREC+1
      WRITE(18,REC=MREC) TIME,XNAME,(((XM(I,J,K),I=1,II),J=1,JJ),K=1,KK)
      RETURN
      END


      SUBROUTINE WRTMAP(MREC,II,JJ,KK,R4TIME,XMULT,
     & P,SO,SW,SG,RPO,RPW,RPG,CAPOW,CAPGO,PBOT,RSOA,QWAQ)

      INCLUDE 'PARAMS.FOR'
      INCLUDE 'COMMON.INC'

      REAL*4 R4TIME,XMULT 
      CHARACTER*30  PARNAM

      DIMENSION P(IMX,JMX,KMX),
     &    SO(IMX,JMX,KMX),    SW(IMX,JMX,KMX),   SG(IMX,JMX,KMX),
     &   RPO(IMX,JMX,KMX),   RPW(IMX,JMX,KMX),  RPG(IMX,JMX,KMX),
     & CAPOW(IMX,JMX,KMX), CAPGO(IMX,JMX,KMX), PBOT(IMX,JMX,KMX),
     & RSOA(IMX,JMX,KMX),   QWAQ(IMX,JMX,KMX)

      XMULT = 0.0

      IF(KPHIMP.GT.0) THEN
         PARNAM  = 'Porosity, pct                 '
         XMULT = 100.
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,PHI,XMULT,PARNAM)
         XMULT = 0.0
      ENDIF

      IF(IPMAP.GT.0) THEN
         PARNAM  = 'Po, psia                      '
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,P,XMULT,PARNAM)
      ENDIF

      IF(ISOMAP.GT.0) THEN
         PARNAM  = 'So, percent                   '
         XMULT = 100.
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,SO,XMULT,PARNAM)
         XMULT = 0.0
      ENDIF

      IF(ISWMAP.GT.0) THEN
         PARNAM  = 'Sw, percent                   '
         XMULT = 100.
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,SW,XMULT,PARNAM)
         XMULT = 0.0
      ENDIF

      IF(ISGMAP.GT.0) THEN
         PARNAM  = 'Sg, percent                   '
         XMULT = 100.
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,SG,XMULT,PARNAM)
         XMULT = 0.0
      ENDIF

      IF(KROMP.GT.0) THEN
         PARNAM = 'Kro, percent                   '
         XMULT = 100.
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,RPO,XMULT,PARNAM)
         XMULT = 0.0
      ENDIF

      IF(KRWMP.GT.0) THEN
         PARNAM = 'Krw, percent                   '
         XMULT = 100.
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,RPW,XMULT,PARNAM)
         XMULT = 0.0
      ENDIF

      IF(KRGMP.GT.0) THEN
         PARNAM = 'Krg, percent                   '
         XMULT = 100.
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,RPG,XMULT,PARNAM)
         XMULT = 0.0
      ENDIF

      IF(IPCOW.GT.0) THEN
         PARNAM = 'Pcow, psia                     '
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,CAPOW,XMULT,PARNAM)
      ENDIF

      IF(IPCGO.GT.0) THEN
         PARNAM = 'Pcgo, psia                     '
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,CAPGO,XMULT,PARNAM)
      ENDIF
         
      IF(IPBMAP.GT.0) THEN
         PARNAM  = 'Pb, psia                      '
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,PBOT,XMULT,PARNAM)
      ENDIF

      IF(IRSOMP.GT.0) THEN
         XMULT   = 5.615
         PARNAM  = 'Rso, scf/stb                  '
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,RSOA,XMULT,PARNAM)
         XMULT = 0.0
      ENDIF

      IF(IAQMAP.GT.0) THEN
         PARNAM  = 'Aquifer influx, stb/day       '
         CALL OUTMAP(MREC,II,JJ,KK,R4TIME,QWAQ,XMULT,PARNAM)
      ENDIF

      RETURN
      END
