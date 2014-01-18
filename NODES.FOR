      SUBROUTINE NODES(NVQN)

      INCLUDE 'PARAMS.FOR'

      CHARACTER*5 WELNM,VAR1

      COMMON /SARRAY/ PN(LP1,LP2,LP3),IOCODE,IDMAX,
     & SON(LP1,LP2,LP3),SWN(LP1,LP2,LP3),SGN(LP1,LP2,LP3),
     & A1(LP1,LP2,LP3),A2(LP1,LP2,LP3),A3(LP1,LP2,LP3),
     & SUM(LP1,LP2,LP3),GAM(LP1,LP2,LP3),QS(LP1,LP2,LP3)

      COMMON /SPRTPS/ P(LP1,LP2,LP3),SO(LP1,LP2,LP3),SW(LP1,LP2,LP3),
     & SG(LP1,LP2,LP3)

      COMMON /SRATE/ PID(LP11,LP3),PWF(LP11,LP3),PWFC(LP11,LP3),
     & KIP(LP11),LAYER(LP11),QVO(LP11),CUMG(LP11,LP3),
     & GMO(LP11,LP3),GMW(LP11,LP3),GMG(LP11,LP3),
     & QVW(LP11),QVG(LP11),QVT(LP11),CUMO(LP11,LP3),CUMW(LP11,LP3),
     & IDWELL(LP11),ALIT(LP11),BLIT(LP11)

      COMMON /VECTOR/ DX(LP1,LP2,LP3),DY(LP1,LP2,LP3),DZ(LP1,LP2,LP3),
     & DZNET(LP1,LP2,LP3),IQN1(LP11),IQN2(LP11),IQN3(LP11),IHEDIN(80)

      COMMON /CHAR/ WELNM(LP11)

      DIMENSION NCOUNT(LP11)

      DATA NCOUNT/LP11*0/


C**** Establish rate-specified & pressure-specified wells
      READ(20,69)
C.... NWELLN = No. of new wells;  NWELL0 = No. of old wells ............
      READ(20,*) NWELLN,NWELLO
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(*,1113) NWELLN,NWELLO
C      WRITE(9,1113) NWELLN,NWELLO
C1113  FORMAT(/1X,'NODES:  NWELN=',I4,3X,'NWELLO=',I4//)
C      PAUSE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      IF(NWELLN.EQ.0.AND.NWELLO.EQ.0) RETURN

      NCHANG=NWELLN+NWELLO


      WRITE(IOCODE,67)
      WRITE(IOCODE,68)

      IF(NWELLN.EQ.0) GO TO 2200

C.... Read header for new wells ........................................
      READ(20,69)

      DO 2000 J=1,NWELLN

C**   Input rate signs by convention:
C**   Injectors are negative; producers are positive.

      READ(20,3) VAR1,I1,I2,I3,I4,I5
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(*,311) VAR1,I1,I2,I3,I4,I5
C      WRITE(9,311) VAR1,I1,I2,I3,I4,I5
C311   FORMAT(/1X,'VAR1,I1,I2,I3,I4,I5:',A5,5I3/)
C      PAUSE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
3     FORMAT(A5,5I3)
      IF(IDMAX.LT.I1) IDMAX=I1
      WELNM(I1)=VAR1
      IDWELL(I1)=I1
      IQN1(I1)=I2
      IQN2(I1)=I3
      IQN3(I1)=I4
      LAYER(I1)=I5
      IQ3=IQN3(I1)
      LAY=IQ3+(LAYER(I1)-1)
      READ(20,*) (PID(I1,K),K=IQ3,LAY)
      READ(20,*) (PWF(I1,K),K=IQ3,LAY)
      READ(20,6) VAR1,IDWELL(I1),KIP(I1),QVO(I1),QVW(I1),QVG(I1),QVT(I1)
6     FORMAT(A5,2I3,4F10.0)

      IF(KIP(I1).EQ.-4) THEN
         READ(20,8) VAR1,IDWELL(I1),ALIT(I1),BLIT(I1)
      ENDIF
8     FORMAT(A5,I3,2E12.5)

      NCOUNT(J)=IDWELL(I1)

      DO 1900 K=IQ3,LAY
      QWV = QVW(I1)
      IF(KIP(I1).EQ.-1) QWV = 0.0

      WRITE(IOCODE,70) 
     & WELNM(I1),IDWELL(I1), IQN1(I1),IQN2(I1),K,
     & QVO(I1),QWV,QVG(I1),QVT(I1), PWF(I1,K),PID(I1,K),
     & ALIT(I1),BLIT(I1)
70    FORMAT(2X,A5,I5,1X, 3I3, 3X, F11.2,4F13.2, F10.4,2E10.3)

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(*,701) 
C     & WELNM(I1),IDWELL(I1), IQN1(I1),IQN2(I1),K,
C     & QVO(I1),QWV,QVG(I1),QVT(I1), PWF(I1,K),PID(I1,K),
C     & ALIT(I1),BLIT(I1)

C      WRITE(9,701) 
C     & WELNM(I1),IDWELL(I1), IQN1(I1),IQN2(I1),K,
C     & QVO(I1),QWV,QVG(I1),QVT(I1), PWF(I1,K),PID(I1,K),
C     & ALIT(I1),BLIT(I1)

C701   FORMAT(2X,A5,I5,1X, 3I3, 3X, F11.2,4F13.2, F10.4,2E10.3//)

C       PAUSE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

1900  CONTINUE

2000  CONTINUE


      NVQN=IDMAX
 2200 CONTINUE

      IF(NWELLO.EQ.0) GO TO 2600

      READ(20,69)

      DO 2500 NC=1,NWELLO
      READ(20,6) VAR1,I1,I2,F1,F2,F3,F4
      IDWELL(I1)=I1
      KIP(I1)=I2
      QVO(I1)=F1
      QVW(I1)=F2
      QVG(I1)=F3
      QVT(I1)=F4
      IQ3=IQN3(I1)
      LAY=IQ3+(LAYER(I1)-1)
      NCOUNT(NWELLN+NC)=IDWELL(I1)

      DO 2400 K=IQ3,LAY
      WRITE(IOCODE,70) WELNM(I1),IDWELL(I1),IQN1(I1),IQN2(I1),K,
     & QVO(I1),QVW(I1),QVG(I1),
     & QVT(I1),PWF(I1,K),PID(I1,K),
     & ALIT(I1),BLIT(I1)
 2400 CONTINUE
 2500 CONTINUE

 2600 CONTINUE

      WRITE(IOCODE,33)

      DO 3000 I=1,NCHANG
      J=NCOUNT(I)
      IQ3=IQN3(J)
      LAY=IQ3+(LAYER(J)-1)
      DO 3000 K=IQ3,LAY
      IF(KIP(J).EQ.1.AND.QVO(J).GT.0.0)
     & WRITE(IOCODE,2995) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.1.AND.QVT(J).GT.0.0)
     & WRITE(IOCODE,2995) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.1.AND.QVW(J).GT.0.0)
     & WRITE(IOCODE,2996) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.1.AND.QVG(J).GT.0.0)
     & WRITE(IOCODE,2997) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.2)   WRITE(IOCODE,2998) IQN1(J),IQN2(J),K,IDWELL(J)

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
      IF(KIP(J).EQ.3) THEN
         WRITE(IOCODE,2999) IQN1(J),IQN2(J),K,IDWELL(J)
C         WRITE(*,29991) IQN1(J),IQN2(J),K,IDWELL(J)
C         WRITE(9,29991) IQN1(J),IQN2(J),K,IDWELL(J)
C29991    FORMAT(/1X,'IQN1(J),IQN2(J),K,IDWELL(J):',4I5//)
      ENDIF
      IF(KIP(J).EQ.-1)  THEN
         WRITE(IOCODE,3005) IQN1(J),IQN2(J),K,IDWELL(J)
C         WRITE(*,29991) IQN1(J),IQN2(J),K,IDWELL(J)
C         WRITE(9,29991) IQN1(J),IQN2(J),K,IDWELL(J)
       ENDIF
C      PAUSE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

      IF(KIP(J).EQ.-2)  WRITE(IOCODE,3006) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.-3)  WRITE(IOCODE,3007) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.-4)  WRITE(IOCODE,3008) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.-11) WRITE(IOCODE,3015) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.-12) WRITE(IOCODE,3016) IQN1(J),IQN2(J),K,IDWELL(J)
      IF(KIP(J).EQ.-13) WRITE(IOCODE,3017) IQN1(J),IQN2(J),K,IDWELL(J)
3000  CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE(*,11111)
C      WRITE(9,11111)
C      PAUSE 'END OF 3000-LOOP IN NODES' 
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

69    FORMAT(40A2)
691   FORMAT(/1X,40A2/)

67    FORMAT(///T35,21('*'),/,T35,
     & '*   WELL   UPDATE   *',/,T35,21('*'),//,
     & T20,'RESERVOIR CONTAINS THE FOLLOWING RATE NODES:'/)

68    FORMAT(T3,' WELL  NO.     NODE   ',3X,'  OIL(STBD)',
     & 3X,'WATER(RBD)',3X,'GAS(MCFD)',3X,'TOTAL(RBD)',3X,'BHFP(PSIA)',
     & 6X,'PID',6X,'ALIT',6X,'BLIT'/)


33    FORMAT(/)

2995  FORMAT(1X,'Block ',3I3,
     & ' :  Oil rate specified producing well #',I5)
 2996 FORMAT(1X,'Block ',3I3,
     & ' :  Water rate specified producing well #',I5)
 2997 FORMAT(1X,'Block ',3I3,
     & ' :  Gas rate specified producing well #',I5)
 2998 FORMAT(1X,'Block ',3I3,
     & ' :  Rate specified water injection well #',I5)
 2999 FORMAT(1X,'Block ',3I3,
     & ' :  Rate specified gas injection well #',I5)
 3005 FORMAT(1X,'Block ',3I3,
     & ' :  Explicit pressure specified producing well #',I5)

 3006 FORMAT(1X,'Block ',3I3,
     &' :  Explicit pressure specified water injection well #',I5)
 3007 FORMAT(1X,'Block ',3I3,
     &' :  Explicit pressure specified gas injection well #',I5)
 3008 FORMAT(1X,'Block ',3I3,
     &' :  Explicit pressure specified gas production well #',I5)
 3015 FORMAT(1X,'Block ',3I3,
     &' :  Implicit pressure specified producing well #',I5)
 3016 FORMAT(1X,'Block ',3I3,
     &' :  Implicit pressure specified water injection well #',I5)
 3017 FORMAT(1X,'Block ',3I3,
     &' :  Implicit pressure specified gas injection well #',I5)

      RETURN
      END
